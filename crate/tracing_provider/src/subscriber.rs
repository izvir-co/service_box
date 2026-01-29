use chrono::Utc;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;
use std::fs::{self, File, OpenOptions};
use std::io::Write;
use std::path::PathBuf;
use std::sync::{Arc, Mutex, OnceLock};
use tokio::sync::{mpsc, oneshot};
use tracing::field::{Field, Visit};
use tracing::span::{Attributes, Id};
use tracing::{Event, Subscriber};
use tracing_appender::non_blocking::WorkerGuard;
use tracing_subscriber::filter::LevelFilter;
use tracing_subscriber::fmt::MakeWriter;
use tracing_subscriber::layer::{Context, SubscriberExt};
use tracing_subscriber::registry::LookupSpan;
use tracing_subscriber::util::SubscriberInitExt;
use tracing_subscriber::{EnvFilter, Layer};

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct Config {
    pub service_name: String,
    pub axiom: Option<AxiomConfig>,
    pub file_logging: Option<FileLoggingConfig>,
}

impl Config {
    fn load_opt() -> Option<Self> {
        cfg::load(".config/tracing.toml")
            .unwrap_or_else(|err| panic!("Failed to load tracing config: {err:?}"))
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AxiomConfig {
    pub url: String,
    pub token: String,
    pub dataset: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FileLoggingConfig {
    #[serde(default = "default_true")]
    pub enabled: bool,
    #[serde(default = "default_log_directory")]
    pub directory: String,
}

impl Default for FileLoggingConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            directory: default_log_directory(),
        }
    }
}

fn default_true() -> bool {
    true
}

fn default_log_directory() -> String {
    "./logs".to_string()
}

const MAX_LOG_SIZE_BYTES: u64 = 100 * 1024 * 1024; // 100 MB
const AXIOM_EVENT_CHANNEL_CAPACITY: usize = 10_000;

/// A writer that rotates log files when they exceed MAX_LOG_SIZE_BYTES.
/// When rotation occurs, the current file is renamed with a timestamp suffix.
struct RotatingFileWriter {
    directory: PathBuf,
    service_name: String,
    current_file: Mutex<Option<File>>,
}

impl RotatingFileWriter {
    fn new(
        directory: PathBuf,
        service_name: String,
    ) -> std::io::Result<Self> {
        fs::create_dir_all(&directory)?;
        let writer = Self {
            directory,
            service_name,
            current_file: Mutex::new(None),
        };
        // Initialize the file
        let _ = writer.get_or_create_file()?;
        Ok(writer)
    }

    fn log_file_path(&self) -> PathBuf {
        self.directory.join(format!("{}.log", self.service_name))
    }

    fn get_or_create_file(&self) -> std::io::Result<File> {
        let path = self.log_file_path();
        OpenOptions::new().create(true).append(true).open(&path)
    }

    fn rotate_if_needed(&self) -> std::io::Result<()> {
        let path = self.log_file_path();
        if path.exists() {
            let metadata = fs::metadata(&path)?;
            if metadata.len() >= MAX_LOG_SIZE_BYTES {
                let timestamp = Utc::now().format("%Y-%m-%d_%H-%M-%S");
                let rotated_name = format!("{}.{}.log", self.service_name, timestamp);
                let rotated_path = self.directory.join(rotated_name);

                {
                    let mut guard = self.current_file.lock().unwrap();
                    *guard = None;
                }

                fs::rename(&path, &rotated_path)?;
            }
        }
        Ok(())
    }
}

impl Write for &RotatingFileWriter {
    fn write(
        &mut self,
        buf: &[u8],
    ) -> std::io::Result<usize> {
        self.rotate_if_needed()?;

        let mut guard = self.current_file.lock().unwrap();
        if guard.is_none() {
            *guard = Some(self.get_or_create_file()?);
        }

        if let Some(ref mut file) = *guard {
            file.write(buf)
        } else {
            Err(std::io::Error::other("Failed to get log file"))
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        let mut guard = self.current_file.lock().unwrap();
        if let Some(ref mut file) = *guard {
            file.flush()
        } else {
            Ok(())
        }
    }
}

#[derive(Clone)]
struct RotatingFileWriterMaker {
    inner: Arc<RotatingFileWriter>,
}

impl<'a> MakeWriter<'a> for RotatingFileWriterMaker {
    type Writer = RotatingFileWriterRef;

    fn make_writer(&'a self) -> Self::Writer {
        RotatingFileWriterRef {
            inner: Arc::clone(&self.inner),
        }
    }
}

struct RotatingFileWriterRef {
    inner: Arc<RotatingFileWriter>,
}

impl Write for RotatingFileWriterRef {
    fn write(
        &mut self,
        buf: &[u8],
    ) -> std::io::Result<usize> {
        (&*self.inner).write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        (&*self.inner).flush()
    }
}

static SHUTDOWN_TX: OnceLock<mpsc::Sender<()>> = OnceLock::new();
static SHUTDOWN_COMPLETE_RX: OnceLock<tokio::sync::Mutex<Option<oneshot::Receiver<()>>>> =
    OnceLock::new();
static FILE_WRITER_GUARD: OnceLock<WorkerGuard> = OnceLock::new();

#[derive(Debug, Serialize)]
struct LogEvent {
    timestamp: String,
    level: String,
    target: String,
    message: String,
    service: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    error_trace: Option<String>,
    #[serde(flatten)]
    fields: HashMap<String, Value>,
}

#[derive(Debug, Default, Clone)]
struct SpanFields {
    name: String,
    fields: HashMap<String, Value>,
    error_trace: Option<String>,
}

#[derive(Debug, Default, Clone)]
struct SpanFieldLayer;

impl<S> Layer<S> for SpanFieldLayer
where
    S: Subscriber + for<'a> LookupSpan<'a>,
{
    fn on_new_span(
        &self,
        attrs: &Attributes<'_>,
        id: &Id,
        ctx: Context<'_, S>,
    ) {
        let span = ctx.span(id).expect("Span not found");

        let mut visitor = FieldVisitor::new();
        attrs.record(&mut visitor);

        let mut error_trace = visitor.fields.remove("error_trace").and_then(|v| match v {
            Value::String(s) => Some(s),
            _ => None,
        });

        if error_trace.is_none()
            && let Some(parent) = span.parent()
        {
            let parent_extensions = parent.extensions();
            if let Some(parent_fields) = parent_extensions.get::<SpanFields>() {
                error_trace = parent_fields.error_trace.clone();
            }
        }

        let span_fields = SpanFields {
            name: span.name().to_string(),
            fields: visitor.fields,
            error_trace,
        };

        let mut extensions = span.extensions_mut();
        if let Some(existing) = extensions.get_mut::<SpanFields>() {
            if existing.error_trace.is_none() {
                existing.error_trace = span_fields.error_trace;
            }
            existing.fields.extend(span_fields.fields);
            existing.name = span_fields.name;
        } else {
            extensions.insert(span_fields);
        }
    }

    fn on_record(
        &self,
        id: &Id,
        values: &tracing::span::Record<'_>,
        ctx: Context<'_, S>,
    ) {
        let span = ctx.span(id).expect("Span not found");
        let mut extensions = span.extensions_mut();

        if let Some(span_fields) = extensions.get_mut::<SpanFields>() {
            let mut visitor = FieldVisitor::new();
            values.record(&mut visitor);

            if let Some(Value::String(trace)) = visitor.fields.remove("error_trace") {
                span_fields.error_trace = Some(trace);
            }

            span_fields.fields.extend(visitor.fields);
        }
    }
}

struct AxiomLayer {
    tx: mpsc::Sender<LogEvent>,
    service_name: String,
}

impl AxiomLayer {
    fn new(
        service_name: String,
        config: AxiomConfig,
    ) -> Self {
        let (event_tx, event_rx) = mpsc::channel::<LogEvent>(AXIOM_EVENT_CHANNEL_CAPACITY);
        let (shutdown_tx, shutdown_rx) = mpsc::channel::<()>(1);
        let (complete_tx, complete_rx) = oneshot::channel::<()>();

        let _ = SHUTDOWN_TX.set(shutdown_tx);
        let _ = SHUTDOWN_COMPLETE_RX.set(tokio::sync::Mutex::new(Some(complete_rx)));

        let axiom_url = config.url.trim_end_matches('/').to_string();
        let dataset = config.dataset;
        let token = config.token;

        std::thread::spawn(move || {
            let rt = tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .expect("Failed to create tokio runtime for axiom layer");

            rt.block_on(background_sender(
                event_rx,
                shutdown_rx,
                complete_tx,
                axiom_url,
                dataset,
                token,
            ));
        });

        Self {
            tx: event_tx,
            service_name,
        }
    }
}

pub fn current_span_field(field: &str) -> Option<String> {
    tracing::Span::current()
        .with_subscriber(|(id, dispatch)| {
            let registry = dispatch.downcast_ref::<tracing_subscriber::Registry>()?;
            let span_ref = registry.span(id)?;
            let extensions = span_ref.extensions();
            let span_fields = extensions.get::<SpanFields>()?;

            if field == "error_trace" {
                return span_fields.error_trace.clone();
            }

            match span_fields.fields.get(field) {
                Some(Value::String(value)) => Some(value.clone()),
                Some(Value::Number(value)) => Some(value.to_string()),
                Some(Value::Bool(value)) => Some(value.to_string()),
                _ => None,
            }
        })
        .flatten()
}

struct FieldVisitor {
    fields: HashMap<String, Value>,
    message: Option<String>,
}

impl FieldVisitor {
    fn new() -> Self {
        Self {
            fields: HashMap::new(),
            message: None,
        }
    }

    fn into_parts(self) -> (Option<String>, HashMap<String, Value>) {
        (self.message, self.fields)
    }
}

impl Visit for FieldVisitor {
    fn record_debug(
        &mut self,
        field: &Field,
        value: &dyn std::fmt::Debug,
    ) {
        let value_str = format!("{:?}", value);
        if field.name() == "message" {
            self.message = Some(value_str);
        } else {
            self.fields
                .insert(field.name().to_string(), Value::String(value_str));
        }
    }

    fn record_str(
        &mut self,
        field: &Field,
        value: &str,
    ) {
        if field.name() == "message" {
            self.message = Some(value.to_string());
        } else {
            self.fields
                .insert(field.name().to_string(), Value::String(value.to_string()));
        }
    }

    fn record_i64(
        &mut self,
        field: &Field,
        value: i64,
    ) {
        self.fields
            .insert(field.name().to_string(), Value::Number(value.into()));
    }

    fn record_u64(
        &mut self,
        field: &Field,
        value: u64,
    ) {
        self.fields
            .insert(field.name().to_string(), Value::Number(value.into()));
    }

    fn record_bool(
        &mut self,
        field: &Field,
        value: bool,
    ) {
        self.fields
            .insert(field.name().to_string(), Value::Bool(value));
    }

    fn record_f64(
        &mut self,
        field: &Field,
        value: f64,
    ) {
        if let Some(n) = serde_json::Number::from_f64(value) {
            self.fields
                .insert(field.name().to_string(), Value::Number(n));
        } else {
            self.fields
                .insert(field.name().to_string(), Value::String(value.to_string()));
        }
    }
}

impl<S> Layer<S> for AxiomLayer
where
    S: Subscriber + for<'a> LookupSpan<'a>,
{
    fn on_new_span(
        &self,
        attrs: &Attributes<'_>,
        id: &Id,
        ctx: Context<'_, S>,
    ) {
        let span = ctx.span(id).expect("Span not found");

        let mut visitor = FieldVisitor::new();
        attrs.record(&mut visitor);

        let mut error_trace = visitor.fields.remove("error_trace").and_then(|v| match v {
            Value::String(s) => Some(s),
            _ => None,
        });

        // If not in attributes, inherit from parent span
        if error_trace.is_none()
            && let Some(parent) = span.parent()
        {
            let parent_extensions = parent.extensions();
            if let Some(parent_fields) = parent_extensions.get::<SpanFields>() {
                error_trace = parent_fields.error_trace.clone();
            }
        }

        let span_fields = SpanFields {
            name: span.name().to_string(),
            fields: visitor.fields,
            error_trace,
        };

        let mut extensions = span.extensions_mut();
        if let Some(existing) = extensions.get_mut::<SpanFields>() {
            if existing.error_trace.is_none() {
                existing.error_trace = span_fields.error_trace;
            }
            existing.fields.extend(span_fields.fields);
            existing.name = span_fields.name;
        } else {
            extensions.insert(span_fields);
        }
    }

    fn on_record(
        &self,
        id: &Id,
        values: &tracing::span::Record<'_>,
        ctx: Context<'_, S>,
    ) {
        let span = ctx.span(id).expect("Span not found");
        let mut extensions = span.extensions_mut();

        if let Some(span_fields) = extensions.get_mut::<SpanFields>() {
            let mut visitor = FieldVisitor::new();
            values.record(&mut visitor);

            if let Some(Value::String(trace)) = visitor.fields.remove("error_trace") {
                span_fields.error_trace = Some(trace);
            }

            span_fields.fields.extend(visitor.fields);
        }
    }

    fn on_event(
        &self,
        event: &Event<'_>,
        ctx: Context<'_, S>,
    ) {
        let mut visitor = FieldVisitor::new();
        event.record(&mut visitor);
        let (message, mut fields) = visitor.into_parts();

        let mut error_trace: Option<String> = fields.remove("error_trace").and_then(|v| match v {
            Value::String(s) => Some(s),
            _ => None,
        });

        // Collect span context - walk up the span tree (innermost to outermost)
        // Insert each span as a nested object: { "span_name": { field: value, ... } }
        // Also look for error_trace in span context
        if let Some(scope) = ctx.event_scope(event) {
            for span in scope {
                let extensions = span.extensions();
                if let Some(span_fields) = extensions.get::<SpanFields>() {
                    if error_trace.is_none()
                        && let Some(ref trace) = span_fields.error_trace
                    {
                        error_trace = Some(trace.clone());
                    }

                    if !span_fields.fields.is_empty() {
                        let span_obj = Value::Object(
                            span_fields
                                .fields
                                .iter()
                                .map(|(k, v)| (k.clone(), v.clone()))
                                .collect(),
                        );
                        fields.entry(span_fields.name.clone()).or_insert(span_obj);
                    }
                }
            }
        }

        // Fallback: check task-local storage for error context (for spawned tasks)
        if error_trace.is_none() {
            error_trace = crate::error_context::current_error_trace();
        }

        let metadata = event.metadata();
        let log_event = LogEvent {
            timestamp: Utc::now().to_rfc3339(),
            level: metadata.level().to_string(),
            target: metadata.target().to_string(),
            message: message.unwrap_or_default(),
            service: self.service_name.clone(),
            error_trace,
            fields,
        };

        let _ = self.tx.try_send(log_event);
    }
}

async fn background_sender(
    mut event_rx: mpsc::Receiver<LogEvent>,
    mut shutdown_rx: mpsc::Receiver<()>,
    complete_tx: oneshot::Sender<()>,
    axiom_url: String,
    dataset: String,
    token: String,
) {
    let client = reqwest::Client::new();
    let url = format!("{}/v1/datasets/{}/ingest", axiom_url, dataset);

    let mut batch: Vec<LogEvent> = Vec::with_capacity(100);
    let mut interval = tokio::time::interval(std::time::Duration::from_secs(2));
    interval.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Skip);

    loop {
        tokio::select! {
            event = event_rx.recv() => {
                match event {
                    Some(e) => {
                        batch.push(e);
                        if batch.len() >= 100 {
                            flush_batch(&client, &url, &token, &mut batch).await;
                        }
                    }
                    None => {
                        // Channel closed, flush and exit
                        flush_batch(&client, &url, &token, &mut batch).await;
                        break;
                    }
                }
            }
            // Periodic flush
            _ = interval.tick() => {
                if !batch.is_empty() {
                    flush_batch(&client, &url, &token, &mut batch).await;
                }
            }
            // Shutdown signal
            _ = shutdown_rx.recv() => {
                while let Ok(e) = event_rx.try_recv() {
                    batch.push(e);
                }
                flush_batch(&client, &url, &token, &mut batch).await;
                break;
            }
        }
    }

    // Signal completion
    let _ = complete_tx.send(());
}

async fn flush_batch(
    client: &reqwest::Client,
    url: &str,
    token: &str,
    batch: &mut Vec<LogEvent>,
) {
    if batch.is_empty() {
        return;
    }

    // Build NDJSON body
    let mut body = String::new();
    for event in batch.iter() {
        if let Ok(json) = serde_json::to_string(event) {
            body.push_str(&json);
            body.push('\n');
        }
    }

    let result = client
        .post(url)
        .header("Authorization", format!("Bearer {}", token))
        .header("Content-Type", "application/x-ndjson")
        .body(body)
        .send()
        .await;

    match result {
        Ok(response) => {
            if !response.status().is_success() {
                let status = response.status();
                let body = response.text().await.unwrap_or_default();
                eprintln!(
                    "[axiom-layer] Failed to send logs: HTTP {} - {}",
                    status, body
                );
            }
        },
        Err(e) => {
            eprintln!("[axiom-layer] Failed to send logs: {}", e);
        },
    }

    batch.clear();
}

/// Creates the file logging layer if enabled.
fn create_file_layer<S>(
    service_name: &str,
    file_config: Option<&FileLoggingConfig>,
) -> Option<Box<dyn Layer<S> + Send + Sync + 'static>>
where
    S: Subscriber + for<'a> LookupSpan<'a>,
{
    let config = file_config
        .cloned()
        .unwrap_or_else(FileLoggingConfig::default);

    if !config.enabled {
        return None;
    }

    let directory = PathBuf::from(&config.directory);
    let writer = match RotatingFileWriter::new(directory, service_name.to_string()) {
        Ok(w) => w,
        Err(e) => {
            eprintln!("[file-logging] Failed to create log file writer: {}", e);
            return None;
        },
    };

    let writer_maker = RotatingFileWriterMaker {
        inner: Arc::new(writer),
    };

    let (non_blocking, guard) = tracing_appender::non_blocking(writer_maker.make_writer());
    let _ = FILE_WRITER_GUARD.set(guard);

    Some(Box::new(
        tracing_subscriber::fmt::layer()
            .json()
            .with_writer(non_blocking),
    ))
}

/// Tries to initialize global tracing subscriber based on env filters, if none are present it
/// defaults to INFO level. In case another global subscriber is already initialized nothing will
/// happen. To modify log levels and scopes modify the RUST_LOG env. var.
pub fn init() {
    if let Some(config) = Config::load_opt() {
        init_with_config(config);
        return;
    }

    // Default: console output only, file logging enabled with defaults
    let env_filter_layer: EnvFilter = EnvFilter::builder()
        .with_default_directive(LevelFilter::INFO.into())
        .from_env_lossy();
    let fmt_layer = tracing_subscriber::fmt::layer();

    let base = tracing_subscriber::registry()
        .with(SpanFieldLayer)
        .with(env_filter_layer)
        .with(fmt_layer);

    let file_layer: Option<Box<dyn Layer<_> + Send + Sync>> = create_file_layer("app", None);

    let subscriber = base.with(file_layer);
    let _ = subscriber.try_init();
}

/// Initialize tracing with a provided configuration.
/// Useful for testing or when config is loaded from a different source.
pub fn init_with_config(config: Config) {
    let env_filter_layer: EnvFilter = EnvFilter::builder()
        .with_default_directive(LevelFilter::INFO.into())
        .from_env_lossy();
    let fmt_layer = tracing_subscriber::fmt::layer();

    let base = tracing_subscriber::registry()
        .with(SpanFieldLayer)
        .with(env_filter_layer)
        .with(fmt_layer);

    let file_layer: Option<Box<dyn Layer<_> + Send + Sync>> =
        create_file_layer(&config.service_name, config.file_logging.as_ref());

    let axiom_layer = config
        .axiom
        .map(|axiom_config| AxiomLayer::new(config.service_name.clone(), axiom_config));

    let subscriber = base.with(file_layer).with(axiom_layer);
    let _ = subscriber.try_init();

    if SHUTDOWN_TX.get().is_some() {
        tracing::info!("Exporting logs using axiom layer");
    }
}

/// Signals the background sender to flush remaining events and shut down.
/// This should be called before the application exits to ensure all logs are sent.
/// Waits for the flush to complete before returning.
pub async fn shutdown_flush() {
    // Send shutdown signal
    if let Some(tx) = SHUTDOWN_TX.get() {
        let _ = tx.send(()).await;
    }

    // Wait for completion
    if let Some(mutex) = SHUTDOWN_COMPLETE_RX.get()
        && let Some(rx) = mutex.lock().await.take()
    {
        let _ = rx.await;
    }
}
