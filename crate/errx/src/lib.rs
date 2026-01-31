use std::fmt::{Debug, Display};

use serde::{Deserialize, Serialize};
use tracing_provider::error_context::ErrorContextGuard;

mod external;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Trace {
    id: String,
}

impl Trace {
    fn new() -> Self {
        let id = uuid::Uuid::now_v7().simple().to_string();
        Self { id }
    }

    fn from_current_span() -> Option<Self> {
        let trace_id = tracing_provider::subscriber::current_span_field("trace_id")
            .or_else(|| tracing_provider::subscriber::current_span_field("error_trace"));

        trace_id
            .map(Self::normalize_trace_id)
            .and_then(|value| Self::try_from(value).ok())
    }

    fn normalize_trace_id(value: String) -> String {
        if value.len() >= 2 && value.starts_with('"') && value.ends_with('"') {
            value[1..value.len() - 1].to_string()
        } else {
            value
        }
    }
}

impl Display for Trace {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(f, "{}", self.id)
    }
}

impl TryFrom<String> for Trace {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        if value.len() != 32 {
            return Err(format!("Invalid length, expected 32 got {}", value.len()));
        }

        Ok(Trace { id: value })
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ErrorLocation {
    file: String,
    line: u32,
    col: u32,
}

impl ErrorLocation {
    pub fn from_std(value: &'static std::panic::Location<'static>) -> Self {
        Self {
            file: value.file().to_owned(),
            line: value.line(),
            col: value.column(),
        }
    }
}

impl Display for ErrorLocation {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line, self.col)
    }
}

#[derive(Debug, Clone, thiserror::Error, Serialize, Deserialize)]
#[error("trace: {trace}, stack: {stack:?}, message: {message}")]
pub struct Error {
    trace: Trace,
    message: String,
    stack: Vec<ErrorLocation>,
}

impl axum::response::IntoResponse for Error {
    fn into_response(self) -> axum::response::Response {
        self.capture();
        let trace_id = self.trace.to_string();
        let mut headers = axum::http::HeaderMap::new();
        headers.insert("errx_error", axum::http::HeaderValue::from_static("true"));
        if let Ok(trace_header) = axum::http::HeaderValue::from_str(&trace_id) {
            headers.insert("x-error-trace", trace_header);
        }
        (
            axum::http::StatusCode::INTERNAL_SERVER_ERROR,
            headers,
            axum::Json(self),
        )
            .into_response()
    }
}

// TODO Figure out if we can use #![feature(specialization)] for
// converting values into Error and Error into Error while pushing new location
// to the stack
impl Error {
    /// Returns a reference to the error's unique trace identifier.
    pub fn trace(&self) -> &Trace {
        &self.trace
    }

    /// Returns the trace ID as a string for use in logging and headers.
    pub fn trace_id(&self) -> String {
        self.trace.to_string()
    }

    /// Logs the error with structured fields for Axiom correlation.
    /// Emits: error_trace, error_message, error_stack fields.
    pub fn capture(&self) {
        let stack = self
            .stack
            .iter()
            .map(|loc| loc.to_string())
            .collect::<Vec<_>>();
        tracing::error!(
            error_trace = %self.trace,
            error_message = %self.message,
            error_stack = ?stack,
            "Error captured"
        );
    }

    /// Attaches this error's trace ID to the current tracing span.
    /// All subsequent logs within this span (and child spans) will include the error_trace.
    pub fn attach_to_span(&self) {
        tracing::Span::current().record("error_trace", self.trace.to_string());
    }

    /// Enters an error context that propagates across async task boundaries.
    /// Returns a guard that clears the context when dropped.
    pub fn enter_context(&self) -> ErrorContextGuard {
        tracing_provider::error_context::enter_error_context(self.trace.to_string())
    }

    #[track_caller]
    pub fn new_traced<E>(err: E) -> Self
    where
        E: Debug,
    {
        let err = Self {
            trace: Trace::from_current_span().unwrap_or_else(Trace::new),
            message: format!("{err:?}"),
            stack: vec![ErrorLocation::from_std(std::panic::Location::caller())],
        };
        err.capture();
        err.attach_to_span();
        err
    }

    #[track_caller]
    pub fn trace_stack(mut self) -> Self {
        self.stack
            .push(ErrorLocation::from_std(std::panic::Location::caller()));
        self
    }

    pub fn push_stack_prefix(
        &mut self,
        value: &str,
    ) {
        for loc in &mut self.stack {
            if !loc.file.starts_with("[") {
                loc.file = format!("[{value}] {}", loc.file)
            }
        }
    }
}

pub trait ErrorCapture<T> {
    #[track_caller]
    fn trace_err(self) -> Result<T, Error>;
}

impl<T, E> ErrorCapture<T> for Result<T, E>
where
    E: Debug + 'static,
{
    #[track_caller]
    fn trace_err(self) -> Result<T, Error> {
        match self {
            Ok(val) => Ok(val),
            Err(err) => {
                #[cfg(feature = "no_unsafe_opt")]
                {
                    if let Some(err) = (&err as &dyn std::any::Any).downcast_ref::<Error>() {
                        Err(err.clone().trace_stack())
                    } else {
                        Err(Error::new_traced(err))
                    }
                }
                #[cfg(not(feature = "no_unsafe_opt"))]
                {
                    if std::any::TypeId::of::<E>() == std::any::TypeId::of::<Error>() {
                        // SAFETY: The TypeId guard ensures E is exactly Error.
                        // ManuallyDrop prevents double-drop of the original E.
                        // ptr::read moves the Error without cloning.
                        let err = std::mem::ManuallyDrop::new(err);
                        let err = unsafe { std::ptr::read(&*err as *const E as *const Error) };
                        Err(err.trace_stack())
                    } else {
                        Err(Error::new_traced(err))
                    }
                }
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Once;
    use tracing_provider::subscriber::{Config, FileLoggingConfig};

    fn init_tracing() {
        static INIT: Once = Once::new();
        INIT.call_once(|| {
            let config = Config {
                service_name: "errx-tests".to_string(),
                axiom: None,
                file_logging: Some(FileLoggingConfig {
                    enabled: false,
                    directory: ".log".to_string(),
                }),
            };
            tracing_provider::subscriber::init_with_config(config);
        });
    }

    #[test]
    fn trace_err_wraps_non_errx_error() {
        let res: Result<(), &str> = Err("boom");
        let err = res.trace_err().expect_err("expected error");

        assert_eq!(err.message, "\"boom\"");
        assert_eq!(err.stack.len(), 1);
        assert!(err.stack[0].file.ends_with("crate/errx/src/lib.rs"));
    }

    #[test]
    fn trace_err_appends_stack_for_errx_error() {
        let err = Error::new_traced("boom");
        assert_eq!(err.stack.len(), 1);

        let res: Result<(), Error> = Err(err);
        let err = res.trace_err().expect_err("expected error");

        assert_eq!(err.stack.len(), 2);
        assert!(err.stack[1].file.ends_with("crate/errx/src/lib.rs"));
    }

    #[test]
    fn new_traced_uses_span_trace_id() {
        init_tracing();
        let trace_id = "0123456789abcdef0123456789abcdef".to_string();
        let span = tracing::info_span!("request", trace_id = %trace_id);
        let _enter = span.enter();

        let err = Error::new_traced("boom");
        assert_eq!(err.trace_id(), trace_id);
    }

    #[test]
    fn new_traced_falls_back_for_invalid_span_trace_id() {
        init_tracing();
        let trace_id = "short".to_string();
        let span = tracing::info_span!("request", trace_id = %trace_id);
        let _enter = span.enter();

        let err = Error::new_traced("boom");
        assert_ne!(err.trace_id(), trace_id);
        assert_eq!(err.trace_id().len(), 32);
    }

    #[test]
    fn new_traced_generates_trace_id_without_span() {
        init_tracing();
        let err = Error::new_traced("boom");
        assert_eq!(err.trace_id().len(), 32);
    }
}
