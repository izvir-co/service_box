use axum::body::Body;
use axum::http::{HeaderName, Request};
use axum::response::Response;
use std::future::Future;
use std::pin::Pin;
use std::task::{Context, Poll};
use tower::{Layer, Service};
use tower_http::request_id::{
    MakeRequestId, PropagateRequestIdLayer, RequestId, SetRequestIdLayer,
};
use tracing::Instrument;
use uuid::Uuid;

pub const TRACE_ID_HEADER: HeaderName = HeaderName::from_static("x-trace-id");

pub fn propagate_request_id_layer() -> PropagateRequestIdLayer {
    PropagateRequestIdLayer::new(TRACE_ID_HEADER)
}

pub fn set_request_id_layer() -> SetRequestIdLayer<MakeRequestTraceId> {
    SetRequestIdLayer::new(TRACE_ID_HEADER, MakeRequestTraceId)
}

pub fn tracing_layer() -> TracingLayer {
    TracingLayer
}

pub async fn shutdown_signal() {
    tokio::signal::ctrl_c()
        .await
        .expect("failed to install CTRL+C handler");
    tracing::info!("Shutdown signal received");
}

#[derive(Clone, Copy, Default)]
pub struct MakeRequestTraceId;

impl MakeRequestId for MakeRequestTraceId {
    fn make_request_id<B>(
        &mut self,
        _request: &Request<B>,
    ) -> Option<RequestId> {
        let request_id = Uuid::now_v7().simple().to_string().parse().unwrap();
        tracing::info!("created req id {request_id:?}");
        Some(RequestId::new(request_id))
    }
}

#[derive(Clone, Copy, Default)]
pub struct TracingLayer;

impl<S> Layer<S> for TracingLayer {
    type Service = TracingService<S>;

    fn layer(
        &self,
        inner: S,
    ) -> Self::Service {
        TracingService { inner }
    }
}

#[derive(Clone)]
pub struct TracingService<S> {
    inner: S,
}

impl<S> Service<Request<Body>> for TracingService<S>
where
    S: Service<Request<Body>, Response = Response> + Clone + Send + 'static,
    S::Future: Send + 'static,
    S::Error: Send + 'static,
{
    type Response = Response;
    type Error = S::Error;
    type Future = Pin<Box<dyn Future<Output = Result<Self::Response, Self::Error>> + Send>>;

    fn poll_ready(
        &mut self,
        cx: &mut Context<'_>,
    ) -> Poll<Result<(), Self::Error>> {
        self.inner.poll_ready(cx)
    }

    fn call(
        &mut self,
        req: Request<Body>,
    ) -> Self::Future {
        let mut inner = self.inner.clone();
        let start = std::time::Instant::now();

        let trace_id = req
            .extensions()
            .get::<RequestId>()
            .and_then(|id| id.header_value().to_str().ok())
            .unwrap_or("-")
            .to_owned();

        let span = tracing::info_span!(
            "request",
            trace_id = %trace_id,
            method = %req.method(),
            uri = %req.uri(),
            status = tracing::field::Empty,
            duration_ms = tracing::field::Empty,
            error_trace = tracing::field::Empty,
        );

        Box::pin(async move {
            let response = inner.call(req).instrument(span.clone()).await?;

            let duration_ms = start.elapsed().as_secs_f64() * 1000.0;
            let status = response.status().as_u16();
            span.record("status", tracing::field::display(status));
            span.record("duration_ms", duration_ms);

            tracing::info!(parent: &span, "request_completed");

            Ok(response)
        })
    }
}
