use axum::body::Body;
use axum::http::{HeaderName, Request};
use axum::middleware::Next;
use axum::response::Response;
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

pub async fn tracing_middleware(
    req: Request<Body>,
    next: Next,
) -> Response {
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

    let mut response = next.run(req).instrument(span.clone()).await;
    if let Ok(trace_header) = axum::http::HeaderValue::from_str(&trace_id) {
        response.headers_mut().insert(TRACE_ID_HEADER, trace_header);
    }

    let duration_ms = start.elapsed().as_secs_f64() * 1000.0;
    let status = response.status().as_u16();
    span.record("status", tracing::field::display(status));
    span.record("duration_ms", duration_ms);

    tracing::info!(parent: &span, "request_completed");

    response
}
