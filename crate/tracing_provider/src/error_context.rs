//! Task-local error context propagation for tracing integration.
//!
//! This module provides mechanisms to propagate error trace IDs across async task
//! boundaries, ensuring that spawned tasks can inherit the error context from their
//! parent task.
//!
//! This is used by the Axiom tracing layer to inject `error_trace` into logs,
//! enabling correlation of all logs related to a specific error.

use std::cell::RefCell;
use std::future::Future;
use tokio::task::JoinHandle;

tokio::task_local! {
    /// Task-local storage for the current error trace ID.
    static ERROR_TRACE: RefCell<Option<String>>;
}

/// Guard that clears the error context when dropped.
pub struct ErrorContextGuard {
    _private: (),
}

impl Drop for ErrorContextGuard {
    fn drop(&mut self) {
        // Clear the task-local error trace when guard is dropped
        let _ = ERROR_TRACE.try_with(|cell| {
            cell.borrow_mut().take();
        });
    }
}

/// Enters an error context, storing the trace ID in task-local storage.
/// Returns a guard that clears the context when dropped.
pub fn enter_error_context(trace_id: String) -> ErrorContextGuard {
    let _ = ERROR_TRACE.try_with(|cell| {
        *cell.borrow_mut() = Some(trace_id);
    });
    ErrorContextGuard { _private: () }
}

/// Returns the current error trace ID from task-local storage, if any.
/// This is used by the tracing subscriber to inject error_trace into logs.
pub fn current_error_trace() -> Option<String> {
    ERROR_TRACE
        .try_with(|cell| cell.borrow().clone())
        .ok()
        .flatten()
}

/// Spawns a new async task that inherits the current error context.
///
/// Use this instead of `tokio::spawn` when you want the spawned task's logs
/// to include the parent's error_trace.
///
/// # Example
///
/// ```rust,ignore
/// use tracing_provider::error_context::spawn_with_error_context;
///
/// // Inside an error context...
/// spawn_with_error_context(async {
///     tracing::info!("background work"); // Will have error_trace
/// });
/// ```
pub fn spawn_with_error_context<F, T>(future: F) -> JoinHandle<T>
where
    F: Future<Output = T> + Send + 'static,
    T: Send + 'static,
{
    let ctx = current_error_trace();
    tokio::spawn(async move { ERROR_TRACE.scope(RefCell::new(ctx), future).await })
}

/// Runs a future with a specific error context.
/// Useful for manually propagating error context without spawning.
pub async fn with_error_context<F, T>(
    trace_id: Option<String>,
    future: F,
) -> T
where
    F: Future<Output = T>,
{
    ERROR_TRACE.scope(RefCell::new(trace_id), future).await
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_error_context_propagation() {
        let trace_id = "trace_123_abc".to_string();

        with_error_context(Some(trace_id.clone()), async {
            assert_eq!(current_error_trace(), Some(trace_id.clone()));

            // Verify spawned task inherits context
            let handle = spawn_with_error_context(async { current_error_trace() });

            let result = handle.await.unwrap();
            assert_eq!(result, Some(trace_id));
        })
        .await;
    }

    #[tokio::test]
    async fn test_guard_clears_context() {
        with_error_context(None, async {
            {
                let _guard = enter_error_context("trace_456_def".to_string());
                assert!(current_error_trace().is_some());
            }
            // Guard dropped, context should be cleared
            assert!(current_error_trace().is_none());
        })
        .await;
    }
}
