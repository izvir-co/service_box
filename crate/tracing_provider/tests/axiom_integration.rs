//! Integration test for Axiom log ingestion.
//!
//! This test requires the following environment variables to be set:
//! - AXIOM_TOKEN: Your Axiom API token
//! - AXIOM_DATASET: The dataset to ingest logs into
//! - AXIOM_URL: (Optional) The Axiom API URL, defaults to https://api.axiom.co
//!
//! Run with:
//! ```sh
//! AXIOM_TOKEN=your-token AXIOM_DATASET=your-dataset cargo test -p tracing_provider --test axiom_integration -- --nocapture
//! ```

use std::env;
use tracing::{debug, error, info, warn};
use tracing_provider::subscriber::{init_with_config, shutdown_flush, AxiomConfig, Config};

/// Integration test that sends logs to Axiom.
/// Skipped if AXIOM_TOKEN is not set.
#[tokio::test]
async fn test_axiom_ingest() {
    let token = match env::var("AXIOM_TOKEN") {
        Ok(t) => t,
        Err(_) => {
            eprintln!("Skipping test: AXIOM_TOKEN not set");
            return;
        },
    };

    let dataset = match env::var("AXIOM_DATASET") {
        Ok(d) => d,
        Err(_) => {
            eprintln!("Skipping test: AXIOM_DATASET not set");
            return;
        },
    };

    let url = env::var("AXIOM_URL").unwrap_or_else(|_| "https://api.axiom.co".to_string());

    let config = Config {
        service_name: "tracing-provider-integration-test".to_string(),
        axiom: Some(AxiomConfig {
            url,
            token,
            dataset,
        }),
        file_logging: None, // Use defaults (enabled, ./logs directory)
    };

    // Initialize tracing with Axiom
    init_with_config(config);

    // Generate a unique test ID to find these logs in Axiom
    let test_id = format!(
        "test-{}",
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_millis()
    );

    // Send various log levels
    info!(test_id = %test_id, "INFO level test message");
    warn!(test_id = %test_id, "WARN level test message");
    error!(test_id = %test_id, "ERROR level test message");
    debug!(test_id = %test_id, "DEBUG level test message (may not appear if RUST_LOG filters it)");

    // Test structured fields
    info!(
        test_id = %test_id,
        user_id = 12345,
        request_path = "/api/test",
        duration_ms = 42.5,
        success = true,
        "Structured log with various field types"
    );

    // Flush to ensure logs are sent
    shutdown_flush().await;

    eprintln!();
    eprintln!("=== Test completed ===");
    eprintln!(
        "Check your Axiom dashboard for logs with test_id: {}",
        test_id
    );
    eprintln!("Query: test_id == \"{}\"", test_id);
    eprintln!();
}
