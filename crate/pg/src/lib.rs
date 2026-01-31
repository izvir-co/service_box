use std::sync::OnceLock;

use deadpool_postgres::{Manager, ManagerConfig, Pool, RecyclingMethod, tokio_postgres};
use serde::Deserialize;
use thiserror::Error;

static POOL: OnceLock<Pool> = OnceLock::new();

#[derive(Clone, Deserialize)]
pub struct Config {
    pub url: String,
    pub max_connections: usize,
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("pg pool not initialized; call pg::init_pool(...) first")]
    NotInitialized,
    #[error("pg pool already initialized")]
    AlreadyInitialized,
    #[error("failed to parse url into tokio_postgres config: {0}")]
    InvalidUrl(String),
    #[error("failed to build connection pool for tokio_postgres: {0}")]
    BuildPool(String),
    #[error(transparent)]
    Pool(#[from] deadpool_postgres::PoolError),
}

fn build_pool(config: &Config) -> Result<Pool, Error> {
    let pg_config: tokio_postgres::Config = config
        .url
        .parse::<tokio_postgres::Config>()
        .map_err(|err| Error::InvalidUrl(err.to_string()))?;
    let pool_manager_config = ManagerConfig {
        recycling_method: RecyclingMethod::Fast,
    };
    // TODO: Support optional tls (currently not needed yet)
    let manager = Manager::from_config(pg_config, tokio_postgres::NoTls, pool_manager_config);

    Pool::builder(manager)
        .max_size(config.max_connections)
        .build()
        .map_err(|err| Error::BuildPool(err.to_string()))
}
pub type Client = deadpool_postgres::Object;
pub type Transaction<'a> = deadpool_postgres::Transaction<'a>;

/// Initialize the global connection pool.
pub fn init_pool(config: Config) -> Result<(), Error> {
    let pool = build_pool(&config)?;
    POOL.set(pool).map_err(|_| Error::AlreadyInitialized)
}

pub fn pool() -> Result<Pool, Error> {
    POOL.get().cloned().ok_or(Error::NotInitialized)
}

pub async fn client() -> Result<Client, Error> {
    let pool = pool()?;
    pool.get().await.map_err(Error::from)
}

/// This will always be just compiled away
/// The real use case for this is DX, you can write a treesitter injection
/// for your editor to get syntax highlighting, here is what works for Helix:
///
/// ```scheme
/// ; Highlight SQL in pg::sql_str() functions
/// (call_expression
///   function: (scoped_identifier
///     path: (identifier) @_pg (#eq? @_pg "pg")
///     name: (identifier) @_sql_str (#eq? @_sql_str "sql_str"))
///   arguments: (arguments (string_literal (string_content) @injection.content))
///   (#set! injection.language "sql"))
/// ```
pub const fn sql_str(query: &str) -> &str {
    query
}
