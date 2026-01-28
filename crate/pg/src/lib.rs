use std::{path::Path, sync::OnceLock};

use deadpool_postgres::{Manager, ManagerConfig, Pool, RecyclingMethod, tokio_postgres};
use serde::Deserialize;

#[derive(Deserialize)]
struct Config {
    url: String,
    max_connections: usize,
}

impl Config {
    fn load() -> Config {
        let path = Path::new(".config/pg.toml");
        cfg::load_or_panic(path)
    }
}

fn build_pool() -> Pool {
    let file_config = Config::load();
    let pg_config: tokio_postgres::Config = file_config
        .url
        .parse()
        .unwrap_or_else(|err| panic!("Failed to parse url into tokio_postgres config: {err}"));
    let pool_manager_config = ManagerConfig {
        recycling_method: RecyclingMethod::Fast,
    };
    // TODO: Support optional tls (currently not needed yet)
    let manager = Manager::from_config(pg_config, tokio_postgres::NoTls, pool_manager_config);

    Pool::builder(manager)
        .max_size(file_config.max_connections)
        .build()
        .unwrap_or_else(|err| panic!("Failed to build connection pool for tokio_postgres: {err}"))
}

pub type Error = deadpool_postgres::PoolError;
pub type Client = deadpool_postgres::Object;
pub type Transaction<'a> = deadpool_postgres::Transaction<'a>;

pub fn pool() -> Pool {
    static POOL: OnceLock<Pool> = OnceLock::new();
    POOL.get_or_init(build_pool).clone()
}

pub async fn client() -> Result<Client, Error> {
    pool().get().await
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
