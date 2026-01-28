use std::{
    io::ErrorKind,
    path::{Path, PathBuf},
};

use serde::de::DeserializeOwned;

pub fn load<T, P>(path: P) -> Result<Option<T>, Error>
where
    T: DeserializeOwned,
    P: AsRef<Path>,
{
    match std::fs::read_to_string(path.as_ref()) {
        Ok(content) => toml::from_str(&content).map_err(|err| Error::Parse {
            err,
            path: path.as_ref().to_path_buf(),
        }),
        Err(err) if err.kind() == ErrorKind::NotFound => Ok(None),
        Err(err) => Err(Error::Read {
            err,
            path: path.as_ref().to_path_buf(),
        }),
    }
}

#[track_caller]
pub fn load_or_panic<T, P>(path: P) -> T
where
    T: DeserializeOwned,
    P: AsRef<Path>,
{
    load(&path)
        .unwrap_or_else(|err| panic!("{err}"))
        .unwrap_or_else(|| {
            let path = path.as_ref();
            let abs: PathBuf = if path.is_absolute() {
                path.to_path_buf()
            } else {
                std::env::current_dir()
                    .expect("Failed to get current_dir")
                    .join(path)
            };
            panic!("Missing `{:?}` config file", abs.display())
        })
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("I/O error reading `{path:?}`: {err}")]
    Read {
        #[source]
        err: std::io::Error,
        path: PathBuf,
    },
    #[error("Parse error in `{path:?}`: {err}")]
    Parse {
        #[source]
        err: toml::de::Error,
        path: PathBuf,
    },
}
