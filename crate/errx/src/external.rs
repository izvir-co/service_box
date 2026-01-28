#[cfg(feature = "pg")]
impl From<tokio_postgres::Error> for crate::Error {
    #[track_caller]
    fn from(value: tokio_postgres::Error) -> Self {
        crate::Error::new_traced(value)
    }
}

#[cfg(feature = "pg")]
impl From<pg::Error> for crate::Error {
    #[track_caller]
    fn from(value: pg::Error) -> Self {
        crate::Error::new_traced(value)
    }
}

#[cfg(feature = "sf")]
impl From<sf::Error> for crate::Error {
    #[track_caller]
    fn from(value: sf::Error) -> Self {
        crate::Error::new_traced(value)
    }
}
