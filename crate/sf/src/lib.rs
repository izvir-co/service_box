use std::{
    fmt::{self, Display},
    num::ParseIntError,
    str::FromStr,
    sync::OnceLock,
};

use serde::{de, Deserialize, Deserializer, Serialize, Serializer};

use arktype_ast::ArkType;

fn snowflake_generator() -> &'static Result<sonyflake::Sonyflake, sonyflake::Error> {
    static LOCK: OnceLock<Result<sonyflake::Sonyflake, sonyflake::Error>> = OnceLock::new();

    LOCK.get_or_init(|| sonyflake::Sonyflake::builder().finalize())
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Failed to initialize snowflake generator, {0}")]
    Generator(String),
    #[error("Failed to generate snowflake, {0}")]
    Next(#[source] sonyflake::Error),
}

// TODO This should be behind a feature
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, ArkType)]
#[arktype(type_override = "\"string\"")]
pub struct Snowflake(u64);

impl Snowflake {
    #[inline]
    pub fn wrap(id: u64) -> Self {
        Self(id)
    }

    pub fn new() -> Result<Self, Error> {
        snowflake_generator()
            .as_ref()
            .map_err(|err| Error::Generator(format!("{err:?}")))?
            .next_id()
            .map(Self::wrap)
            .map_err(Error::Next)
    }

    pub fn inner(self) -> u64 {
        self.0
    }
}

impl Display for Snowflake {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for Snowflake {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let integer = s.parse::<u64>()?;
        Ok(Self::wrap(integer))
    }
}

// TODO There is probably a way to have this active only for when serializing responses
// that will end up in external systems, but the efficiency of this change is not worth it currently.
// But my OCD will win at some point and I'll take care of this shit then...
impl Serialize for Snowflake {
    fn serialize<S>(
        &self,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        // Always serialize as a string to avoid JS bigint issues
        // I despise the fact that this is necessary, what a fucking joke of a language
        serializer.serialize_str(&self.0.to_string())
    }
}

/// Gotta love serde on how flexible implementing this shit is
impl<'de> Deserialize<'de> for Snowflake {
    fn deserialize<D>(deserializer: D) -> Result<Snowflake, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct SnowflakeVisitor;

        impl<'de> de::Visitor<'de> for SnowflakeVisitor {
            type Value = Snowflake;

            fn expecting(
                &self,
                f: &mut fmt::Formatter,
            ) -> fmt::Result {
                f.write_str("a snowflake as string or unsigned integer")
            }

            /// Allow deserialization unsigned integer for future use.
            fn visit_u64<E>(
                self,
                v: u64,
            ) -> Result<Self::Value, E> {
                Ok(Snowflake(v))
            }

            /// TODO: Is this one even needed, it's checked casted but still...
            fn visit_i64<E>(
                self,
                v: i64,
            ) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                if v < 0 {
                    Err(E::custom("negative value not allowed for Snowflake"))
                } else {
                    Ok(Snowflake(v as u64))
                }
            }

            fn visit_str<E>(
                self,
                v: &str,
            ) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                v.parse::<u64>()
                    .map(Snowflake)
                    .map_err(|_e| E::custom("invalid snowflake string"))
            }

            fn visit_string<E>(
                self,
                v: String,
            ) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                self.visit_str(&v)
            }
        }

        deserializer.deserialize_any(SnowflakeVisitor)
    }
}

#[cfg(feature = "pg")]
impl tokio_postgres::types::ToSql for Snowflake {
    fn to_sql(
        &self,
        ty: &tokio_postgres::types::Type,
        out: &mut tokio_postgres::types::private::BytesMut,
    ) -> Result<tokio_postgres::types::IsNull, Box<dyn std::error::Error + Sync + Send>>
    where
        Self: Sized,
    {
        if *ty == tokio_postgres::types::Type::INT8 {
            // TODO This will overflow in about 200 years, fix this sometime before that
            let value = self.0 as i64;
            <i64 as tokio_postgres::types::ToSql>::to_sql(&value, ty, out)
        } else {
            Err(format!("Type {} not supported for Snowflake", ty).into())
        }
    }

    fn accepts(ty: &tokio_postgres::types::Type) -> bool
    where
        Self: Sized,
    {
        *ty == tokio_postgres::types::Type::INT8
    }

    tokio_postgres::types::to_sql_checked!();
}

#[cfg(feature = "pg")]
impl<'a> tokio_postgres::types::FromSql<'a> for Snowflake {
    fn from_sql(
        ty: &tokio_postgres::types::Type,
        raw: &'a [u8],
    ) -> Result<Self, Box<dyn std::error::Error + Sync + Send>> {
        if *ty != tokio_postgres::types::Type::INT8 {
            return Err(format!("Expected BIGINT, got {}", ty).into());
        }

        let pg_value = <i64 as tokio_postgres::types::FromSql>::from_sql(ty, raw)?;

        if pg_value < 0 {
            return Err("Snowflake values cannot be negative".into());
        }

        Ok(Snowflake(pg_value as u64))
    }

    fn accepts(ty: &tokio_postgres::types::Type) -> bool {
        *ty == tokio_postgres::types::Type::INT8
    }
}
