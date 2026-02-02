//! Integration tests for the rpc crate macros.
//!
//! This crate tests the `impl_rpc!` and `register_server_rpc!` macros by using them
//! on real structs and verifying the generated RPC infrastructure.

use arktype_ast::ArkType;
use serde::{Deserialize, Serialize};

// Re-export errx and import the trait needed by macro-generated code
#[cfg(feature = "service")]
pub use errx;

#[derive(Debug, Clone, Serialize, Deserialize, ArkType)]
pub struct GetUserProps {
    pub user_id: String,
    pub include_email: Option<bool>,
    pub expected_role: Option<UserRole>,
}

#[derive(Debug, Clone, Serialize, Deserialize, ArkType)]
pub struct CreateUserProps {
    pub name: String,
    pub email: String,
    pub nickname: Option<String>,
    pub role: UserRole,
    pub status: Option<AccountStatus>,
    pub audit: Option<AuditStamp>,
}

#[derive(Debug, Clone, Serialize, Deserialize, ArkType)]
pub struct EmptyProps {}

#[derive(Debug, Clone, Serialize, Deserialize, ArkType)]
pub enum UserRole {
    Admin,
    Member,
    Guest,
}

#[derive(Debug, Clone, Serialize, Deserialize, ArkType)]
pub enum AccountStatus {
    Active,
    Suspended { reason: String },
    Disabled { at: String },
}

#[derive(Debug, Clone, Serialize, Deserialize, ArkType)]
#[serde(tag = "kind")]
pub enum AuditStamp {
    Manual { by: String },
    Automated { system: String },
}

#[derive(Debug, Clone, Serialize, Deserialize, ArkType)]
pub struct NestedProps {
    pub user: CreateUserProps,
    pub admin: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize, ArkType, rpc::Response)]
pub struct GetUserResponse {
    pub id: String,
    pub name: String,
    pub email: String,
    pub role: UserRole,
    pub status: Option<AccountStatus>,
    pub last_login: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, ArkType, rpc::Response)]
pub struct CreateUserResponse {
    pub id: String,
    pub name: String,
    pub email: String,
    pub role: UserRole,
    pub status: Option<AccountStatus>,
    pub audit: Option<AuditStamp>,
}

#[derive(Debug, Clone, Serialize, Deserialize, ArkType, rpc::Response)]
pub struct PingResponse {
    pub success: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize, ArkType, rpc::Response)]
#[rpc(code = 201)]
pub struct CreatedResponse {
    pub id: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, ArkType, rpc::Response)]
pub enum AuthResponse {
    Data { id: String },
    #[rpc(code = 401)]
    AuthRequired,
}

#[derive(Debug, Clone, Serialize, Deserialize, ArkType, rpc::Response)]
#[rpc(code = 202)]
pub enum JobResponse {
    Queued,
    #[rpc(code = 409)]
    Conflict { message: String },
}

#[derive(Debug, Clone, Serialize, Deserialize, ArkType, rpc::Response)]
pub struct NestedResponse {
    pub user: GetUserResponse,
    pub created_at: String,
}

#[cfg(feature = "service")]
pub async fn get_user(
    _context: rpc::EmptyContext,
    props: GetUserProps,
) -> Result<GetUserResponse, errx::Error> {
    let role = props.expected_role.unwrap_or(UserRole::Member);
    let email = if props.include_email.unwrap_or(true) {
        "test@example.com".to_string()
    } else {
        "".to_string()
    };
    Ok(GetUserResponse {
        id: props.user_id,
        name: "Test User".to_string(),
        email,
        role,
        status: Some(AccountStatus::Active),
        last_login: None,
    })
}

#[cfg(feature = "service")]
pub async fn create_user(
    _context: rpc::EmptyContext,
    props: CreateUserProps,
) -> Result<CreateUserResponse, errx::Error> {
    let status = props.status.or(Some(AccountStatus::Active));
    Ok(CreateUserResponse {
        id: "new-user-123".to_string(),
        name: props.name,
        email: props.email,
        role: props.role,
        status,
        audit: props.audit,
    })
}

#[cfg(feature = "service")]
pub async fn ping(
    _context: rpc::EmptyContext,
    _props: EmptyProps,
) -> Result<PingResponse, errx::Error> {
    Ok(PingResponse { success: true })
}

rpc::impl_rpc!(v1, GetUserProps, GetUserResponse, rpc::EmptyContext, get_user);
rpc::impl_rpc!(v1, CreateUserProps, CreateUserResponse, rpc::EmptyContext, create_user);
rpc::impl_rpc!(v1, EmptyProps, PingResponse, rpc::EmptyContext, ping);

#[cfg(test)]
mod tests {
    use super::*;
    use rpc::Rpc;

    #[test]
    fn impl_rpc_sets_correct_name_get_user() {
        let name = <GetUserResponse as Rpc>::NAME;
        assert!(
            name.starts_with("/rpc/"),
            "RPC name should start with /rpc/, got: {}",
            name
        );
        assert!(
            name.contains("v1"),
            "RPC name should contain version group, got: {}",
            name
        );
        assert!(
            name.contains("get_user"),
            "RPC name should contain function name, got: {}",
            name
        );
    }

    #[test]
    fn impl_rpc_sets_correct_name_create_user() {
        let name = <CreateUserResponse as Rpc>::NAME;
        assert!(
            name.starts_with("/rpc/v1/"),
            "RPC name should start with /rpc/v1/, got: {}",
            name
        );
        assert!(
            name.contains("create_user"),
            "RPC name should contain function name, got: {}",
            name
        );
    }

    #[test]
    fn impl_rpc_sets_correct_name_ping() {
        let name = <PingResponse as Rpc>::NAME;
        assert!(
            name.starts_with("/rpc/v1/"),
            "RPC name should start with /rpc/v1/, got: {}",
            name
        );
        assert!(
            name.contains("ping"),
            "RPC name should contain function name, got: {}",
            name
        );
    }

    #[test]
    fn impl_rpc_props_type_is_correct() {
        // The Props associated type should be the input type
        // We verify this compiles by using the type
        fn assert_props_type<T: Rpc>()
        where
            T::Props: Serialize + serde::de::DeserializeOwned,
        {
        }

        assert_props_type::<GetUserResponse>();
        assert_props_type::<CreateUserResponse>();
        assert_props_type::<PingResponse>();
    }

    #[test]
    fn response_code_defaults_to_200() {
        let res = PingResponse { success: true };
        assert_eq!(rpc::Response::code(&res), 200);

        let enum_res = AuthResponse::Data { id: "x".to_string() };
        assert_eq!(rpc::Response::code(&enum_res), 200);
    }

    #[test]
    fn response_code_struct_override() {
        let res = CreatedResponse { id: "new".to_string() };
        assert_eq!(rpc::Response::code(&res), 201);
    }

    #[test]
    fn response_code_enum_variant_override() {
        let res = AuthResponse::AuthRequired;
        assert_eq!(rpc::Response::code(&res), 401);
    }

    #[test]
    fn response_code_enum_default_and_override() {
        let queued = JobResponse::Queued;
        let conflict = JobResponse::Conflict {
            message: "busy".to_string(),
        };
        assert_eq!(rpc::Response::code(&queued), 202);
        assert_eq!(rpc::Response::code(&conflict), 409);
    }

    #[cfg(feature = "bindings")]
    mod bindings_tests {
        use super::*;
        use std::{
            env, fs,
            path::PathBuf,
            sync::{Mutex, OnceLock},
        };

        fn bindings_lock() -> std::sync::MutexGuard<'static, ()> {
            static LOCK: OnceLock<Mutex<()>> = OnceLock::new();
            LOCK.get_or_init(|| Mutex::new(())).lock().unwrap()
        }

        fn generate_bindings_output() -> String {
            let _guard = bindings_lock();
            let export_dir =
                env::temp_dir().join(format!("rpc_test_bindings_{}", std::process::id()));

            if export_dir.exists() {
                let _ = fs::remove_dir_all(&export_dir);
            }

            let result = rpc::generate_bindings(&export_dir, rpc::BindgenFilter::All);
            assert!(result.is_ok(), "Bindgen finished with error: {:?}", result);

            let out_file_name = format!("{}_rpc.gen.ts", env!("CARGO_PKG_NAME"));
            let generated_path = export_dir.join(out_file_name);
            fs::read_to_string(&generated_path).unwrap_or_else(|err| {
                panic!(
                    "Failed to read generated bindings at {}: {}",
                    generated_path.display(),
                    err
                )
            })
        }

        #[test]
        fn bindings_registered_for_props() {
            // The arktype macro generates bindings that should be accessible
            let bindings = GetUserProps::arktype_bindings();
            assert!(!bindings.is_empty(), "Props should have arktype bindings");
        }

        #[test]
        fn bindings_registered_for_response() {
            let bindings = GetUserResponse::arktype_bindings();
            assert!(
                !bindings.is_empty(),
                "Response should have arktype bindings"
            );
        }

        #[test]
        fn nested_types_have_bindings() {
            let bindings = NestedProps::arktype_bindings();
            // NestedProps contains CreateUserProps, so there should be multiple bindings
            assert!(
                bindings.len() >= 2,
                "Nested type should include child type bindings, got: {}",
                bindings.len()
            );
        }

        #[test]
        fn nested_response_has_bindings() {
            let bindings = NestedResponse::arktype_bindings();
            // NestedResponse contains GetUserResponse
            assert!(
                bindings.len() >= 2,
                "Nested response should include child type bindings, got: {}",
                bindings.len()
            );
        }

        #[test]
        fn bindings_match_golden_standard() {
            let generated = generate_bindings_output();
            let golden_file = format!("{}_rpc.gen.ts", env!("CARGO_PKG_NAME"));
            let golden_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                .join("bindings")
                .join(golden_file);

            if env::var("RPC_BINDGEN_UPDATE_GOLDEN").is_ok() {
                if let Some(parent) = golden_path.parent() {
                    fs::create_dir_all(parent).unwrap();
                }
                fs::write(&golden_path, generated).unwrap();
                return;
            }

            let golden = fs::read_to_string(&golden_path).unwrap_or_else(|err| {
                panic!(
                    "Failed to read golden bindings at {}: {}",
                    golden_path.display(),
                    err
                )
            });

            assert_eq!(
                generated, golden,
                "Generated bindings did not match the golden standard"
            );
        }
    }

    #[cfg(feature = "service")]
    mod service_tests {
        #[test]
        fn router_is_created() {
            // The router() function should return a valid axum Router
            let router = rpc::router();
            // If we got here without panic, the router was created successfully
            let _ = router;
        }

        #[test]
        fn endpoints_are_registered() {
            // Check that our endpoints are in the inventory
            let mut found_get_user = false;
            let mut found_create_user = false;
            let mut found_ping = false;

            for endpoint in inventory::iter::<rpc::Endpoint> {
                if endpoint.path.contains("get_user") {
                    found_get_user = true;
                }
                if endpoint.path.contains("create_user") {
                    found_create_user = true;
                }
                if endpoint.path.contains("ping") {
                    found_ping = true;
                }
            }

            assert!(found_get_user, "get_user endpoint should be registered");
            assert!(
                found_create_user,
                "create_user endpoint should be registered"
            );
            assert!(found_ping, "ping endpoint should be registered");
        }

        #[test]
        fn endpoint_paths_have_correct_format() {
            for endpoint in inventory::iter::<rpc::Endpoint> {
                assert!(
                    endpoint.path.starts_with("/rpc/"),
                    "Endpoint path should start with /rpc/, got: {}",
                    endpoint.path
                );
            }
        }
    }
}
