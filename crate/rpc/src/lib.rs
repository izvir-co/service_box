use std::time::Instant;
use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    path::PathBuf,
    sync::{Mutex, OnceLock},
};

use serde::{Serialize, de::DeserializeOwned};

use arktype_ast::Schema;

pub use arktype_ast::ArkType;
pub use inventory::submit;
pub use rpc_attr::handler;

pub mod server;

pub struct Endpoint {
    pub path: &'static str,
    pub route: fn() -> axum::routing::MethodRouter,
}

inventory::collect!(Endpoint);

pub struct Binding {
    pub generator: fn(),
}

inventory::collect!(Binding);

#[macro_export]
macro_rules! register_server_rpc {
    ($target:ident, $exec_fn:path) => {
        #[cfg(feature = "service")]
        const _: () = {
            use axum::{
                extract::Json,
                http::StatusCode,
                response::{IntoResponse, Response},
                routing::{MethodRouter, post},
            };
            use errx::ErrorCapture as _;

            async fn __rpc_handler(
                Json(payload): Json<<$target as $crate::Rpc>::Props>
            ) -> Result<Response, errx::Error> {
                let res: Result<$target, errx::Error> = $exec_fn(payload).await;
                res.map(|o| (StatusCode::OK, axum::Json(o)).into_response())
                    .trace_err()
            }

            fn __route() -> MethodRouter {
                post(__rpc_handler)
            }

            $crate::submit! {
                $crate::Endpoint {
                    path: <$target as $crate::Rpc>::NAME,
                    route: __route,
                }
            }
        };
    };
}

/// Important: This function needs to be called in the crate that has rpc functions,
/// If you are using main.rs and lib.rs, it needs to be called somewhere in the lib.rs path
/// If you call this directly from main.rs, you will get router without any endpoints
pub fn router() -> axum::Router {
    let mut r = axum::Router::new();
    for ep in inventory::iter::<Endpoint> {
        r = r.route(ep.path, (ep.route)());
        tracing::info!("endpoint available: {}", ep.path);
    }
    r
}

/// Important: This function needs to be called in the crate that has rpc functions,
/// If you are using main.rs and lib.rs, it needs to be called somewhere in the lib.rs path
/// If you call this directly from main.rs, you will get no bindings
pub fn generate_bindings() {
    let global_start = Instant::now();
    let out_dir = match std::env::var("RPC_BINDGEN_EXPORT_DIR") {
        Ok(d) => std::path::PathBuf::from(d),
        Err(_) => {
            eprintln!("env RPC_BINDGEN_EXPORT_DIR not set; skipping generation");
            return;
        },
    };
    let service_name = match std::env::var("RPC_BINDGEN_SERVICE_NAME") {
        Ok(d) => d,
        Err(_) => {
            eprintln!("env RPC_BINDGEN_SERVICE_NAME not set; skipping generation");
            return;
        },
    };
    for b in inventory::iter::<Binding> {
        tracing::info!("bindgen running...");
        let gen_start = Instant::now();
        (b.generator)();
        let gen_end = Instant::now() - gen_start;
        tracing::info!("Done in {gen_end:?}");
    }

    let mut instance = RpcCalls::instance().lock().unwrap();

    let out_rpc_file_name = format!("{service_name}_rpc.gen.ts");

    instance.override_existing(&out_dir, &out_rpc_file_name, &service_name);

    let global_end = Instant::now() - global_start;
    tracing::info!("Finished bindgen in {global_end:?}");
}

pub trait Rpc {
    const NAME: &'static str;
    type Props: Serialize + DeserializeOwned;
}

#[macro_export]
macro_rules! impl_rpc {
    ($group:ident, $props:ty, $target:ident, $exec_fn:path) => {
        impl $crate::Rpc for $target {
            const NAME: &'static str =
                concat!("/rpc/", stringify!($group), "/", stringify!($exec_fn));
            type Props = $props;
        }

        #[cfg(feature = "bindings")]
        const _: () = {
            #[allow(non_snake_case)]
            fn $target() {
                let props_type_bindgen = <$props>::arktype_bindings();
                let target_type_bindgen = <$target>::arktype_bindings();
                $crate::rpc_bindgen(
                    stringify!($exec_fn),
                    stringify!($group),
                    ::std::any::type_name::<$props>(),
                    ::std::any::type_name::<$target>(),
                    props_type_bindgen,
                    target_type_bindgen,
                )
            }

            $crate::submit! {
                $crate::Binding {
                    generator: $target,
                }
            }
        };

        $crate::register_server_rpc!($target, $exec_fn);
    };
}

pub struct RpcCalls {
    pub imports: BTreeMap<String, Schema>,
    pub calls: BTreeMap<String, Vec<String>>,
}

pub fn order_schemas(schemas: &BTreeMap<String, Schema>) -> Result<Vec<(String, Schema)>, String> {
    let all_names: BTreeSet<String> = schemas.keys().cloned().collect();

    let mut deps: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
    let mut rev: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();

    for (name, schema) in schemas.iter() {
        let mut dset: BTreeSet<String> = BTreeSet::new();
        for dep in schema.deps() {
            if dep == *name {
                continue;
            }
            if all_names.contains(&dep) {
                dset.insert(dep);
            }
        }
        deps.insert(name.clone(), dset);
    }

    for (name, dset) in deps.iter() {
        for dep in dset {
            rev.entry(dep.clone()).or_default().insert(name.clone());
        }
        rev.entry(name.clone()).or_default();
    }

    let mut indegree: BTreeMap<String, usize> = BTreeMap::new();
    for (name, dset) in deps.iter() {
        indegree.insert(name.clone(), dset.len());
    }

    let mut zero: BTreeSet<String> = indegree
        .iter()
        .filter_map(|(n, deg)| if *deg == 0 { Some(n.clone()) } else { None })
        .collect();

    let mut ordered: Vec<String> = Vec::with_capacity(schemas.len());

    while let Some(n) = zero.iter().next().cloned() {
        zero.remove(&n);
        ordered.push(n.clone());

        if let Some(children) = rev.get(&n) {
            for m in children {
                let deg = indegree.get_mut(m).expect("node missing indegree");
                *deg -= 1;
                if *deg == 0 {
                    zero.insert(m.clone());
                }
            }
        }
    }

    if ordered.len() != schemas.len() {
        let mut stuck: Vec<String> = indegree
            .iter()
            .filter_map(|(n, deg)| if *deg > 0 { Some(n.clone()) } else { None })
            .collect();
        stuck.sort();

        let mut msg = String::from("Cyclic or unresolved dependency detected among: ");
        msg.push_str(&stuck.join(", "));
        msg.push_str(".\nExample edges:\n");
        for n in stuck.iter().take(5) {
            if let Some(ds) = deps.get(n) {
                let ds_list: Vec<&str> = ds.iter().map(|s| s.as_str()).collect();
                msg.push_str(&format!("  {n} -> [{}]\n", ds_list.join(", ")));
            }
        }
        return Err(msg);
    }

    let result = ordered
        .into_iter()
        .map(|name| {
            let src = schemas.get(&name).expect("missing schema").clone();
            (name, src)
        })
        .collect();

    Ok(result)
}

/// Replaces identifiers in the input string based on the provided mapping.
fn replace_identifiers(
    input: &str,
    mapping: &HashMap<String, String>,
) -> String {
    let mut output = String::with_capacity(input.len());
    let bytes = input.as_bytes();
    let len = input.len();
    let mut i = 0;

    while i < len {
        // Copy non-identifier characters
        let start_non_ident = i;
        while i < len {
            let c = bytes[i];
            let is_start = c.is_ascii_alphabetic() || c == b'_' || c == b'$';
            if is_start {
                break;
            }
            i += 1;
        }
        output.push_str(&input[start_non_ident..i]);

        if i >= len {
            break;
        }

        // Extract identifier
        let start_ident = i;
        i += 1; // consume start char
        while i < len {
            let c = bytes[i];
            let is_cont = c.is_ascii_alphanumeric() || c == b'_' || c == b'$';
            if !is_cont {
                break;
            }
            i += 1;
        }

        let ident = &input[start_ident..i];
        // Replace if found in map, otherwise keep original
        if let Some(replacement) = mapping.get(ident) {
            output.push_str(replacement);
        } else {
            output.push_str(ident);
        }
    }

    output
}

/// Computes the shortest unique suffix for each name in the set.
fn compute_short_names(names: &BTreeSet<String>) -> Result<HashMap<String, String>, String> {
    let mut mapping = HashMap::new();
    // Store (original_name, current_depth)
    let mut status: Vec<(&str, usize)> = names.iter().map(|n| (n.as_str(), 1)).collect();

    loop {
        let mut collision = false;
        let mut name_counts: HashMap<String, usize> = HashMap::new();

        // Calculate counts for current depth generation
        for (orig, depth) in &status {
            let parts: Vec<&str> = orig.split('$').collect();
            let start = if *depth > parts.len() {
                0
            } else {
                parts.len() - depth
            };
            // JOIN WITH EMPTY STRING
            let candidate = parts[start..].join("");
            *name_counts.entry(candidate).or_insert(0) += 1;
        }

        // Identify who needs to grow
        for (orig, depth) in status.iter_mut() {
            let parts: Vec<&str> = orig.split('$').collect();
            let start = if *depth > parts.len() {
                0
            } else {
                parts.len() - *depth
            };
            // JOIN WITH EMPTY STRING
            let candidate = parts[start..].join("");

            // If this candidate name is used by more than one key
            if name_counts[&candidate] > 1 && *depth < parts.len() {
                // We can grow to resolve ambiguity
                *depth += 1;
                collision = true;
            }
            // If depth >= parts.len(), we can't grow anymore.
            // We rely on the *other* colliding key to grow and eventually diverge.
            // If two keys are identical, they will collide forever, but input keys are unique.
        }

        if !collision {
            break;
        }
    }

    // Build final map
    for (orig, depth) in status {
        let parts: Vec<&str> = orig.split('$').collect();
        let start = if depth > parts.len() {
            0
        } else {
            parts.len() - depth
        };
        // JOIN WITH EMPTY STRING
        let short_name = parts[start..].join("");
        mapping.insert(orig.to_string(), short_name);
    }

    let mut collisions: HashMap<String, Vec<String>> = HashMap::new();
    for (orig, short_name) in mapping.iter() {
        collisions
            .entry(short_name.clone())
            .or_default()
            .push(orig.clone());
    }

    let mut collision_msgs: Vec<String> = collisions
        .into_iter()
        .filter_map(|(short, items)| {
            if items.len() > 1 {
                Some(format!("{short}: [{}]", items.join(", ")))
            } else {
                None
            }
        })
        .collect();

    if !collision_msgs.is_empty() {
        collision_msgs.sort();
        return Err(format!(
            "Could not compute unique short names. Collisions: {}",
            collision_msgs.join("; ")
        ));
    }

    Ok(mapping)
}

impl RpcCalls {
    pub fn instance() -> &'static Mutex<RpcCalls> {
        static LOCK: OnceLock<Mutex<RpcCalls>> = OnceLock::new();
        LOCK.get_or_init(|| {
            Mutex::new(RpcCalls {
                imports: BTreeMap::new(),
                calls: BTreeMap::new(),
            })
        })
    }

    pub fn override_existing(
        &mut self,
        out_dir: &PathBuf,
        out_file: &str,
        _service_name: &str,
    ) {
        let mut out = String::new();
        out.push_str("// This file was generated by toolbox package [rpc](https://github.com/izvir-co/service_box).\n// Editing this file manually can result in a loss of your fingers and your job :)\n\n");

        out.push_str("import { type } from \"arktype\"\n");
        out.push_str("import { route } from \"@izvir/rpc\";\n\n");

        let imports = order_schemas(&self.imports).expect("could not order schemas, good luck lol");

        let all_keys: BTreeSet<String> = self.imports.keys().cloned().collect();
        let name_mapping =
            compute_short_names(&all_keys).expect("could not compute unique short names");

        for (original_name, schema) in imports {
            let short_name = name_mapping
                .get(&original_name)
                .cloned()
                .unwrap_or(original_name);
            let shortened_schema = schema.rename(&name_mapping);
            let rendered = shortened_schema.render(&short_name);

            let i = rendered.split('\n');
            for i in i {
                if !i.is_empty() {
                    out.push_str("export ");
                    out.push_str(i);
                }
                if !out.ends_with('\n') {
                    out.push('\n');
                }
            }

            out.push('\n');
        }

        // 4. Generate calls with shortened names
        out.push_str("export const ServiceDefinitions = {\n");
        for (group, calls) in self.calls.iter_mut() {
            // We apply replacement to the call definition strings
            let mut shortened_calls: Vec<String> = calls
                .iter()
                .map(|call| replace_identifiers(call, &name_mapping))
                .collect();

            shortened_calls.sort();

            out.push_str(&format!("  {group}: {{\n"));
            for call in shortened_calls {
                out.push_str(&call);
                if !out.ends_with('\n') {
                    out.push('\n');
                }
            }
            out.push_str("  },\n");
        }
        out.push_str("} as const;\n\n");
        out.push_str("export type ServiceDefinitions = typeof ServiceDefinitions;");

        std::fs::create_dir_all(out_dir).unwrap();
        std::fs::write(out_dir.join(out_file), out).unwrap();
    }
}

fn lower_camel_case(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut capitalize_next = false;

    for ch in s.chars() {
        if ch == '_' {
            capitalize_next = true;
            continue;
        }

        if result.is_empty() {
            for lc in ch.to_lowercase() {
                result.push(lc);
            }
            capitalize_next = false;
        } else if capitalize_next {
            for uc in ch.to_uppercase() {
                result.push(uc);
            }
            capitalize_next = false;
        } else {
            result.push(ch);
        }
    }

    result
}

fn upper_camel_case(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut capitalize_next = true;
    for ch in s.chars() {
        if ch == '$' {
            capitalize_next = true;
            result.push(ch);
            continue;
        }
        if ch == '_' {
            capitalize_next = true;
            continue;
        }
        if capitalize_next {
            for uc in ch.to_uppercase() {
                result.push(uc);
            }
            capitalize_next = false;
        } else {
            result.push(ch);
        }
    }
    result
}

pub fn rpc_bindgen(
    function_name: &str,
    group: &str,
    props: &str,
    target: &str,
    props_type_bindgen: BTreeMap<String, Schema>,
    target_type_bindgen: BTreeMap<String, Schema>,
) {
    let _service_name = match std::env::var("RPC_BINDGEN_SERVICE_NAME") {
        Ok(d) => d,
        Err(_) => {
            eprintln!("env RPC_BINDGEN_SERVICE_NAME not set; skipping generation");
            return;
        },
    };

    let props = upper_camel_case(&props.replace("::", "$"));
    let target = upper_camel_case(&target.replace("::", "$"));

    let mut instance = RpcCalls::instance().lock().unwrap();

    for prop_type in props_type_bindgen {
        instance.imports.entry(prop_type.0).or_insert(prop_type.1);
    }

    for target_type in target_type_bindgen {
        instance
            .imports
            .entry(target_type.0)
            .or_insert(target_type.1);
    }

    let key = lower_camel_case(function_name);
    let impl_rpc_string = || {
        format!(
            "    {}: route({{ endpoint: \"{}/{}\", input: {}, output: {} }}),",
            key, group, function_name, props, target
        )
    };

    instance
        .calls
        .entry(group.to_owned().to_lowercase())
        .and_modify(|g| g.push(impl_rpc_string()))
        .or_insert(vec![impl_rpc_string()]);
}

#[cfg(test)]
mod tests {
    use super::*;
    use arktype_ast::{Expr, Field, Primitive, Schema};

    #[test]
    fn lower_camel_case_snake() {
        assert_eq!(lower_camel_case("user_name"), "userName");
    }

    #[test]
    fn lower_camel_case_single() {
        assert_eq!(lower_camel_case("name"), "name");
    }

    #[test]
    fn lower_camel_case_multi_underscore() {
        assert_eq!(lower_camel_case("get_user_by_id"), "getUserById");
    }

    #[test]
    fn lower_camel_case_already_camel() {
        assert_eq!(lower_camel_case("userName"), "userName");
    }

    #[test]
    fn lower_camel_case_leading_upper() {
        assert_eq!(lower_camel_case("User_name"), "userName");
    }

    #[test]
    fn lower_camel_case_empty() {
        assert_eq!(lower_camel_case(""), "");
    }

    #[test]
    fn upper_camel_case_snake() {
        assert_eq!(upper_camel_case("user_name"), "UserName");
    }

    #[test]
    fn upper_camel_case_with_dollar() {
        assert_eq!(upper_camel_case("foo$bar_baz"), "Foo$BarBaz");
    }

    #[test]
    fn upper_camel_case_all_lower() {
        assert_eq!(upper_camel_case("foo"), "Foo");
    }

    #[test]
    fn upper_camel_case_already_pascal() {
        assert_eq!(upper_camel_case("UserName"), "UserName");
    }

    #[test]
    fn upper_camel_case_double_underscore() {
        assert_eq!(upper_camel_case("foo__bar"), "FooBar");
    }

    #[test]
    fn upper_camel_case_empty() {
        assert_eq!(upper_camel_case(""), "");
    }

    #[test]
    fn replace_identifiers_basic() {
        let mut map = HashMap::new();
        map.insert("Foo".to_string(), "Bar".to_string());
        assert_eq!(replace_identifiers("Foo", &map), "Bar");
    }

    #[test]
    fn replace_identifiers_multiple() {
        let mut map = HashMap::new();
        map.insert("Foo".to_string(), "A".to_string());
        map.insert("Bar".to_string(), "B".to_string());
        assert_eq!(replace_identifiers("Foo + Bar", &map), "A + B");
    }

    #[test]
    fn replace_identifiers_preserves_non_idents() {
        let mut map = HashMap::new();
        map.insert("x".to_string(), "y".to_string());
        assert_eq!(replace_identifiers("x + (x * x)", &map), "y + (y * y)");
    }

    #[test]
    fn replace_identifiers_no_partial_match() {
        let mut map = HashMap::new();
        map.insert("Foo".to_string(), "X".to_string());
        // FooBar should NOT be replaced
        assert_eq!(replace_identifiers("FooBar", &map), "FooBar");
    }

    #[test]
    fn replace_identifiers_empty_map() {
        let map = HashMap::new();
        assert_eq!(replace_identifiers("Foo Bar Baz", &map), "Foo Bar Baz");
    }

    #[test]
    fn replace_identifiers_case_sensitive() {
        let mut map = HashMap::new();
        map.insert("foo".to_string(), "X".to_string());
        // Foo (uppercase) should NOT be replaced
        assert_eq!(replace_identifiers("Foo foo", &map), "Foo X");
    }

    #[test]
    fn replace_identifiers_complex_typescript() {
        let mut map = HashMap::new();
        map.insert("mod$Inner".to_string(), "Inner".to_string());
        map.insert("mod$Outer".to_string(), "Outer".to_string());
        let input = "const X = type({ inner: mod$Inner, outer: mod$Outer })";
        let expected = "const X = type({ inner: Inner, outer: Outer })";
        assert_eq!(replace_identifiers(input, &map), expected);
    }

    #[test]
    fn compute_short_names_single() {
        let names: BTreeSet<String> = ["Foo"].into_iter().map(|s| s.to_string()).collect();
        let result = compute_short_names(&names).unwrap();
        assert_eq!(result.get("Foo"), Some(&"Foo".to_string()));
    }

    #[test]
    fn compute_short_names_no_collision() {
        let names: BTreeSet<String> = ["a$Foo", "b$Bar"]
            .into_iter()
            .map(|s| s.to_string())
            .collect();
        let result = compute_short_names(&names).unwrap();
        assert_eq!(result.get("a$Foo"), Some(&"Foo".to_string()));
        assert_eq!(result.get("b$Bar"), Some(&"Bar".to_string()));
    }

    #[test]
    fn compute_short_names_collision_resolved() {
        let names: BTreeSet<String> = ["a$Foo", "b$Foo"]
            .into_iter()
            .map(|s| s.to_string())
            .collect();
        let result = compute_short_names(&names).unwrap();
        // Both end in Foo, so they need to include the parent segment
        assert_eq!(result.get("a$Foo"), Some(&"aFoo".to_string()));
        assert_eq!(result.get("b$Foo"), Some(&"bFoo".to_string()));
    }

    #[test]
    fn compute_short_names_deep_path() {
        let names: BTreeSet<String> = ["a$b$c$Foo"].into_iter().map(|s| s.to_string()).collect();
        let result = compute_short_names(&names).unwrap();
        // No collision, so just use the last segment
        assert_eq!(result.get("a$b$c$Foo"), Some(&"Foo".to_string()));
    }

    #[test]
    fn compute_short_names_full_path_fallback() {
        // Two names that share all suffixes except the first segment
        let names: BTreeSet<String> = ["x$a$Foo", "y$a$Foo"]
            .into_iter()
            .map(|s| s.to_string())
            .collect();
        let result = compute_short_names(&names).unwrap();
        // Need to go all the way up to distinguish
        assert_eq!(result.get("x$a$Foo"), Some(&"xaFoo".to_string()));
        assert_eq!(result.get("y$a$Foo"), Some(&"yaFoo".to_string()));
    }

    #[test]
    fn compute_short_names_mixed_depths() {
        let names: BTreeSet<String> = ["Foo", "a$Foo", "b$Bar"]
            .into_iter()
            .map(|s| s.to_string())
            .collect();
        let result = compute_short_names(&names).unwrap();
        // Foo and a$Foo collide on suffix "Foo"
        assert_eq!(result.get("Foo"), Some(&"Foo".to_string()));
        assert_eq!(result.get("a$Foo"), Some(&"aFoo".to_string()));
        assert_eq!(result.get("b$Bar"), Some(&"Bar".to_string()));
    }

    #[test]
    fn order_schemas_no_deps() {
        let mut schemas = BTreeMap::new();
        schemas.insert(
            "A".to_string(),
            Schema::new(Expr::Primitive(Primitive::String)),
        );
        schemas.insert(
            "B".to_string(),
            Schema::new(Expr::Primitive(Primitive::Number)),
        );
        schemas.insert(
            "C".to_string(),
            Schema::new(Expr::Primitive(Primitive::Boolean)),
        );

        let result = order_schemas(&schemas).unwrap();
        // All independent, should be in lexicographic order (BTreeMap)
        let names: Vec<&str> = result.iter().map(|(n, _)| n.as_str()).collect();
        assert_eq!(names, vec!["A", "B", "C"]);
    }

    #[test]
    fn order_schemas_linear_deps() {
        let mut schemas = BTreeMap::new();
        // C depends on B, B depends on A
        schemas.insert(
            "A".to_string(),
            Schema::new(Expr::Primitive(Primitive::String)),
        );
        schemas.insert(
            "B".to_string(),
            Schema::new(Expr::TypeCall(Box::new(Expr::Object(vec![Field {
                name: "a".to_string(),
                expr: Expr::Ref("A".to_string()),
            }])))),
        );
        schemas.insert(
            "C".to_string(),
            Schema::new(Expr::TypeCall(Box::new(Expr::Object(vec![Field {
                name: "b".to_string(),
                expr: Expr::Ref("B".to_string()),
            }])))),
        );

        let result = order_schemas(&schemas).unwrap();
        let names: Vec<&str> = result.iter().map(|(n, _)| n.as_str()).collect();
        // A must come before B, B must come before C
        assert_eq!(names, vec!["A", "B", "C"]);
    }

    #[test]
    fn order_schemas_diamond_deps() {
        let mut schemas = BTreeMap::new();
        // D depends on B and C, B and C both depend on A
        schemas.insert(
            "A".to_string(),
            Schema::new(Expr::Primitive(Primitive::String)),
        );
        schemas.insert(
            "B".to_string(),
            Schema::new(Expr::TypeCall(Box::new(Expr::Object(vec![Field {
                name: "a".to_string(),
                expr: Expr::Ref("A".to_string()),
            }])))),
        );
        schemas.insert(
            "C".to_string(),
            Schema::new(Expr::TypeCall(Box::new(Expr::Object(vec![Field {
                name: "a".to_string(),
                expr: Expr::Ref("A".to_string()),
            }])))),
        );
        schemas.insert(
            "D".to_string(),
            Schema::new(Expr::TypeCall(Box::new(Expr::Object(vec![
                Field {
                    name: "b".to_string(),
                    expr: Expr::Ref("B".to_string()),
                },
                Field {
                    name: "c".to_string(),
                    expr: Expr::Ref("C".to_string()),
                },
            ])))),
        );

        let result = order_schemas(&schemas).unwrap();
        let names: Vec<&str> = result.iter().map(|(n, _)| n.as_str()).collect();

        // A must come first, D must come last
        assert_eq!(names[0], "A");
        assert_eq!(names[3], "D");
        // B and C can be in any order between A and D
        assert!(names[1] == "B" || names[1] == "C");
        assert!(names[2] == "B" || names[2] == "C");
    }

    #[test]
    fn order_schemas_self_reference() {
        let mut schemas = BTreeMap::new();
        // Self-referencing type (like a linked list)
        schemas.insert(
            "Node".to_string(),
            Schema::new(Expr::TypeCall(Box::new(Expr::Object(vec![Field {
                name: "next".to_string(),
                expr: Expr::Ref("Node".to_string()),
            }])))),
        );

        let result = order_schemas(&schemas).unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].0, "Node");
    }

    #[test]
    fn order_schemas_cycle_detected() {
        let mut schemas = BTreeMap::new();
        // A depends on B, B depends on A
        schemas.insert(
            "A".to_string(),
            Schema::new(Expr::TypeCall(Box::new(Expr::Object(vec![Field {
                name: "b".to_string(),
                expr: Expr::Ref("B".to_string()),
            }])))),
        );
        schemas.insert(
            "B".to_string(),
            Schema::new(Expr::TypeCall(Box::new(Expr::Object(vec![Field {
                name: "a".to_string(),
                expr: Expr::Ref("A".to_string()),
            }])))),
        );

        let result = order_schemas(&schemas);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.contains("Cyclic"));
    }

    #[test]
    fn order_schemas_external_refs() {
        let mut schemas = BTreeMap::new();
        // References to types not in the schema set should be ignored
        schemas.insert(
            "A".to_string(),
            Schema::new(Expr::TypeCall(Box::new(Expr::Object(vec![Field {
                name: "ext".to_string(),
                expr: Expr::Ref("ExternalType".to_string()),
            }])))),
        );
        schemas.insert(
            "B".to_string(),
            Schema::new(Expr::TypeCall(Box::new(Expr::Object(vec![Field {
                name: "a".to_string(),
                expr: Expr::Ref("A".to_string()),
            }])))),
        );

        let result = order_schemas(&schemas).unwrap();
        let names: Vec<&str> = result.iter().map(|(n, _)| n.as_str()).collect();
        assert_eq!(names, vec!["A", "B"]);
    }

    #[test]
    fn order_schemas_empty() {
        let schemas: BTreeMap<String, Schema> = BTreeMap::new();
        let result = order_schemas(&schemas).unwrap();
        assert!(result.is_empty());
    }
}
