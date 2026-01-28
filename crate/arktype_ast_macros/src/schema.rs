use quote::quote;
use syn::{DataEnum, DataStruct, DeriveInput, LitStr, spanned::Spanned as _};

use crate::type_parser::{ParsedFieldType, PrimitiveType};

#[derive(Debug)]
pub(crate) enum Supported {
    Struct(syn::DataStruct),
    Enum(syn::DataEnum),
}

fn unsuported_err(
    span: proc_macro2::Span,
    thing: &str,
) -> syn::Error {
    syn::Error::new(span, format!("ArkType bindgen does not support {thing}"))
}

fn compile_error(
    span: proc_macro2::Span,
    err: syn::Error,
) -> proc_macro2::TokenStream {
    let msg = err.to_string();
    let tokens = quote::quote_spanned! {span=>
        compile_error!(#msg);
    };
    tokens
}

pub(crate) fn data_checked(ast: &DeriveInput) -> Result<Supported, syn::Error> {
    if let Some(param) = ast.generics.params.iter().next() {
        return Err(match param {
            syn::GenericParam::Lifetime(lifetime_param) => {
                unsuported_err(lifetime_param.span(), "lifetimes")
            },
            syn::GenericParam::Type(type_param) => {
                unsuported_err(type_param.span(), "generic types")
            },
            syn::GenericParam::Const(const_param) => {
                unsuported_err(const_param.span(), "const generics")
            },
        });
    }

    match &ast.data {
        syn::Data::Struct(data_struct) => Ok(Supported::Struct(data_struct.clone())),
        syn::Data::Enum(data_enum) => Ok(Supported::Enum(data_enum.clone())),
        syn::Data::Union(_) => Err(unsuported_err(
            ast.span(),
            "union data types, the fuck you even doing over there???",
        )),
    }
}

pub fn generate(
    ast: DeriveInput,
    type_override: Option<String>,
) -> proc_macro2::TokenStream {
    let span = ast.span();
    match data_checked(&ast) {
        Ok(supported) => match supported_bindgen(ast, supported, type_override) {
            Ok(tokens) => tokens,
            Err(err) => compile_error(span, err),
        },
        Err(err) => compile_error(span, err),
    }
}

fn impl_bindings(
    ident: &syn::Ident,
    impl_body: proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    let ident_fn = quote::format_ident!("__arktype_bindings_ident");
    let helper_fn = quote::format_ident!("__arktype_bindings");
    let public_fn = quote::format_ident!("arktype_bindings");

    let tokens = quote! {
        impl #ident {
            pub fn #public_fn() -> ::std::collections::BTreeMap<::std::string::String, ::arktype_ast::Schema> {
                let mut visited: ::std::collections::BTreeMap<::std::string::String, ::arktype_ast::Schema> =
                    ::std::collections::BTreeMap::new();
                Self::#helper_fn(&mut visited);
                visited
            }

            pub fn #helper_fn(
                __visited: &mut ::std::collections::BTreeMap<::std::string::String, ::arktype_ast::Schema>,
            ) {
                let __ident_name = Self::#ident_fn();
                #impl_body
            }

            pub fn #ident_fn() -> String {
                let opt: &str = ::std::any::type_name::<#ident>();
                let opt: String = opt.replace("::", "$");
                let mut result = String::with_capacity(opt.len());
                let mut capitalize_next = true;
                for ch in opt.chars() {
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
        }
    };

    tokens
}

fn supported_bindgen(
    ast: DeriveInput,
    supported: Supported,
    type_override: Option<String>,
) -> Result<proc_macro2::TokenStream, syn::Error> {
    let ident = ast.ident.clone();
    let name = ident.to_string();
    let body = match supported {
        Supported::Struct(data_struct) => {
            struct_impl_body(&ident, &name, data_struct, type_override)?
        },
        Supported::Enum(data_enum) => {
            let serde_tag = serde_tag_name(&ast.attrs)?;
            enum_impl_body(&ident, &name, data_enum, serde_tag)?
        },
    };
    let bindgen = impl_bindings(&ident, body);
    Ok(bindgen)
}

fn serde_tag_name(attrs: &[syn::Attribute]) -> Result<Option<String>, syn::Error> {
    let mut tag_name: Option<String> = None;

    for attr in attrs {
        if !attr.path().is_ident("serde") {
            continue;
        }

        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("tag") {
                let val: LitStr = meta.value()?.parse()?;
                if tag_name.is_some() {
                    return Err(meta.error("duplicate serde tag"));
                }
                tag_name = Some(val.value());
                Ok(())
            } else {
                Ok(())
            }
        })?;
    }

    Ok(tag_name)
}

fn struct_impl_body(
    ident: &syn::Ident,
    _name: &str,
    data: DataStruct,
    type_override: Option<String>,
) -> Result<proc_macro2::TokenStream, syn::Error> {
    if let Some(type_override) = type_override {
        return Ok(quote! {
            let __schema = ::arktype_ast::Schema::new(
                ::arktype_ast::Expr::TypeCall(Box::new(::arktype_ast::Expr::Raw(#type_override.to_string()))),
            );
            __visited.insert(__ident_name, __schema);
        });
    }

    let fieds = match data.fields {
        syn::Fields::Named(fields_named) => parse_named_fields(fields_named)?,
        syn::Fields::Unnamed(_fields_unnamed) => {
            return Err(unsuported_err(
                ident.span(),
                "unamed structs without type override",
            ));
        },
        syn::Fields::Unit => return Err(unsuported_err(ident.span(), "unit structs")),
    };

    let exported = fieds
        .into_iter()
        .map(|f| {
            let field_name = into_camel_case(&f.0.to_string(), false);
            let expr = field_expr_quote(f.2);
            quote! {
                __fields.push(::arktype_ast::Field {
                    name: #field_name.to_string(),
                    expr: #expr,
                });
            }
        })
        .collect::<Vec<_>>();

    let body = quote! {
        let mut __fields: ::std::vec::Vec<::arktype_ast::Field> = ::std::vec::Vec::new();
        #(#exported)*
        let __schema = ::arktype_ast::Schema::new(
            ::arktype_ast::Expr::TypeCall(Box::new(::arktype_ast::Expr::Object(__fields))),
        );
        __visited.insert(__ident_name.clone(), __schema);
    };
    Ok(body)
}

pub(crate) fn into_camel_case(
    opt: &str,
    capitalize_first: bool,
) -> String {
    let opt: String = opt.replace("::", "_");
    let mut result = String::with_capacity(opt.len());
    let mut capitalize_next = capitalize_first;
    for ch in opt.chars() {
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

fn field_expr_quote(f: ParsedFieldType) -> proc_macro2::TokenStream {
    match f {
        ParsedFieldType::Primitive(primitive) => match primitive {
            PrimitiveType::String => {
                quote! { ::arktype_ast::Expr::Primitive(::arktype_ast::Primitive::String) }
            },
            PrimitiveType::Boolean => {
                quote! { ::arktype_ast::Expr::Primitive(::arktype_ast::Primitive::Boolean) }
            },
            PrimitiveType::Number => {
                quote! { ::arktype_ast::Expr::Primitive(::arktype_ast::Primitive::Number) }
            },
        },
        ParsedFieldType::Array(parsed_field_type) => {
            let inner = field_expr_quote(*parsed_field_type);
            quote! {
                ::arktype_ast::Expr::Array(Box::new(#inner))
            }
        },
        ParsedFieldType::Option(parsed_field_type) => {
            let inner = field_expr_quote(*parsed_field_type);
            quote! {
                ::arktype_ast::Expr::Optional(Box::new(#inner))
            }
        },
        ParsedFieldType::ArkTyped(typ) => {
            quote! {
                {
                    let typed_ident = <#typ>::__arktype_bindings_ident();
                    <#typ>::__arktype_bindings(__visited);
                    ::arktype_ast::Expr::Ref(typed_ident)
                }
            }
        },
    }
}

enum EnumVariant {
    Unit {
        name: String,
    },
    Named {
        name: String,
        fields: Vec<(syn::Ident, syn::Type, ParsedFieldType)>,
    },
}

fn enum_variant_expr(
    variant: EnumVariant,
    serde_tag: Option<&str>,
) -> proc_macro2::TokenStream {
    match variant {
        EnumVariant::Unit { name } => {
            let type_value = format!("\"'{}'\"", name);
            if let Some(tag) = serde_tag {
                quote! {
                    ::arktype_ast::Expr::Object(vec![
                        ::arktype_ast::Field {
                            name: #tag.to_string(),
                            expr: ::arktype_ast::Expr::Raw(#type_value.to_string()),
                        }
                    ])
                }
            } else {
                quote! {
                    ::arktype_ast::Expr::Raw(#type_value.to_string())
                }
            }
        },
        EnumVariant::Named { name, fields } => {
            let type_value = format!("\"'{}'\"", name);
            let exported = fields
                .into_iter()
                .map(|f| {
                    let field_name = into_camel_case(&f.0.to_string(), false);
                    let expr = field_expr_quote(f.2);
                    quote! {
                        ::arktype_ast::Field {
                            name: #field_name.to_string(),
                            expr: #expr,
                        }
                    }
                })
                .collect::<Vec<_>>();

            if let Some(tag) = serde_tag {
                quote! {
                    ::arktype_ast::Expr::Object(vec![
                        ::arktype_ast::Field {
                            name: #tag.to_string(),
                            expr: ::arktype_ast::Expr::Raw(#type_value.to_string()),
                        },
                        #(#exported),*
                    ])
                }
            } else {
                quote! {
                    ::arktype_ast::Expr::Object(vec![
                        #(#exported),*
                    ])
                }
            }
        },
    }
}

fn enum_impl_body(
    _ident: &syn::Ident,
    _name: &str,
    data: DataEnum,
    serde_tag: Option<String>,
) -> Result<proc_macro2::TokenStream, syn::Error> {
    let mut parsed_variants = Vec::new();
    for variant in data.variants {
        let variant_name = variant.ident.to_string();
        let parsed = match variant.fields {
            syn::Fields::Unnamed(_fields_unnamed) => {
                let span = _fields_unnamed.span();
                return Err(unsuported_err(span, "unnamed enum variants"));
            },
            syn::Fields::Named(fields_named) => {
                let fields = parse_named_fields(fields_named)?;
                EnumVariant::Named {
                    name: variant_name,
                    fields,
                }
            },
            syn::Fields::Unit => EnumVariant::Unit { name: variant_name },
        };
        parsed_variants.push(parsed);
    }

    let exported = parsed_variants
        .into_iter()
        .map(|variant| enum_variant_expr(variant, serde_tag.as_deref()))
        .collect::<Vec<_>>();

    let body = quote! {
        let mut __variants: ::std::vec::Vec<::arktype_ast::Expr> = ::std::vec::Vec::new();
        #(
            __variants.push(#exported);
        )*
        let mut __iter = __variants.into_iter();
        let __first = __iter.next().expect("expected at least one enum variant");
        let __base = ::arktype_ast::Expr::TypeCall(Box::new(__first));
        let __others: ::std::vec::Vec<::arktype_ast::Expr> = __iter.collect();
        let __expr = if __others.is_empty() {
            __base
        } else {
            ::arktype_ast::Expr::Or(Box::new(__base), __others)
        };
        let __schema = ::arktype_ast::Schema::new(__expr);
        __visited.insert(__ident_name.clone(), __schema);
    };
    Ok(body)
}

fn parse_named_fields(
    fields_named: syn::FieldsNamed
) -> Result<Vec<(syn::Ident, syn::Type, ParsedFieldType)>, syn::Error> {
    fields_named
        .named
        .into_iter()
        .try_fold(Vec::new(), |mut vec, field| {
            let span = field.span();
            let ty = field.ty;
            let field_ident = field
                .ident
                .ok_or_else(|| syn::Error::new(span, "expected named field"))?;
            let parsed = ParsedFieldType::parse_field_type(&ty)?;
            vec.push((field_ident, ty, parsed));
            Ok::<_, syn::Error>(vec)
        })
}

#[cfg(test)]
mod tests {
    use super::*;
    use syn::parse_quote;

    // === into_camel_case tests ===
    #[test]
    fn into_camel_case_lowercase_first() {
        assert_eq!(into_camel_case("user_name", false), "userName");
    }

    #[test]
    fn into_camel_case_uppercase_first() {
        assert_eq!(into_camel_case("user_name", true), "UserName");
    }

    #[test]
    fn into_camel_case_single_word() {
        assert_eq!(into_camel_case("name", false), "name");
        assert_eq!(into_camel_case("name", true), "Name");
    }

    #[test]
    fn into_camel_case_multiple_underscores() {
        assert_eq!(into_camel_case("get_user_by_id", false), "getUserById");
    }

    #[test]
    fn into_camel_case_with_module_path() {
        assert_eq!(
            into_camel_case("my_module::MyType", false),
            "myModuleMyType"
        );
    }

    #[test]
    fn into_camel_case_already_camel() {
        assert_eq!(into_camel_case("userName", false), "userName");
    }

    #[test]
    fn into_camel_case_empty_string() {
        assert_eq!(into_camel_case("", false), "");
    }

    #[test]
    fn into_camel_case_trailing_underscore() {
        assert_eq!(into_camel_case("name_", false), "name");
    }

    #[test]
    fn into_camel_case_leading_underscore() {
        assert_eq!(into_camel_case("_name", false), "Name");
    }

    // === data_checked tests ===
    #[test]
    fn data_checked_accepts_simple_struct() {
        let input: DeriveInput = parse_quote! {
            struct User { name: String }
        };
        assert!(data_checked(&input).is_ok());
    }

    #[test]
    fn data_checked_accepts_enum() {
        let input: DeriveInput = parse_quote! {
            enum Status { Active, Inactive }
        };
        assert!(data_checked(&input).is_ok());
    }

    #[test]
    fn data_checked_accepts_struct_multiple_fields() {
        let input: DeriveInput = parse_quote! {
            struct User {
                name: String,
                age: i32,
                active: bool,
            }
        };
        assert!(data_checked(&input).is_ok());
    }

    #[test]
    fn data_checked_rejects_generic_type() {
        let input: DeriveInput = parse_quote! {
            struct Container<T> { value: T }
        };
        let err = data_checked(&input).unwrap_err();
        assert!(err.to_string().contains("generic types"));
    }

    #[test]
    fn data_checked_rejects_multiple_generics() {
        let input: DeriveInput = parse_quote! {
            struct Pair<K, V> { key: K, value: V }
        };
        let err = data_checked(&input).unwrap_err();
        assert!(err.to_string().contains("generic types"));
    }

    #[test]
    fn data_checked_rejects_lifetime() {
        let input: DeriveInput = parse_quote! {
            struct Borrowed<'a> { value: &'a str }
        };
        let err = data_checked(&input).unwrap_err();
        assert!(err.to_string().contains("lifetimes"));
    }

    #[test]
    fn data_checked_rejects_const_generic() {
        let input: DeriveInput = parse_quote! {
            struct FixedArray<const N: usize> { data: [u8; N] }
        };
        let err = data_checked(&input).unwrap_err();
        assert!(err.to_string().contains("const generics"));
    }

    #[test]
    fn data_checked_rejects_union() {
        let input: DeriveInput = parse_quote! {
            union MyUnion { a: i32, b: f32 }
        };
        let err = data_checked(&input).unwrap_err();
        assert!(err.to_string().contains("union"));
    }

    #[test]
    fn data_checked_accepts_enum_with_named_fields() {
        let input: DeriveInput = parse_quote! {
            enum Result {
                Ok { value: String },
                Err { code: i32 },
            }
        };
        assert!(data_checked(&input).is_ok());
    }
}
