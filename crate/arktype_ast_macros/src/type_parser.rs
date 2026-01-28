use syn::spanned::Spanned;

fn path_ident_is(
    path: &syn::Path,
    want: &str,
) -> bool {
    path.segments
        .last()
        .map(|s| s.ident == want)
        .unwrap_or(false)
}

fn is_box_path(tp: &syn::TypePath) -> bool {
    path_ident_is(&tp.path, "Box")
}

fn one_generic_arg_type(tp: &syn::TypePath) -> Option<&syn::Type> {
    let last = tp.path.segments.last()?;
    if let syn::PathArguments::AngleBracketed(ab) = &last.arguments {
        for arg in &ab.args {
            if let syn::GenericArgument::Type(t) = arg {
                return Some(t);
            }
        }
    }
    None
}

fn extract_option_inner(ty: &syn::Type) -> Option<&syn::Type> {
    if let syn::Type::Path(tp) = ty
        && path_ident_is(&tp.path, "Option")
    {
        return one_generic_arg_type(tp);
    }
    None
}

fn extract_vec_inner(ty: &syn::Type) -> Option<&syn::Type> {
    if let syn::Type::Path(tp) = ty
        && path_ident_is(&tp.path, "Vec")
    {
        return one_generic_arg_type(tp);
    }
    None
}

fn map_primitive_dsl(ty: &syn::Type) -> Result<Option<PrimitiveType>, syn::Error> {
    match ty {
        syn::Type::Path(tp) => {
            let last = match tp.path.segments.last() {
                Some(l) => l,
                None => return Ok(None),
            };
            let id = last.ident.to_string();

            // Map chrono/chronos DateTime<Utc> -> "string"
            // This matches both fully-qualified and imported forms:
            // - chrono::DateTime<chrono::Utc>
            // - DateTime<Utc>
            if id.as_str() == "DateTime"
                && let syn::PathArguments::AngleBracketed(ab) = &last.arguments
            {
                for arg in &ab.args {
                    if let syn::GenericArgument::Type(syn::Type::Path(tz_path)) = arg
                        && tz_path.path.segments.last().map(|s| s.ident == "Utc") == Some(true)
                    {
                        return Ok(Some(PrimitiveType::String));
                    }
                }
            }

            match id.as_str() {
                "String" => Ok(Some(PrimitiveType::String)),
                "bool" => Ok(Some(PrimitiveType::Boolean)),
                "i8" | "i16" | "i32" | "u8" | "u16" | "u32" | "f32" => {
                    Ok(Some(PrimitiveType::Number))
                },
                "i64" | "i128" | "isize" | "u64" | "u128" | "usize" | "f64" => Err(
                    syn::Error::new(ty.span(), format!("Integer {id} is not supported")),
                ),
                _ => Ok(None),
            }
        },
        syn::Type::Reference(tr) => {
            // &str
            if let syn::Type::Path(tp) = tr.elem.as_ref() {
                let last = match tp.path.segments.last() {
                    Some(l) => l,
                    None => return Ok(None),
                };
                let id = last.ident.to_string();
                if id.as_str() == "str" {
                    return Ok(Some(PrimitiveType::String));
                }
            }
            Ok(None)
        },
        _ => Ok(None),
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PrimitiveType {
    String,
    Boolean,
    Number,
}

#[allow(clippy::large_enum_variant)]
pub enum ParsedFieldType {
    Primitive(PrimitiveType),
    Array(Box<ParsedFieldType>),
    Option(Box<ParsedFieldType>),
    ArkTyped(syn::Type),
}

impl ParsedFieldType {
    pub fn parse_field_type(input_type: &syn::Type) -> Result<Self, syn::Error> {
        let mut peel = true;
        let mut ty = input_type;
        while peel {
            peel = false;
            if let syn::Type::Reference(syn::TypeReference { elem, .. }) = ty {
                ty = elem;
                peel = true;
            }
            if let syn::Type::Path(tp) = ty
                && is_box_path(tp)
                && let Some(inner) = one_generic_arg_type(tp)
            {
                ty = inner;
                peel = true;
            }
        }

        if let Some(inner) = extract_vec_inner(ty) {
            let inner = Self::parse_field_type(inner)?;
            return Ok(Self::Array(Box::new(inner)));
        }

        if let syn::Type::Array(syn::TypeArray { elem, .. }) = ty {
            let inner = Self::parse_field_type(elem)?;
            return Ok(Self::Array(Box::new(inner)));
        }

        if let Some(inner) = extract_option_inner(ty) {
            let inner = Self::parse_field_type(inner)?;
            return Ok(Self::Option(Box::new(inner)));
        }

        if let Some(ts_prim) = map_primitive_dsl(ty)? {
            return Ok(Self::Primitive(ts_prim));
        }

        // We assume the type is another struct or enum tagged with arktype macro
        // Of course the checks above don't cover every case so this will need to be improved at some point
        // Not a huge deal since any cases that we didn't check will cause a compile error
        Ok(Self::ArkTyped(input_type.clone()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use syn::parse_quote;

    // === path_ident_is tests ===
    #[test]
    fn path_ident_is_matches_simple() {
        let path: syn::Path = parse_quote!(Option);
        assert!(path_ident_is(&path, "Option"));
    }

    #[test]
    fn path_ident_is_matches_qualified() {
        let path: syn::Path = parse_quote!(std::option::Option);
        assert!(path_ident_is(&path, "Option"));
    }

    #[test]
    fn path_ident_is_no_match() {
        let path: syn::Path = parse_quote!(Vec);
        assert!(!path_ident_is(&path, "Option"));
    }

    // === is_box_path tests ===
    #[test]
    fn is_box_path_true() {
        let tp: syn::TypePath = parse_quote!(Box<String>);
        assert!(is_box_path(&tp));
    }

    #[test]
    fn is_box_path_false() {
        let tp: syn::TypePath = parse_quote!(Vec<String>);
        assert!(!is_box_path(&tp));
    }

    // === one_generic_arg_type tests ===
    #[test]
    fn one_generic_arg_type_extracts_inner() {
        let tp: syn::TypePath = parse_quote!(Option<String>);
        let inner = one_generic_arg_type(&tp);
        assert!(inner.is_some());
    }

    #[test]
    fn one_generic_arg_type_none_for_no_generics() {
        let tp: syn::TypePath = parse_quote!(String);
        let inner = one_generic_arg_type(&tp);
        assert!(inner.is_none());
    }

    // === extract_option_inner tests ===
    #[test]
    fn extract_option_inner_works() {
        let ty: syn::Type = parse_quote!(Option<String>);
        assert!(extract_option_inner(&ty).is_some());
    }

    #[test]
    fn extract_option_inner_none_for_vec() {
        let ty: syn::Type = parse_quote!(Vec<String>);
        assert!(extract_option_inner(&ty).is_none());
    }

    // === extract_vec_inner tests ===
    #[test]
    fn extract_vec_inner_works() {
        let ty: syn::Type = parse_quote!(Vec<i32>);
        assert!(extract_vec_inner(&ty).is_some());
    }

    #[test]
    fn extract_vec_inner_none_for_option() {
        let ty: syn::Type = parse_quote!(Option<i32>);
        assert!(extract_vec_inner(&ty).is_none());
    }

    // === map_primitive_dsl tests ===
    #[test]
    fn map_primitive_string() {
        let ty: syn::Type = parse_quote!(String);
        assert_eq!(map_primitive_dsl(&ty).unwrap(), Some(PrimitiveType::String));
    }

    #[test]
    fn map_primitive_bool() {
        let ty: syn::Type = parse_quote!(bool);
        assert_eq!(
            map_primitive_dsl(&ty).unwrap(),
            Some(PrimitiveType::Boolean)
        );
    }

    #[test]
    fn map_primitive_i32() {
        let ty: syn::Type = parse_quote!(i32);
        assert_eq!(map_primitive_dsl(&ty).unwrap(), Some(PrimitiveType::Number));
    }

    #[test]
    fn map_primitive_u16() {
        let ty: syn::Type = parse_quote!(u16);
        assert_eq!(map_primitive_dsl(&ty).unwrap(), Some(PrimitiveType::Number));
    }

    #[test]
    fn map_primitive_f32() {
        let ty: syn::Type = parse_quote!(f32);
        assert_eq!(map_primitive_dsl(&ty).unwrap(), Some(PrimitiveType::Number));
    }

    #[test]
    fn map_primitive_i8() {
        let ty: syn::Type = parse_quote!(i8);
        assert_eq!(map_primitive_dsl(&ty).unwrap(), Some(PrimitiveType::Number));
    }

    #[test]
    fn map_primitive_u8() {
        let ty: syn::Type = parse_quote!(u8);
        assert_eq!(map_primitive_dsl(&ty).unwrap(), Some(PrimitiveType::Number));
    }

    #[test]
    fn map_primitive_i16() {
        let ty: syn::Type = parse_quote!(i16);
        assert_eq!(map_primitive_dsl(&ty).unwrap(), Some(PrimitiveType::Number));
    }

    #[test]
    fn map_primitive_u32() {
        let ty: syn::Type = parse_quote!(u32);
        assert_eq!(map_primitive_dsl(&ty).unwrap(), Some(PrimitiveType::Number));
    }

    #[test]
    fn map_primitive_i64_error() {
        let ty: syn::Type = parse_quote!(i64);
        assert!(map_primitive_dsl(&ty).is_err());
    }

    #[test]
    fn map_primitive_u64_error() {
        let ty: syn::Type = parse_quote!(u64);
        assert!(map_primitive_dsl(&ty).is_err());
    }

    #[test]
    fn map_primitive_i128_error() {
        let ty: syn::Type = parse_quote!(i128);
        assert!(map_primitive_dsl(&ty).is_err());
    }

    #[test]
    fn map_primitive_u128_error() {
        let ty: syn::Type = parse_quote!(u128);
        assert!(map_primitive_dsl(&ty).is_err());
    }

    #[test]
    fn map_primitive_isize_error() {
        let ty: syn::Type = parse_quote!(isize);
        assert!(map_primitive_dsl(&ty).is_err());
    }

    #[test]
    fn map_primitive_usize_error() {
        let ty: syn::Type = parse_quote!(usize);
        assert!(map_primitive_dsl(&ty).is_err());
    }

    #[test]
    fn map_primitive_f64_error() {
        let ty: syn::Type = parse_quote!(f64);
        assert!(map_primitive_dsl(&ty).is_err());
    }

    #[test]
    fn map_primitive_str_reference() {
        let ty: syn::Type = parse_quote!(&str);
        assert_eq!(map_primitive_dsl(&ty).unwrap(), Some(PrimitiveType::String));
    }

    #[test]
    fn map_primitive_datetime_utc() {
        let ty: syn::Type = parse_quote!(DateTime<Utc>);
        assert_eq!(map_primitive_dsl(&ty).unwrap(), Some(PrimitiveType::String));
    }

    #[test]
    fn map_primitive_datetime_qualified() {
        let ty: syn::Type = parse_quote!(chrono::DateTime<chrono::Utc>);
        assert_eq!(map_primitive_dsl(&ty).unwrap(), Some(PrimitiveType::String));
    }

    #[test]
    fn map_primitive_custom_none() {
        let ty: syn::Type = parse_quote!(MyCustomType);
        assert_eq!(map_primitive_dsl(&ty).unwrap(), None);
    }

    // === ParsedFieldType::parse_field_type tests ===
    #[test]
    fn parse_field_type_primitive() {
        let ty: syn::Type = parse_quote!(String);
        let parsed = ParsedFieldType::parse_field_type(&ty).unwrap();
        assert!(matches!(
            parsed,
            ParsedFieldType::Primitive(PrimitiveType::String)
        ));
    }

    #[test]
    fn parse_field_type_bool() {
        let ty: syn::Type = parse_quote!(bool);
        let parsed = ParsedFieldType::parse_field_type(&ty).unwrap();
        assert!(matches!(
            parsed,
            ParsedFieldType::Primitive(PrimitiveType::Boolean)
        ));
    }

    #[test]
    fn parse_field_type_option() {
        let ty: syn::Type = parse_quote!(Option<String>);
        let parsed = ParsedFieldType::parse_field_type(&ty).unwrap();
        assert!(matches!(parsed, ParsedFieldType::Option(_)));
    }

    #[test]
    fn parse_field_type_vec() {
        let ty: syn::Type = parse_quote!(Vec<i32>);
        let parsed = ParsedFieldType::parse_field_type(&ty).unwrap();
        assert!(matches!(parsed, ParsedFieldType::Array(_)));
    }

    #[test]
    fn parse_field_type_fixed_array() {
        let ty: syn::Type = parse_quote!([i32; 5]);
        let parsed = ParsedFieldType::parse_field_type(&ty).unwrap();
        assert!(matches!(parsed, ParsedFieldType::Array(_)));
    }

    #[test]
    fn parse_field_type_option_vec() {
        let ty: syn::Type = parse_quote!(Option<Vec<String>>);
        let parsed = ParsedFieldType::parse_field_type(&ty).unwrap();
        if let ParsedFieldType::Option(inner) = parsed {
            assert!(matches!(*inner, ParsedFieldType::Array(_)));
        } else {
            panic!("Expected Option variant");
        }
    }

    #[test]
    fn parse_field_type_vec_option() {
        let ty: syn::Type = parse_quote!(Vec<Option<i32>>);
        let parsed = ParsedFieldType::parse_field_type(&ty).unwrap();
        if let ParsedFieldType::Array(inner) = parsed {
            assert!(matches!(*inner, ParsedFieldType::Option(_)));
        } else {
            panic!("Expected Array variant");
        }
    }

    #[test]
    fn parse_field_type_box_peeled() {
        let ty: syn::Type = parse_quote!(Box<String>);
        let parsed = ParsedFieldType::parse_field_type(&ty).unwrap();
        assert!(matches!(parsed, ParsedFieldType::Primitive(_)));
    }

    #[test]
    fn parse_field_type_reference_peeled() {
        let ty: syn::Type = parse_quote!(&String);
        let parsed = ParsedFieldType::parse_field_type(&ty).unwrap();
        assert!(matches!(parsed, ParsedFieldType::Primitive(_)));
    }

    #[test]
    fn parse_field_type_box_reference_peeled() {
        let ty: syn::Type = parse_quote!(&Box<String>);
        let parsed = ParsedFieldType::parse_field_type(&ty).unwrap();
        assert!(matches!(parsed, ParsedFieldType::Primitive(_)));
    }

    #[test]
    fn parse_field_type_custom() {
        let ty: syn::Type = parse_quote!(MyStruct);
        let parsed = ParsedFieldType::parse_field_type(&ty).unwrap();
        assert!(matches!(parsed, ParsedFieldType::ArkTyped(_)));
    }

    #[test]
    fn parse_field_type_option_custom() {
        let ty: syn::Type = parse_quote!(Option<MyStruct>);
        let parsed = ParsedFieldType::parse_field_type(&ty).unwrap();
        if let ParsedFieldType::Option(inner) = parsed {
            assert!(matches!(*inner, ParsedFieldType::ArkTyped(_)));
        } else {
            panic!("Expected Option variant");
        }
    }

    #[test]
    fn parse_field_type_vec_custom() {
        let ty: syn::Type = parse_quote!(Vec<MyStruct>);
        let parsed = ParsedFieldType::parse_field_type(&ty).unwrap();
        if let ParsedFieldType::Array(inner) = parsed {
            assert!(matches!(*inner, ParsedFieldType::ArkTyped(_)));
        } else {
            panic!("Expected Array variant");
        }
    }

    #[test]
    fn parse_field_type_deeply_nested() {
        let ty: syn::Type = parse_quote!(Option<Vec<Option<String>>>);
        let parsed = ParsedFieldType::parse_field_type(&ty).unwrap();
        if let ParsedFieldType::Option(inner) = parsed {
            if let ParsedFieldType::Array(inner2) = *inner {
                if let ParsedFieldType::Option(inner3) = *inner2 {
                    assert!(matches!(*inner3, ParsedFieldType::Primitive(_)));
                } else {
                    panic!("Expected innermost Option");
                }
            } else {
                panic!("Expected Array");
            }
        } else {
            panic!("Expected Option");
        }
    }
}
