//! Integration tests for arktype_ast proc-macro.
//!
//! This crate tests the ArkType derive by using it on real structs/enums
//! and verifying the generated ArkType DSL output.

use arktype_ast::ArkType;
use serde::{Deserialize, Serialize};

#[derive(ArkType)]
pub struct SimpleStruct {
    pub name: String,
    pub age: i32,
    pub active: bool,
}

#[derive(ArkType)]
pub struct AllPrimitives {
    pub string_field: String,
    pub bool_field: bool,
    pub i8_field: i8,
    pub i16_field: i16,
    pub i32_field: i32,
    pub u8_field: u8,
    pub u16_field: u16,
    pub u32_field: u32,
    pub f32_field: f32,
}

#[derive(ArkType)]
pub struct OptionalFields {
    pub required: String,
    pub optional: Option<String>,
    pub optional_number: Option<i32>,
    pub optional_bool: Option<bool>,
}

#[derive(ArkType)]
pub struct ArrayFields {
    pub items: Vec<String>,
    pub numbers: Vec<i32>,
    pub bools: Vec<bool>,
}

#[derive(ArkType)]
pub struct NestedOption {
    pub data: Option<Vec<String>>,
}

#[derive(ArkType)]
pub struct NestedArray {
    pub matrix: Vec<Vec<i32>>,
}

#[derive(ArkType)]
pub struct VecOfOption {
    pub items: Vec<Option<String>>,
}

#[derive(ArkType)]
pub struct OptionOfOption {
    pub maybe_maybe: Option<Option<String>>,
}

#[derive(ArkType)]
#[arktype(type_override = "\"custom.type\"")]
#[allow(dead_code)]
pub struct OverriddenType(String);

#[derive(ArkType)]
pub struct Inner {
    pub value: i32,
}

#[derive(ArkType)]
pub struct Outer {
    pub inner: Inner,
    pub name: String,
}

#[derive(ArkType)]
pub struct DeeplyNested {
    pub outer: Outer,
    pub count: i32,
}

#[derive(Serialize, Deserialize, ArkType)]
pub enum UnitEnum {
    Active,
    Inactive,
    Pending,
}

#[derive(Serialize, Deserialize, ArkType)]
pub enum NamedEnum {
    Success { data: String },
    Error { code: i32, message: String },
}

#[derive(Serialize, Deserialize, ArkType)]
pub enum MixedEnum {
    Empty,
    WithData { value: i32 },
    WithMultiple { name: String, count: i32 },
}

#[derive(Serialize, Deserialize, ArkType)]
pub enum SingleVariant {
    Only { value: String },
}

#[derive(Serialize, Deserialize, ArkType)]
#[serde(tag = "kind")]
pub enum TaggedEnum {
    Alpha,
    Beta { value: String },
}

#[derive(ArkType)]
pub struct SnakeCaseFields {
    pub user_name: String,
    pub created_at: String,
    pub is_active: bool,
    pub total_count: i32,
}

#[derive(ArkType)]
pub struct BoxedPrimitive {
    pub boxed_string: Box<String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn render_first(bindings: &::arktype_ast::SchemaMap) -> String {
        let (name, schema) = bindings.iter().next().unwrap();
        schema.render(name)
    }

    #[test]
    fn simple_struct_has_all_fields() {
        let bindings = SimpleStruct::arktype_bindings();
        let output = render_first(&bindings);

        assert!(output.contains("name: type.string"), "Missing name field");
        assert!(output.contains("age: type.number"), "Missing age field");
        assert!(
            output.contains("active: type.boolean"),
            "Missing active field"
        );
    }

    #[test]
    fn simple_struct_has_correct_format() {
        let bindings = SimpleStruct::arktype_bindings();
        let output = render_first(&bindings);

        assert!(output.contains("const "), "Missing const declaration");
        assert!(output.contains("= type({"), "Missing type declaration");
        assert!(output.contains("})"), "Missing closing brace");
        assert!(output.contains("type "), "Missing type alias");
        assert!(output.contains(".infer"), "Missing .infer");
    }

    #[test]
    fn all_primitives_map_correctly() {
        let bindings = AllPrimitives::arktype_bindings();
        let output = render_first(&bindings);

        assert!(output.contains("stringField: type.string"));
        assert!(output.contains("boolField: type.boolean"));
        assert!(output.contains("i8Field: type.number"));
        assert!(output.contains("i16Field: type.number"));
        assert!(output.contains("i32Field: type.number"));
        assert!(output.contains("u8Field: type.number"));
        assert!(output.contains("u16Field: type.number"));
        assert!(output.contains("u32Field: type.number"));
        assert!(output.contains("f32Field: type.number"));
    }

    #[test]
    fn optional_fields_use_or_null() {
        let bindings = OptionalFields::arktype_bindings();
        let output = render_first(&bindings);

        assert!(
            output.contains("required: type.string"),
            "Required field wrong"
        );
        assert!(
            output.contains(".or(type.null)"),
            "Missing .or(type.null) for optional"
        );
    }

    #[test]
    fn optional_field_count() {
        let bindings = OptionalFields::arktype_bindings();
        let output = render_first(&bindings);

        // Count occurrences of .or(type.null) - should be 3 (optional, optional_number, optional_bool)
        let count = output.matches(".or(type.null)").count();
        assert_eq!(count, 3, "Expected 3 optional fields, got {}", count);
    }

    #[test]
    fn array_fields_use_array() {
        let bindings = ArrayFields::arktype_bindings();
        let output = render_first(&bindings);

        assert!(
            output.contains(".array()"),
            "Missing .array() for Vec fields"
        );
    }

    #[test]
    fn array_field_count() {
        let bindings = ArrayFields::arktype_bindings();
        let output = render_first(&bindings);

        let count = output.matches(".array()").count();
        assert_eq!(count, 3, "Expected 3 array fields, got {}", count);
    }

    #[test]
    fn nested_option_vec() {
        let bindings = NestedOption::arktype_bindings();
        let output = render_first(&bindings);

        // Option<Vec<String>> should produce .array().or(type.null)
        assert!(output.contains(".array()"), "Missing .array()");
        assert!(output.contains(".or(type.null)"), "Missing .or(type.null)");
    }

    #[test]
    fn nested_array_vec() {
        let bindings = NestedArray::arktype_bindings();
        let output = render_first(&bindings);

        // Vec<Vec<i32>> should produce .array().array()
        let count = output.matches(".array()").count();
        assert_eq!(
            count, 2,
            "Expected 2 .array() for nested Vec, got {}",
            count
        );
    }

    #[test]
    fn vec_of_option() {
        let bindings = VecOfOption::arktype_bindings();
        let output = render_first(&bindings);

        // Vec<Option<String>> should produce .or(type.null).array()
        assert!(output.contains(".or(type.null)"));
        assert!(output.contains(".array()"));
    }

    #[test]
    fn type_override_uses_custom_type() {
        let bindings = OverriddenType::arktype_bindings();
        let output = render_first(&bindings);

        assert!(
            output.contains("\"custom.type\""),
            "Type override not applied"
        );
    }

    #[test]
    fn nested_struct_includes_both() {
        let bindings = Outer::arktype_bindings();

        // Should have both Inner and Outer types
        assert_eq!(bindings.len(), 2, "Expected 2 types (Inner and Outer)");
    }

    #[test]
    fn nested_struct_references_inner() {
        let bindings = Outer::arktype_bindings();

        // Find the Outer binding
        let outer_binding = bindings
            .iter()
            .find(|(k, _)| k.contains("Outer"))
            .map(|(_, v)| v);
        assert!(outer_binding.is_some(), "Outer binding not found");

        let output = outer_binding.unwrap().render("Outer");
        // Should reference Inner type
        assert!(output.contains("Inner"), "Outer should reference Inner");
    }

    #[test]
    fn deeply_nested_includes_all() {
        let bindings = DeeplyNested::arktype_bindings();

        // Should have Inner, Outer, and DeeplyNested
        assert_eq!(bindings.len(), 3, "Expected 3 types for deeply nested");
    }

    #[test]
    fn unit_enum_has_all_variants() {
        let bindings = UnitEnum::arktype_bindings();
        let output = render_first(&bindings);

        assert!(output.contains("'Active'"), "Missing Active variant");
        assert!(output.contains("'Inactive'"), "Missing Inactive variant");
        assert!(output.contains("'Pending'"), "Missing Pending variant");
    }

    #[test]
    fn unit_enum_uses_or_for_variants() {
        let bindings = UnitEnum::arktype_bindings();
        let output = render_first(&bindings);

        // First variant uses type('Variant'), subsequent use .or('Variant')
        assert!(output.contains("type(\"'"), "Missing initial type literal");
        assert!(
            output.contains(".or(\"'"),
            "Missing .or literal for subsequent variants"
        );
    }

    #[test]
    fn named_enum_has_variant_names() {
        let bindings = NamedEnum::arktype_bindings();
        let output = render_first(&bindings);

        assert!(
            output.contains("type({ data: type.string"),
            "Missing Success variant shape"
        );
        assert!(
            output.contains(".or({ code: type.number"),
            "Missing Error variant shape"
        );
    }

    #[test]
    fn named_enum_has_fields() {
        let bindings = NamedEnum::arktype_bindings();
        let output = render_first(&bindings);

        assert!(output.contains("data: type.string"), "Missing data field");
        assert!(output.contains("code: type.number"), "Missing code field");
        assert!(
            output.contains("message: type.string"),
            "Missing message field"
        );
    }

    #[test]
    fn mixed_enum_handles_both_types() {
        let bindings = MixedEnum::arktype_bindings();
        let output = render_first(&bindings);

        assert!(
            output.contains("type(\"'Empty'\")"),
            "Missing Empty variant"
        );
        assert!(
            output.contains(".or({ value: type.number"),
            "Missing WithData variant"
        );
        assert!(
            output.contains(".or({ name: type.string"),
            "Missing WithMultiple variant"
        );
        assert!(output.contains("value: type.number"), "Missing value field");
    }

    #[test]
    fn snake_case_converts_to_camel_case() {
        let bindings = SnakeCaseFields::arktype_bindings();
        let output = render_first(&bindings);

        assert!(
            output.contains("userName:"),
            "user_name should become userName"
        );
        assert!(
            output.contains("createdAt:"),
            "created_at should become createdAt"
        );
        assert!(
            output.contains("isActive:"),
            "is_active should become isActive"
        );
        assert!(
            output.contains("totalCount:"),
            "total_count should become totalCount"
        );
    }

    #[test]
    fn no_snake_case_in_output() {
        let bindings = SnakeCaseFields::arktype_bindings();
        let output = render_first(&bindings);

        assert!(
            !output.contains("user_name"),
            "Should not contain user_name"
        );
        assert!(
            !output.contains("created_at"),
            "Should not contain created_at"
        );
        assert!(
            !output.contains("is_active"),
            "Should not contain is_active"
        );
        assert!(
            !output.contains("total_count"),
            "Should not contain total_count"
        );
    }

    #[test]
    fn ident_function_returns_pascal_case() {
        let ident = SimpleStruct::__arktype_bindings_ident();
        assert!(
            ident.contains("SimpleStruct"),
            "Ident should contain SimpleStruct"
        );
    }

    #[test]
    fn ident_uses_dollar_for_modules() {
        let ident = SimpleStruct::__arktype_bindings_ident();
        // Module path separators become $
        assert!(
            ident.contains("$"),
            "Ident should use $ as module separator"
        );
    }

    #[test]
    fn boxed_primitive_is_peeled() {
        let bindings = BoxedPrimitive::arktype_bindings();
        let output = render_first(&bindings);

        // Box<String> should just become type.string
        assert!(
            output.contains("boxedString: type.string"),
            "Box String should become type.string"
        );
    }

    #[test]
    fn boxed_primitive_single_binding() {
        let bindings = BoxedPrimitive::arktype_bindings();
        assert_eq!(bindings.len(), 1, "BoxedPrimitive should have 1 binding");
    }

    #[test]
    fn single_variant_enum_works() {
        let bindings = SingleVariant::arktype_bindings();
        let output = render_first(&bindings);

        assert!(output.contains("value: type.string"), "Missing value field");
    }

    #[test]
    fn tagged_enum_uses_custom_tag() {
        let bindings = TaggedEnum::arktype_bindings();
        let output = render_first(&bindings);

        assert!(output.contains("kind: \"'Alpha'\""));
        assert!(output.contains("kind: \"'Beta'\""));
    }

    #[test]
    fn simple_struct_single_binding() {
        let bindings = SimpleStruct::arktype_bindings();
        assert_eq!(bindings.len(), 1, "Simple struct should have 1 binding");
    }

    #[test]
    fn enum_single_binding() {
        let bindings = UnitEnum::arktype_bindings();
        assert_eq!(bindings.len(), 1, "Enum should have 1 binding");
    }
}
