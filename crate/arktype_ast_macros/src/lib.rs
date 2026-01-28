use proc_macro::TokenStream;
use syn::{DeriveInput, LitStr, parse_macro_input};

mod schema;
mod type_parser;

#[proc_macro_derive(ArkType, attributes(arktype))]
pub fn arktype_derive(input: TokenStream) -> TokenStream {
    let input_ast = parse_macro_input!(input as DeriveInput);
    let type_override = match parse_type_override(&input_ast.attrs) {
        Ok(value) => value,
        Err(err) => return err.to_compile_error().into(),
    };

    let out = schema::generate(input_ast, type_override);
    out.into()
}

fn parse_type_override(attrs: &[syn::Attribute]) -> Result<Option<String>, syn::Error> {
    let mut type_override = None;

    for attr in attrs {
        if !attr.path().is_ident("arktype") {
            continue;
        }

        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("type_override") {
                let val: LitStr = meta.value()?.parse()?;
                type_override = Some(val.value());
                Ok(())
            } else {
                Err(meta.error("unsupported arktype property"))
            }
        })?;
    }

    Ok(type_override)
}
