use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{
    parse_macro_input, AngleBracketedGenericArguments, Attribute, Data, DeriveInput, Fields,
    GenericArgument, ItemFn, Path, PathArguments, ReturnType, Type, TypePath,
};

#[proc_macro_attribute]
pub fn handler(
    attr: TokenStream,
    item: TokenStream,
) -> TokenStream {
    // Parse attribute like #[handler(v0)]
    let version: Path = parse_macro_input!(attr as Path);

    // Only support free functions for now
    let func = match syn::parse::<ItemFn>(item.clone()) {
        Ok(f) => f,
        Err(_) => {
            return syn::Error::new(
                Span::call_site(),
                "#[handler(...)] can only be applied to free functions",
            )
            .to_compile_error()
            .into();
        },
    };

    // Validate async
    if func.sig.asyncness.is_none() {
        return syn::Error::new_spanned(func.sig.fn_token, "rpc handlers must be async fn")
            .to_compile_error()
            .into();
    }

    // Exactly two parameters: &context or &mut context, props
    if func.sig.inputs.len() != 2 {
        return syn::Error::new_spanned(
            &func.sig.inputs,
            "rpc handlers must take exactly two parameters: &context or &mut context then props",
        )
        .to_compile_error()
        .into();
    }

    let ctx_ty: Type = match func.sig.inputs.first().unwrap() {
        syn::FnArg::Typed(pat_ty) => (*pat_ty.ty).clone(),
        syn::FnArg::Receiver(_) => {
            return syn::Error::new_spanned(
                &func.sig.inputs,
                "rpc handlers must be free functions (no self/receiver)",
            )
            .to_compile_error()
            .into();
        },
    };

    let ctx_ty = match ctx_ty {
        Type::Reference(reference) => (*reference.elem).clone(),
        other => {
            return syn::Error::new_spanned(
                other,
                "rpc handlers must take &Context or &mut Context as the first parameter",
            )
            .to_compile_error()
            .into();
        },
    };

    let props_ty: Type = match func.sig.inputs.iter().nth(1).unwrap() {
        syn::FnArg::Typed(pat_ty) => (*pat_ty.ty).clone(),
        syn::FnArg::Receiver(_) => {
            return syn::Error::new_spanned(
                &func.sig.inputs,
                "rpc handlers must be free functions (no self/receiver)",
            )
            .to_compile_error()
            .into();
        },
    };

    // Extract Ok type from Result<Ok, errx::Error>
    let ok_ty = match extract_ok_type(&func.sig.output) {
        Ok(t) => t,
        Err(e) => return e.to_compile_error().into(),
    };

    let fn_ident = func.sig.ident.clone();

    // If your macro path isn't ::rpc::impl_rpc!, change it here.
    let expanded = quote! {
        #func

        ::rpc::impl_rpc!(#version, #props_ty, #ok_ty, #ctx_ty, #fn_ident);
    };

    expanded.into()
}

fn extract_ok_type(ret: &ReturnType) -> syn::Result<Type> {
    let ty = match ret {
        ReturnType::Default => {
            return Err(syn::Error::new(
                Span::call_site(),
                "rpc handlers must return Result<Ok, errx::Error>",
            ));
        },
        ReturnType::Type(_, ty) => ty,
    };

    // Expect Result<Ok, errx::Error>
    let (ok_ty, err_ty) = match ty.as_ref() {
        Type::Path(TypePath { path, .. }) => {
            let last = path.segments.last().ok_or_else(|| {
                syn::Error::new_spanned(path, "invalid return type for rpc handler")
            })?;

            if last.ident != "Result" {
                return Err(syn::Error::new_spanned(
                    &last.ident,
                    "rpc handlers must return Result<Ok, errx::Error>",
                ));
            }

            let args = match &last.arguments {
                PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) => args,
                _ => {
                    return Err(syn::Error::new_spanned(
                        &last.arguments,
                        "Result must have generic args: Result<Ok, errx::Error>",
                    ));
                },
            };

            let mut types = args.iter().filter_map(|ga| {
                if let GenericArgument::Type(t) = ga {
                    Some(t.clone())
                } else {
                    None
                }
            });

            let ok_ty = types.next().ok_or_else(|| {
                syn::Error::new_spanned(
                    args,
                    "Result must be Result<Ok, errx::Error> (missing Ok type)",
                )
            })?;

            let err_ty = types.next().ok_or_else(|| {
                syn::Error::new_spanned(
                    args,
                    "Result must be Result<Ok, errx::Error> (missing error type)",
                )
            })?;

            (ok_ty, err_ty)
        },
        other => {
            return Err(syn::Error::new_spanned(
                other,
                "rpc handlers must return Result<Ok, errx::Error>",
            ));
        },
    };

    // Enforce errx::Error specifically
    if !is_errx_error_type(&err_ty) {
        return Err(syn::Error::new_spanned(
            err_ty,
            "rpc handlers must use errx::Error as the error type",
        ));
    }

    Ok(ok_ty)
}

fn is_errx_error_type(ty: &Type) -> bool {
    let Type::Path(TypePath { path, .. }) = ty else {
        return false;
    };

    // Accept errx::Error or ::errx::Error
    let segs = &path.segments;
    if segs.len() < 2 {
        return false;
    }
    let last = segs.last().unwrap().ident.to_string();
    let prev = segs.iter().nth(segs.len() - 2).unwrap().ident.to_string();
    prev == "errx" && last == "Error"
}

#[proc_macro_derive(Response, attributes(rpc))]
pub fn derive_response(item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as DeriveInput);
    let name = input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let default_code = match parse_rpc_code(&input.attrs) {
        Ok(code) => code.unwrap_or(200),
        Err(err) => return err.to_compile_error().into(),
    };

    let body = match input.data {
        Data::Struct(_) => {
            quote! { #default_code }
        },
        Data::Enum(data) => {
            let mut arms = Vec::with_capacity(data.variants.len());
            for variant in data.variants.iter() {
                let variant_code = match parse_rpc_code(&variant.attrs) {
                    Ok(code) => code.unwrap_or(default_code),
                    Err(err) => return err.to_compile_error().into(),
                };
                let ident = &variant.ident;
                let pat = match &variant.fields {
                    Fields::Named(_) => quote! { Self::#ident { .. } },
                    Fields::Unnamed(_) => quote! { Self::#ident ( .. ) },
                    Fields::Unit => quote! { Self::#ident },
                };
                arms.push(quote! { #pat => #variant_code });
            }

            quote! {
                match self {
                    #(#arms),*
                }
            }
        },
        Data::Union(_) => {
            return syn::Error::new_spanned(name, "Response derive is not supported for unions")
                .to_compile_error()
                .into();
        },
    };

    let expanded = quote! {
        impl #impl_generics ::rpc::Response for #name #ty_generics #where_clause {
            fn code(&self) -> u16 {
                #body
            }
        }
    };

    expanded.into()
}

fn parse_rpc_code(attrs: &[Attribute]) -> syn::Result<Option<u16>> {
    let mut code = None;
    for attr in attrs.iter().filter(|attr| attr.path().is_ident("rpc")) {
        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("code") {
                if code.is_some() {
                    return Err(meta.error("duplicate rpc code"));
                }
                let value = meta.value()?;
                let lit: syn::LitInt = value.parse()?;
                let parsed = lit.base10_parse::<u16>()?;
                if !(100..=599).contains(&parsed) {
                    return Err(meta.error("rpc code must be in 100..=599"));
                }
                code = Some(parsed);
                Ok(())
            } else {
                Err(meta.error("unsupported rpc attribute"))
            }
        })?;
    }
    Ok(code)
}
