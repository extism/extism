use std::iter;

use manyhow::{ensure, error_message, manyhow, Result};
use proc_macro_crate::{crate_name, FoundCrate};
use quote::{format_ident, quote, ToTokens};
use syn::{parse_quote, Attribute, DeriveInput, Path};

/// Tries to resolve the path to `extism_convert` dynamically, falling back to feature flags when unsuccessful.
fn convert_path() -> Path {
    match (
        crate_name("extism"),
        crate_name("extism-convert"),
        crate_name("extism-pdk"),
    ) {
        (Ok(FoundCrate::Name(name)), ..) => {
            let ident = format_ident!("{name}");
            parse_quote!(::#ident::convert)
        }
        (_, Ok(FoundCrate::Name(name)), ..) | (.., Ok(FoundCrate::Name(name))) => {
            let ident = format_ident!("{name}");
            parse_quote!(::#ident)
        }
        (Ok(FoundCrate::Itself), ..) => parse_quote!(::extism::convert),
        (_, Ok(FoundCrate::Itself), ..) => parse_quote!(::extism_convert),
        (.., Ok(FoundCrate::Itself)) => parse_quote!(::extism_pdk),
        _ if cfg!(feature = "extism-path") => parse_quote!(::extism::convert),
        _ if cfg!(feature = "extism-pdk-path") => parse_quote!(::extism_pdk),
        _ => parse_quote!(::extism_convert),
    }
}

fn extract_encoding(attrs: &[Attribute]) -> Result<Path> {
    let encodings: Vec<_> = attrs
        .iter()
        .filter(|attr| attr.path().is_ident("encoding"))
        .collect();
    ensure!(!encodings.is_empty(), "encoding needs to be specified"; try = "`#[encoding(Json)]`");
    ensure!(encodings.len() < 2, encodings[1], "only one encoding can be specified"; try = "remove `{}`", encodings[1].to_token_stream());

    Ok(encodings[0].parse_args().map_err(
        |e| error_message!(e.span(), "{e}"; note= "expects a path"; try = "`#[encoding(Json)]`"),
    )?)
}

#[manyhow]
#[proc_macro_derive(ToBytes, attributes(encoding))]
pub fn to_bytes(
    DeriveInput {
        attrs,
        ident,
        generics,
        ..
    }: DeriveInput,
) -> Result {
    let encoding = extract_encoding(&attrs)?;
    let convert = convert_path();

    let (_, type_generics, _) = generics.split_for_impl();

    let mut generics = generics.clone();
    generics.make_where_clause().predicates.push(
        parse_quote!(for<'__to_bytes_b> #encoding<&'__to_bytes_b Self>: #convert::ToBytes<'__to_bytes_b>)
    );
    generics.params = iter::once(parse_quote!('__to_bytes_a))
        .chain(generics.params)
        .collect();
    let (impl_generics, _, where_clause) = generics.split_for_impl();

    Ok(quote! {
        impl #impl_generics #convert::ToBytes<'__to_bytes_a> for #ident #type_generics #where_clause
        {
            type Bytes = ::std::vec::Vec<u8>;

            fn to_bytes(&self) -> Result<Self::Bytes, #convert::Error> {
                #convert::ToBytes::to_bytes(&#encoding(self)).map(|__bytes| __bytes.as_ref().to_vec())
            }
        }

    })
}

#[manyhow]
#[proc_macro_derive(FromBytes, attributes(encoding))]
pub fn from_bytes(
    DeriveInput {
        attrs,
        ident,
        mut generics,
        ..
    }: DeriveInput,
) -> Result {
    let encoding = extract_encoding(&attrs)?;
    let convert = convert_path();
    generics
        .make_where_clause()
        .predicates
        .push(parse_quote!(#encoding<Self>: #convert::FromBytesOwned));
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();
    Ok(quote! {
        impl #impl_generics #convert::FromBytesOwned for #ident #type_generics #where_clause
        {
            fn from_bytes_owned(__data: &[u8]) -> Result<Self, #convert::Error> {
                <#encoding<Self> as #convert::FromBytesOwned>::from_bytes_owned(__data).map(|__encoding| __encoding.0)
            }
        }

    })
}
