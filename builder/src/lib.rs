#![feature(result_into_ok_or_err)]
use quote::{quote, quote_spanned};

use proc_macro::TokenStream;
use syn::{Data, DeriveInput, parse_macro_input, parse2};
use proc_macro2::{Ident, Span};

fn join_spans(i1: Span, i2: Span) -> Result<Span, proc_macro2::TokenStream> {
    match i1.join(i2) {
        Some(span) => Ok(span),
        None => Err(quote! {
            compile_error!(
                "Currently, Builder proc macro suppors nightly compiler only."
            );
        }),
    }
}

fn ensure_struct_data(data: Data, ident_span: Span) -> Result<syn::DataStruct, TokenStream> {
    match data {
        Data::Struct(d) => Ok(d),
        Data::Union(u) => Err(
            join_spans(u.union_token.span, ident_span)
            .map(|s| quote_spanned! {
                s => compile_error!("Builder suppors structs only.");
            })
            .into_ok_or_err()
            .into()
        ),
        Data::Enum(e) => Err(
            join_spans(e.enum_token.span, ident_span)
            .map(|s| quote_spanned! {
                s => compile_error!("Builder suppors structs only.");
            })
            .into_ok_or_err()
            .into()
        ),
    }
}

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let DeriveInput { ident, vis, data, .. } = parse_macro_input!(input as DeriveInput);
    let data = match ensure_struct_data(data, ident.span()) {
        Ok(d) => d,
        Err(err) => return err.into(),
    };

    let fields = match data.fields {
        syn::Fields::Named(fields) => fields,
        _ => {
            return join_spans(data.struct_token.span, ident.span())
                .map(|s| quote_spanned! {
                    s => compile_error!("Builder suppors structs with named fields only.");
                })
                .into_ok_or_err()
                .into();
        }
    };

    let field_names = fields.named
        .iter()
        .filter_map(|f| f.ident.clone())
        .collect::<Vec<_>>();
    let field_types = fields.named
        .iter()
        .map(|syn::Field { ty, .. }| parse2::<syn::Type>(quote!(Option<#ty>))
             .unwrap())
        .collect::<Vec<_>>();
    
    let builder_ident = Ident::new(&format!("{}Builder", ident), Span::call_site());

    let builder_struct = quote! {
        #vis struct #builder_ident {
            #(#field_names: #field_types),*
        }
    };

    let builder_impl = quote! {
        impl #ident {
            #vis fn builder () -> #builder_ident {
                #builder_ident {
                    #(#field_names: None),*
                }
            }
        }
    };

    return quote! {
        #builder_struct
        #builder_impl
    }.into();
}
