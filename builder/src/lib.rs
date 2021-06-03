#![feature(result_into_ok_or_err)]
use quote::{quote, quote_spanned};

use proc_macro::TokenStream;
use syn::{Fields, PathArguments, GenericArgument, Data, DeriveInput, parse_macro_input, parse2, Type, Field, DataStruct};
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

fn ensure_struct_data(data: Data, ident_span: Span) -> Result<DataStruct, TokenStream> {
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

fn is_option_type(ty: &Type) -> bool {
    let type_path = if let Type::Path(p) = ty {
        p
    } else {
        return false;
    };

    for segment in &type_path.path.segments {
        if segment.ident.to_string() == "Option" {
            return true;
        }
    }

    return false;
}

fn get_inner_option_type(ty: &Type) -> Option<Type> {
    if !is_option_type(ty) {
        return None;
    }

    let type_path = if let Type::Path(p) = ty {
        p
    } else {
        unreachable!("get_inner_option_type(ty: must be Type::Path)");
    };

    for segment in &type_path.path.segments {
        if segment.ident.to_string() == "Option" {
            return match &segment.arguments {
                PathArguments::AngleBracketed(path_args) => Some({
                    match path_args.args.iter().next().unwrap() {
                        GenericArgument::Type(ty) => ty.clone(),
                        _ => unreachable!("get_inner_option_type - unknown generic arg"),
                    }
                }),
                _ => unreachable!(),
            };
        }
    }

    return None;
}

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let DeriveInput { ident, vis, data, .. } = parse_macro_input!(input as DeriveInput);
    let data = match ensure_struct_data(data, ident.span()) {
        Ok(d) => d,
        Err(err) => return err.into(),
    };

    let fields = match data.fields {
        Fields::Named(fields) => fields,
        _ => {
            return join_spans(data.struct_token.span, ident.span())
                .map(|s| quote_spanned! {
                    s => compile_error!("Builder suppors structs with named fields only.");
                })
                .into_ok_or_err()
                .into();
        }
    };

    let (field_names, field_opt_names) = fields.named
        .iter()
        .filter(|f| f.ident.is_some())
        .fold::<(Vec<Ident>, Vec<Ident>), _>((Vec::new(), Vec::new()), |mut acc, f| {
            if is_option_type(&f.ty) {
                acc.1.push(f.ident.clone().unwrap());
            } else {
                acc.0.push(f.ident.clone().unwrap());
            }
            acc
        });
    let (field_types, field_opt_types) = fields.named
        .iter()
        .fold::<(Vec<Type>, Vec<Type>), _>((Vec::new(), Vec::new()), |mut acc, f| {
            if let Some(t) = get_inner_option_type(&f.ty) {
                acc.1.push(t);
            } else {
                acc.0.push(f.ty.clone());
            }
            acc
        });
    let (builder_types, builder_opt_types) = fields.named
        .iter()
        .fold::<(Vec<Type>, Vec<Type>), _>(
            (Vec::new(), Vec::new()),
            |mut acc, Field { ty, .. }| {
                if is_option_type(&ty) {
                    acc.1.push(ty.clone());
                } else {
                    acc.0.push(
                        parse2::<Type>(quote!(Option<#ty>)).unwrap()
                    );
                }

                return acc;
            }
        );

    let builder_ident = Ident::new(&format!("{}Builder", ident), Span::call_site());

    let builder_struct = quote! {
        #vis struct #builder_ident {
            #(#field_names: #builder_types,)*
            #(#field_opt_names: #builder_opt_types,)*
        }

        impl #builder_ident {
            fn build(&mut self) -> Result<#ident, Box<dyn std::error::Error>> {
                #(if (self.#field_names.is_none()) {
                    return Err(format!(
                        "Field {} is missing",
                        stringify!(self.#field_names),
                    ).into());
                })*

                Ok(#ident {
                    #(#field_names: self.#field_names.clone().unwrap(),)*
                    #(#field_opt_names: self.#field_opt_names.clone(),)*
                })
            }

            #(fn #field_names (&mut self, value: #field_types) -> &mut Self {
                self.#field_names = Some(value);
                self
            })*

            #(fn #field_opt_names (&mut self, value: #field_opt_types) -> &mut Self {
                self.#field_opt_names = Some(value);
                self
            })*
        }
    };

    let builder_impl = quote! {
        impl #ident {
            #vis fn builder () -> #builder_ident {
                #builder_ident {
                    #(#field_names: None,)*
                    #(#field_opt_names: None,)*
                }
            }
        }
    };

    return quote! {
        #builder_struct
        #builder_impl
    }.into();
}
