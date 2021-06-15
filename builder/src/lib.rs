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
                s => compile_error!{"Builder suppors structs only."};
            })
            .into_ok_or_err()
            .into()
        ),
        Data::Enum(e) => Err(
            join_spans(e.enum_token.span, ident_span)
            .map(|s| quote_spanned! {
                s => compile_error!{"Builder suppors structs only."};
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

fn get_attr_ident(f: &Field) -> Option<Result<(Field, Ident), TokenStream>> {
    for attr in &f.attrs {
        let meta = attr.parse_meta();
        if meta.is_err() {
            continue;
        }

        let nv_meta = match meta.unwrap() {
            syn::Meta::List(l) => match l.nested.iter().next().unwrap().clone() {
                syn::NestedMeta::Meta(m) => match m {
                    syn::Meta::NameValue(nv) => nv,
                    _ => continue,
                }
                _ => continue,
            },
            _ => continue,
        };

        let arg_ident = &nv_meta.path.segments
            .iter()
            .next()
            .expect("at least one path segment")
            .ident;

        if arg_ident != "each" {
            return Some(Err(quote_spanned!{
                arg_ident.span() => compile_error!{
                    "builder attribute supports \"each\" argument only"
                }
            }.into()));
        }

        return match nv_meta.lit {
            syn::Lit::Str(s) => Some(Ok((
                f.clone(),
                Ident::new(&s.value(), arg_ident.span())
            ))),
            _ => Some(Err(quote! {
                    compile_error!{
                        "requires a string literal as a value"
                        },
            }.into()))
        }
    }

    None
}

fn get_inner_vec_type(field: &Field) -> Result<Type, TokenStream> {
    let type_path = match &field.ty {
        Type::Path(p) => p,
        _ => {
            let span = field.ident.clone().map_or_else(|| Span::call_site(), |i| i.span());

            return Err(quote_spanned! {
                span => compile_error!(
                    "Only Vec<_> types are supported on fields with \"each\" attribute"
                )
            }.into()
            );
        }
    };

    let segment = type_path.path.segments.iter().next().expect("type path with at least one elem");
    if segment.ident != "Vec" {
        return Err(quote_spanned! {
            segment.ident.span() => compile_error!(
                "Only Vec<_> types are supported on fields with \"each\" attribute"
            ),
        }.into());
    }

    match &segment.arguments {
        PathArguments::AngleBracketed(path_args) => match path_args.args.iter().next().unwrap() {
            GenericArgument::Type(ty) => Ok(ty.clone()),
            _ => unreachable!(),
        },
        _ => unreachable!(),
    }
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let DeriveInput { ident, vis, data, .. } = parse_macro_input!(input as DeriveInput);
    let data = match ensure_struct_data(data, ident.span()) {
        Ok(d) => d,
        Err(err) => return err,
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

    let (attr_fields, seq_field_names, fields) = fields.named
        .iter()
        .fold::<(Vec<_>, Vec<_>, Vec<_>), _>(
            (Vec::new(), Vec::new(), Vec::new()),
            |mut acc, field| {
                if get_attr_ident(field).is_some() {
                    acc.0.push(field.clone());
                    acc.1.push(field.ident.clone());
                } else {
                    acc.2.push(field.clone());
                }
                acc
            }
        );

    let seq_methods = attr_fields
        .iter()
        .map(get_attr_ident)
        .filter_map(|i| i)
        .collect::<Vec<_>>();

    if let Some(err) = seq_methods.iter().find_map(|r| r.clone().err()) {
        return err;
    }

    let (seq_method_names, seq_method_types) = seq_methods
        .iter()
        .map(|r| r.clone().unwrap())
        .map(|(field, ident)| {
            (ident, get_inner_vec_type(&field))
        })
        .fold::<(Vec::<Ident>, Vec::<Result<Type, TokenStream>>), _>(
            (Vec::new(), Vec::new()),
            |mut acc, (ident, ty)| {
                acc.0.push(ident.clone());
                acc.1.push(ty.clone());
                acc
            }
        );

    if let Some(err) = seq_method_types.iter().find_map(|r| r.clone().err()) {
        eprintln!("{:?}", err);
        return err;
    }

    let seq_method_types = seq_method_types.into_iter().map(|r| r.unwrap()).collect::<Vec<_>>();

    let (field_names, field_opt_names) = fields
        .iter()
        .fold::<(Vec<Ident>, Vec<Ident>), _>((Vec::new(), Vec::new()), |mut acc, f| {
            if is_option_type(&f.ty) {
                acc.1.push(f.ident.clone().unwrap());
            } else {
                acc.0.push(f.ident.clone().unwrap());
            }
            acc
        });
    let (field_types, field_opt_types) = fields
        .iter()
        .fold::<(Vec<Type>, Vec<Type>), _>((Vec::new(), Vec::new()), |mut acc, f| {
            if let Some(t) = get_inner_option_type(&f.ty) {
                acc.1.push(t);
            } else {
                acc.0.push(f.ty.clone());
            }
            acc
        });
    let (builder_types, builder_opt_types) = fields
        .iter()
        .fold::<(Vec<Type>, Vec<Type>), _>(
            (Vec::new(), Vec::new()),
            |mut acc, Field { ty, .. }| {
                if is_option_type(&ty) {
                    acc.1.push(ty.clone());
                } else {
                    acc.0.push(
                        parse2::<Type>(quote!(std::option::Option<#ty>)).unwrap()
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
            #(#seq_field_names: std::vec::Vec<#seq_method_types>,)*
        }

        impl #builder_ident {
            fn build(&mut self) -> std::result::Result<#ident, std::boxed::Box<dyn std::error::Error>> {
                #(if (self.#field_names.is_none()) {
                    return Err(format!(
                        "Field {} is missing",
                        stringify!(self.#field_names),
                    ).into());
                })*

                Ok(#ident {
                    #(#field_names: self.#field_names.clone().unwrap(),)*
                    #(#field_opt_names: self.#field_opt_names.clone(),)*
                    #(#seq_field_names: self.#seq_field_names.clone(),)*
                })
            }

            #(fn #field_names (&mut self, value: #field_types) -> &mut Self {
                self.#field_names = std::option::Option::Some(value);
                self
            })*

            #(fn #field_opt_names (&mut self, value: #field_opt_types) -> &mut Self {
                self.#field_opt_names = std::option::Option::Some(value);
                self
            })*

            #(fn #seq_method_names (&mut self, value: #seq_method_types) -> &mut Self {
                self.#seq_field_names.push(value);
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
                    #(#seq_field_names: Vec::new(),)*
                }
            }
        }
    };

    return quote! {
        #builder_struct
        #builder_impl
    }.into();
}
