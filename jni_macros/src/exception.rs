use crate::utils::ClassPath;
use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{spanned::Spanned, LitStr};

pub fn from_exception_struct(st: syn::ItemStruct) -> syn::Result<TokenStream> {
    let class = get_class_attribute(&st.attrs)?
        .to_token_stream()
        .to_string();

    if !st.fields.is_empty() {
        return Err(syn::Error::new(st.fields.span(), "TODO: get fields from Object"))
    }
    let fields = st.fields;

    let st_ident = st.ident;
    let st_generics = &st.generics;
    let st_generic_params = &st.generics.params;
    Ok(quote! {
        impl #st_generic_params ::ez_jni::FromException for #st_ident #st_generics {
            fn from_exception(env: &mut ::jni::JNIEnv, exception: &::jni::objects::JThrowable) -> Option<Self> {
                if !::ez_jni::utils::object_is_descendant_of(env, exception, #class) {
                    return None;
                }

                Some(Self #fields)
            }
        }
    })
}

pub fn from_exception_enum(enm: syn::ItemEnum) -> syn::Result<TokenStream> {
    let mut errors = Vec::new();

    let class_checks = enm.variants.iter()
        // Use only the good variants
        .filter_map(|var| match get_class_attribute(&var.attrs) {
            Ok(class) => Some((var, class)),
            Err(err) => {
                errors.push(err);
                return None;
            }
        })
        .enumerate()
        .filter_map(|(i, (variant, class))| {
            let class = class.to_token_stream().to_string();

            if !variant.fields.is_empty() {
                todo!("get fields from Object")
            }
            let fields = &variant.fields;

            let ident = &variant.ident;
            let _if = if i == 0 {
                quote! { if }
            } else {
                quote! { else if }
            };
            // Check if Exception is the class that this Variant uses, and construct the variant
            Some(quote! {
                #_if ::ez_jni::utils::object_is_descendant_of(env, exception, #class) {
                    Some(Self::#ident #fields)
                }
            })
        })
        .collect::<Box<_>>();

    // Check if there are errors
    if let Some(last) = errors.pop() /*Order doesn't matter*/ {
        let errors = errors.into_iter()
            .fold(last, |mut errors, err| {
                errors.combine(err);
                errors
            });
        return Err(errors);
    }

    let enm_ident = enm.ident;
    let enm_generics = &enm.generics;
    let enm_generic_params = &enm.generics.params;
    Ok(quote! {
        impl #enm_generic_params ez_jni::FromException for #enm_ident #enm_generics {
            fn from_exception(env: &mut ::jni::JNIEnv, exception: &::jni::objects::JThrowable) -> Option<Self> {
                #(#class_checks)* else {
                    None
                }
            }
        }
    })
}

fn get_class_attribute(attributes: &[syn::Attribute]) -> syn::Result<ClassPath> {
    let mut iter = attributes.iter()
        .filter(|attr| {
            attr.path()
                .get_ident()
                .is_some_and(|ident| ident.to_string() == "class")
        });
    let attr = iter
        .next()
        .ok_or_else(|| syn::Error::new(Span::call_site(), "Must have \"class\" attribute"))?;
    if let Some(attr) = iter.next() {
        return Err(syn::Error::new(
            attr.span(),
            "must have only 1 \"class\" attribute",
        ));
    }

    // Get attribute value
    match &attr.meta {
        // Can be #[class(java.class.path)] or #[class("java.class.path")]
        syn::Meta::List(syn::MetaList { tokens, .. }) => match syn::parse2::<LitStr>(tokens.clone()).ok() {
            Some(s) => syn::parse_str::<ClassPath>(&s.value()),
            None => syn::parse2::<ClassPath>(tokens.clone())
        },
        // Can only be #[class("java.class.path")]
        syn::Meta::NameValue(syn::MetaNameValue { value, .. }) => match value {
            syn::Expr::Lit(syn::ExprLit { lit: syn::Lit::Str(value), .. }) => syn::parse_str::<ClassPath>(&value.value()),
            _ => return Err(syn::Error::new(value.span(), "Try using a string literal here"))
        },
        syn::Meta::Path(path) => return Err(syn::Error::new(path.span(), "\"class\" attribute must have a value of a Java ClassPath (e.g. #[class(java.lang.Exception)])")) 
    }
}
