//! Common functions used by all macros in this crate.

use either::Either;
use proc_macro2::{TokenStream, Span};
use quote::{ToTokens, TokenStreamExt as _};
use syn::{spanned::Spanned as _, ItemEnum, ItemStruct, LitStr};
use crate::types::{ClassPath, SigType};

/// Collect multiple syn errors into one error.
pub fn merge_errors(mut errors: Vec<syn::Error>) -> syn::Result<()> {
    // Check if there are errors
    if let Some(last) = errors.pop() /*Order doesn't matter*/ {
        let errors = errors.into_iter()
            .fold(last, |mut errors, err| {
                errors.combine(err);
                errors
            });
        Err(errors)
    } else {
        Ok(())
    }
}

/// Creates a span by collecting all spans into one, similar to [`Span::join()`].
/// 
/// This is done by building a [`TokenStream`] with fake tokens with the spans,
/// and then returning the span of that [`TokenStream`].
/// 
/// See [this page](https://docs.rs/syn/latest/syn/spanned/index.html#limitations) for why this doesn't really work.
pub fn join_spans(spans: impl IntoIterator<Item = Span>) -> Span {
    let mut tt = TokenStream::new();
    for span in spans {
        tt.append_all(syn::Ident::new("a", span).to_token_stream())
    }
    tt.span()
}

/// Converts the Derive input into a real *struct or enum*.
pub fn item_from_derive_input(input: syn::DeriveInput) -> Either<ItemStruct, ItemEnum> {
    match input.data {
        syn::Data::Struct(st) => Either::Left(ItemStruct {
            attrs: input.attrs,
            vis: input.vis,
            ident: input.ident,
            generics: input.generics,
            struct_token: st.struct_token,
            fields: st.fields,
            semi_token: st.semi_token,
        }),
        syn::Data::Enum(enm) => Either::Right(ItemEnum {
            attrs: input.attrs,
            vis: input.vis,
            ident: input.ident,
            generics: input.generics,
            enum_token: enm.enum_token,
            brace_token: enm.brace_token,
            variants: enm.variants,
        }),
        syn::Data::Union(_) => panic!("Unions not supported"),
    }
}

/// Convert the first letter of a String into uppercase
pub fn first_char_uppercase(s: String) -> String {
    let mut c = s.chars();
    match c.next() {
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
        None => String::new(),
    }
}

/// Generate the signature string for a JNI call.
/// 
/// The **params** and **return_type** can be any type in this library that implements [`SigType`],
/// as each type has its own signature component that it adds to the whole signature.
/// 
/// The returned string will look something like this: `(Ljava.lang.String;)V`.
pub fn gen_signature<'a, P, R>(params: impl Iterator<Item = &'a P>, return_type: &R) -> LitStr
where P: SigType + 'a,
      R: SigType
{
    let params_sig = params
        .map(|p| p.sig_type().value())
        .collect::<String>();
    let return_sig = return_type.sig_type().value();
    LitStr::new(&format!("({params_sig}){return_sig}"), Span::call_site())
}

/// Find and parse the `class` attribute of a **function**, **struct** or **enum variant** and return the Path to the Java Class.
/// 
/// This will remove the attribute from the list of attributes because `class` is just a helper.
/// 
/// Returns [`None`] if there is no `class` attribute.
pub fn take_class_attribute(attributes: &mut Vec<syn::Attribute>) -> syn::Result<Option<ClassPath>> {
    // Filter by attributes named "class"
    let mut iter = attributes.iter()
        .enumerate()
        .filter(|(_, attr)| {
            attr.path()
                .get_ident()
                .is_some_and(|ident| ident.to_string() == "class")
        });

    // Get the index of first instance (will be taken later)
    let index = match iter.next() {
        Some((i, _)) => i,
        None => return Ok(None)
    };

    // Check that there only exists 1 class attribute
    if let Some((_, attr)) = iter.next() {
        return Err(syn::Error::new(
            attr.span(),
            "must have only 1 \"class\" attribute",
        ));
    }

    drop(iter);

    // Take the attribute
    let attr = attributes.remove(index);

    // Get attribute value
    Ok(Some(match &attr.meta {
        // Can be #[class(java.class.path)] or #[class("java.class.path")]
        syn::Meta::List(syn::MetaList { tokens, .. }) => match syn::parse2::<LitStr>(tokens.clone()).ok() {
            Some(s) => syn::parse_str::<ClassPath>(&s.value())?,
            None => syn::parse2::<ClassPath>(tokens.clone())?
        },
        // Can only be #[class = "java.class.path"]
        syn::Meta::NameValue(syn::MetaNameValue { value, .. }) => match value {
            syn::Expr::Lit(syn::ExprLit { lit: syn::Lit::Str(value), .. }) => syn::parse_str::<ClassPath>(&value.value())?,
            _ => return Err(syn::Error::new(value.span(), "Try using a string literal here"))
        },
        syn::Meta::Path(path) => return Err(syn::Error::new(path.span(), "\"class\" attribute must have a value of a Java ClassPath (e.g. #[class(java.lang.Exception)])")) 
    }))
}

/// Same as [`get_class_attribute()`], but requires that the attribute is present.
/// 
/// Takes the [`Span`] of the struct or enum variant's Name for errors.
pub fn get_class_attribute_required(attributes: &mut Vec<syn::Attribute>, item_span: Span) -> syn::Result<ClassPath> {
    take_class_attribute(attributes)
        .and_then(|res| res.ok_or_else(|| syn::Error::new(item_span, "Must have \"class\" attribute")))
}
