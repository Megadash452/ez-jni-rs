//! Common functions used by all macros in this crate.

mod step;

pub use step::*;

use either::Either;
use proc_macro2::{Span, TokenStream, TokenTree};
use quote::{ToTokens, TokenStreamExt as _};
use syn::{ItemEnum, ItemStruct, LitStr};
use crate::types::{Class, SigType};


/// The same as [`syn::spanned::Spanned`].
/// 
/// Made this trait to get around the fact that implementors of [`syn::spanned::Spanned`] must also implement [`ToTokens`].
pub trait Spanned {
    /// Returns a `Span` covering the complete contents of this syntax tree
    /// node, or [`Span::call_site()`] if this node is empty.
    ///
    /// [`Span::call_site()`]: proc_macro2::Span::call_site
    fn span(&self) -> Span;
}
impl<T> Spanned for T
where T: syn::spanned::Spanned {
    #[inline]
    fn span(&self) -> Span {
        syn::spanned::Spanned::span(self)
    }
}

/// Provides unwrapping shortcuts for [`TokenTree`].
#[allow(unused)]
pub trait TokenTreeExt {
    /// Unwraps the [`TokenTree`], expecting it is a [`Group`][TokenTree::Group].
    fn group(self) -> syn::Result<proc_macro2::Group>;
    /// Unwraps the [`TokenTree`], expecting it is a [`Ident`][TokenTree::Ident].
    fn ident(self) -> syn::Result<proc_macro2::Ident>;
    /// Unwraps the [`TokenTree`], expecting it is a [`Punct`][TokenTree::Punct].
    fn punct(self) -> syn::Result<proc_macro2::Punct>;
    /// Unwraps the [`TokenTree`], expecting it is a [`Literal`][TokenTree::Literal].
    fn lit(self) -> syn::Result<proc_macro2::Literal>;
}
impl TokenTreeExt for TokenTree {
    fn group(self) -> syn::Result<proc_macro2::Group> {
        if let TokenTree::Group(group) = self {
            Ok(group)
        } else {
            Err(syn::Error::new(self.span(), format!("Expected TokenTree::Group, found {}", tt_variant(&self))))
        }
    }
    fn ident(self) -> syn::Result<proc_macro2::Ident> {
        if let TokenTree::Ident(ident) = self {
            Ok(ident)
        } else {
            Err(syn::Error::new(self.span(), format!("Expected TokenTree::Ident, found {}", tt_variant(&self))))
        }
    }
    fn punct(self) -> syn::Result<proc_macro2::Punct> {
        if let TokenTree::Punct(punct) = self {
            Ok(punct)
        } else {
            Err(syn::Error::new(self.span(), format!("Expected TokenTree::Punct, found {}", tt_variant(&self))))
        }
    }
    fn lit(self) -> syn::Result<proc_macro2::Literal> {
        if let TokenTree::Literal(literal) = self {
            Ok(literal)
        } else {
            Err(syn::Error::new(self.span(), format!("Expected TokenTree::Literal, found {}", tt_variant(&self))))
        }
    }
}

fn tt_variant(tt: &TokenTree) -> &'static str {
    match tt {
        TokenTree::Group(_) => "TokenTree::Group",
        TokenTree::Ident(_) => "TokenTree::Ident",
        TokenTree::Punct(_) => "TokenTree::Punct",
        TokenTree::Literal(_) => "TokenTree::Literal",
    }
}


/// Collect multiple syn errors into one error.
pub fn merge_errors(errors: impl IntoIterator<Item = syn::Error>) -> syn::Result<()> {
    let mut errors = errors.into_iter();
    // Check if there are errors
    if let Some(first) = errors.next() {
        let errors = errors.fold(first, |mut errors, err| {
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
pub fn take_class_attribute(attributes: &mut Vec<syn::Attribute>) -> syn::Result<Option<Class>> {
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
            Some(s) => syn::parse_str::<Class>(&s.value())?,
            None => syn::parse2::<Class>(tokens.clone())?
        },
        // Can only be #[class = "java.class.path"]
        syn::Meta::NameValue(syn::MetaNameValue { value, .. }) => match value {
            syn::Expr::Lit(syn::ExprLit { lit: syn::Lit::Str(value), .. }) => syn::parse_str::<Class>(&value.value())?,
            _ => return Err(syn::Error::new(value.span(), "Try using a string literal here"))
        },
        syn::Meta::Path(path) => return Err(syn::Error::new(path.span(), "\"class\" attribute must have a value of a Java Class (e.g. #[class(java.lang.Exception)])")) 
    }))
}

/// Same as [`take_class_attribute()`], but requires that the attribute is present.
/// 
/// Takes the [`Span`] of the struct or enum variant's Name for errors.
pub fn take_class_attribute_required(attributes: &mut Vec<syn::Attribute>, item_span: Span) -> syn::Result<Class> {
    take_class_attribute(attributes)
        .and_then(|res| res.ok_or_else(|| syn::Error::new(item_span, "Must have \"class\" attribute")))
}
