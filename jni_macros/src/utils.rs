use std::fmt::Display;
use itertools::Itertools;
use proc_macro2::{TokenStream, Span};
use quote::{quote, quote_spanned, ToTokens, TokenStreamExt as _};
use syn::{parse::Parse, punctuated::{Pair, Punctuated}, spanned::Spanned as _, Ident, Token};

/// Create an error from a **msg** to return in a proc_macro.
pub fn error(err: impl AsRef<str>) -> TokenStream {
    let err = err.as_ref();
    quote! { compile_error!(#err); }
}
/// Create an error from a **msg**, with the specified **span**, to return in a proc_macro.
pub fn error_spanned(span: Span, err: impl AsRef<str>) -> TokenStream {
    let err = err.as_ref();
    quote_spanned! {span=>
        compile_error!(#err);
    }
}

/// Represents the path in the JVM of a *Java Class*, such as `java.lang.String`.
///
/// Parsed as [`Punctuated`] Tokens of [`Ident`]s and `Dot`s, disallowing trailing Dots.
/// Must have *at least one* package ident, so it will need at least 2 idents in total.
/// 
/// Example: `MyClass`, `java.lang.` are not allowed.
pub struct ClassPath {
    pub packages: Vec<(Ident, Token![.])>,
    /// The last Ident in the Punctuated list.
    pub class: Ident,
}
impl ClassPath {
    /// Returns the Type that is used in the signature of the method call. e.g. `V` or `Ljava/lang/String;`
    pub fn sig_type(&self) -> String {
        format!("L{};", self.to_string_with_slashes())
    }
    pub fn span(&self) -> Span {
        let mut tt = TokenStream::new();
        tt.append_all(self.packages.iter().map(|(t, p)| Pair::new(t, Some(p))));
        tt.append_all(self.class.to_token_stream());
        tt.span()
    }
    
    /// Converts the [`ClassPath`] to a string of its components, where each component is separated by a slash.
    /// e.g. `java/lang/String`.
    pub fn to_string_with_slashes(&self) -> String {
        Itertools::intersperse(
            self.packages
                .iter()
                .map(|pair| pair.0.to_string())
                .chain(Some(self.class.to_string())),
            "/".to_string(),
        )
        .collect::<String>()
    }
}
impl Parse for ClassPath {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let path = Punctuated::<Ident, Token![.]>::parse_separated_nonempty(input)?;
        let span = path.span();
        let class = path.last().unwrap().clone();
        let packages = path
            .into_pairs()
            .into_iter()
            .map(|pair| pair.into_tuple())
            .filter_map(|pair| match pair.1 {
                Some(punct) => Some((pair.0, punct)),
                None => None, // Skips the last pair, because it will not have punct
            })
            .collect::<Vec<_>>();
        
        if packages.is_empty() {
            return Err(syn::Error::new(span, "Java Class Path must have at least one component for the packages"))
        }

        Ok(Self { class, packages })
    }
}
impl ToTokens for ClassPath {
    /// Converts the path back to the same tokens it was obtained from: e.g. `java.lang.String`.
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for (package, dot) in &self.packages {
            tokens.append(package.clone());
            tokens.append_all(dot.to_token_stream())
        }
        tokens.append(self.class.clone())
    }
}
