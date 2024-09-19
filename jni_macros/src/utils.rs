use std::{fmt::Display, str::FromStr};

use itertools::Itertools;
use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned, ToTokens, TokenStreamExt as _};
use syn::{
    parse::Parse,
    punctuated::{Pair, Punctuated},
    spanned::Spanned as _,
    Ident, Token,
};

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

/// Convert the first letter of a String into uppercase
pub fn first_char_uppercase(s: String) -> String {
    let mut c = s.chars();
    match c.next() {
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
        None => String::new(),
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
    /// Returns the Type that is used in the signature of the method call. e.g. `Ljava/lang/String;`
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
impl Display for ClassPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.to_token_stream().to_string())
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum RustPrimitive {
    Bool, Char,
    U8, U16, U32, U64,
    I8, I16, I32, I64,
    F32, F64
}
impl RustPrimitive {
    pub fn is_unsigned(self) -> bool {
        match self {
            Self::U8 | Self::U16 | Self::U32 | Self::U64 => true,
            _ => false,
        }
    }
    /// Handle special cases of primitives that must be converted from another type to get the target type.
    /// Returns [`None`] if the primitive doesn't need any conversion.
    /// 
    /// **value** is the tokens representing the value that will be *operated on*.
    pub fn special_case_conversion(self, value: TokenStream) -> Option<TokenStream> {
        if self.is_unsigned() {
            // Transmute to the unsigned type
            let target_ty = Ident::new(&self.to_string(), Span::call_site());
            Some(quote_spanned! {value.span()=> unsafe { ::std::mem::transmute::<_, #target_ty>(#value) } })
        } else if self == RustPrimitive::Char {
            // Decode UTF-16
            Some(quote_spanned! {value.span()=>
                char::decode_utf16(Some(#value))
                    .next().unwrap()
                    .unwrap_or(char::REPLACEMENT_CHARACTER)
            })
        } else {
            None
        }
    }
}
impl FromStr for RustPrimitive {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "bool" => Ok(Self::Bool),
            "char" => Ok(Self::Char),
            "u8"   => Ok(Self::U8),
            "i8"   => Ok(Self::I8),
            "u16"  => Ok(Self::U16),
            "i16"  => Ok(Self::I16),
            "u32"  => Ok(Self::U32),
            "i32"  => Ok(Self::I32),
            "u64"  => Ok(Self::U64),
            "i64"  => Ok(Self::I64),
            "f32"  => Ok(Self::F32),
            "f64"  => Ok(Self::F64),
            _ => Err(()),
        }
    }
}
impl Display for RustPrimitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Bool => "bool",
            Self::Char => "char",
            Self::U8   => "u8",
            Self::I8   => "i8",
            Self::U16  => "u16",
            Self::I16  => "i16",
            Self::U32  => "u32",
            Self::I32  => "i32",
            Self::U64  => "u64",
            Self::I64  => "i64",
            Self::F32  => "f32",
            Self::F64  => "f64",
        })
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum JavaPrimitive {
    Byte, Boolean, Char,
    Short, Int, Long,
    Float, Double
}
impl JavaPrimitive {
    /// Returns the character/letter (lowercase) that is used to convert from JValue to a concrete type.
    pub fn sig_char(self) -> char {
        match self {
            Self::Byte    => 'b',
            Self::Boolean => 'z',
            Self::Char    => 'c',
            Self::Short   => 's',
            Self::Int     => 'i',
            Self::Long    => 'j',
            Self::Float   => 'f',
            Self::Double  => 'd',
        }
    }
}
impl FromStr for JavaPrimitive {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "byte"    => Ok(Self::Byte),
            "boolean" => Ok(Self::Boolean),
            "char"    => Ok(Self::Char),
            "short"   => Ok(Self::Short),
            "int"     => Ok(Self::Int),
            "long"    => Ok(Self::Long),
            "float"   => Ok(Self::Float),
            "double"  => Ok(Self::Double),
            _ => Err(()),
        }
    }
}
impl Display for JavaPrimitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Byte    => "byte",
            Self::Boolean => "boolean",
            Self::Char    => "char",
            Self::Short   => "short",
            Self::Int     => "int",
            Self::Long    => "long",
            Self::Float   => "float",
            Self::Double  => "double",
        })
    }
}
impl From<RustPrimitive> for JavaPrimitive {
    fn from(value: RustPrimitive) -> Self {
        match value {
            RustPrimitive::Bool => JavaPrimitive::Boolean,
            RustPrimitive::Char => JavaPrimitive::Char,
            RustPrimitive::U8   => JavaPrimitive::Byte,
            RustPrimitive::I8   => JavaPrimitive::Byte,
            RustPrimitive::U16  => JavaPrimitive::Short,
            RustPrimitive::I16  => JavaPrimitive::Short,
            RustPrimitive::U32  => JavaPrimitive::Int,
            RustPrimitive::I32  => JavaPrimitive::Int,
            RustPrimitive::U64  => JavaPrimitive::Long,
            RustPrimitive::I64  => JavaPrimitive::Long,
            RustPrimitive::F32  => JavaPrimitive::Float,
            RustPrimitive::F64  => JavaPrimitive::Double,
        }
    }
}
