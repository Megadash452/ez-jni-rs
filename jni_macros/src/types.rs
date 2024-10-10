use proc_macro2::{Span, TokenStream};
use quote::quote_spanned;
use syn::{bracketed, parse::{discouraged::Speculative as _, Parse}, punctuated::Punctuated, spanned::Spanned as _, token::Bracket, Ident, LitStr, Token};
use itertools::Itertools as _;
use std::{fmt::{Display, Debug}, str::FromStr};

use crate::utils::join_spans;

/// Indicates that a type is data about Java Type signature, such as a *parameter type* or *return type*
pub trait SigType {
    /// The name of the function that maps from a `JValue` to a concrete type.
    /// This will always be a single *lowercase character*.
    /// Must be one of the single-letter functions found in [`JValueGen`][https://docs.rs/jni/latest/jni/objects/enum.JValueGen.html#method.l].
    fn sig_char(&self) -> Ident;
    /// The Type that is used in the signature of the method call. e.g. `"V"` or `"Ljava/lang/String;"`.
    /// It is the *uppercase [`SigType::sig_char`]*, and if it is `L` it is followed by the ClassPath and a `;`.
    fn sig_type(&self) -> LitStr;
}

/// Perform some kind of necessary conversion between *Rust* and *Java* values.
/// For example, convert a *slice* to a *Java Array*, convert *UTF-8* to *UTF-16*, etc.
pub trait SpecialCaseConversion {
    /// Returns code that handles *special case conversion* for a **Java** value to be converted to a **Rust** value.
    /// In other words, if a macro expects a [`Type`], run some middle step on a *Java value* to obtain a valid instance of this [`Type`].
    /// 
    /// Returns [`None`] if no conversion is necessary.
    /// 
    /// **value** is the tokens/expression representing the value that must be converted.
    /// 
    /// This is used for the *return* type.
    fn convert_java_to_rust(&self, value: &TokenStream) -> Option<TokenStream>;
    /// Returns code that handles *special case conversion* for a **Rust** value to be converted to a **Java** value.
    /// 
    /// Returns [`None`] if no conversion is necessary.
    /// 
    /// **value** is the tokens/expression representing the value that must be converted.
    /// 
    /// This is used for the *parameter* type.
    fn convert_rust_to_java(&self, value: &TokenStream) -> Option<TokenStream>;
}

pub enum Type {
    Single(InnerType),
    Array(ArrayType),
    // Might also add generics
}
impl Type {
    pub fn span(&self) -> Span {
        match self {
            Self::Single(ty) => ty.span(),
            Self::Array(array) => array.span()
        }
    }
    /// Returns whether the [`InnerType`] of the type is a *primitive*.
    /// Returns `false` if this is an [`Array`][Type::Array].
    pub fn is_primitive(&self) -> bool {
        match self {
            Self::Single(InnerType::RustPrimitive { .. } | InnerType::JavaPrimitive { .. }) => true,
            Self::Single(_) | Self::Array(_) => false,
        }
    }
}
impl SigType for Type {
    fn sig_char(&self) -> Ident {
        match self {
            Self::Single(ty) => ty.sig_char(),
            Self::Array(array) => array.sig_char(),
        }
    }
    fn sig_type(&self) -> LitStr {
        match self {
            Self::Single(ty) => ty.sig_type(),
            Self::Array(array) => array.sig_type(),
        }
    }
}
impl SpecialCaseConversion for Type {
    fn convert_java_to_rust(&self, value: &TokenStream) -> Option<TokenStream> {
        match self {
            Self::Single(ty) => ty.convert_java_to_rust(value),
            Self::Array(array) => array.convert_java_to_rust(value)
        }
    }
    fn convert_rust_to_java(&self, value: &TokenStream) -> Option<TokenStream> {
        match self {
            Self::Single(ty) => ty.convert_rust_to_java(value),
            Self::Array(array) => array.convert_rust_to_java(value)
        }
    }
}
impl Parse for Type {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        // Check if the Type is wrapped in Brackets (Array)
        Ok(if input.lookahead1().peek(syn::token::Bracket) {
            Self::Array(input.parse()?)
        } else {
            Self::Single(input.parse()?)
        })
    }
}
impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Single(ty) => Display::fmt(ty, f),
            Self::Array(array) => Display::fmt(array, f)
        }
    }
}

#[derive(Clone)]
pub struct ArrayType {
    pub brackets: Bracket,
    pub ty: InnerType
}
impl ArrayType {
    pub fn span(&self) -> Span {
        join_spans([self.brackets.span.span(), self.ty.span()])
    }
}
impl SigType for ArrayType {
    /// Returns 'l' because Arrays are Objects.
    /// See [`origin`](SigType::sig_char()).
    fn sig_char(&self) -> Ident {
        Ident::new("l", self.ty.span())
    }
    fn sig_type(&self) -> LitStr {
        let sig_type = self.ty.sig_type();
        LitStr::new(&format!("[{}", sig_type.value()), sig_type.span())
    }
}
impl SpecialCaseConversion for ArrayType {
    /// Returns code that converts a *Java Array* to a *Rust [`Box`]*.
    /// 
    /// Always returns [`Some`].
    /// 
    /// See [`origin`](SpecialCaseConversion::convert_java_to_rust()).
    fn convert_java_to_rust(&self, value: &TokenStream) -> Option<TokenStream> {
        // There are different JNI functions for each primitive array and Object arrays
        Some(match &self.ty {
            // Build a Rust boxed slice from a Java Array in which the inner type is a primitive.
            InnerType::JavaPrimitive { .. }
            | InnerType::RustPrimitive { .. } => {
                // Use the Java Primitive to build the names of the JNI functions
                // Use the Rust Primitive for the special case conversion
                let (j_prim, r_prim) = match &self.ty {
                    InnerType::JavaPrimitive { ty, .. } => (*ty, RustPrimitive::from(*ty)),
                    InnerType::RustPrimitive { ty, .. } => (JavaPrimitive::from(*ty), *ty),
                    InnerType::Object(_) => panic!("Passed Object to function that clearly wants a primitive")
                };

                let inner_ty = j_prim.sig_type();
                // The name of the JNI function that puts the Java Array's elements in the slice
                let fill_slice = Ident::new(&format!("get_{j_prim}_array_region"), value.span());
                // The inner type of the array might require some conversion
                let conversion = {
                    let element_tokens = quote_spanned!(value.span()=> v); 
                    // Must also convert to Rust bool (jni, why only here?)
                    if r_prim == RustPrimitive::Bool {
                        quote_spanned!(element_tokens.span()=> #element_tokens != 0)
                    } else {
                        // Convert using the variable
                        r_prim.convert_java_to_rust(&element_tokens)
                            .unwrap_or(element_tokens)
                    }
                };

                quote_spanned! {value.span()=> {
                    use ::std::borrow::BorrowMut as _;
                    let _array = ::jni::objects::JPrimitiveArray::from(#value);
                    let _len = ::ez_jni::utils::__obj_array_len(&_array, #inner_ty, env.borrow_mut());
                    let mut _box = vec![unsafe { std::mem::zeroed() }; _len].into_boxed_slice();
                    env.#fill_slice(&_array, 0, &mut _box)
                        .unwrap_or_else(|err| panic!("Failed to read Array elements: {err}"));
                    _box.into_vec()
                        .into_iter()
                        .map(|v| #conversion)
                        .collect::<::std::boxed::Box<[_]>>()
                } }
            },
            // Build a Rust boxed slice from a Java Array in which the inner type is an Object.
            InnerType::Object(class) => {
                // For some reason, class.getName() returns a ClassPath with .dots. instead of /slashes/, so can't use sig_type().
                // This is the only place where this happens. why??
                let inner_ty = LitStr::new(&format!("L{};", class.to_string()), class.span());

                // The inner Class of the array might require some conversion
                let conversion = {
                    let element_tokens = quote_spanned!(value.span()=> _element); 
                    // Convert using the variable
                    class.convert_java_to_rust(&element_tokens)
                        .unwrap_or(element_tokens)
                };

                quote_spanned! {value.span() => {
                    use ::std::borrow::BorrowMut as _;
                    let _array = ::jni::objects::JObjectArray::from(#value);
                    let _len = ::ez_jni::utils::__obj_array_len(&_array, #inner_ty, env.borrow_mut());
                    let mut _vec = ::std::vec::Vec::with_capacity(_len);
                    for i in 0.._len {
                        let _element = env.get_object_array_element(&_array, i as ::jni::sys::jsize)
                            .unwrap_or_else(|err| panic!("Failed to read Array elements: {err}"));
                        _vec.push(#conversion);
                    }
                    _vec.into_boxed_slice()
                } }
            }
        })
    }
    /// Returns code that converts a *Rust slice* to a *Java Array*.
    /// 
    /// Always returns [`Some`].
    /// 
    /// See [`origin`](SpecialCaseConversion::convert_java_to_rust()).
    fn convert_rust_to_java(&self, value: &TokenStream) -> Option<TokenStream> {
        // There are different JNI functions for each primitive array and Object arrays
        Some(match &self.ty {
            // Build a Java Array in which the inner type is a primitive.
            InnerType::JavaPrimitive { .. }
            | InnerType::RustPrimitive { .. } => {
                // Use the java primitive to build the names of the JNI functions
                let j_prim = match &self.ty {
                    InnerType::JavaPrimitive { ty, .. } => *ty,
                    InnerType::RustPrimitive { ty, .. } => JavaPrimitive::from(*ty),
                    InnerType::Object(_) => panic!("Passed Object to function that clearly wants a primitive")
                };
                
                let new_array_fn = Ident::new(&format!("new_{j_prim}_array"), value.span());
                let new_array_err = LitStr::new(
                    &format!("Failed to create Java {j_prim} array: {{err}}"),
                    value.span(),
                );
                let fill_array_fn =
                    Ident::new(&format!("set_{j_prim}_array_region"), value.span());
                let fill_array_err = LitStr::new(
                    &format!("Error filling {j_prim} array: {{err}}"),
                    value.span(),
                );
                // The inner type of the array might require some conversion
                let converted = {
                    match self.ty.convert_rust_to_java(&quote_spanned! {value.span()=> v}) {
                        Some(conversion) => &quote_spanned!(value.span()=>
                            #value.iter()
                                .map(|&v| #conversion)
                                .collect::<::std::boxed::Box<[_]>>()
                        ),
                        None => value
                    }
                };
    
                quote_spanned! {value.span()=> {
                    let _slice = &(#converted);
                    let _slice = ::std::convert::AsRef::<[_]>::as_ref(_slice);
                    let _jarray = env.#new_array_fn(_slice.len() as ::jni::sys::jsize)
                        .unwrap_or_else(|err| panic!(#new_array_err));
                    env.#fill_array_fn(&_jarray, 0, _slice)
                        .inspect_err(|err| println!(#fill_array_err)).unwrap();
                    ::jni::objects::JObject::from(_jarray)
                } }
            },
            // Build a Java Array in which the inner type is an Object.
            InnerType::Object(class) => {
                let new_array_err = LitStr::new(
                    &format!("Failed to create Java Object \"{}\" array: {{err}}", class.to_string()),
                    value.span(),
                );
                let set_val_err = LitStr::new(
                    &format!("Failed to set the value of Object array at index {{_i}}: {{err}}"),
                    value.span(),
                );
                let class_path = LitStr::new(&class.to_jni_class_path(), value.span());
                // The inner Class of the array might require some conversion
                let conversion = {
                    let element_tokens = quote_spanned!(value.span()=> _element); 
                    // Convert using the variable
                    class.convert_rust_to_java(&element_tokens)
                        .unwrap_or(element_tokens)
                };

                quote_spanned! {value.span()=> {
                    let _slice = &(#value);
                    let _slice = ::std::convert::AsRef::<[_]>::as_ref(_slice);
                    let _jarray = env.new_object_array(
                        _slice.len() as ::jni::sys::jsize,
                        #class_path,
                        unsafe { ::jni::objects::JObject::from_raw(::std::ptr::null_mut()) }
                    )
                        .unwrap_or_else(|err| panic!(#new_array_err));
                    for (_i, _element) in _slice.into_iter().enumerate() {
                        env.set_object_array_element(&_jarray, _i as ::jni::sys::jsize, #conversion)
                            .unwrap_or_else(|err| panic!(#set_val_err));
                    }
                    ::jni::objects::JObject::from(_jarray)
                } }
            },
        })
    }
}
impl Parse for ArrayType {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let inner;
        Ok(Self {
            brackets: bracketed!(inner in input),
            ty: inner.parse()?
        })
    }
}
impl Display for ArrayType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}]", self.ty)
    }
}


/// An InnerType is a top-level type that can be used for **arrays** and **generics**
/// 
/// [`Parameter`]: crate::call::Parameter
/// [`Return`]: crate::call::Return
/// [`ResultType`]: crate::call::ResultType
#[derive(Debug, Clone)]
pub enum InnerType {
    JavaPrimitive { ident: Ident, ty: JavaPrimitive },
    RustPrimitive { ident: Ident, ty: RustPrimitive },
    Object(ClassPath),
}
impl InnerType {
    pub fn span(&self) -> Span {
        match self {
            Self::JavaPrimitive { ident, .. }
            | Self::RustPrimitive { ident, .. } => ident.span(),
            Self::Object(class) => class.span(),
        }
    }
}
impl SigType for InnerType {
    fn sig_char(&self) -> Ident {
        match self {
            Self::JavaPrimitive { ident, ty } => {
                let mut sig_char = ty.sig_char();
                sig_char.set_span(ident.span());
                sig_char
            },
            Self::RustPrimitive { ident, ty, .. } => {
                let mut sig_char = JavaPrimitive::from(*ty).sig_char();
                sig_char.set_span(ident.span());
                sig_char
            },
            Self::Object(class) => class.sig_char(),
        }
    }
    fn sig_type(&self) -> LitStr {
        match self {
            Self::JavaPrimitive { ident, ty } => {
                let mut sig_type = ty.sig_type();
                sig_type.set_span(ident.span());
                sig_type
            },
            Self::RustPrimitive { ident, ty } => {
                let mut sig_type = JavaPrimitive::from(*ty).sig_type();
                sig_type.set_span(ident.span());
                sig_type
            },
            Self::Object(class) => class.sig_type(),
        }
    }
}
impl SpecialCaseConversion for InnerType {
    fn convert_java_to_rust(&self, value: &TokenStream) -> Option<TokenStream> {
        match self {
            Self::RustPrimitive { ty, .. } => ty.convert_java_to_rust(value),
            Self::JavaPrimitive { ty, .. } => RustPrimitive::from(*ty).convert_java_to_rust(value),
            Self::Object(class) => class.convert_java_to_rust(value),
        }
    }
    fn convert_rust_to_java(&self, value: &TokenStream) -> Option<TokenStream> {
        match self {
            Self::RustPrimitive { ty, .. } => ty.convert_rust_to_java(value),
            Self::JavaPrimitive { ty, .. } => RustPrimitive::from(*ty).convert_rust_to_java(value),
            Self::Object(class) => class.convert_rust_to_java(value),
        }
    }
}
impl Parse for InnerType {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let fork = input.fork();
        let ident = fork.parse::<Ident>()?;
        let ident_str = ident.to_string();
        match RustPrimitive::from_str(&ident_str).ok() {
            Some(ty) => {
                input.advance_to(&fork);
                Ok(Self::RustPrimitive { ident, ty })
            },
            // JavaPrimitive::Char will never be constructued here because the RustPrimitive takes priority
            None => match JavaPrimitive::from_str(&ident_str).ok() {
                Some(ty) => {
                    input.advance_to(&fork);
                    Ok(Self::JavaPrimitive { ident, ty })
                }
                None => return Ok(Self::Object(input.parse()?)),
            },
        }
    }
}
impl Display for InnerType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::JavaPrimitive { ty, .. } => ty.to_string(),
            // Convert form Rust to Java
            Self::RustPrimitive { ty, .. } => JavaPrimitive::from(*ty).to_string(),
            Self::Object(class) => format!("Object({})", class.to_jni_class_path()),
        };
        f.write_str(&s)
    }
}

/// Represents the path in the JVM of a *Java Class*, such as `java.lang.String`.
///
/// Parsed as [`Punctuated`] Tokens of [`Ident`]s and `Dot`s, disallowing trailing Dots.
/// Must have *at least one* package ident, so it will need at least 2 idents in total.
/// 
/// There is one exception: this can parse `String`, which will be converted to `java.lang.String`.
///
/// Example: `MyClass`, `java.lang.` are not allowed.
#[derive(Clone)]
pub struct ClassPath {
    pub packages: Vec<(Ident, Token![.])>,
    /// The last Ident in the Punctuated list.
    pub class: Ident,
    pub nested_path: Option<NestedPath>
}
#[derive(Clone)]
pub struct NestedPath {
    pub classes: Vec<(Ident, Token![$])>,
    pub final_class: Ident
}
impl ClassPath {
    /// Builds a [`TokenStream`] out of this path's items and returns its [`span`][TokenStream::span()].
    pub fn span(&self) -> Span {
        let mut spans = Vec::new();
        spans.extend(self.packages.iter()
            .map(|(ident, dot)| join_spans([ident.span(), dot.span]))
        );
        spans.push(self.class.span());
        if let Some(nested_path) = &self.nested_path {
            spans.extend(nested_path.classes.iter()
                .map(|(ident, dol)| join_spans([ident.span(), dol.span]))
            );
            spans.push(nested_path.final_class.span())
        }
        
        join_spans(spans.into_iter())
    }

    /// Parses the [`ClassPath`] with [`Parse::parse()`], but the final path *component* is a method name.
    /// 
    /// Returns the parsed [`ClassPath`] and the **method name**.
    pub fn parse_with_trailing_method(input: syn::parse::ParseStream) -> syn::Result<(Self, Ident)> {
        let mut path = Self::parse(input)?;

        let method_name = match &path.nested_path {
            // Parse the Dot and method name
            Some(_) => {
                input.parse::<Token![.]>()?;
                input.parse::<Ident>()?
            },
            // If the path had no Nested Classes (i.e. it was only separated by Dots)
            // the method name would have already been parsed, so take it out of the path.
            None => {
                // This implies that the path must be at least 3 components long: 2 for the path and 1 for the method name
                if path.packages.len() < 2 {
                    return Err(syn::Error::new(path.span(), "Java Class Path must have at least 1 package component (aside from the Class and method), such as `me.Class.method`"))
                }

                let method_name = path.class;
                path.class = path.packages.pop().unwrap().0;
                method_name
            }
        };

        Ok((path, method_name))
    }

    /// Converts the [`ClassPath`] to a string used by `JNI`, where each component is separated by a slash.
    /// e.g. `java/lang/String` or `me/author/Class$Nested`.
    pub fn to_jni_class_path(&self) -> String {
        self.to_string_helper("/", "$")
    }

    /// Helps create the string in [`Self::to_jni_class_path()`] and [`Self::fmt()`].
    /// 
    /// **sep** is the separator between the package and class component.
    /// **nested_sep** is the separator between the nested classes
    #[allow(unstable_name_collisions)]
    fn to_string_helper(&self, sep: &str, nested_sep: &str) -> String {
        let nested_classes = match &self.nested_path {
            Some(nested_path) => Box::new(
                // Put nested_sep between packages and nested classes
                Some(nested_sep.to_string()).into_iter()
                    .chain(nested_path.classes.iter()
                        .map(|(ident, _)| ident.to_string())
                        .chain(Some(nested_path.final_class.to_string()))
                        .intersperse(nested_sep.to_string())
                    )
            ) as Box<dyn Iterator<Item = String>>,
            None => Box::new(None.into_iter()) as Box<dyn Iterator<Item = String>>
        };

        self.packages.iter()
            .map(|(ident, _)| ident.to_string())
            .chain(Some(self.class.to_string()))
            .intersperse(sep.to_string())
            .chain(nested_classes)
            .collect::<String>()
    }
}
impl SigType for ClassPath {
    fn sig_char(&self) -> Ident {
        Ident::new("l", self.span())
    }
    fn sig_type(&self) -> LitStr {
        LitStr::new(&format!("L{};", self.to_jni_class_path()), self.span())
    }
}
impl SpecialCaseConversion for ClassPath {
    fn convert_java_to_rust(&self, value: &TokenStream) -> Option<TokenStream> {
        // Special cases for Java Objects
        if self.to_jni_class_path() == "java/lang/String" {
            // Wrap java.lang.String in JString
            Some(quote_spanned! {value.span()=> ::jni::objects::JString::from(#value) })
        } else {
            None
        }
    }
    fn convert_rust_to_java(&self, value: &TokenStream) -> Option<TokenStream> {
        match self.to_jni_class_path().as_str() {
            // Convert Rust String to Java String
            "java/lang/String" => Some(quote_spanned! {value.span()=>
                env.new_string(::std::convert::AsRef::<str>::as_ref(&#value))
                    .unwrap_or_else(|err| panic!("Failed to create convert Rust String to Java String: {err}"))
            }),
            _ => None
        }
    }
}
impl Parse for ClassPath {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        /// Converts a [`Punctuated`] whose last element is [`None`] to a [`Vec`] of pairs.
        fn punctuated_to_pairs<T, P>(punctuated: Punctuated<T, P>) -> Vec<(T, P)> {
            punctuated.into_pairs()
                .into_iter()
                .map(|pair| pair.into_tuple())
                .filter_map(|pair| match pair.1 {
                    Some(punct) => Some((pair.0, punct)),
                    None => None, // Skips the last pair, because it will not have punct
                })
                .collect()
        }

        // Parse the dot-separated section `me.author.Class`
        let mut path = Punctuated::<Ident, Token![.]>::parse_separated_nonempty(input)?;
        // The class is the final component of the path
        let class = match path.pop() {
            Some(class) => class.into_value(),
            None => return Err(syn::Error::new(path.span(), "Java Class Path must not be empty"))
        };
        let packages = if path.is_empty() {
            // ClassPath can be `String`, make it into `java.lang.String`
            if class.to_string() == "String" {
                punctuated_to_pairs::<Ident, Token![.]>(
                    syn::parse_quote_spanned!(class.span()=> java.lang.)
                )
            } else {
                return Err(syn::Error::new(class.span(), "Java Class Path must have more than one component, such as `me.author`"))
            }
        } else {
            // Take the rest of the path components
            punctuated_to_pairs(path)
        };
        
        // Path could have Nested classes, separated by `$`
        let nested_path = if input.parse::<Token![$]>().is_ok() {
            // Parse the Nested Class part of the path `Nested$Nested2`.
            let mut path = Punctuated::<Ident, Token![$]>::parse_separated_nonempty(input)?;
            // Is valid even if it containes only 1 component

            Some(NestedPath {
                // The class is the final component of the path
                final_class: match path.pop() {
                    Some(class) => class.into_value(),
                    None => return Err(syn::Error::new(path.span(), "Nested Class Path must not be empty"))
                },
                classes: punctuated_to_pairs(path)
            })
        } else {
            None
        };

        Ok(Self { class, packages, nested_path })
    }
}
// impl ToTokens for ClassPath {
//     /// Converts the path back to the same tokens it was obtained from: e.g. `java.lang.String`.
//     fn to_tokens(&self, tokens: &mut TokenStream) {
//         tokens.append_all(syn::parse_str::<TokenStream>(&self.to_string()).unwrap())
//     }
// }
impl Display for ClassPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.to_string_helper(".", "."))
    }
}
impl Debug for ClassPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self, f)
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
}
impl SpecialCaseConversion for RustPrimitive {
    fn convert_java_to_rust(&self, value: &TokenStream) -> Option<TokenStream> {
        match *self {
            // // jni::sys::jboolean (u8) must be converted to rust bool
            // Self::Bool => Some(quote_spanned!(value.span()=> bool::from(#value))),
            // Decode UTF-16
            Self::Char => Some(quote_spanned! {value.span()=>
                char::decode_utf16(Some(#value))
                    .next().unwrap()
                    .unwrap_or(char::REPLACEMENT_CHARACTER)
            }),
            // Transmute from the regular (signed) Java Type to the unsigned type
            _ if self.is_unsigned() => {
                let target_ty = Ident::new(&self.to_string(), Span::call_site());
                Some(quote_spanned! {value.span()=> unsafe { ::std::mem::transmute::<_, #target_ty>(#value) } })
            },
            // No more conversion
            _ => None
        }
    }
    fn convert_rust_to_java(&self, value: &TokenStream) -> Option<TokenStream> {
        match *self {
            // Bool must be cast to jni::sys::jboolean (u8)
            Self::Bool => Some(quote_spanned!(value.span()=> #value as ::jni::sys::jboolean)),
            // Char must be encoded to UTF-16 (will panic! if the conversion fails)
            Self::Char => Some(quote_spanned!(value.span()=> #value.encode_utf16(&mut [0;1])[0])),
            // Transmute Rust unsigned integers to Java signed integers
            _ if self.is_unsigned() => Some(quote_spanned!(value.span()=> unsafe { ::std::mem::transmute(#value) })),
            // No more conversion
            _ => None
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
impl From<JavaPrimitive> for RustPrimitive {
    fn from(value: JavaPrimitive) -> Self {
        match value {
            JavaPrimitive::Boolean => RustPrimitive::Bool,
            JavaPrimitive::Char => RustPrimitive::Char,
            JavaPrimitive::Byte => RustPrimitive::I8,
            JavaPrimitive::Short => RustPrimitive::I16,
            JavaPrimitive::Int => RustPrimitive::I32,
            JavaPrimitive::Long => RustPrimitive::I64,
            JavaPrimitive::Float => RustPrimitive::F32,
            JavaPrimitive::Double => RustPrimitive::F64,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum JavaPrimitive {
    Byte, Boolean, Char,
    Short, Int, Long,
    Float, Double
}
impl SigType for JavaPrimitive {
    fn sig_char(&self) -> Ident {
        let c = match self {
            Self::Byte    => 'b',
            Self::Boolean => 'z',
            Self::Char    => 'c',
            Self::Short   => 's',
            Self::Int     => 'i',
            Self::Long    => 'j',
            Self::Float   => 'f',
            Self::Double  => 'd',
        };
        Ident::new(&c.to_string(), Span::call_site())
    }
    fn sig_type(&self) -> LitStr {
        LitStr::new(
            self.sig_char()
                .to_string()
                .chars()
                .next()
                .unwrap()
                .to_uppercase()
                .collect::<String>()
                .as_str(),
            Span::call_site()
        )
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

#[cfg(test)]
mod tests {
    use syn::parse::Parser;

    use super::*;

    #[test]
    fn class_path() {
        fn parse_str_with<P>(s: &str, parser: P) -> syn::Result<P::Output>
        where P: syn::parse::Parser {
            Parser::parse2(parser, syn::parse_str(s)?)
        }

        syn::parse_str::<ClassPath>("me.author.MyClass").unwrap();
        syn::parse_str::<ClassPath>("me.MyClass").unwrap();
        syn::parse_str::<ClassPath>("me.author.MyClass$Nested").unwrap();
        syn::parse_str::<ClassPath>("me.author.MyClass$Nested$Nested2").unwrap();
        parse_str_with("me.author.MyClass.method", ClassPath::parse_with_trailing_method).unwrap();
        parse_str_with("me.MyClass.method", ClassPath::parse_with_trailing_method).unwrap();
        parse_str_with("me.author.MyClass$Nested.method", ClassPath::parse_with_trailing_method).unwrap();
        parse_str_with("me.author.MyClass$Nested$Nested2.method", ClassPath::parse_with_trailing_method).unwrap();
        
        // Test errors
        syn::parse_str::<ClassPath>("").unwrap_err();
        syn::parse_str::<ClassPath>("me").unwrap_err();
        syn::parse_str::<ClassPath>("me.author.MyClass.").unwrap_err();
        syn::parse_str::<ClassPath>(".me.author.MyClass").unwrap_err();
        syn::parse_str::<ClassPath>("MyClass$Nested").unwrap_err();
        syn::parse_str::<ClassPath>("me.author.MyClass$").unwrap_err();
        syn::parse_str::<ClassPath>("me.author.MyClass$Nested$").unwrap_err();
        parse_str_with("MyClass.method", ClassPath::parse_with_trailing_method).unwrap_err();
        parse_str_with("MyClass$Nested.method", ClassPath::parse_with_trailing_method).unwrap_err();
    }
}
