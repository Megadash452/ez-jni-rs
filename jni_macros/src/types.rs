use proc_macro2::{Span, TokenStream};
use quote::quote_spanned;
use syn::{bracketed, parse::{discouraged::Speculative as _, Parse}, punctuated::Punctuated, Ident, LitStr, Token};
use itertools::Itertools as _;
use std::{fmt::{Display, Debug}, str::FromStr};
use crate::utils::{join_spans, Spanned};

/// A keyword only used in the *[`Parameter`] value* to indicate that the value is `JOBject::null()`.
pub static NULL_KEYWORD: &str = "null";

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

/// A general `Type` to interface between Java and Rust types.
#[derive(Debug)]
pub enum Type {
    /// Any **primitive** or **Class**.
    /// 
    /// Asserts that the value can't be `null`.
    /// Will cause a `panic!` the value assossiated with this type is `null`.
    Assertive(InnerType),
    Array(ArrayType),
    /// A Nullable **Object** or **Array**.
    Option {
        /// The `"Option"` token.
        ident: Ident,
        ty: OptionType,
    },
    // Might also add generics
}
/// Declares that a Java value can be **null**.
/// Can't have *primitive* types themselves, but can have *array of primitive*.
#[derive(Debug)]
pub enum OptionType {
    Object(Class),
    Array(ArrayType)
}
impl Type {
    /// Returns whether the [`InnerType`] of the type is a *primitive*.
    /// Returns `false` if this is an [`Array`][Type::Array].
    pub fn is_primitive(&self) -> bool {
        match self {
            Self::Assertive(InnerType::RustPrimitive { .. } | InnerType::JavaPrimitive { .. }) => true,
            _ => false,
        }
    }
}
impl SigType for Type {
    fn sig_char(&self) -> Ident {
        match self {
            Self::Assertive(ty) => ty.sig_char(),
            Self::Array(array) => array.sig_char(),
            Self::Option { ty: OptionType::Object(class), .. } => class.sig_char(),
            Self::Option { ty: OptionType::Array(array), .. } => array.sig_char(),
        }
    }
    fn sig_type(&self) -> LitStr {
        match self {
            Self::Assertive(ty) => ty.sig_type(),
            Self::Array(array) => array.sig_type(),
            Self::Option { ty: OptionType::Object(class), .. } => class.sig_type(),
            Self::Option { ty: OptionType::Array(array), .. } => array.sig_type(),
        }
    }
}
impl SpecialCaseConversion for Type {
    fn convert_java_to_rust(&self, value: &TokenStream) -> Option<TokenStream> {
        match self {
            // Panic if the value is NULL and user did not use Option
            Self::Assertive(InnerType::Object(_))
            | Self::Array(_) => Some({
                let conversion = {
                    let value = quote_spanned! {value.span()=> v};
                    match self {
                        Self::Assertive(InnerType::Object(class)) => class.convert_java_to_rust(&value),
                        Self::Array(array) => array.convert_java_to_rust(&value),
                        _ => panic!("Unreachable")
                    }.unwrap_or(value)
                };
                
                quote::quote_spanned! {conversion.span()=> {
                    let v = #value;
                    if v.is_null() {
                        panic!("Expected Object to not be NULL")
                    }
                    #conversion
                } }
            }),
            Self::Assertive(ty) => ty.convert_java_to_rust(value),
            // Wrap the Type in Option if user expects value to possibly be NULL
            Self::Option { ident, ty } => Some({
                let some = quote_spanned! {ident.span()=> ::std::option::#ident::Some};
                let none = quote_spanned! {ident.span()=> ::std::option::#ident::None};
                let conversion = {
                    let value = quote_spanned! {value.span()=> v};
                    match ty {
                        OptionType::Array(array) => array.convert_java_to_rust(&value),
                        OptionType::Object(class) => class.convert_java_to_rust(&value),
                    }.unwrap_or(value)
                };

                quote::quote_spanned! {conversion.span()=> {
                    let v = #value;
                    if v.is_null() {
                        #none
                    } else {
                        #some(#conversion)
                    }
                } }
            }),
        }
    }
    fn convert_rust_to_java(&self, value: &TokenStream) -> Option<TokenStream> {
        match self {
            Self::Assertive(ty) => ty.convert_rust_to_java(value),
            Self::Array(array) => array.convert_rust_to_java(value),
            Self::Option { ty: OptionType::Object(class), .. } => class.convert_rust_to_java(value),
            Self::Option { ty: OptionType::Array(array), .. } => array.convert_rust_to_java(value),
        }
    }
}
impl Spanned for Type {
    fn span(&self) -> Span {
        match self {
            Self::Assertive(ty) => ty.span(),
            Self::Array(array) => array.span(),
            Self::Option { ident, ty: OptionType::Object(class) } => join_spans([ident.span(), class.span()]),
            Self::Option { ident, ty: OptionType::Array(array) } => join_spans([ident.span(), array.span()]),
        }
    }
}
impl Parse for Type {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let fork = input.fork();
        // Check if caller uses void, Option, or Result
        if let Ok(ident) = fork.parse::<Ident>() {
            match ident.to_string().as_str() {
                "void" => return Err(syn::Error::new(ident.span(), "Can't use 'void' here.")),
                "Result" => return Err(syn::Error::new(ident.span(), "Can't use 'Result' here.")),
                "Option" => {
                    input.advance_to(&fork);
                    return Ok(Self::Option { ident, ty: input.parse()? })
                },
                _ => {},
            }
        }
        drop(fork);

        // Attempt to parse Array, and finally InnerType (primitive or Object).
        Ok(if input.lookahead1().peek(syn::token::Bracket) {
            Self::Array(input.parse()?)
        } else {
            Self::Assertive(input.parse()?)
        })
    }
}
impl Parse for OptionType {
    /// Parses the inner part of the Option Type (i.e. `<Type>`).
    /// Caller must ensure that they parse an [`Ident`] of `Option` directly before calling this.
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        // Parse inner type of Option
        input.parse::<Token![<]>()
        .map_err(|err| input.error(format!("Option takes generic arguments; {err}")))?;
        // Check if the Type is wrapped in Brackets (Array)
        let option_ty = if input.lookahead1().peek(syn::token::Bracket) {
            OptionType::Array(input.parse()?)
        } else {
            match input.parse::<InnerType>()? {
                InnerType::Object(class) => OptionType::Object(class),
                ty => return Err(syn::Error::new(ty.span(), "Primitives are not allowed as the Option's inner type. Only Classes are allowed here."))
            }
        };
        input.parse::<Token![>]>()
            .map_err(|err| input.error(format!("Option takes only 1 generic argument; {err}")))?;

        Ok(option_ty)
    }
}
impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Assertive(ty) => Display::fmt(ty, f),
            Self::Array(array) => Display::fmt(array, f),
            Self::Option { ty, .. } => {
                f.write_str("Option<")?;
                match ty {
                    OptionType::Object(class) => Display::fmt(class, f),
                    OptionType::Array(array) => Display::fmt(array, f),
                }?;
                f.write_str(">")
            }
        }
    }
}

#[derive(Debug)]
pub enum ArrayType {
    /// Regular Array with **non-null** values.
    Assertive(InnerType),
    /// Array with **nullable** values.
    Option {
        /// The `"Option"` token.
        ident: Ident,
        // TODO: Use OptionType instead when there is support for multi-dimensional Arrays
        class: Class,
    },
}
impl Spanned for ArrayType {
    fn span(&self) -> Span {
        // join_spans([self.brackets.span.span(), self.ty.span()])
        match self {
            Self::Assertive(ty) => ty.span(),
            Self::Option { ident, class } => join_spans([ident.span(), class.span()]),
        }
    }
}
impl SigType for ArrayType {
    /// Returns 'l' because Arrays are Objects.
    /// See [`origin`](SigType::sig_char()).
    fn sig_char(&self) -> Ident {
        Ident::new("l", self.span())
    }
    fn sig_type(&self) -> LitStr {
        let sig_type = match self {
            Self::Assertive(ty) => ty.sig_type(),
            Self::Option { class, .. } => class.sig_type(),
        };
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
        /// Build a Rust *boxed slice* from a *Java Array* in which the inner type is a **primitive**.
        fn convert_primitive(r_prim: RustPrimitive, value: &TokenStream) -> TokenStream {
            // Use the Rust Primitive for the special case conversion
            // Use the Java Primitive to build the names of the JNI functions
            let j_prim = JavaPrimitive::from(r_prim);

            // The name of the JNI function that puts the Java Array's elements in the slice
            let filler = Ident::new(&format!("get_{j_prim}_array_region"), value.span());
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
                IntoIterator::into_iter(::ez_jni::utils::get_java_prim_array(&(#value), ::jni::JNIEnv::#filler, env.borrow_mut()))
                    .map(|v| #conversion)
                    .collect::<::std::boxed::Box<[_]>>()
            } }
        }
        // Build a Rust *boxed slice* from a *Java Array* in which the inner type is an **Object**.
        let convert_object = |class: &Class, nullable: bool| -> TokenStream {
            // Must be the full Array Type. E.g. "[Ljava/lang/String;"
            let array_ty = self.sig_type();

            // The inner Class of the array might require some conversion
            let conversion = {
                let element_tokens = quote_spanned!(value.span()=> _element); 
                // Convert using the variable
                class.convert_java_to_rust(&element_tokens)
                    .unwrap_or(element_tokens)
            };
            let null_check = if nullable {
                quote_spanned! {value.span() => {
                    if _element.is_null() {
                        ::std::option::Option::None
                    } else {
                        ::std::option::Option::Some(#conversion)
                    }
                } }
            } else {
                // Panic if the Array contains `null` objects.
                let null_err = format!("Array of {class} contains null elements. If this is intended, wrap the Class with 'Option' (e.g. Option<{class}>)");
                quote_spanned! {value.span() => {
                    if _element.is_null() {
                        panic!(#null_err)
                    } else {
                        #conversion
                    }
                } }
            };

            quote_spanned! {value.span() => {
                use ::std::borrow::BorrowMut as _;
                IntoIterator::into_iter(
                    ::ez_jni::utils::get_object_array(&(#value), Some(#array_ty), env.borrow_mut())
                        // Error can only be ClassMismatch
                        .unwrap_or_else(|err| panic!("{err}"))
                )
                    .map(|_element| #null_check)
                    .collect::<Box<[_]>>()
            } }
        };

        Some(match &self {
            Self::Assertive(ty) => match ty {
                InnerType::JavaPrimitive { ty, .. } => convert_primitive(RustPrimitive::from(*ty), value),
                InnerType::RustPrimitive { ty, .. } => convert_primitive(*ty, value),
                InnerType::Object(class) => convert_object(class, false),
            },
            Self::Option { class, .. } => convert_object(class, true),
        })
    }
    /// Returns code that converts a *Rust slice* to a *Java Array*.
    /// 
    /// Always returns [`Some`].
    /// 
    /// See [`origin`](SpecialCaseConversion::convert_java_to_rust()).
    fn convert_rust_to_java(&self, value: &TokenStream) -> Option<TokenStream> {
        /// Build a *Java Array* from a Rust *slice* in which the inner type is a **primitive**.
        fn convert_primitive(r_prim: RustPrimitive, value: &TokenStream) -> TokenStream {
            // Use the Rust Primitive for the special case conversion
            // Use the java primitive to build the names of the JNI functions
            let j_prim = JavaPrimitive::from(r_prim);

            let alloc = Ident::new(&format!("new_{j_prim}_array"), value.span());
            let filler = Ident::new(&format!("set_{j_prim}_array_region"), value.span());
            // The inner type of the array might require some conversion
            let converted = {
                match r_prim.convert_rust_to_java(&quote_spanned! {value.span()=> v}) {
                    Some(conversion) => &quote_spanned!(value.span()=>
                        #value.iter()
                            .map(|&v| #conversion)
                            .collect::<::std::boxed::Box<[_]>>()
                    ),
                    None => value
                }
            };

            quote_spanned! {value.span()=> {
                use ::std::borrow::BorrowMut as _;
                let _slice = &(#converted);
                ::ez_jni::utils::create_java_prim_array(
                    ::std::convert::AsRef::<[_]>::as_ref(_slice),
                    ::jni::JNIEnv::#alloc,
                    ::jni::JNIEnv::#filler,
                env.borrow_mut())
            } }
        }
        /// Build a *Java Array* from a Rust *slice* in which the inner type is an **Object**.
        fn convert_object(class: &Class, value: &TokenStream) -> TokenStream {
            // Must be only the Class, without the Object char or Array part. E.g. "java/lang/String"
            let class_path = LitStr::new(&class.to_jni_class_path(), value.span());

            // The inner Class of the array might require some conversion
            let converted = {
                match class.convert_rust_to_java(&quote_spanned! {value.span()=> v}) {
                    Some(conversion) => &quote_spanned!(value.span()=>
                        (&(#value)).iter()
                            .map(|v| #conversion)
                            .collect::<::std::boxed::Box<[_]>>()
                    ),
                    None => value
                }
            };

            quote_spanned! {value.span()=> {
                use ::std::borrow::BorrowMut as _;
                let _slice = &(#converted);
                ::ez_jni::utils::create_object_array(
                    ::std::convert::AsRef::<[_]>::as_ref(_slice),
                    #class_path,
                env.borrow_mut())
            } }
        }

        Some(match &self {
            Self::Assertive(ty) => match ty {
                InnerType::JavaPrimitive { ty, .. } => convert_primitive(RustPrimitive::from(*ty), value),
                InnerType::RustPrimitive { ty, .. } => convert_primitive(*ty, value),
                InnerType::Object(class) => convert_object(class, value),
            },
            Self::Option { class, .. } => todo!("Option not yet supported when converting Rust value to Java value")
        })
    }
}
impl Parse for ArrayType {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let inner;
        bracketed!(inner in input);
        let fork = inner.fork();

        match fork.parse::<Ident>() {
            Ok(ident) if ident.to_string() == "Option" => {
                inner.advance_to(&fork);
                drop(fork);
                match inner.parse::<OptionType>()? {
                    OptionType::Object(class) => Ok(Self::Option { ident, class }),
                    OptionType::Array(array) => Err(syn::Error::new(array.span(), "Multi-dimensional Arrays not yet supported.")),
                }
            },
            _ => Ok(Self::Assertive(inner.parse()?))
        }
    }
}
impl Display for ArrayType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("[")?;
        match self {
            Self::Assertive(ty) => Display::fmt(ty, f),
            Self::Option { class, .. } => {
                f.write_str("Option<")?;
                Display::fmt(class, f)?;
                f.write_str(">")
            }
        }?;
        f.write_str("]")
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
    Object(Class),
}
impl Spanned for InnerType {
    fn span(&self) -> Span {
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
        
        // Found void, but is not a ClassPath
        if ident_str == "void" && fork.parse::<Token![.]>().is_err() {
            return Err(syn::Error::new(ident.span(), "'void' is not allowed here."))
        }
        if ident_str == "Result" && fork.parse::<Token![<]>().is_ok() {
            return Err(syn::Error::new(ident.span(), "'Result' is not allowed as an inner type, it must be the outermost type."))
        }
        if ident_str == "Option" && fork.parse::<Token![<]>().is_ok() {
            return Err(syn::Error::new(ident.span(), "'Option' is not allowed here."))
        }

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
/// Allows some *short hands* like `String` or `Object`, which are automatically expanded to their full Class Path.
/// Otherwise, a class with a *single component* is not allowed.
#[derive(Clone)]
pub enum Class {
    Short(Ident),
    Path {
        packages: Vec<(Ident, Token![.])>,
        /// The last Ident in the Punctuated list.
        class: Ident,
        nested_path: Option<NestedPath>
    }
}
#[derive(Clone)]
pub struct NestedPath {
    pub classes: Vec<(Ident, Token![$])>,
    pub final_class: Ident
}
impl Class {
    const VALID_SHORTHANDS: [&str; 2] = ["String", "Object"];

    /// Parses the [`ClassPath`] with [`Parse::parse()`], but the final path *component* is a method name.
    /// 
    /// Returns the parsed [`ClassPath`] and the **method name**.
    pub fn parse_with_trailing_method(input: syn::parse::ParseStream) -> syn::Result<(Self, Ident)> {
        match Self::parse(input)? {
            Self::Short(class) => return Err(syn::Error::new(class.span(), format!("Must provide method name; e.g. \"{class}\".method"))),
            Self::Path { mut packages, mut class, nested_path } => {
                match &nested_path {
                    // If the ClassPath contains a NestedPath with `$`,
                    // it will not have parsed the next Dot and component 
                    // Parse the Dot and method name
                    Some(_) => {
                        input.parse::<Token![.]>()?;
                        let method_name = input.parse::<Ident>()?;
                        Ok((Self::Path { packages, class, nested_path }, method_name))
                    },
                    // If the path had no Nested Classes (i.e. it was only separated by Dots)
                    // the method name would have already been parsed, so take it out of the path.
                    None => {
                        // This implies that the path must be at least 3 components long: 2 for the path and 1 for the method name
                        if packages.len() < 2 {
                            let first = &packages[0].0;
                            return if Self::VALID_SHORTHANDS.contains(&first.to_string().as_str()) {
                                Ok((Self::Short(first.clone()), class))
                            } else {
                                Err(syn::Error::new(first.span(), "Java Class Path must have at least 1 package component (aside from the Class and method), such as `package.Class.method`"))
                            }
                        }
        
                        let method_name = class;
                        class = packages.pop().unwrap().0;
                        Ok((Self::Path { packages, class, nested_path: None }, method_name))
                    }
                }
            }
        }
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
        match self {
            Self::Short(class) => {
                let expanded_path = match class.to_string().as_str() {
                    "String" => ["java", "lang", "String"],
                    "Object" => ["java", "lang", "Object"],
                    _ => panic!("Unreachable")
                };
                expanded_path.into_iter()
                    .intersperse(sep)
                    .collect::<String>()
            },
            Self::Path { packages, class, nested_path } => {
                let nested_classes = match &nested_path {
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
        
                packages.iter()
                    .map(|(ident, _)| ident.to_string())
                    .chain(Some(class.to_string()))
                    .intersperse(sep.to_string())
                    .chain(nested_classes)
                    .collect::<String>()
            }
        }
    }
}
impl SigType for Class {
    fn sig_char(&self) -> Ident {
        Ident::new("l", self.span())
    }
    fn sig_type(&self) -> LitStr {
        LitStr::new(&format!("L{};", self.to_jni_class_path()), self.span())
    }
}
impl SpecialCaseConversion for Class {
    fn convert_java_to_rust(&self, value: &TokenStream) -> Option<TokenStream> {
        fn string_conversion(value: &TokenStream) -> TokenStream {
            quote_spanned! {value.span()=> {
                use ::std::borrow::BorrowMut as _;
                ::ez_jni::utils::get_string(::jni::objects::JString::from(#value), env.borrow_mut())
            } }
        }
        // Special cases for Java Objects
        match self {
            Self::Short(class) if class.to_string() == "String" => Some(string_conversion(value)),
            _ if self.to_jni_class_path() == "java/lang/String" => Some(string_conversion(value)),
            _ => None
        }
    }
    fn convert_rust_to_java(&self, value: &TokenStream) -> Option<TokenStream> {
        fn string_conversion(value: &TokenStream) -> TokenStream {
            quote_spanned! {value.span()=> {
                use ::std::borrow::BorrowMut as _;
                use ::ez_jni::utils::AsNullableStrArg as _;
                (&(#value)).as_string_arg(env.borrow_mut())
            } }
        }
        match self {
            // Convert Rust String to Java String
            Self::Short(class) if class.to_string() == "String" => Some(string_conversion(value)),
            _ if self.to_jni_class_path() == "java/lang/String" => Some(string_conversion(value)),
            _ => None
        }
    }
}
impl Spanned for Class {
    /// Builds a [`TokenStream`] out of this path's items and returns its [`span`][TokenStream::span()].
    fn span(&self) -> Span {
        match self {
            Self::Short(ident) => ident.span(),
            Self::Path { packages, class, nested_path } => {
                let mut spans = Vec::new();
                spans.extend(packages.iter()
                    .map(|(ident, dot)| join_spans([ident.span(), dot.span]))
                );
                spans.push(class.span());
                if let Some(nested_path) = &nested_path {
                    spans.extend(nested_path.classes.iter()
                        .map(|(ident, dol)| join_spans([ident.span(), dol.span]))
                    );
                    spans.push(nested_path.final_class.span())
                }
                
                join_spans(spans.into_iter())
            }
        }
    }
}
impl Parse for Class {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        /// Converts a [`Punctuated`] whose last element is [`None`] to a [`Vec`] of pairs.
        fn punctuated_to_pairs<T, P>(punctuated: Punctuated<T, P>) -> Vec<(T, P)> {
            punctuated.into_pairs()
                .into_iter()
                .map(|pair| {
                    let pair = pair.into_tuple();
                    (pair.0, pair.1.expect("Parsed without accepting trailing punct, but still found trailing punct"))
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
        // Check for short hands
        let packages = if path.is_empty() {
            return if Self::VALID_SHORTHANDS.contains(&class.to_string().as_str()) {
                Ok(Self::Short(class))
            } else {
                Err(syn::Error::new(class.span(), "Java Class Path must have more than one component, such as `me.author`"))
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

        Ok(Self::Path { class, packages, nested_path })
    }
}
// impl ToTokens for Class {
//     /// Converts the path back to the same tokens it was obtained from: e.g. `java.lang.String`.
//     fn to_tokens(&self, tokens: &mut TokenStream) {
//         tokens.append_all(syn::parse_str::<TokenStream>(&self.to_string()).unwrap())
//     }
// }
impl Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.to_string_helper(".", "."))
    }
}
impl Debug for Class {
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
            Self::Bool => Some(quote_spanned!(value.span()=> (#value) as ::jni::sys::jboolean)),
            // Char must be encoded to UTF-16 (will panic! if the conversion fails)
            Self::Char => Some(quote_spanned!(value.span()=> (#value).encode_utf16(&mut [0;1])[0])),
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

        syn::parse_str::<Class>("me.author.MyClass").unwrap();
        syn::parse_str::<Class>("me.MyClass").unwrap();
        syn::parse_str::<Class>("me.author.MyClass$Nested").unwrap();
        syn::parse_str::<Class>("me.author.MyClass$Nested$Nested2").unwrap();
        parse_str_with("me.author.MyClass.method", Class::parse_with_trailing_method).unwrap();
        parse_str_with("me.MyClass.method", Class::parse_with_trailing_method).unwrap();
        parse_str_with("me.author.MyClass$Nested.method", Class::parse_with_trailing_method).unwrap();
        parse_str_with("me.author.MyClass$Nested$Nested2.method", Class::parse_with_trailing_method).unwrap();
        
        // Test errors
        syn::parse_str::<Class>("").unwrap_err();
        syn::parse_str::<Class>("me").unwrap_err();
        syn::parse_str::<Class>("me.author.MyClass.").unwrap_err();
        syn::parse_str::<Class>(".me.author.MyClass").unwrap_err();
        syn::parse_str::<Class>("MyClass$Nested").unwrap_err();
        syn::parse_str::<Class>("me.author.MyClass$").unwrap_err();
        syn::parse_str::<Class>("me.author.MyClass$Nested$").unwrap_err();
        parse_str_with("MyClass.method", Class::parse_with_trailing_method).unwrap_err();
        parse_str_with("MyClass$Nested.method", Class::parse_with_trailing_method).unwrap_err();
    }
}
