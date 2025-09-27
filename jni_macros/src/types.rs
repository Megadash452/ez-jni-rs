use proc_macro2::{Span, TokenStream};
use quote::{quote_spanned, ToTokens};
use syn::{bracketed, parse::{discouraged::Speculative as _, Parse}, punctuated::Punctuated, token::Bracket, Ident, LitStr, Token};
use itertools::Itertools as _;
use std::{fmt::{Debug, Display}, num::NonZeroU32, str::FromStr};
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

/// Generate code to perform some kind of necessary conversion between *Rust* and *Java* values.
/// For example, convert a *slice* to a *Java Array*, convert *UTF-8* to *UTF-16*, etc.
/// 
/// All types in this file implement this trait.
/// 
/// This is mainly used for [`jni_fn`][crate::jni_fn::jni_fn()].
pub trait Conversion {
    /// Returns the code necessary (if any) for *converting* a value of a certain **Java** type to a corresponding **Rust** type.
    /// 
    /// Returns [`None`] if no conversion is necessary.
    /// 
    /// **value** is the tokens/expression representing the value that must be converted.
    /// 
    /// This is used for the *return* type.
    fn convert_java_to_rust(&self, value: &TokenStream) -> Option<TokenStream>;
    /// Returns the code necessary (if any) for *converting* a value of a certain **Rust** type to a corresponding **Java** type.
    /// 
    /// Returns [`None`] if no conversion is necessary.
    /// 
    /// **value** is the tokens/expression representing the value that must be converted.
    /// 
    /// This is used for *parameter* types.
    fn convert_rust_to_java(&self, value: &TokenStream) -> Option<TokenStream>;
    /// Gets the **Rust** type *name/path* of a [`Type`].
    /// 
    /// **as_ref**: Whether the resulting Rust type should be the Unsized reference type.
    /// E.g. `[char]` instead of `Box<[char]>`.
    /// Is `true` when used for [`ToJValue`] and `false` for [`FromValue`].
    /// 
    /// **is_nested**: Whether the `Type` `self` is nested within another `Type`.
    /// E.g. `char` is nested in `Box<[char]>` and `Option<char>`.
    /// 
    /// An optional **lifetime** can be passed to when class is [`Object`][Class::is_jobject()] type.
    /// This is useful for [`JniFn`][crate::jni_fn::JniFn], which needs the same explicit lifetime for all parameters.
    fn type_tokens(&self, as_ref: bool, is_nested: bool, lifetime: Option<syn::Lifetime>) -> TokenStream;
}

/// A general `Type` to interface between Java and Rust types.
/// This type is ***RECURSIVE*** because of [`InnerType::Array`].
#[derive(Debug, Clone)]
pub enum Type {
    /// Any other [`InnerType`].
    /// 
    /// Asserts that the value can't be `null`.
    /// Will cause a `panic!` the value assossiated with this type is `null`.
    Assertive(InnerType),
    /// A Nullable **Object** or **Array**.
    Option {
        /// The `"Option"` token.
        ident: Ident,
        ty: InnerType,
    },
    // Might also add generics
}
impl Type {
    /// Returns whether the [`InnerType`] of the type is a *primitive*.
    /// Returns `false` if this is an [`Array`][Type::Array].
    #[allow(unused)]
    pub fn is_primitive(&self) -> bool {
        match self {
            Self::Assertive(InnerType::RustPrimitive { .. } | InnerType::JavaPrimitive { .. }) => true,
            _ => false,
        }
    }

    /// General function to convert a [`JValue`][jni::objects::JValueGen] to a **Rust value**.
    /// 
    /// Used to generate the conversion for the *return value* of a JNI call.
    pub fn convert_jvalue_to_rvalue(&self, value: &TokenStream) -> TokenStream {
        // Get a concrete Rust type to tell FromJValue to use
        let ty = match self {
            // Check if the Array's inner type is also Array:
            // Arrays above 1 dimensions have a FromJValue implementation, but using the generated conversion code is more stable
            Self::Assertive(InnerType::Array(array))
            | Self::Option { ty: InnerType::Array(array), .. }
            if matches!(&*array.ty, Type::Assertive(InnerType::Array(_)) | Type::Option { ty: InnerType::Array(_), .. })
                // Unwrap JValue to JObject and generate conversion code
                => return array.convert_java_to_rust(&quote_spanned! {value.span()=> (#value).l().unwrap() }),
            // Use special trait to unwrap Owned version of Object
            Self::Assertive(InnerType::Object(class))
            | Self::Option { ty: InnerType::Object(class), .. }
            if class.is_jobject() => {
                let ty = self.type_tokens(false, false, None);
                return quote_spanned! {value.span()=> <#ty as ::ez_jni::utils::FromJValueOwned>::from_jvalue_owned_env(#value, env) }
            },
            _ => self.type_tokens(false, false, None),
        };
        // use the FromJValue implementation
        quote_spanned! {value.span()=> <#ty as ::ez_jni::FromJValue>::from_jvalue_env((#value).borrow(), env).unwrap_display() }
    }

    /// General function to convert a **Rust value** to a [`JValue`][jni::objects::JValue].
    /// The expression resolves specifically to the *borrowed* version of [`JValue`][jni::objects::JValue].
    /// 
    /// Used to generate the conversion for the *argument values* of a JNI call.
    pub fn convert_rvalue_to_jvalue(&self, value: &TokenStream) -> TokenStream {
        // Get a concrete Rust type to tell ToJValue to use
        let ty = match self {
            // Check if the Array's inner type is also Array:
            // Arrays above 1 dimensions don't have ToJValue implementations and require generated conversion code
            Self::Assertive(InnerType::Array(array))
            | Self::Option { ty: InnerType::Array(array), .. }
            if matches!(&*array.ty, Type::Assertive(InnerType::Array(_)) | Type::Option { ty: InnerType::Array(_), .. }) => {
                // Wrap the conversion code in a JValue so it can be passed to the arguments
                let conversion = array.convert_rust_to_java(&value);
                return quote_spanned! {value.span()=> ::jni::objects::JValue::Object(&(#conversion)) }
            },
            // Strings must be converted using AsRef instead of Borrow.
            Self::Assertive(InnerType::Object(class))
            | Self::Option { ty: InnerType::Object(class), .. }
            if class.rust_type() == ClassRustType::String => return quote_spanned! {value.span()=>
                ::jni::objects::JValueGen::borrow(&::ez_jni::ToJValue::to_jvalue_env(::std::convert::AsRef::<str>::as_ref(&(#value)), env))
            },
            // Wrap in JValue if type is JObject; no need to allocate another object
            Self::Assertive(InnerType::Object(class))
            | Self::Option { ty: InnerType::Object(class), .. }
            if class.is_jobject() => {
                // JClass or JThrowable can be converted to JObject, which is converted to JValue
                let converted = class.convert_rust_to_java(value);
                let converted = converted.as_ref().unwrap_or(value);
                return quote_spanned! {value.span()=> ::jni::objects::JValue::Object(&(#converted)) };
            },
            _ => self.type_tokens(true, false, None),
        };
        // use the ToJValue implementation
        quote_spanned! {value.span()=> ::jni::objects::JValueGen::borrow(&<#ty as ::ez_jni::ToJValue>::to_jvalue_env((#value).borrow(), env)) }
    }

    /// Convert a *Java `void`* value to a *Rust Unit `()`* value.
    /// 
    /// Even though `void` and `()` are the same, it must still be *unwrapped* from the [`JValue`][::jni::objects::JValueGen].
    pub fn convert_void_to_unit(value: &TokenStream) -> TokenStream {
        quote_spanned! {value.span()=> <() as ::ez_jni::FromJValue>::from_jvalue_env((#value).borrow(), env).unwrap_display() }
    }
}
impl Spanned for Type {
    fn span(&self) -> Span {
        match self {
            Self::Assertive(ty) => ty.span(),
            Self::Option { ident, ty } => join_spans([ident.span(), ty.span()])
        }
    }
}
impl SigType for Type {
    fn sig_char(&self) -> Ident {
        match self {
            Self::Assertive(ty) => ty.sig_char(),
            // Option with primitive is actually a class
            Self::Option { ty: InnerType::JavaPrimitive { ident, ty }, .. }
                => Class::PrimitiveObject { ident: ident.clone(), ty: *ty }.sig_char(),
            Self::Option { ty: InnerType::RustPrimitive { ident, ty }, .. }
                => Class::PrimitiveObject { ident: ident.clone(), ty: JavaPrimitive::from(*ty) }.sig_char(),
            Self::Option { ty, .. } => ty.sig_char(),
        }
    }
    fn sig_type(&self) -> LitStr {
        match self {
            Self::Assertive(ty) => ty.sig_type(),
            // Option with primitive is actually a class
            Self::Option { ty: InnerType::JavaPrimitive { ident, ty }, .. }
                => Class::PrimitiveObject { ident: ident.clone(), ty: *ty }.sig_type(),
            Self::Option { ty: InnerType::RustPrimitive { ident, ty }, .. }
                => Class::PrimitiveObject { ident: ident.clone(), ty: JavaPrimitive::from(*ty) }.sig_type(),
            Self::Option { ty, .. } => ty.sig_type(),
        }
    }
}
impl Conversion for Type {
    fn convert_java_to_rust(&self, value: &TokenStream) -> Option<TokenStream> {
        match self {
            Self::Assertive(ty) => ty.convert_java_to_rust(value),
            // Wrap the Type in Option if user expects value to possibly be NULL
            Self::Option { ty, .. } => Some({
                let ty_tokens = self.type_tokens(false, false, None);
                match ty {
                    // Use FromObjectOwned implementation
                    InnerType::Object(class) if class.is_jobject() =>
                        quote_spanned! {value.span()=> <#ty_tokens as ::ez_jni::FromObjectOwned>::from_object_owned_env(#value, env) },
                    // Use regular FromObject implementation
                    _ => quote_spanned! {value.span()=> <#ty_tokens as ::ez_jni::FromObject>::from_object_env(&(#value), env).unwrap_display() }
                }
            }),
        }
    }
    fn convert_rust_to_java(&self, value: &TokenStream) -> Option<TokenStream> {
        match self {
            Self::Assertive(ty) => ty.convert_rust_to_java(value),
            Self::Option { ty, .. } => Some({
                // DRY HELPERS
                // Simple conversion using the ToObject implementation
                let to_object = || -> TokenStream {
                    // The rust value passed to to_object(). Option needs to map to AsRef because the type_tokens is as_ref
                    let arg = if matches!(ty, InnerType::Array(_))
                    || matches!(ty, InnerType::Object(class) if class.rust_type() == ClassRustType::String) {
                        quote_spanned! {value.span()=> &::std::option::Option::as_ref(&(#value)).map(::std::convert::AsRef::as_ref) }
                    } else {
                        quote_spanned! {value.span()=> &(#value) }
                    };
                    let ty = self.type_tokens(true, false, None);
                    quote_spanned! {value.span()=> <#ty as ::ez_jni::ToObject>::to_object_env(#arg, env) }
                };
                let option_value = &quote_spanned! {value.span()=> v };
                // Converts the Option value manually with a match statement.
                // Callers must use `option_value` as the value instead of the actual `value` argument.
                let match_option = |conversion: Option<TokenStream>| -> TokenStream {
                    let conversion = conversion.unwrap_or_else(|| option_value.clone());
                    quote_spanned! {conversion.span()=>
                        match #value {
                            ::std::option::Option::Some(v) => #conversion,
                            ::std::option::Option::None => ::jni::objects::JObject::null(),
                        }
                    }
                };
                

                match ty {
                    // Manually conver Option<JObject> and its variants
                    InnerType::Object(class) if class.is_jobject() => match_option(class.convert_rust_to_java(option_value)),
                    /* If the InnerType of the Option is a multi-dimensional Array or an Array of String,
                    do the conversion manually to allow use of both the borrowed and owned types (e.g. &str and String, &[_] and Box<[_]>) */
                    InnerType::Array(array) => match array.ty.as_ref() {
                        Type::Assertive(InnerType::Array(_))
                        | Type::Option { ty: InnerType::Array(_), .. } => match_option(Some(array.convert_rust_to_java(option_value))),
                        Type::Assertive(InnerType::Object(class))
                        | Type::Option { ty: InnerType::Object(class), .. }
                        if class.rust_type() == ClassRustType::String => match_option(Some(array.convert_rust_to_java(option_value))),
                        // Otherwise, use ToObject implementation
                        _ => to_object()
                    }
                    // Use ToObject implementation for the type
                    _ => to_object()
                }
            }),
        }
    }
    fn type_tokens(&self, as_ref: bool, is_nested: bool, lifetime: Option<syn::Lifetime>) -> TokenStream {
        match self {
            Type::Assertive(ty) => ty.type_tokens(as_ref, is_nested, lifetime),
            Type::Option { ident: opt_ident, ty } => {
                let ty = ty.type_tokens(as_ref, true, lifetime);
                let option = quote_spanned! {opt_ident.span()=> ::std::option::Option };
                quote_spanned! {ty.span()=> #option<#ty> }
            },
        }
    }
}
impl Parse for Type {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let fork = input.fork();
        // Try parsing "Option"
        match fork.parse::<Ident>() {
            Ok(ident) if ident == "Option" => {
                input.advance_to(&fork);

                // Parse inner type of Option
                input.parse::<Token![<]>()
                    .map_err(|err| input.error(format!("Option takes generic arguments; {err}")))?;
                // Check if the Type is wrapped in Brackets (Array)
                let inner_ty = input.parse::<InnerType>()?;
                input.parse::<Token![>]>()
                    .map_err(|err| input.error(format!("Option takes only 1 generic argument; {err}")))?;

                return Ok(Self::Option { ident, ty: inner_ty })
            },
            _ => { },
        }
        drop(fork);

        // Or parse any other Type
        Ok(Self::Assertive(input.parse()?))
    }
}
impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Assertive(ty) => Display::fmt(ty, f),
            Self::Option { ty, .. } => {
                f.write_str("Option<")?;
                Display::fmt(ty, f)?;
                f.write_str(">")
            }
        }
    }
}

/// A **Type** that can be used as the **inner type** of a generic [`Type`].
#[derive(Debug, Clone)]
pub enum InnerType {
    JavaPrimitive { ident: Ident, ty: JavaPrimitive },
    RustPrimitive { ident: Ident, ty: RustPrimitive },
    Object(Class),
    Array(ArrayType),
}
impl Spanned for InnerType {
    fn span(&self) -> Span {
        match self {
            Self::JavaPrimitive { ident, .. }
            | Self::RustPrimitive { ident, .. } => ident.span(),
            Self::Object(class) => class.span(),
            Self::Array(array) => array.span(),
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
            Self::Array(array) => array.sig_char(),
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
            Self::Array(array) => array.sig_type(),
        }
    }
}
impl Conversion for InnerType {
    fn convert_java_to_rust(&self, value: &TokenStream) -> Option<TokenStream> {
        match self {
            Self::JavaPrimitive { ty, .. } => RustPrimitive::from(*ty).convert_java_to_rust(value),
            Self::RustPrimitive { ty, .. } => ty.convert_java_to_rust(value),
            Self::Object(class) => class.convert_java_to_rust(value),
            Self::Array(array) => <ArrayType as Conversion>::convert_java_to_rust(array, value),
        }
    }
    fn convert_rust_to_java(&self, value: &TokenStream) -> Option<TokenStream> {
        match self {
            Self::JavaPrimitive { ty, .. } => RustPrimitive::from(*ty).convert_rust_to_java(value),
            Self::RustPrimitive { ty, .. } => ty.convert_rust_to_java(value),
            Self::Object(object) => object.convert_rust_to_java(value),
            Self::Array(array) => <ArrayType as Conversion>::convert_rust_to_java(array, value),
        }
    }
    fn type_tokens(&self, as_ref: bool, is_nested: bool, lifetime: Option<syn::Lifetime>) -> TokenStream {
        match self {
            InnerType::JavaPrimitive { ident, ty } => RustPrimitive::from(*ty).to_tokens_spanned(ident.span()),
            InnerType::RustPrimitive { ident, ty } => ty.to_tokens_spanned(ident.span()),
            InnerType::Object(class) => class.type_tokens(as_ref, is_nested, lifetime),
            InnerType::Array(array) => array.type_tokens(as_ref, is_nested, lifetime),
        }
    }
}
impl Parse for InnerType {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        // Attempt to parse Array
        if input.lookahead1().peek(syn::token::Bracket) {
            return Ok(Self::Array(input.parse()?));
        }
        // Attempt to parse Class (save error to return it if parsing all types fails).
        let class_err = match input.parse::<Class>() {
            Ok(class) => return Ok(Self::Object(class)),
            Err(err) => err
        };
        // Parse an Ident, attempt a primitive or some other known symbol
        if let Ok(ident) = input.parse::<Ident>() {
            let ident_str = ident.to_string();

            // Disallowed symbols
            match ident_str.as_str() {
                "void" => return Err(syn::Error::new(ident.span(), "'void' is not allowed here.")),
                "Result" => return Err(syn::Error::new(ident.span(), "'Result' is not allowed as an inner type, it must be the outermost type.")),
                "Option" => return Err(syn::Error::new(ident.span(), "'Option' is not allowed here.")),
                _ => {}
            };

            // Parse primitive
            if let Ok(ty) = RustPrimitive::from_str(&ident_str) {
                return Ok(Self::RustPrimitive { ident, ty });
            } else if let Ok(ty) = JavaPrimitive::from_str(&ident_str) {
                return Ok(Self::JavaPrimitive { ident, ty });
            }
        }

        // All types failed to parse, return Class parse error
        Err(syn::Error::new(class_err.span(), format!("Could not parse a known Type or Class: {class_err}")))
    }
}
impl Display for InnerType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::JavaPrimitive { ty, .. } => Display::fmt(ty, f),
            // Convert form Rust to Java
            Self::RustPrimitive { ty, .. } => Display::fmt(&JavaPrimitive::from(*ty), f),
            Self::Object(class) => Display::fmt(class, f),
            Self::Array(array) => Display::fmt(array, f),
        }
    }
}

#[derive(Clone)]
pub struct ArrayType {
    pub bracket_token: Bracket,
    /// The type of the **Array**'s elements.
    /// 
    /// This type is recursive!!
    pub ty: Box<Type>,
}
impl ArrayType {
    #[allow(unused)]
    pub fn new(inner_ty: Type) -> Self {
        Self::new_spanned(Span::call_site(), Span::call_site(), inner_ty)
    }
    /// Create a new [`ArrayType`] with custom span for the *opening* and *closing* **brackets**.
    pub fn new_spanned(open_span: Span, close_span: Span, inner_ty: Type) -> Self {
        let span = proc_macro2::Group::new(
            proc_macro2::Delimiter::Bracket,
            quote_spanned!(join_spans([open_span, close_span])=> [])
        ).delim_span();
        Self {
            bracket_token: Bracket { span },
            ty: Box::new(inner_ty),
        }
    }

    /// Returns the number of **dimensions** this [`ArrayType`] has.
    /// 
    /// E.g. `[char]` has *1 dimension*, `[[char]]` has *2 dimensions*, and so on...
    #[allow(unused)]
    pub fn dimensions(&self) -> NonZeroU32 {
        let mut dimensions = 0;
        let mut current = self;
        // Iterative implementation :)
        loop {
            dimensions += 1;
            match &*current.ty {
                Type::Assertive(InnerType::Array(next))
                | Type::Option { ty: InnerType::Array(next), .. } => {
                    current = next;
                }
                Type::Assertive(_)
                | Type::Option { .. } => break,
            }
        }

        unsafe { NonZeroU32::new_unchecked(dimensions) } // Safety: Obiously
    }

    /// Returns code that converts a *Java Array* to a *Rust [`Box`]*.
    /// 
    /// If array is *one dimensional*, it defers to the [`ez_jni::FromObject`] implementation.
    /// If array is *multidimensional*, it will generate code to manually convert the Array.
    /// 
    /// See [`origin`](SpecialCaseConversion::convert_java_to_rust()).
    fn convert_java_to_rust(&self, value: &TokenStream) -> TokenStream {
        // DRY helpers
        /// Converts an Object to a Rust slice by calling the respective FromObject implementation.
        /// **ty** is a Rust type, the type of the elements of the slice.
        fn from_object(ty: TokenStream, value: &TokenStream) -> TokenStream {
            quote_spanned! {value.span()=>
                <::std::boxed::Box<[#ty]> as ::ez_jni::FromObject>::from_object_env(&(#value), env).unwrap_display()
            }
        }

        // Build a *Rust Box* from a *Java Array* in which the inner type is an **Object**.
        // 
        // **elem_conversion** is code that applies a conversion to each element to convert it from a *Java Object* to a *Rust Type*.
        // The conversion must use an [`Ident`] **`_element`** as the value.
        // For example, the **elem_conversion** argument can be
        // ```ignore
        // class.convert_java_to_rust(&quote!(_element))
        // ```
        let get_object_arr = |elem_conversion: TokenStream| -> TokenStream {
            quote_spanned! {value.span() => 
                ::ez_jni::utils::get_object_array_converted(&(#value), |_element, #[allow(unused_variables)] env| ::std::result::Result::Ok(#elem_conversion), env).unwrap_display()
            }
        };

        match self.ty.as_ref() {
            // Recursion happens here -v
            Type::Assertive(InnerType::Array(array)) => get_object_arr(array.convert_java_to_rust(&quote_spanned!(value.span()=> _element))),
            // Convert the java value using the FromObject trait
            ty => from_object(ty.type_tokens(false, true, None), value),
        }
    }
    /// Returns code that converts a *Rust slice* to a *Java Array*.
    /// 
    /// If array is *one dimensional*, it defers to the [`ez_jni::ToObject`] implementation.
    /// If array is *multidimensional*, it will generate code to manually convert the Array.
    /// 
    /// See [`origin`](SpecialCaseConversion::convert_java_to_rust()).
    fn convert_rust_to_java(&self, value: &TokenStream) -> TokenStream {
        // DRY helpers
        /// Converts a Rust slice to an Object by calling the respective ToObject implementation.
        /// **ty** is a Rust type, the type of the elements of the slice (in this case its either a primitve or `_`).
        fn to_object(ty: &Type, value: &TokenStream) -> TokenStream {
            let ty = ty.type_tokens(true, true, None);
            quote_spanned! {value.span()=>
                ::ez_jni::ToObject::to_object_env(::std::convert::AsRef::<[#ty]>::as_ref(&(#value)), env)
            }
        }

        /// Build a *Java Array* from a Rust *slice* in which the inner type is an **Object**.
        /// 
        /// **elem_conversion** is code that applies a conversion to each element to convert it from a *Rust Type* to a *Java Object*.
        /// The conversion must use an [`Ident`] **`_element`** as the value.
        /// For example, the **elem_conversion** argument can be
        /// ```ignore
        /// class.convert_rust_to_java(&quote!(_element))
        /// ```
        fn create_obj_array(elem_conversion: TokenStream, value: &TokenStream) -> TokenStream {
            quote_spanned! {value.span()=>
                ::ez_jni::utils::create_object_array_converted(
                    ::std::convert::AsRef::<[_]>::as_ref(&(#value)),
                    |_element, #[allow(unused_variables)] env| #elem_conversion,
                env)
            }
        }

        match self.ty.as_ref() {
            // When the array is multi-dimensionsal the array conversion must be done manually with create_obj_array() to allow use of `Box<[_]>`, `Vec<_>`, `&[_]`.
            // Recursion happens here -v
            Type::Assertive(InnerType::Array(_))
            | Type::Option { ty: InnerType::Array(_), .. } => create_obj_array(
                self.ty.convert_rust_to_java(&quote_spanned! {value.span()=> _element}).unwrap(),
                value
            ),
            /* When the inner type of the array is String (for any number of dimensions),
               the array conversion must be done manually with create_obj_array() to allow use of both `String` and `&str`. */
            Type::Assertive(InnerType::Object(class))
            | Type::Option { ty: InnerType::Object(class), .. }
            if class.rust_type() == ClassRustType::String => create_obj_array(
                self.ty.convert_rust_to_java(&quote_spanned! {value.span()=> _element}).unwrap(),
                value
            ),
            // All other types can use the ToObject implementation normally
            ty => to_object(ty, value),
        }
    }
}
impl Spanned for ArrayType {
    fn span(&self) -> Span {
        join_spans([self.bracket_token.span.open(), self.ty.span(), self.bracket_token.span.close()])
    }
}
impl SigType for ArrayType {
    /// Returns 'l' because Arrays are Objects.
    /// See [`origin`](SigType::sig_char()).
    fn sig_char(&self) -> Ident {
        Ident::new("l", self.span())
    }
    fn sig_type(&self) -> LitStr {
        let sig_type = self.ty.sig_type();
        LitStr::new(&format!("[{}", sig_type.value()), sig_type.span())
    }
}
impl Conversion for ArrayType {
    fn convert_java_to_rust(&self, value: &TokenStream) -> Option<TokenStream> {
        Some(Self::convert_java_to_rust(&self, value))
    }
    fn convert_rust_to_java(&self, value: &TokenStream) -> Option<TokenStream> {
        Some(Self::convert_rust_to_java(&self, value))
    }
    /// WARNING: recursive
    /// 
    /// See [`origin`][Conversion::type_tokens()].
    fn type_tokens(&self, as_ref: bool, is_nested: bool, lifetime: Option<syn::Lifetime>) -> TokenStream {
        // vvv Recursion occurs here vvv
        let ty = self.ty.type_tokens(as_ref, true, lifetime);
        if as_ref && is_nested {
            quote_spanned! {self.span()=> &[#ty] }
        } else if as_ref {
            quote_spanned! {self.span()=> [#ty] }
        } else {
            quote_spanned! {self.span()=> ::std::boxed::Box<[#ty]> }
        }
    }
}
impl Parse for ArrayType {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let inner;
        let bracket_token = bracketed!(inner in input);

        Ok(Self {
            bracket_token,
            ty: inner.parse()?,
        })
    }
}
impl Display for ArrayType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("[")?;
        Display::fmt(&self.ty, f)?;
        f.write_str("]")
    }
}
impl Debug for ArrayType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ArrayType")
            .field("ty", &self.ty)
            .finish()
    }
}

/// A representation of the *Rust type* that should be used for a value.
/// 
/// This is vague, as the type could be a reference, or could be an unsized counterpart (i.e. `str`),
/// So [`Class`] is needed for that context.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClassRustType {
    JObject, JClass, JThrowable, JString, String
}
impl FromStr for ClassRustType {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Object"    => Ok(Self::JObject),
            "Class"     => Ok(Self::JClass),
            "Throwable" => Ok(Self::JThrowable),
            "Exception" => Ok(Self::JThrowable),
            "String"    => Ok(Self::String),
            _ => Err(()),
        }
    }
}
impl Display for ClassRustType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::JObject    => "Object",
            Self::JClass     => "Class",
            Self::JThrowable => "Throwable",
            Self::JString    => "JString",
            Self::String     => "String",
        })
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
    /// The class is a *shorthand* of a full *class path*. E.g. `String` for `java.lang.String`
    Short(Ident),
    /// The class is a *shorthand* of a class that wraps a **primitive**. E.g. `Boolean` for `java.lang.Boolean`
    PrimitiveObject { ident: Ident, ty: JavaPrimitive },
    // A full *class path*.
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
    const EMPTY_PATH_ERROR: &str = "Java Class Path must not be empty";
    const SINGLE_COMPONENT_ERROR: &str = "Java Class Path must have more than one component, such as `me.author`";

    /// Constructs the [`Class::Path`] variant using the provided **path**.
    /// All tokens will inherit the given **span**.
    pub fn new_path(path: &[&str], span: Span) -> syn::Result<Self> {
        if path.is_empty() {
            return Err(syn::Error::new(span, Self::EMPTY_PATH_ERROR));
        }
        if path.len() == 1 {
            return Err(syn::Error::new(span, Self::SINGLE_COMPONENT_ERROR));
        }
        let dot = syn::parse2::<Token![.]>(quote_spanned!(span=> . )).unwrap();

        let packages = path[..(path.len() - 2)].into_iter()
            .map(|&package| (Ident::new(package, span), dot.clone()))
            .collect::<Vec<_>>();
        let class = Ident::new(path.last().unwrap(), span);

        Ok(Self::Path { packages, class, nested_path: None })
    }

    #[allow(unused)]
    /// Creates a [`Class`] from a known *Rust Type* that can represent an object Class.
    pub fn from_rust_type(ty: ClassRustType, span: Span) -> Self {
        match ty {
            ClassRustType::JString => Self::new_path(&["java", "lang", "String"], span).unwrap(),
            _ => Self::Short(Ident::new(&ty.to_string(), span)),
        }
    }

    /// Get the final *Rust type* that will be used for a value with this [`Class`].
    /// 
    /// Will only return something other than [`JObject`][ClassRustType::JObject] if the [`Class`] is a [shorthand][Class::Short].
    /// 
    /// E.g. `String` -> [`ClassRustType::String`], but `java.lang.String` -> [`ClassRustType::JObject`].
    pub fn rust_type(&self) -> ClassRustType {
        match self {
            Self::Path { .. } if self.to_jni_class_path() == "java/lang/String" => ClassRustType::JString,
            Self::Short(ident) => ClassRustType::from_str(&ident.to_string())
                .expect("Unreachable; Class::Short can only be constructed from select valid strings. See [ClassRustType][#impl-FromStr-for-ClassRustType] for that list"),
            _ => ClassRustType::JObject,
        }
    }
    /// Whether the Rust type of the [`Class`] is a [`JObject`][jni::objects::JObject] or one of its **wrappers**,
    /// or if it's another Rust type.
    /// 
    /// See [`Class::rust_type`].
    pub fn is_jobject(&self) -> bool {
        match self.rust_type() {
            ClassRustType::JObject | ClassRustType::JClass | ClassRustType::JThrowable | ClassRustType::JString => true,
            ClassRustType::String => false,
        }
    }
    
    /// Whether the [`Class`] can be used for an `Exception`.
    /// 
    /// Returns an [`Error`][syn::Error] if the [`Class`] can't be used.
    pub fn is_throwable(&self) -> syn::Result<()> {
        // Error Class has to be Throwable (valid if is JThrowable or a full path)
        match self {
            Class::Path { .. } => Ok(()),
            Class::Short(_) if self.rust_type() == ClassRustType::JThrowable => Ok(()),
            Class::Short(class)
            | Class::PrimitiveObject { ident: class, .. } => Err(syn::Error::new(self.span(), format!("{class} does not extend 'java.lang.Throwable'")))
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
        // Creates a full **Class path** for a given **class name**.
        let full_class = |class_name: &str| -> String {
            ["java", "lang", class_name].into_iter()
                    .intersperse(sep)
                    .collect::<String>()
        };

        match self {
            Self::Short(ident) => full_class(&ident.to_string()),
            Self::PrimitiveObject { ty, .. } => full_class(ty.class_name()),
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
impl Spanned for Class {
    /// Builds a [`TokenStream`] out of this path's items and returns its [`span`][TokenStream::span()].
    fn span(&self) -> Span {
        match self {
            Self::Short(ident) => ident.span(),
            Self::PrimitiveObject { ident, .. } => ident.span(),
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

        // Parsing [`Class`] can partially parse some tokens, advancing the [`ParseStream`],
        // but the [`Class`] itself can fail the parse.
        // This leaves some tokens unparsed and leads to weird errors, so a **fork** must be used.
        // !IMPORTANT: Remember to ADVANCE the ParseStream before returning Ok().
        let fork = &input.fork();

        // Parse the dot-separated section `me.author.Class`
        let mut path = Punctuated::<Ident, Token![.]>::parse_separated_nonempty(fork)?;
        // The class is the final component of the path
        let class = match path.pop() {
            Some(class) => class.into_value(),
            None => return Err(syn::Error::new(path.span(), Self::EMPTY_PATH_ERROR))
        };
        // Check for short hands
        let packages = if path.is_empty() {
            return if ClassRustType::from_str(&class.to_string()).is_ok() {
                input.advance_to(fork);
                Ok(Self::Short(class))
            } else if let Some(ty) = JavaPrimitive::from_class_name(&class.to_string()) {
                input.advance_to(fork);
                Ok(Self::PrimitiveObject { ident: class, ty })
            } else {
                Err(syn::Error::new(class.span(), Self::SINGLE_COMPONENT_ERROR))
            }
        } else {
            // Take the rest of the path components
            punctuated_to_pairs(path)
        };
        
        // Path could have Nested classes, separated by `$`
        let nested_path = if fork.parse::<Token![$]>().is_ok() {
            // Parse the Nested Class part of the path `Nested$Nested2`.
            let mut path = Punctuated::<Ident, Token![$]>::parse_separated_nonempty(fork)?;
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

        input.advance_to(fork);
        Ok(Self::Path { class, packages, nested_path })
    }
}
impl Conversion for Class {
    fn convert_java_to_rust(&self, value: &TokenStream) -> Option<TokenStream> {
        Some(match self {
            Self::PrimitiveObject { ident, ty } => {
                let ty = RustPrimitive::from(*ty).to_tokens_spanned(ident.span());
                quote_spanned! {value.span()=>
                    <#ty as ::ez_jni::FromObject>::from_object_env(&(#value), env).unwrap_display()
                }
            },
            _ => {
                let ty = self.type_tokens(false, false, None);
                if self.is_jobject() {
                    quote_spanned! {value.span()=>
                        <#ty as ::ez_jni::FromObjectOwned>::from_object_owned_env(#value, env).unwrap_display()
                    }
                } else {
                    quote_spanned! {value.span()=>
                        <#ty as ::ez_jni::FromObject>::from_object_env(&(#value), env).unwrap_display()
                    }
                }
            }
        })
    }
    fn convert_rust_to_java(&self, value: &TokenStream) -> Option<TokenStream> {
        match self {
            Self::PrimitiveObject { ident, ty } => Some({
                let ty = RustPrimitive::from(*ty).to_tokens_spanned(ident.span());
                quote_spanned! {value.span()=>
                    <#ty as ::ez_jni::ToObject>::to_object_env(&(#value), env)
                }
            }),
            _ => {
                let ty = self.type_tokens(true, false, None);
                match self.rust_type() {
                    ClassRustType::JObject => None,
                    ClassRustType::JClass
                    | ClassRustType::JThrowable
                    | ClassRustType::JString => Some(quote_spanned! {value.span()=>
                        <#ty as Into<::jni::objects::JObject<'_>>>::into(#value)
                    }),
                    ClassRustType::String => Some(quote_spanned! {value.span()=>
                        <#ty as ::ez_jni::ToObject>::to_object_env(::std::convert::AsRef::<str>::as_ref(&(#value)), env)
                    }),
                }
            }
        }
    }
    fn type_tokens(&self, as_ref: bool, is_nested: bool, lifetime: Option<syn::Lifetime>) -> TokenStream {
        let lifetime = lifetime.unwrap_or(syn::Lifetime::new("'_", self.span()));
        match self {
            Self::PrimitiveObject { ident, ty } => RustPrimitive::from(*ty).to_tokens_spanned(ident.span()),
            _ => match self.rust_type() {
                ClassRustType::JObject => quote_spanned! {self.span()=> ::jni::objects::JObject::<#lifetime> },
                ClassRustType::JClass => quote_spanned! {self.span()=> ::jni::objects::JClass::<#lifetime> },
                ClassRustType::JThrowable => quote_spanned! {self.span()=> ::jni::objects::JThrowable::<#lifetime> },
                ClassRustType::JString => quote_spanned! {self.span()=> ::jni::objects::JString::<#lifetime> },
                ClassRustType::String =>
                    if as_ref && is_nested {
                        // str nested in another type must be used with Reference (&)
                        quote_spanned! {self.span()=> &str }
                    } else if as_ref {
                        quote_spanned! {self.span()=> str }
                    } else {
                        quote_spanned! {self.span()=> String }
                    },
            }
        }
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
        match self {
            Self::Short(ty) => f.debug_tuple("Class::Short")
                .field(&ty.to_string())
                .finish(),
            Self::PrimitiveObject { ident, .. } => f.debug_tuple("Class::Primitive")
                .field(&ident.to_string())
                .finish(),
            Self::Path { .. } => f.debug_tuple("Class::Path")
                .field(&self.to_string())
                .finish()
        }
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
    /// Generate tokens that represent the type of a Rust primitive.
    pub fn to_tokens_spanned(self, span: Span) -> TokenStream {
        Ident::new(&self.to_string(), span).into_token_stream()
    }
}
impl Conversion for RustPrimitive {
    fn convert_java_to_rust(&self, value: &TokenStream) -> Option<TokenStream> {
        match self {
            Self::Bool => Some(quote_spanned! {value.span()=> ::ez_jni::utils::jboolean_to_bool(#value) }),
            Self::Char => Some(quote_spanned! {value.span()=> ::ez_jni::utils::jchar_to_char(#value) }),
            _ if self.is_unsigned() => Some({
                let rtype = Ident::new(&self.to_string(), Span::call_site());
                quote_spanned! {value.span()=> (#value) as #rtype }
            }),
            _ => None,
        }
    }
    fn convert_rust_to_java(&self, value: &TokenStream) -> Option<TokenStream> {
        // Use primitive cast to convert to the Java primitive
        let cast = || {
            let jtype = Ident::new(&format!("j{}", JavaPrimitive::from(*self)), Span::call_site());
            quote_spanned! {value.span()=> (#value) as ::jni::sys::#jtype }
        };
        match self {
            Self::Char => Some(quote_spanned! {value.span()=> ::ez_jni::utils::char_to_jchar(#value) }),
            Self::Bool => Some(cast()),
            _ if self.is_unsigned() => Some(cast()),
            _ => None,
        }
    }
    fn type_tokens(&self, _: bool, _: bool, _: Option<syn::Lifetime>) -> TokenStream {
        std::unimplemented!("WILL NOT IMPLEMENT. Use Self::to_tokens_spanned() instead.");
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
impl JavaPrimitive {
    /// Returns the name of the **class** that wraps this **primitive** in Java.
    pub fn class_name(self) -> &'static str {
        match self {
            Self::Boolean => "Boolean",
            Self::Char    => "Character",
            Self::Byte    => "Byte",
            Self::Short   => "Short",
            Self::Int     => "Integer",
            Self::Long    => "Long",
            Self::Float   => "Float",
            Self::Double  => "Double",
        }
    }
    /// Like [`JavaPrimitive::from_str()`], but the string is the *name* of a Java **class** that wraps a **primitive**.
    pub fn from_class_name(class: &str) -> Option<Self> {
        match class {
            "Boolean"   => Some(Self::Boolean),
            "Character" => Some(Self::Char),
            "Byte"      => Some(Self::Byte),
            "Short"     => Some(Self::Short),
            "Integer"   => Some(Self::Int),
            "Long"      => Some(Self::Long),
            "Float"     => Some(Self::Float),
            "Double"    => Some(Self::Double),
            _ => None
        }
    }
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
    use super::*;

    #[test]
    fn class_path() {
        syn::parse_str::<Class>("me.author.MyClass").unwrap();
        syn::parse_str::<Class>("me.MyClass").unwrap();
        syn::parse_str::<Class>("me.author.MyClass$Nested").unwrap();
        syn::parse_str::<Class>("me.author.MyClass$Nested$Nested2").unwrap();
        
        // Test errors
        syn::parse_str::<Class>("").unwrap_err();
        syn::parse_str::<Class>("me").unwrap_err();
        syn::parse_str::<Class>("me.author.MyClass.").unwrap_err();
        syn::parse_str::<Class>(".me.author.MyClass").unwrap_err();
        syn::parse_str::<Class>("MyClass$Nested").unwrap_err();
        syn::parse_str::<Class>("me.author.MyClass$").unwrap_err();
        syn::parse_str::<Class>("me.author.MyClass$Nested$").unwrap_err();
    }
}
