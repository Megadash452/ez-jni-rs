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
    /// The Type that is used in the signature of the method call. e.g. `"V"` or `"Ljava/lang/String;"`.
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
    pub fn is_pure_primitive(&self) -> bool {
        match self {
            Self::Assertive(InnerType::Primitive(_)) => true,
            _ => false,
        }
    }

    /// General function to convert a [`JValue`][jni::objects::JValueGen] to a **Rust value**.
    /// 
    /// Used to generate the conversion for the *return value* of a JNI call.
    pub fn convert_jvalue_to_rvalue(&self, value: &TokenStream) -> TokenStream {
        // Get a concrete Rust type to tell FromJValue to use
        let ty = match self {
            // Use special trait to unwrap Owned version of Object
            Self::Assertive(InnerType::Object(class))
            | Self::Option { ty: InnerType::Object(class), .. }
            if class.is_object_ref() => {
                let ty = self.type_tokens(false, false, None);
                return quote_spanned! {value.span()=>
                    <#ty as ::ez_jni::FromObjectOwned>::from_object_owned_env(
                        ::ez_jni::utils::jvalue_to_jobject(#value).unwrap_display(),
                        env,
                    ).unwrap_display() }
            },
            _ => self.type_tokens(false, false, None),
        };
        // use the FromJValue implementation
        // NOTE that using FromJValue is much simpler than using ToJValue in these macros.
        quote_spanned! {value.span()=> <#ty as ::ez_jni::FromJValue>::from_jvalue_env((#value).borrow(), env).unwrap_display() }
    }

    /// General function to convert a **Rust value** to a [`JValue`][jni::objects::JValue].
    /// The expression resolves specifically to the *borrowed* version of [`JValue`][jni::objects::JValue].
    /// 
    /// Used to generate the conversion for the *argument values* of a JNI call.
    pub fn convert_rvalue_to_jvalue(&self, value: &TokenStream) -> TokenStream {
        // Get a concrete Rust type to tell ToJValue to use
        let ty = match self {
            // Some cases with Array should not be converted with ToJValue because the type will be incorrect.
            Self::Assertive(InnerType::Array(array))
            | Self::Option { ty: InnerType::Array(array), .. }
            if array.manually_converted_to_java() => {
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
            // Force conversion with `ToObject` instead of `ToJValue` if the Type is a Object.
            Self::Assertive(InnerType::Object(class))
            | Self::Option { ty: InnerType::Object(class), .. } => {
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
    fn sig_type(&self) -> LitStr {
        match self {
            Self::Assertive(ty) => ty.sig_type(),
            // Option with primitive is actually a class
            Self::Option { ty: InnerType::Primitive(prim), .. } => prim.to_class().sig_type(),
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
                    InnerType::Object(class) if class.is_object_ref() =>
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
                    InnerType::Object(class) if class.is_object_ref() => match_option(class.convert_rust_to_java(option_value)),
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
    Primitive(Primitive),
    Object(Class),
    Array(ArrayType),
}
impl Spanned for InnerType {
    fn span(&self) -> Span {
        match self {
            Self::Primitive(prim) => prim.span(),
            Self::Object(class) => class.span(),
            Self::Array(array) => array.span(),
        }
    }
}
impl SigType for InnerType {
    fn sig_type(&self) -> LitStr {
        match self {
            Self::Primitive(prim) => prim.sig_type(),
            Self::Object(class) => class.sig_type(),
            Self::Array(array) => array.sig_type(),
        }
    }
}
impl Conversion for InnerType {
    fn convert_java_to_rust(&self, value: &TokenStream) -> Option<TokenStream> {
        match self {
            Self::Primitive(prim) => prim.convert_java_to_rust(value),
            Self::Object(class) => class.convert_java_to_rust(value),
            Self::Array(array) => <ArrayType as Conversion>::convert_java_to_rust(array, value),
        }
    }
    fn convert_rust_to_java(&self, value: &TokenStream) -> Option<TokenStream> {
        match self {
            Self::Primitive(prim) => prim.convert_rust_to_java(value),
            Self::Object(class) => class.convert_rust_to_java(value),
            Self::Array(array) => <ArrayType as Conversion>::convert_rust_to_java(array, value),
        }
    }
    fn type_tokens(&self, as_ref: bool, is_nested: bool, lifetime: Option<syn::Lifetime>) -> TokenStream {
        match self {
            Self::Primitive(prim) => prim.type_tokens(as_ref, is_nested, lifetime),
            Self::Object(class) => class.type_tokens(as_ref, is_nested, lifetime),
            Self::Array(array) => array.type_tokens(as_ref, is_nested, lifetime),
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

        // Parse primitive
        if let Ok(prim) = input.parse::<Primitive>() {
            return Ok(Self::Primitive(prim));
        }

        // Parse an Ident, attempt some other known symbol...
        if let Ok(ident) = input.parse::<Ident>() {
            let ident_str = ident.to_string();

            // Disallowed symbols
            match ident_str.as_str() {
                "void" => return Err(syn::Error::new(ident.span(), "'void' is not allowed in arguments or within 'Option' in the return type.")),
                "Result" => return Err(syn::Error::new(ident.span(), "'Result' is not allowed as an inner type, it must be the outermost type.")),
                "Option" => return Err(syn::Error::new(ident.span(), "'Option' is not allowed to be nested within itself.")),
                _ => {}
            }
        }

        // All types failed to parse, return Class parse error
        Err(syn::Error::new(class_err.span(), format!("Tokens did not match any known Type or a Class: {class_err}")))
    }
}
impl Display for InnerType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Primitive(prim) => Display::fmt(prim, f),
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

    /// Whether the [`ArrayType`] must convert the *Rust value* to a *Java Object* by **manually** converting each *array dimension* one at a time.
    /// 
    /// This is used because some [`Types`][Type] have ambiguous *Rust representations*.
    /// For instance, `[[String]]` could be represented in Rust as `[&[&str]]`, or `[Vec<&str>]`, or `[&[String]]`, etc.
    pub fn manually_converted_to_java(&self) -> bool {
        match self.ty.as_ref() {
            /* In Rust, multi-dimensional Arrays have strict inner types.
               So when the array is multi-dimensionsal,
               the array conversion must be done manually to allow use of `Box<[_]>`, `Vec<_>`, `&[_]`. */
            Type::Assertive(InnerType::Array(_))
            | Type::Option { ty: InnerType::Array(_), .. } => true,
            /* When the inner type of the array is String (for any number of dimensions),
               the array conversion must be done manually to allow use of both `String` and `&str`. */
            Type::Assertive(InnerType::Object(class))
            | Type::Option { ty: InnerType::Object(class), .. }
            if class.rust_type() == ClassRustType::String
            /* Arrays of Primitive Objects should be manually converted.
            The default implementation converts Arrays of Rust primitives to Java primitives, but an Object is required. */
            || matches!(class.rust_type(), ClassRustType::Primitive(_))
            /* When the inner type of the array is a Java Class Path,
               the array must be converted manually to provide an elem_class. */
            || class.is_object_ref() => true,
            // All other types can use the ToObject implementation normally
            _ => false,
        }
    }

    /// Returns code that converts a *Java Array* to a *Rust [`Box`]*.
    /// 
    /// If array is *one dimensional*, it defers to the [`ez_jni::FromObject`] implementation.
    /// If array is *multidimensional*, it will generate code to manually convert the Array.
    /// 
    /// See [`origin`](Conversion::convert_java_to_rust()).
    fn convert_java_to_rust(&self, value: &TokenStream) -> TokenStream {
        let array_ty = self.type_tokens(false, false, None);
        quote_spanned! {value.span()=>
            <#array_ty as ::ez_jni::FromObject>::from_object_env(&(#value), env).unwrap_display()
        }
    }
    /// Returns code that converts a *Rust slice* to a *Java Array*.
    /// 
    /// Can return the [`ez_jni::ToObject`] implementation,
    /// or can generate code to manualyl convert the Array.
    /// This depends on the criteria set by [`ArrayType::manually_converted_to_java()`]. 
    /// 
    /// See [`origin`](Conversion::convert_java_to_rust()).
    fn convert_rust_to_java(&self, value: &TokenStream) -> TokenStream {
        /// Build a *Java Array* from a Rust *slice*.
        /// The Array' inner type is **Object**.
        /// 
        /// **elem_conversion** is code that applies a conversion to each element to convert it from a *Rust Type* to a *Java Object*.
        /// The conversion must use an [`Ident`] **`_element`** as the value.
        /// For example, the **elem_conversion** argument can be
        /// ```ignore
        /// class.convert_rust_to_java(&quote!(_element))
        /// ```
        fn create_obj_array(elem_class: &LitStr, elem_conversion: TokenStream, value: &TokenStream) -> TokenStream {
            quote_spanned! {value.span()=>
                ::ez_jni::utils::create_object_array_converted(
                    ::std::convert::AsRef::<[_]>::as_ref(&(#value)), // No need to include the type in the AsRef because the elem_conversion already enforces the element Type.
                    |_element, #[allow(unused_variables)] env| #elem_conversion,
                    #elem_class,
                env)
            }
        }

        if self.manually_converted_to_java() {
            let elem_class = self.ty.sig_type();

            match &*self.ty {
                // Use the ToObject implementation for ObjectArray with any Rust slice.
                Type::Assertive(InnerType::Object(class))
                | Type::Option { ty: InnerType::Object(class), .. }
                if class.is_object_ref() => {
                    // TODO: should the ObjectArrayElement be the concrete type extpected by the Class? Or should it not care?
                    // // The Object Ref's concrete Rust type varies depending on the expected Class. See [`Class::rust_type()`].
                    // let obj_rust_ty = self.ty.type_tokens(false, false, None);
                    let class = class.to_jni_class_path();
                    quote_spanned! {value.span()=>
                        ::ez_jni::ToObject::to_object_env(
                            &::ez_jni::ObjectArray::new_ref(
                                ::std::convert::AsRef::<[_ /* #obj_rust_ty */]>::as_ref(&(#value)),
                                #class,
                            ),
                        env)
                    }
                },
                // Convert elements using the Type's Conversion.
                _ => create_obj_array(
                    &elem_class,
                    self.ty.convert_rust_to_java(&quote_spanned! {value.span()=> _element})
                        .expect("Array inner Type must have a conversion to have made it to this point"),
                    value
                )
            }
        } else {
            let ty = self.ty.type_tokens(true, true, None);
            quote_spanned! {value.span()=>
                ::ez_jni::ToObject::to_object_env(::std::convert::AsRef::<[#ty]>::as_ref(&(#value)), env)
            }
        }
    }
}
impl Spanned for ArrayType {
    fn span(&self) -> Span {
        join_spans([self.bracket_token.span.open(), self.ty.span(), self.bracket_token.span.close()])
    }
}
impl SigType for ArrayType {
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
        let span = self.span();
        // vvv Recursion occurs here vvv
        let elem_ty = self.ty.type_tokens(as_ref, true, lifetime);
        
        match &*self.ty {
            // Arrays of Object Refs use the special ObjectArray type so it can store the Array's element Class.
            Type::Assertive(InnerType::Object(class))
            | Type::Option { ty: InnerType::Object(class), .. }
            if class.is_object_ref() => {
                quote_spanned! {span=> ::ez_jni::ObjectArray<#elem_ty> }
            }
            _ => if as_ref && is_nested {
                quote_spanned! {span=> &[#elem_ty] }
            } else if as_ref {
                quote_spanned! {span=> [#elem_ty] }
            } else {
                quote_spanned! {span=> ::std::boxed::Box<[#elem_ty]> }
            }
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
    JObject, JClass, JThrowable, JString, String,
    Primitive(RustPrimitive)
}
impl FromStr for ClassRustType {
    type Err = ();

    fn from_str(class: &str) -> Result<Self, Self::Err> {
        match class {
            "Object"    => Ok(Self::JObject),
            "Class"     => Ok(Self::JClass),
            "Throwable" => Ok(Self::JThrowable),
            "Exception" => Ok(Self::JThrowable),
            "String"    => Ok(Self::String),
            _ => JavaPrimitive::from_class_name(class)
                .map(|prim| Self::Primitive(RustPrimitive::from(prim)))
                .ok_or(()),
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
            Self::Primitive(prim) => JavaPrimitive::from(*prim).class_name()
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
    /// Whether the *Rust type* of the [`Class`] is a [`JObject`][jni::objects::JObject] or one of its **wrappers**,
    /// or if it's another Rust type.
    /// 
    /// See [`Class::rust_type`].
    pub fn is_object_ref(&self) -> bool {
        match self.rust_type() {
            ClassRustType::JObject | ClassRustType::JClass | ClassRustType::JThrowable | ClassRustType::JString => true,
            ClassRustType::String | ClassRustType::Primitive(_) => false,
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
            Class::Short(class) => Err(syn::Error::new(self.span(), format!("{class} does not extend 'java.lang.Throwable'")))
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
    fn sig_type(&self) -> LitStr {
        LitStr::new(&format!("L{};", self.to_jni_class_path()), self.span())
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
        let ty = self.type_tokens(false, false, None);

        Some(match self.rust_type() {
            ClassRustType::Primitive(_) => quote_spanned! {value.span()=>
                <#ty as ::ez_jni::FromObject>::from_object_env(&(#value), env).unwrap_display()
            },
            _ if self.is_object_ref()  => quote_spanned! {value.span()=>
                <#ty as ::ez_jni::FromObjectOwned>::from_object_owned_env(#value, env).unwrap_display()
            },
            _ => quote_spanned! {value.span()=>
                <#ty as ::ez_jni::FromObject>::from_object_env(&(#value), env).unwrap_display()
            }
        })
    }
    fn convert_rust_to_java(&self, value: &TokenStream) -> Option<TokenStream> {
        let ty = self.type_tokens(true, false, None);

        match self.rust_type() {
            ClassRustType::Primitive(_) => Some(quote_spanned! {value.span()=>
                <#ty as ::ez_jni::ToObject>::to_object_env(&(#value), env)
            }),
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
    fn type_tokens(&self, as_ref: bool, is_nested: bool, lifetime: Option<syn::Lifetime>) -> TokenStream {
        let span = self.span();
        let lifetime = lifetime.unwrap_or(syn::Lifetime::new("'_", span));

        match self.rust_type() {
            ClassRustType::Primitive(prim) => RustPrimitive::from(prim).to_tokens_spanned(span),
            ClassRustType::JObject    => quote_spanned! {span=> ::jni::objects::JObject::<#lifetime> },
            ClassRustType::JClass     => quote_spanned! {span=> ::jni::objects::JClass::<#lifetime> },
            ClassRustType::JThrowable => quote_spanned! {span=> ::jni::objects::JThrowable::<#lifetime> },
            ClassRustType::JString    => quote_spanned! {span=> ::jni::objects::JString::<#lifetime> },
            ClassRustType::String =>
                if as_ref && is_nested {
                    // str nested in another type must be used with Reference (&)
                    quote_spanned! {span=> &str }
                } else if as_ref {
                    quote_spanned! {span=> str }
                } else {
                    quote_spanned! {span=> String }
                },
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
            Self::Path { .. } => f.debug_tuple("Class::Path")
                .field(&self.to_string())
                .finish()
        }
    }
}

/// A [`Primitive`] can be represented in the macros as either a [`RustPrimitive`] or a [`JavaPrimitive`].
/// 
/// These are **pure primitives**.
/// *Object Primitives* can be stored with [`Class::Short`].
#[derive(Debug, Clone)]
pub enum Primitive {
    Java { ident: Ident, ty: JavaPrimitive },
    Rust { ident: Ident, ty: RustPrimitive }
}
impl Primitive {
    /// Returns a [`Class`] that represents this [`Primitive`] as an **Object Primitive**.
    pub fn to_class(&self) -> Class {
        let ident = match self {
            Self::Java { ident, ty } => Ident::new(ty.class_name(), ident.span()),
            Self::Rust { ident, ty } => Ident::new(JavaPrimitive::from(*ty).class_name(), ident.span()),
        };
        Class::Short(ident)
    }

    /// Returns a [`JavaPrimitive`].
    /// 
    /// Even if the the underlying type is [`RustPrimitive`],
    /// this will map it to a [`JavaPrimitive`].
    pub fn java_prim(&self) -> JavaPrimitive {
        match self {
            Self::Java { ty, .. } => *ty,
            Self::Rust { ty, .. } => JavaPrimitive::from(*ty)
        }
    }
    /// Returns a [`RustPrimitive`].
    /// 
    /// Even if the the underlying type is [`JavaPrimitive`],
    /// this will map it to a [`RustPrimitive`].
    #[allow(unused)]
    pub fn rust_prim(&self) -> RustPrimitive {
        match self {
            Self::Java { ty, .. } => RustPrimitive::from(*ty),
            Self::Rust { ty, .. } => *ty
        }
    }
}
impl Spanned for Primitive {
    fn span(&self) -> Span {
        match self {
            Self::Java { ident, .. } | Self::Rust { ident, .. } => ident.span(),
        }
    }
}
impl SigType for Primitive {
    fn sig_type(&self) -> LitStr {
        match self {
            Self::Java { ident, ty } => {
                let mut sig_type = ty.sig_type();
                sig_type.set_span(ident.span());
                sig_type
            },
            Self::Rust { ident, ty } => {
                let mut sig_type = JavaPrimitive::from(*ty).sig_type();
                sig_type.set_span(ident.span());
                sig_type
            },
        }
    }
}
impl Conversion for Primitive {
    fn convert_java_to_rust(&self, value: &TokenStream) -> Option<TokenStream> {
        match self {
            Self::Java { ty, .. } => RustPrimitive::from(*ty).convert_java_to_rust(value),
            Self::Rust { ty, .. } => ty.convert_java_to_rust(value),
        }
    }
    fn convert_rust_to_java(&self, value: &TokenStream) -> Option<TokenStream> {
        match self {
            Self::Java { ty, .. } => RustPrimitive::from(*ty).convert_rust_to_java(value),
            Self::Rust { ty, .. } => ty.convert_rust_to_java(value),
        }
    }
    fn type_tokens(&self, _: bool, _: bool, _: Option<syn::Lifetime>) -> TokenStream {
        match self {
            Primitive::Java { ident, ty } => RustPrimitive::from(*ty).to_tokens_spanned(ident.span()),
            Primitive::Rust { ident, ty } => ty.to_tokens_spanned(ident.span()),
        }
    }
}
impl Parse for Primitive {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        // Speculative parsing is needed because when the Ident is not a Primitive, the input MUST NOT be advanced.
        let fork = &input.fork();

        let ident = fork.parse::<Ident>()?;
        let ident_str = &ident.to_string();

        let prim = if let Ok(ty) = RustPrimitive::from_str(ident_str) {
            Self::Rust { ident, ty }
        } else if let Ok(ty) = JavaPrimitive::from_str(ident_str) {
            Self::Java { ident, ty }
        } else {
            return Err(syn::Error::new(ident.span(), "Tokens did not match any Rust or Java primitives."));
        };

        input.advance_to(fork);
        Ok(prim)
    }
}
impl Display for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Java { ty, .. } => Display::fmt(ty, f),
            Self::Rust { ty, .. } => Display::fmt(&JavaPrimitive::from(*ty), f),
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
    /// Returns the *[`Class`] name* that represents this [`JavaPrimitive`].
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
    /// Like [`JavaPrimitive::from_str()`], but the string is a *[`Class`] name* that represents a [`JavaPrimitive`].
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
    fn sig_type(&self) -> LitStr {
        let c = match self {
            Self::Byte    => 'B',
            Self::Boolean => 'Z',
            Self::Char    => 'C',
            Self::Short   => 'S',
            Self::Int     => 'I',
            Self::Long    => 'J',
            Self::Float   => 'F',
            Self::Double  => 'D',
        };
        LitStr::new(&c.to_string(), Span::call_site())
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
