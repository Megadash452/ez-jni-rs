use either::Either;
use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned, ToTokens, TokenStreamExt};
use std::{fmt::Display, str::FromStr};
use syn::{
    braced, bracketed, parenthesized,
    parse::{discouraged::Speculative, Parse},
    punctuated::Punctuated,
    spanned::Spanned,
    Expr, Ident, LitInt, LitStr, Token,
};

use crate::utils::ClassPath;

/// Processes input for macro call [super::call!].
pub fn jni_call(call: MethodCall) -> TokenStream {
    let name = call.method_name.to_string();

    let signature = {
        let mut buf = String::from("(");
        for param in &call.parameters {
            // Array types in signature have an opening bracket prepended to the type
            if param.is_array() {
                buf.push('[');
            }
            buf.push_str(&param.ty().sig_type());
        }
        buf.push(')');
        buf.push_str(&match &call.return_type {
            Return::Result(ResultType::Void(_), _) | Return::Void(_) => "V".to_string(),
            Return::Assertive(ty) | Return::Result(ResultType::Assertive(ty), _) => ty.sig_type(),
            Return::Option(class) | Return::Result(ResultType::Option(class), _) => class.sig_type()
        });
        LitStr::new(&buf, Span::call_site())
    };

    // Put the Value of the parameters in variables to prevent them being dropped (since JValue takes references),
    // and to mitigate borrow checker error if the param value borrows env (since the call itself borrows &mut env).
    let param_vars = call
        .parameters
        .iter()
        .enumerate()
        .map(|(i, param)| {
            let value = param.value();
            let var_name = Ident::new(&format!("__param_{i}"), value.span());
            // Parameters of type object must always be passed by reference.
            if param.is_primitive() {
                quote_spanned! {value.span()=> let #var_name = #value; }
            } else {
                quote_spanned! {value.span()=> let #var_name = &(#value); }
            }
        })
        .collect::<proc_macro2::TokenStream>();
    // Parameters are just the variable names
    let parameters = {
        let params = call.parameters.iter().enumerate().map(|(i, param)| {
            param.variant(Ident::new(&format!("__param_{i}"), param.value().span()))
        });
        quote! { &[ #( #params ),* ] }
    };

    // Common checks done by all Return variants, such as .l() to make the result into a JObject.
    let common = {
        // Induce panic when fails to call method
        let call_failed_msg = match &call.call_type {
            Either::Left(StaticMethod(path))
                => format!("Failed to call static method {name}() on {}: {{err}}", path.to_token_stream()),
            Either::Right(ObjectMethod(_)) => format!("Failed to call {name}(): {{err}}"),
        };
        // Induce panic when the returned value is not the expected type
        let incorrect_type_msg = format!("Expected {name}() to return {}: {{err}}", call.return_type.inner_type());
        let sig_char = Ident::new(&call.return_type.sig_char().to_string(), call.return_type.span());
        // Some Types need to be converted because they are a special case
        let conversion = match (call.return_type.special_case_conversions(quote!(v)), call.return_type.sig_char()) {
            // Move the result of the method call to an Option if it is Object because it could be null.
            (Some(conversion), 'l') => quote! { .map(|v| (!v.is_null()).then_some(#conversion)) },
            (None, 'l') => quote! { .map(|v| (!v.is_null()).then_some(v)) },
            (Some(conversion), _) => quote! { .map(|v| #conversion) },
            (None, _) => quote!()
        };
        quote! {
            .unwrap_or_else(|err| panic!(#call_failed_msg))
            .#sig_char() #conversion
            .unwrap_or_else(|err| panic!(#incorrect_type_msg))
        }
    };

    // Build the macro function call
    let jni_call = match &call.call_type {
        Either::Left(StaticMethod(class)) => {
            let class = LitStr::new(&class.to_string_with_slashes(), class.span());
            quote! { env.call_static_method(#class, #name, #signature, #parameters) }
        }
        Either::Right(ObjectMethod(object)) => quote! {
            env.call_method(&(#object), #name, #signature, #parameters)
        },
    };

    let non_null_msg = format!("Expected Object returned by {name}() to not be NULL");
    // The class or object that the method is being called on. Used for panic message.
    let target = match call.call_type {
        Either::Left(StaticMethod(class)) => {
            let class = class.to_string_with_slashes();
            quote! { ::either::Either::Left(#class) }
        }
        Either::Right(ObjectMethod(obj)) => quote! { ::either::Either::Right(&(#obj)) },
    };
    // The Initial JNI call, before any types or errors are checked
    let initial = quote! {
        use ::std::borrow::BorrowMut as _;
        #param_vars
        let __call = #jni_call;
    };
    match call.return_type {
        Return::Assertive(Type::Object(_)) => quote! { {
            #initial
            ::ez_jni::__throw::panic_uncaught_exception(env.borrow_mut(), #target, #name);
            __call #common
                .unwrap_or_else(|| panic!(#non_null_msg))
        } },
        Return::Assertive(_) | Return::Void(_) | Return::Option(_) => quote! { {
            #initial
            ::ez_jni::__throw::panic_uncaught_exception(env.borrow_mut(), #target, #name);
            __call #common
        } },
        // Move the result of the method call to a Result if the caller expects that the method could throw.
        Return::Result(ResultType::Assertive(Type::Object(_)), err) => quote! { {
            #initial
            ::ez_jni::__throw::catch::<#err>(env.borrow_mut())
                .map(|_| __call #common
                    .unwrap_or_else(|| panic!(#non_null_msg))
                )
        } },
        Return::Result(ResultType::Assertive(_) | ResultType::Void(_) | ResultType::Option(_), err) => quote! { {
            #initial
            ::ez_jni::__throw::catch::<#err>(env.borrow_mut())
                .map(|_| __call #common)
        } },
    }
}

/// Define a JNI call to a Java method with parameters with expected types, and an expected return type.
///
/// See [`crate::call`] for an example.
pub struct MethodCall {
    pub call_type: Either<StaticMethod, ObjectMethod>,
    pub method_name: Ident,
    pub parameters: Punctuated<Parameter, Token![,]>,
    pub return_type: Return,
}
impl Parse for MethodCall {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self {
            call_type: input
                .parse::<StaticMethod>()
                .map(|i| Either::Left(i))
                .or_else(|_| input.parse::<ObjectMethod>().map(|i| Either::Right(i)))?,
            method_name: { input.parse()? },
            parameters: {
                let arg_tokens;
                parenthesized!(arg_tokens in input);
                Punctuated::parse_terminated(&arg_tokens)?
            },
            return_type: {
                input.parse::<Token![->]>()?;
                input.parse()?
            },
        })
    }
}

/// The call is for a static method.
/// ```
/// method_call!(static java.lang.String::methodName ...);
/// ```
pub struct StaticMethod(pub ClassPath);
impl Parse for StaticMethod {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        input.parse::<Token![static]>()?;
        let path = input.parse()?;
        // TODO: use Dot to separate Class path and method name instead
        input.parse::<Token![::]>()?;
        Ok(Self(path))
    }
}

/// The call is for a Method of an existing Object, stored in a variable.
/// If the object is more than an Ident, it must be enclosed in `parenthesis` or `braces`.
/// e.g. `object.methodName(...)` or `(something.object).methodName(...)`.
pub struct ObjectMethod(pub TokenStream);
impl Parse for ObjectMethod {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let expr = if input.peek(syn::token::Paren) {
            let inner;
            parenthesized!(inner in input);
            inner.parse::<TokenStream>()?
        } else if input.peek(syn::token::Brace) {
            let inner;
            braced!(inner in input);
            inner.parse::<TokenStream>()?
        } else {
            input.parse::<Ident>()?.to_token_stream()
        };
        input.parse::<Token![.]>()?;
        Ok(Self(expr))
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

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum JavaPrimitive {
    Byte, Bool, Char,
    Short, Int, Long,
    Float, Double
}
impl JavaPrimitive {
    /// Returns the character/letter (lowercase) that is used to convert from JValue to a concrete type.
    pub fn sig_char(self) -> char {
        match self {
            Self::Byte   => 'b',
            Self::Bool   => 'z',
            Self::Char   => 'c',
            Self::Short  => 's',
            Self::Int    => 'i',
            Self::Long   => 'j',
            Self::Float  => 'f',
            Self::Double => 'd',
        }
    }
}
impl FromStr for JavaPrimitive {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "byte"   => Ok(Self::Byte),
            "bool"   => Ok(Self::Bool),
            "char"   => Ok(Self::Char),
            "short"  => Ok(Self::Short),
            "int"    => Ok(Self::Int),
            "long"   => Ok(Self::Long),
            "float"  => Ok(Self::Float),
            "double" => Ok(Self::Double),
            _ => Err(()),
        }
    }
}
impl Display for JavaPrimitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Byte   => "byte",
            Self::Bool   => "bool",
            Self::Char   => "char",
            Self::Short  => "short",
            Self::Int    => "int",
            Self::Long   => "long",
            Self::Float  => "float",
            Self::Double => "double",
        })
    }
}
impl From<RustPrimitive> for JavaPrimitive {
    fn from(value: RustPrimitive) -> Self {
        match value {
            RustPrimitive::Bool => JavaPrimitive::Bool,
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

/// A Type is an [`Ident`] of one of the following Variants in lowercase, or a [`Type::Object`].
///
/// Note that `Void` isn't a variant here.
/// Since [`Type`] is used by both [`Parameter`] and [`Return`],
/// and [`Paramter`] can't be void, the void variant was moved to [`Return`] (and by extension [`ResultType`]).
pub enum Type {
    JavaPrimitive { ident: Ident, ty: JavaPrimitive },
    RustPrimitive { ident: Ident, ty: RustPrimitive },
    Object(ClassPath),
}
impl Type {
    /// See [JavaPrimitive::sig_char()].
    pub fn sig_char(&self) -> char {
        match self {
            Self::JavaPrimitive { ty, .. } => ty.sig_char(),
            Self::RustPrimitive { ty, .. } => JavaPrimitive::from(*ty).sig_char(),
            Self::Object(_) => 'l',
        }
    }
    /// Returns the Type that is used in the signature of the method call. e.g. `V` or `Ljava/lang/String;`
    pub fn sig_type(&self) -> String {
        match self {
            Self::Object(class) => class.sig_type(),
            _ => self.sig_char().to_uppercase().to_string(),
        }
    }
    pub fn span(&self) -> Span {
        match self {
            Self::JavaPrimitive { ident, .. } | Self::RustPrimitive { ident, .. } => ident.span(),
            Self::Object(class) => class.span(),
        }
    }
}
impl Parse for Type {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let fork = input.fork();
        let ident = fork.parse::<Ident>()?;
        let ident_str = ident.to_string();
        match RustPrimitive::from_str(&ident_str).ok() {
            Some(ty) => {
                input.advance_to(&fork);
                Ok(Self::RustPrimitive { ident, ty })
            },
            // JavaPrimitive::Char and Bool will never be constructued here because the RustPrimitive takes priority
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
impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::JavaPrimitive { ty, .. } => ty.to_string(),
            // Convert form Rust to Java
            Self::RustPrimitive { ty, .. } => JavaPrimitive::from(*ty).to_string(),
            Self::Object(class) => format!("Object({})", class.to_string_with_slashes()),
        };
        f.write_str(&s)
    }
}

/// Parameter to a JNI method call, such as `int(value)` or `java.lang.String(value)`.
pub enum Parameter {
    /// `int(value)` or `java.lang.String(value)`
    Single { ty: Type, value: TokenStream },
    /// `[int](value)` or `[java.lang.String](value)`
    Array { ty: Type, value: TokenStream },
    /// `[int]([value1, value2])` or `[java.lang.String]([value1, value2])`
    ArrayLiteral {
        ty: Type,
        array: Punctuated<Expr, Token![,]>,
    },
}
impl Parameter {
    /// Returns the [`Type`] of the parameter, or the *inner type* if it is an `Array`.
    pub fn ty(&self) -> &Type {
        match self {
            Self::Single { ty, .. } => ty,
            Self::Array { ty, .. } => ty,
            Self::ArrayLiteral { ty, .. } => ty,
        }
    }
    /// Returns whether the parameter represents an Array (literal or by value).
    pub fn is_array(&self) -> bool {
        match self {
            Self::Single { .. } => false,
            Self::Array { .. } | Self::ArrayLiteral { .. } => true,
        }
    }
    /// Returns whether the [`Type`] of the perameter is a *primitive*.
    /// Returns `false` if this is an `Array`.
    pub fn is_primitive(&self) -> bool {
        match self {
            Self::Single { ty: Type::RustPrimitive { .. } | Type::JavaPrimitive { .. }, .. } => true,
            _ => false,
        }
    }

    /// Returns the expression that will be put inside the `JValue::Variant(&(expr))` in the parameter list, a.k.a the *parameter's value*.
    pub fn value(&self) -> TokenStream {
        // Does a lot of checking for **bool** because JNI treats it differently from other types
        match self {
            Self::Single { ty, value } => match ty {
                // Bool must be cast to u8
                Type::JavaPrimitive { ty: JavaPrimitive::Bool, .. }
                | Type::RustPrimitive { ty: RustPrimitive::Bool, .. } => quote_spanned! {value.span()=> #value as u8 },
                _ => value.clone(),
            },
            Self::Array { value, .. } => value.clone(),
            // Create an Java Array from a Rust array literal
            Self::ArrayLiteral { ty, array } => {
                let len = LitInt::new(&array.iter().count().to_string(), array.span());

                let primitive_array = |ident: &Ident, ty: JavaPrimitive| {
                    // Bool must be changed to boolean
                    let ident = match ty {
                        JavaPrimitive::Bool => &Ident::new("boolean", ident.span()),
                        _ => ident,
                    };
                    // Values of Boolean array must be cast to u8
                    let values = array.iter();
                    let values = match ty {
                        JavaPrimitive::Bool => quote! { #(#values as u8),* },
                        _ => quote! { #(#values),* },
                    };
                    let new_array_fn = Ident::new(&format!("new_{ident}_array"), array.span());
                    let new_array_err = LitStr::new(
                        &format!("Failed to create Java {ident} array: {{err}}"),
                        array.span(),
                    );
                    let fill_array_fn =
                        Ident::new(&format!("set_{ident}_array_region"), array.span());
                    let fill_array_err = LitStr::new(
                        &format!("Error filling {ident} array: {{err}}"),
                        array.span(),
                    );
                    // Create a Java array Object from the array literal
                    quote_spanned! {array.span()=> {
                        let array = env.#new_array_fn(#len)
                            .inspect_err(|err| println!(#new_array_err)).unwrap();
                        env.#fill_array_fn(&array, 0, &[ #values ])
                            .inspect_err(|err| println!(#fill_array_err)).unwrap();
                        ::jni::objects::JObject::from(array)
                    } }
                };

                match ty {
                    Type::JavaPrimitive { ident, ty } => primitive_array(ident, *ty),
                    Type::RustPrimitive { ident, ty } => primitive_array(ident, JavaPrimitive::from(*ty)),
                    Type::Object(class) => {
                        let new_array_err = LitStr::new(
                            &format!("Failed to create Java Object \"{}\" array: {{err}}", class.to_token_stream()),
                            array.span(),
                        );
                        let set_val_err = LitStr::new(
                            &format!("Failed to set the value of Object array at index {{i}}: {{err}}"),
                            array.span(),
                        );
                        let class_path = LitStr::new(&class.to_string_with_slashes(), array.span());
                        // Fill the array
                        let mut elements = quote! {};
                        for (i, element) in array.iter().enumerate() {
                            elements = quote! { #elements
                                env.set_object_array_element(&array, #i, #element)
                                    .inspect_err(|err| println!(#set_val_err)).unwrap();
                            }
                        }
                        // Return the array
                        quote_spanned! {array.span()=> {
                            let array = env.new_object_array(#len, #class_path, unsafe { ::jni::objects::JObject::from_raw(::std::ptr::null_mut()) } )
                                .inspect_err(|err| println!(#new_array_err)).unwrap();
                            #elements
                            ::jni::objects::JObject::from(array)
                        } }
                    }
                }
            }
        }
    }
    /// Conver the parameter to a `Jvalue` enum variant that will be used in the JNI call parameter list.
    /// The variant will have one of the parameter variables as the inner value.
    pub fn variant(&self, var_name: Ident) -> TokenStream {
        let (ty_span, ty_variant) = match self {
            Self::Single { ty, .. } => match ty {
                Type::JavaPrimitive { ident, .. } | Type::RustPrimitive { ident, .. } => (
                    ident.span(),
                    Ident::new(&first_char_uppercase(ty.to_string()), ident.span())
                        .to_token_stream(),
                ),
                Type::Object(class) => (class.span(), quote!(Object)),
            },
            Self::Array { ty, .. } | Self::ArrayLiteral { ty, .. } => (ty.span(), quote!(Object)),
        };
        let mut tt = quote! {};
        tt.append_all(quote_spanned! {ty_span=> ::jni::objects::JValue::#ty_variant });
        tt.append_all(quote_spanned! {var_name.span()=> (#var_name) });
        tt
    }
}
impl Parse for Parameter {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        // Check if the Type is wrapped in Brackets (Array)
        Ok(if input.lookahead1().peek(syn::token::Bracket) {
            let ty = {
                let inner;
                bracketed!(inner in input);
                inner.parse::<Type>()?
            };
            let value_tokens;
            parenthesized!(value_tokens in input);
            if value_tokens.is_empty() {
                return Err(value_tokens.error("Must provide a parameter value"));
            }
            // Check if the Value is wrapped in Brackets (ArrayLiteral)
            if value_tokens.lookahead1().peek(syn::token::Bracket) {
                Self::ArrayLiteral {
                    ty,
                    array: {
                        let inner;
                        bracketed!(inner in value_tokens);
                        Punctuated::parse_terminated(&inner)?
                    },
                }
            } else {
                Self::Array {
                    ty,
                    value: value_tokens.parse::<TokenStream>()?,
                }
            }
        } else {
            Self::Single {
                ty: input.parse::<Type>()?,
                value: {
                    let value_tokens;
                    parenthesized!(value_tokens in input);
                    if value_tokens.is_empty() {
                        return Err(value_tokens.error("Must provide a parameter value"));
                    }
                    value_tokens.parse::<TokenStream>()?
                },
            }
        })
    }
}

/// The return type of a JNI method call.
///
/// The caller of the macro can assert that the JNI method call will result in one of the following:
/// 1. [`Type`] (or `void`) if the function being called can't return `NULL` or throw an `exception` (e.g. `bool` or `java.lang.String`).
/// 2. `Option<ClassPath>` if the return type is an [`Object`][Type::Object] that could be **NULL**.
///    Java *does not allow* primitive types (i.e. not Object) to be **NULL**, so [Self::Option] can only be used with a [ClassPath].
/// 3. `Result<Type | void | Option<ClassPath>, E>` if the method call can throw an **Exception**,
///    where `E` is any Rust type that *implements [`FromException`]*.
pub enum Return {
    Void(Ident),
    Assertive(Type),
    Option(ClassPath),
    /// Holds the Ok Type (a Type or Option), and the Err Type.
    Result(ResultType, syn::Path),
}
impl Return {
    /// Helper function that parses the content of the option variant.
    /// Helps avoid code repetition.
    fn parse_option(
        input: syn::parse::ParseStream,
        fork: syn::parse::ParseStream,
    ) -> syn::Result<ClassPath> {
        input.advance_to(&fork);
        // Parse generic arguement
        input
            .parse::<Token![<]>()
            .map_err(|err| input.error(format!("Option takes generic arguments; {err}")))?;
        let class = match input.parse::<Type>()? {
            Type::Object(path) => path,
            t => return Err(syn::Error::new(
                t.span(),
                "Option cannot be used with primitives, only Classes.",
            ))
        };
        input
            .parse::<Token![>]>()
            .map_err(|err| input.error(format!("Option takes only 1 generic argument; {err}")))?;
        Ok(class)
    }

    /// See [Type::sig_char].
    pub fn sig_char(&self) -> char {
        match self {
            Self::Void(_) | Self::Result(ResultType::Void(_), _) => 'v',
            Self::Assertive(ty) | Self::Result(ResultType::Assertive(ty), _) => ty.sig_char(),
            Self::Option(_) | Self::Result(ResultType::Option(_), _) => 'l',
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Self::Void(ident) | Self::Result(ResultType::Void(ident), _) => ident.span(),
            Self::Assertive(ty) | Self::Result(ResultType::Assertive(ty), _) => ty.span(),
            Self::Option(class) | Self::Result(ResultType::Option(class), _) => class.span(),
        }
    }

    /// Returns the string representation of the Return type of the Java method (`void`, `primitive`, or `Object`).
    pub fn inner_type(&self) -> String {
        match self {
            Return::Assertive(Type::Object(class))
            | Return::Option(class)
            | Return::Result(ResultType::Assertive(Type::Object(class)), _)
            | Return::Result(ResultType::Option(class), _) => class.to_string_with_slashes(),
            Return::Assertive(ty) | Return::Result(ResultType::Assertive(ty), _) => ty.to_string(),
            Return::Void(_) | Return::Result(ResultType::Void(_), _) => "void".to_string(),
        }
    }
    
    /// Handle special cases of the call's return value.
    /// e.g. wrap `java.lang.String` in `JString`.
    /// 
    /// **value** is the tokens representing the value that will be *operated on*.
    pub fn special_case_conversions(&self, value: TokenStream) -> Option<TokenStream> {
        match self {
            // Special cases for RustPrimitives
            Return::Assertive(Type::RustPrimitive { ty, ident })
            | Return::Result(ResultType::Assertive(Type::RustPrimitive { ty, ident }), _)
                => if ty.is_unsigned() {
                    // Transmute to the unsigned type
                    Some(quote! { unsafe { ::std::mem::transmute::<_, #ident>(#value) } })
                } else if *ty == RustPrimitive::Char {
                    // Decode UTF-16
                    Some(quote! {
                        char::decode_utf16(Some(#value))
                            .next().unwrap()
                            .unwrap_or(char::REPLACEMENT_CHARACTER)
                    })
                } else {
                    None
                },
            // Special cases for Java Objects
            Return::Assertive(Type::Object(class))
            | Return::Option(class)
            | Return::Result(ResultType::Assertive(Type::Object(class)), _)
            | Return::Result(ResultType::Option(class), _) =>
                if class.to_string_with_slashes() == "java/lang/String" {
                    // Wrap java.lang.String in JString
                    Some(quote! { ::jni::objects::JString::from(#value) })
                } else {
                    None
                },
            _ => None
        }
    }
}
impl Parse for Return {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let fork = input.fork();
        // Check if caller uses Option or Result as the return type
        Ok(match fork.parse::<Ident>() {
            Ok(ident) => match ident.to_string().as_str() {
                "void" => {
                    input.advance_to(&fork);
                    Self::Void(ident)
                }
                "Option" => Self::Option(Self::parse_option(input, &fork)?),
                "Result" => {
                    input.advance_to(&fork);
                    // Parse generic arguements
                    input.parse::<Token![<]>().map_err(|err| {
                        input.error(format!("Result takes generic arguments; {err}"))
                    })?;
                    let ok_type = input.parse()?;
                    input.parse::<Token![,]>().map_err(|err| {
                        input.error(format!("Result takes 2 generic arguments; {err}"))
                    })?;
                    let err_type = input.parse::<syn::Path>()?;
                    input.parse::<Token![>]>()?;
                    Self::Result(ok_type, err_type)
                }
                _ => Self::Assertive(input.parse()?),
            },
            // The return type did not start with Ident... weird, but let's continue
            Err(_) => Self::Assertive(input.parse()?),
        })
    }
}

/// The `Ok` Type of a [`Return::Result`] could be a regular type ([`Void`][ResultType::Void] or [`Assertive`][ResultType::Assertive]), or nullable [`Option`].
pub enum ResultType {
    Void(Ident),
    Assertive(Type),
    Option(ClassPath),
}
impl Parse for ResultType {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let fork = input.fork();
        Ok(match fork.parse::<Ident>() {
            Ok(ident) => match ident.to_string().as_str() {
                "void" => {
                    input.advance_to(&fork);
                    Self::Void(ident)
                }
                "Option" => Self::Option(Return::parse_option(input, &fork)?),
                "Result" => return Err(syn::Error::new(
                    ident.span(),
                    "Can't nest a Result within a Result",
                )),
                _ => Self::Assertive(input.parse()?),
            },
            // The return type did not start with Ident... weird, but let's continue
            Err(_) => Self::Assertive(input.parse()?),
        })
    }
}

/// Convert the first letter of a String into uppercase
fn first_char_uppercase(s: String) -> String {
    let mut c = s.chars();
    match c.next() {
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
        None => String::new(),
    }
}
