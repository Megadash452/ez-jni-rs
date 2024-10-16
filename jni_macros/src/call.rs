use std::fmt::{Display, Debug};
use either::Either;
use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned, ToTokens, TokenStreamExt};
use syn::{
    braced, bracketed, parenthesized, parse::{discouraged::Speculative, Parse}, punctuated::Punctuated, Ident, LitStr, Token
};
use crate::{
    types::{NULL_KEYWORD, ArrayType, ClassPath, InnerType, JavaPrimitive, SigType, SpecialCaseConversion, Type},
    utils::{Spanned, first_char_uppercase, gen_signature, join_spans}
};

/// Processes input for macro call [super::call!].
pub fn jni_call(call: MethodCall) -> TokenStream {
    let name = call.method_name.to_string();
    let signature = gen_signature(call.parameters.iter(), &call.return_type);
    let param_vars = gen_arg_vars_defs(call.parameters.iter());
    let arguments = gen_arguments(call.parameters.iter());

    // Common checks done by all Return variants, such as .l() to make the result into a JObject.
    let common = {
        // Induce panic when fails to call method
        let call_failed_msg = match &call.call_type {
            Either::Left(StaticMethod(path))
                => format!("Failed to call static method {name}() on {}: {{err}}", path.to_string()),
            Either::Right(ObjectMethod(_)) => format!("Failed to call {name}(): {{err}}"),
        };
        // Induce panic when the returned value is not the expected type
        let incorrect_type_msg = format!("Expected {name}() to return {}: {{err}}", call.return_type.inner());
        let sig_char = Ident::new(&call.return_type.sig_char().to_string(), call.return_type.span());
        // Apply special case conversion
        // Also convert value to Option<_> if it is any kind of Object because it could be null.
        let conversion = {
            let conversion = call.return_type.inner().special_case_conversions(quote!(v));

            if call.return_type.sig_char().to_string().as_str() == "l" {
                match conversion {
                    Some(conversion) => quote! { .map(|v| (!v.is_null()).then(|| #conversion)) },
                    None => quote! { .map(|v| (!v.is_null()).then_some(v)) },
                }
            } else {
                match conversion {
                    Some(conversion) => quote! { .map(|v| #conversion) },
                    None => quote!()
                }
            }
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
            let class = LitStr::new(&class.to_jni_class_path(), class.span());
            quote! { env.call_static_method(#class, #name, #signature, #arguments) }
        }
        Either::Right(ObjectMethod(object)) => quote! {
            env.call_method(&(#object), #name, #signature, #arguments)
        },
    };

    let non_null_msg = format!("Expected Object returned by {name}() to not be NULL");
    // The class or object that the method is being called on. Used for panic message.
    let target = match call.call_type {
        Either::Left(StaticMethod(class)) => {
            let class = class.to_jni_class_path();
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
        // For return types that are not Result
        Return::Assertive(ty) => match ty {
            ReturnableType::Assertive(InnerType::Object(_)) | ReturnableType::Array(_) => quote! { {
                #initial
                ::ez_jni::__throw::panic_uncaught_exception(env.borrow_mut(), #target, #name);
                __call #common
                    .unwrap_or_else(|| panic!(#non_null_msg))
            } },
            ReturnableType::Void(_)
            | ReturnableType::Assertive(_)
            | ReturnableType::Option(_) => quote! { {
                #initial
                ::ez_jni::__throw::panic_uncaught_exception(env.borrow_mut(), #target, #name);
                __call #common
            } },
        }
        // Move the result of the method call to a Result if the caller expects that the method could throw.
        Return::Result { ty, err_ty, ..} => match ty {
            ReturnableType::Assertive(InnerType::Object(_)) | ReturnableType::Array(_) => quote! { {
                #initial
                ::ez_jni::__throw::catch::<#err_ty>(env.borrow_mut())
                    .map(|_| __call #common
                        .unwrap_or_else(|| panic!(#non_null_msg))
                    )
            } },
            ReturnableType::Void(_)
            | ReturnableType::Assertive(_)
            | ReturnableType::Option(_) => quote! { {
                #initial
                ::ez_jni::__throw::catch::<#err_ty>(env.borrow_mut())
                    .map(|_| __call #common)
            } },
        }
        
    }
}

/// Processes input for macro call [super::new!].
pub fn jni_call_constructor(call: ConstructorCall) -> TokenStream {
    let class = call.class.to_jni_class_path();
    let signature = gen_signature(call.parameters.iter(), &Return::new_void(Span::call_site()));
    let param_vars = gen_arg_vars_defs(call.parameters.iter());
    let arguments = gen_arguments(call.parameters.iter());
    let method_name = format!("constructor{}", signature.value());
    let call_failed_msg = format!("Failed to call constructor {} on {}: {{err}}", signature.value(), class.to_token_stream());
    
    // The Initial JNI call, before any types or errors are checked
    let initial = quote! {
        use ::std::borrow::BorrowMut as _;
        #param_vars
        let __call = env.new_object(#class, #signature, #arguments);
    };
    match call.err_type {
        Some(err) => quote! { {
            #initial
            ::ez_jni::__throw::catch::<#err>(env.borrow_mut())
                .map(|_| __call.unwrap_or_else(|err| panic!(#call_failed_msg)))
        } },
        None => quote! { {
            #initial
            ::ez_jni::__throw::panic_uncaught_exception(env.borrow_mut(), ::either::Either::Left(#class), #method_name);
            __call
                .unwrap_or_else(|err| panic!(#call_failed_msg))
        } }
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
        let (call_type, method_name) = if input.parse::<Token![static]>().is_ok() {
            let (class_path, method_name) = ClassPath::parse_with_trailing_method(input)?;
            (Either::Left(StaticMethod(class_path)), method_name)
        } else {
            (Either::Right(input.parse::<ObjectMethod>()?), input.parse()?)
        };

        Ok(Self {
            call_type,
            method_name,
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

/// Define a JNI call to a Java Class' constructor.
/// 
/// See [`crate::new`] for an example.
pub struct ConstructorCall {
    pub class: ClassPath,
    pub parameters: Punctuated<Parameter, Token![,]>,
    pub err_type: Option<syn::Path>
}
impl Parse for ConstructorCall {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self {
            class: input.parse()?,
            parameters: {
                let arg_tokens;
                parenthesized!(arg_tokens in input);
                Punctuated::parse_terminated(&arg_tokens)?
            },
            err_type: {
                let fork = input.fork();
                match fork.parse::<Ident>() {
                    Ok(ident) if ident.to_string() == "throws" => {
                        input.advance_to(&fork);
                        Some(input.parse()?)
                    },
                    _ => None
                }
            }
        })
    }
}

/// The call is for a static method.
/// ```ignore
/// call!(static java.lang.String.methodName ...);
/// ```
pub struct StaticMethod(pub ClassPath);

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

/// Parameter to a JNI method call, such as `java.lang.String(value)`.
/// Holds a [`Type`] that can be an [`ArrayType`], or a regular *single* [`InnerType`].
/// Can accept an **Array Literal** if the [`Type`] is array.
#[derive(Debug)]
pub struct Parameter {
    ty: Type,
    value: ParamValue,
}
#[derive(Debug)]
pub enum ParamValue {
    /// The value is `JObject::null()`.
    /// 
    /// *value* [`Null`][ParamValue::Null] and *type* [`Type::is_primitive()`] are mutually exclusive.
    /// This is checked by [`Parameter::parse()`].
    Null(Ident),
    Value(TokenStream)
}
impl Parameter {
    /// Returns the expression that will be put inside the `JValue::Variant(expr)` in the parameter list, a.k.a the *parameter's value*.
    /// 
    /// Don't return the expression wrapped in `JValue::Variant` because the *value* needs to be put in a variable first
    /// to avoid mutable borrowing `env` multiple times and to keep the reference to the *value* alive.
    pub fn expanded_value(&self) -> TokenStream {
        // Already checked (in parse) that value is not 'null' when ty is primitive.
        match &self.value {
            ParamValue::Value(value) => self.ty.convert_rust_to_java(value)
                .unwrap_or_else(|| value.clone()),
            ParamValue::Null(null) => quote_spanned!(null.span()=> ::jni::objects::JObject::null()),
        }
    }
    /// Conver the parameter to a `Jvalue` enum variant that will be used in the JNI call parameter list.
    /// The variant will have one of the parameter variables as the inner value.
    pub fn jni_variant(&self, var_name: Ident) -> TokenStream {
        fn primitive_variant(ty: JavaPrimitive, span: Span) -> TokenStream {
            Ident::new(
                &if ty == JavaPrimitive::Boolean {
                    "Bool".to_string()
                } else {
                    first_char_uppercase(ty.to_string())
                },
            span)
                .to_token_stream()
        }
        
        let (ty_span, ty_variant) = match &self.ty {
            Type::Single(ty) => match ty {
                InnerType::JavaPrimitive { ident, ty } => (ident.span(), primitive_variant(*ty, ident.span())),
                InnerType::RustPrimitive { ident, ty } => (ident.span(), primitive_variant(JavaPrimitive::from(*ty), ident.span())),
                InnerType::Object(class) => (class.span(), quote!(Object)),
            },
            Type::Array(ty) => (ty.span(), quote!(Object)),
        };
        let mut tt = quote! {};
        tt.append_all(quote_spanned! {ty_span=> ::jni::objects::JValue::#ty_variant });
        tt.append_all(quote_spanned! {var_name.span()=> (#var_name) });
        tt
    }
}
impl SigType for Parameter {
    fn sig_char(&self) -> Ident {
        self.ty.sig_char()
    }
    fn sig_type(&self) -> LitStr {
        self.ty.sig_type()
    }
}
impl Parse for Parameter {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        // TODO: detect if user uses Option and disallow it
        let ty = input.parse::<Type>()?;
        let value_tokens;
        parenthesized!(value_tokens in input);

        if value_tokens.is_empty() {
            return Err(value_tokens.error("Must provide a parameter value"));
        }

        // Check if parameter value is 'null'.
        let fork = value_tokens.fork();
        let value = match fork.parse::<Ident>() {
            Ok(ident) if ident.to_string() == NULL_KEYWORD => {
                value_tokens.advance_to(&fork);
                ParamValue::Null(ident)
            },
            _ => ParamValue::Value(value_tokens.parse()?)
        };

        // Check that user did not pass 'null' as the value of a primitive type.
        if let ParamValue::Null(null) = &value {
            if ty.is_primitive() {
                return Err(syn::Error::new(null.span(), "Can't use 'null' as value of primitive parameter type"))
            }
        }

        // If user treis to pass in an expression that resolves to `JObject::null()`, tell them to use 'null' keyword.
        if let ParamValue::Value(value) = &value {
            let v_str = value.to_string().replace(|c: char| c.is_whitespace(), "");
            if v_str.ends_with("::null()")
            || v_str.ends_with("::null_mut()") {
                // If ty is any of these, it will not accept the expression of a null value, so use 'null' instead.
                let should_use_null = match &ty {
                    Type::Array(ArrayType { ty: InnerType::Object(class), .. })
                    | Type::Single(InnerType::Object(class))  => class.to_jni_class_path() == "java/lang/String",
                    _ => false,
                };
                if should_use_null {
                    return Err(syn::Error::new_spanned(value, format!("This expression resolves to a raw object, which is not allowed here. If you want to pass a null value, try using the '{NULL_KEYWORD}' as the value.")))
                }
            }
        }

        Ok(Self { ty, value })
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
    /// The method being called can't throw (will `panic!` if it does).
    Assertive(ReturnableType),
    /// Holds the Ok Type (a Type or Option), and the Err Type.
    Result {
        result: Ident,
        ty: ReturnableType,
        err_ty: syn::Path
    },
}
impl Return {
    /// Create a new [`Return`] Type that is **Void**.
    pub fn new_void(span: Span) -> Self {
        Self::Assertive(ReturnableType::Void(Ident::new("void", span)))
    }
    /// Returns the inner [`ReturnableType`].
    pub fn inner(&self) -> &ReturnableType {
        match self {
            Self::Assertive(ty)
            | Self::Result { ty, .. } => ty
        }
    }
}
impl SigType for Return {
    fn sig_char(&self) -> Ident {
        match self {
            Self::Assertive(ty)
            | Self::Result { ty, .. } => ty.sig_char()
        }
    }
    fn sig_type(&self) -> LitStr {
        match self {
            Self::Assertive(ty)
            | Self::Result{ ty, .. } => ty.sig_type()
        }
    }
}
impl Spanned for Return {
    fn span(&self) -> Span {
        match self {
            Self::Assertive(ty) => ty.span(),
            Self::Result { result, ty, err_ty } =>
                join_spans([result.span(), ty.span(), err_ty.span()])
        }
    }
}
impl Parse for Return {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let fork = input.fork();
        // Check if caller uses Result as the return type
        Ok(match fork.parse::<Ident>() {
            Ok(ident) => match ident.to_string().as_str() {
                "Result" => {
                    input.advance_to(&fork);
                    // Parse generic arguements
                    input.parse::<Token![<]>().map_err(|err| {
                        input.error(format!("Result takes generic arguments; {err}"))
                    })?;
                    let ty = input.parse()?;
                    input.parse::<Token![,]>().map_err(|err| {
                        input.error(format!("Result takes 2 generic arguments; {err}"))
                    })?;
                    let err_ty = input.parse::<syn::Path>()?;
                    input.parse::<Token![>]>()?;
                    Self::Result { result: ident, ty, err_ty }
                }
                _ => Self::Assertive(input.parse()?),
            },
            // The return type did not start with Ident... weird, but let's continue
            Err(_) => Self::Assertive(input.parse()?),
        })
    }
}
impl Debug for Return {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Assertive(ty) => f.debug_tuple("Return::Aserrtive")
                .field(ty)
                .finish(),
            Self::Result { result, ty, err_ty } => f.debug_struct("Return::Result")
                .field("result", result)
                .field("ty", ty)
                .field("err_ty", &err_ty.to_token_stream().to_string())
                .finish()
        }
    }
}

/// A type that can be in the *return type* of `call!`.
/// This is the inner type of [`Return`].
/// 
/// Don't parse this type, as it denies `Result`.
/// Parse [`Return`] instead.
#[derive(Debug)]
pub enum ReturnableType {
    Void(Ident),
    /// Any **primitive** or **Class**.
    /// 
    /// Asserts that the returned value can't be `null`.
    /// Will cause a `panic!` if `call!` returns `null`.
    Assertive(InnerType),
    Array(ReturnArray),
    /// A Nullable **Object** or **Array**.
    Option(OptionType),
}
/// A type that can be in the `Option` of the [*return type*](ReturnableType).
/// Can't have *primitive* types themselves, but can have *array of primitive*.
#[derive(Debug)]
pub enum OptionType {
    Object(ClassPath),
    Array(ReturnArray)
}
impl ReturnableType {
    /// Handle special cases of the call's return value.
    /// 
    /// See [`SpecialCaseConversion::convert_java_to_rust()`].
    pub fn special_case_conversions(&self, value: TokenStream) -> Option<TokenStream> {
        match self {
            Self::Assertive(ty) => ty.convert_java_to_rust(&value),
            Self::Array(array) => array.special_case_conversions(value),
            Self::Option(OptionType::Array(array)) => array.special_case_conversions(value),
            Self::Option(OptionType::Object(class)) => class.convert_java_to_rust(&value),
            Self::Void(_) => None,
        }
    }
}
impl SigType for ReturnableType {
    fn sig_char(&self) -> Ident {
        match self {
            Self::Void(ident) => Ident::new("v", ident.span()),
            Self::Assertive(ty) => ty.sig_char(),
            Self::Array(array) => array.to_array_type().sig_char(),
            Self::Option(OptionType::Array(array)) => array.to_array_type().sig_char(),
            Self::Option(OptionType::Object(class)) => class.sig_char(),
        }
    }
    fn sig_type(&self) -> LitStr {
        match self {
            Self::Void(ident) => LitStr::new("V", ident.span()),
            Self::Assertive(ty) => ty.sig_type(),
            Self::Array(array) => array.to_array_type().sig_type(),
            Self::Option(OptionType::Array(array)) => array.to_array_type().sig_type(),
            Self::Option(OptionType::Object(class)) => class.sig_type(),
        }
    }
}
impl Spanned for ReturnableType {
    fn span(&self) -> Span {
        match self {
            Self::Void(ident) => ident.span(),
            Self::Assertive(ty) => ty.span(),
            Self::Array(array) => array.span(),
            Self::Option(OptionType::Array(array)) => array.span(),
            Self::Option(OptionType::Object(class)) => class.span(),
        }
    }
}
impl Parse for ReturnableType {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let fork = input.fork();
        // Check if caller uses void or Result
        if let Ok(ident) = fork.parse::<Ident>() {
            match ident.to_string().as_str() {
                "void" => return {
                    input.advance_to(&fork);
                    Ok(Self::Void(ident))
                },
                "Result" => return Err(syn::Error::new(
                    ident.span(),
                    "Can't nest a Result within a Result.",
                )),
                _ => {},
            }
        }
        drop(fork);

        // Attempt to parse the rest of the variants
        input.parse::<ReturnArray>()
            .map(|array| Self::Array(array))
            .or_else(|_| input.parse::<OptionType>()
                .map(|option| Self::Option(option))
            )
            .or_else(|_| input.parse::<InnerType>() // InnerType must go last
                .map(|ty| Self::Assertive(ty))
            )
    }
}
impl Parse for OptionType {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        static ERROR: &str = "Expected 'Option'";
        let fork = input.fork();

        match fork.parse::<Ident>() {
            Ok(ident) if ident.to_string() == "Option" => {
                input.advance_to(&fork);
                drop(fork);
                // Parse inner type of Option
                input.parse::<Token![<]>()
                    .map_err(|err| input.error(format!("Option takes generic arguments; {err}")))?;
                // Check if the Type is wrapped in Brackets (Array)
                let option_ty = if input.lookahead1().peek(syn::token::Bracket) {
                    Self::Array(input.parse()?)
                } else {
                    match input.parse::<InnerType>()? {
                        InnerType::Object(class) => Self::Object(class),
                        ty => return Err(syn::Error::new(ty.span(), "Option can't be used with primitives, only Classes."))
                    }
                };
                input.parse::<Token![>]>()
                    .map_err(|err| input.error(format!("Option takes only 1 generic argument; {err}")))?;

                Ok(option_ty)
            },
            Ok(ident) => Err(syn::Error::new(ident.span(), ERROR)),
            Err(_) => Err(input.error(ERROR))
        }
    }
}
impl Display for ReturnableType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Void(ident) => Display::fmt(ident, f),
            Self::Assertive(ty)
            | Self::Array(ReturnArray::Assertive(ty))
            | Self::Option(OptionType::Array(ReturnArray::Assertive(ty))) => Display::fmt(ty, f),
            Self::Array(ReturnArray::Option(class))
            | Self::Option(OptionType::Object(class))
            | Self::Option(OptionType::Array(ReturnArray::Option(class))) => Display::fmt(class, f),
        }
    }
}

#[derive(Debug)]
pub enum ReturnArray {
    /// Regular Array with **non-null** values.
    Assertive(InnerType),
    /// Array with **nullable** values.
    // TODO: Use OptionType instead when there is support for multi-dimensional Arrays
    Option(ClassPath),
}
impl ReturnArray {
    /// Creates an ArrayType to avoid reimplementing the same methods for ReturnArray.
    fn to_array_type(&self) -> ArrayType {
        ArrayType { ty: match self {
            Self::Assertive(ty) => ty.clone(),
            Self::Option(class) => InnerType::Object(class.clone())
        } }
    }
    /// Handle special cases of the call's return value,
    /// specifically when the return is `[Option<T>]`.
    /// 
    /// See [`ArrayType::convert_java_to_rust()`].
    pub fn special_case_conversions(&self, value: TokenStream) -> Option<TokenStream> {
        match self {
            Self::Assertive(_) => self.to_array_type().convert_java_to_rust(&value),
            Self::Option(class) => Some({
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
                    let mut _vec = ::std::vec::Vec::<::std::option::Option<_>>::with_capacity(_len);
                    for i in 0.._len {
                        let _element = env.get_object_array_element(&_array, i as ::jni::sys::jsize)
                            .unwrap_or_else(|err| panic!("Failed to read Array elements: {err}"));
                        _vec.push(if _element.is_null() {
                            ::std::option::Option::None
                        } else {
                            ::std::option::Option::Some(#conversion)
                        });
                    }
                    _vec.into_boxed_slice()
                } }
            }),
        }
    }
}
impl Spanned for ReturnArray {
    fn span(&self) -> Span {
        match self {
            Self::Assertive(ty) => ty.span(),
            Self::Option(class) => class.span(),
        }
    }
}
impl Parse for ReturnArray {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let inner;
        bracketed!(inner in input);
        
        match inner.parse::<OptionType>() {
            Ok(OptionType::Object(class)) => Ok(Self::Option(class)),
            Ok(OptionType::Array(array)) => Err(syn::Error::new(array.span(), "Multi-dimensional Arrays not yet supported.")),
            Err(_) => Ok(Self::Assertive(inner.parse()?))
        }
    }
}

/// Generate *variable definitions* for the arguments that will be passed to the JNI call.
/// 
/// Defines 1 variable for each argument,
/// where the *variable's name* is `__param_i` and *i* is the 0-based index of the argument.
/// 
/// Use [`gen_params()`] to put the resulting variables in the JNI call.
/// 
/// Putting the arguments in variables prevents them from being dropped (since JValue takes references),
/// and mitigates borrow checker error if the argument expression *borrows env* (since the call itself borrows &mut env).
fn gen_arg_vars_defs<'a>(params: impl Iterator<Item = &'a Parameter>) -> TokenStream {
    params
        .enumerate()
        .map(|(i, param)| {
            let value = param.expanded_value();
            let var_name = Ident::new(&format!("__param_{i}"), value.span());
            // Parameters of type object must always be passed by reference.
            if param.ty.is_primitive() {
                quote_spanned! {value.span()=> let #var_name = #value; }
            } else {
                quote_spanned! {value.span()=> let #var_name = &(#value); }
            }
        })
        .collect()
}
/// Generates the argument array that will be passed to the JNI call,
/// but the values are just the variables generated by [`gen_arg_vars_defs()`].
fn gen_arguments<'a>(params: impl Iterator<Item = &'a Parameter>) -> TokenStream {
    let params = params
        .enumerate()
        .map(|(i, param)| {
            param.jni_variant(Ident::new(&format!("__param_{i}"), param.expanded_value().span()))
        });
    quote! { &[ #( #params ),* ] }
}
