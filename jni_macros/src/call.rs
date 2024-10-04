use either::Either;
use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned, ToTokens, TokenStreamExt};
use syn::{
    braced, bracketed, parenthesized,
    parse::{discouraged::Speculative, Parse},
    punctuated::Punctuated,
    spanned::Spanned,
    Expr, Ident, LitInt, LitStr, Token,
};
use crate::utils::{first_char_uppercase, ClassPath, JavaPrimitive, RustPrimitive, SigType, Type};

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
        let incorrect_type_msg = format!("Expected {name}() to return {}: {{err}}", call.return_type.inner_type());
        let sig_char = Ident::new(&call.return_type.sig_char().to_string(), call.return_type.span());
        // Some Types need to be converted because they are a special case
        let conversion = match (call.return_type.special_case_conversions(quote!(v)), call.return_type.sig_char().to_string().as_str()) {
            // Move the result of the method call to an Option if it is Object because it could be null.
            (Some(conversion), "l") => quote! { .map(|v| (!v.is_null()).then_some(#conversion)) },
            (None, "l") => quote! { .map(|v| (!v.is_null()).then_some(v)) },
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

/// Processes input for macro call [super::new!].
pub fn jni_call_constructor(call: ConstructorCall) -> TokenStream {
    let class = call.class.to_jni_class_path();
    let signature = gen_signature(call.parameters.iter(), &Return::Void(Ident::new("void", Span::call_site())));
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

    /// Returns the expression that will be put inside the `JValue::Variant(expr)` in the parameter list, a.k.a the *parameter's value*.
    pub fn value(&self) -> TokenStream {
        fn primitive_conversion(ty: Either<RustPrimitive, JavaPrimitive>, value: TokenStream) -> Option<TokenStream> {
            match ty {
                // Transmute unsigned integers
                Either::Left(ty) if ty.is_unsigned() => Some(quote_spanned!(value.span()=> unsafe { ::std::mem::transmute(#value) })),
                // Bool must be cast to u8
                Either::Right(JavaPrimitive::Boolean)
                | Either::Left(RustPrimitive::Bool) => Some(quote_spanned!(value.span()=> #value as u8)),
                // Char must be encoded to UTF-16 (will panic! if the conversion fails)
                Either::Right(JavaPrimitive::Char)
                | Either::Left(RustPrimitive::Char) => Some(quote_spanned!(value.span()=> #value.encode_utf16(&mut [0;1])[0])), 
                _ => None,
            }
        }
        
        match self {
            Self::Single { ty, value } => match ty {
                Type::JavaPrimitive { ty, .. } => primitive_conversion(Either::Right(*ty), value.clone())
                    .unwrap_or_else(|| value.clone()),
                Type::RustPrimitive { ty, .. } => primitive_conversion(Either::Left(*ty), value.clone())
                    .unwrap_or_else(|| value.clone()),
                Type::Object(_) => value.clone(),
            },
            Self::Array { value, .. } => value.clone(),
            // Create an Java Array from a Rust array literal
            Self::ArrayLiteral { ty, array } => {
                let len = LitInt::new(&array.iter().count().to_string(), array.span());
                
                let primitive_array = |ty: Either<RustPrimitive, JavaPrimitive>| {
                    let values = match primitive_conversion(ty, quote!(v)) {
                        Some(conversion) => quote_spanned!(array.span()=> &[#array].map(|v| #conversion)),
                        None => quote_spanned!(array.span()=> &[#array])
                    };
                    let ty = ty.right_or_else(|ty| JavaPrimitive::from(ty));
                    // Can only use JavaPrimitive from here on
                    
                    let new_array_fn = Ident::new(&format!("new_{ty}_array"), array.span());
                    let new_array_err = LitStr::new(
                        &format!("Failed to create Java {ty} array: {{err}}"),
                        array.span(),
                    );
                    let fill_array_fn =
                        Ident::new(&format!("set_{ty}_array_region"), array.span());
                    let fill_array_err = LitStr::new(
                        &format!("Error filling {ty} array: {{err}}"),
                        array.span(),
                    );
                    // Create a Java array Object from the array literal
                    quote_spanned! {array.span()=> {
                        let array = env.#new_array_fn(#len)
                            .inspect_err(|err| println!(#new_array_err)).unwrap();
                        env.#fill_array_fn(&array, 0, #values)
                            .inspect_err(|err| println!(#fill_array_err)).unwrap();
                        ::jni::objects::JObject::from(array)
                    } }
                };

                match ty {
                    Type::JavaPrimitive { ty, .. } => primitive_array(Either::Right(*ty)),
                    Type::RustPrimitive { ty, .. } => primitive_array(Either::Left(*ty)),
                    Type::Object(class) => {
                        let new_array_err = LitStr::new(
                            &format!("Failed to create Java Object \"{}\" array: {{err}}", class.to_string()),
                            array.span(),
                        );
                        let set_val_err = LitStr::new(
                            &format!("Failed to set the value of Object array at index {{i}}: {{err}}"),
                            array.span(),
                        );
                        let class_path = LitStr::new(&class.to_jni_class_path(), array.span());
                        // Fill the array
                        let mut elements = quote! {};
                        for (i, element) in array.iter().enumerate() {
                            let element = quote_spanned! {array[i].span()=>
                                env.set_object_array_element(&array, #i, #element)
                                    .inspect_err(|err| println!(#set_val_err)).unwrap()
                            };
                            elements = quote! { #elements #element }
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
        
        let (ty_span, ty_variant) = match self {
            Self::Single { ty, .. } => match ty {
                Type::JavaPrimitive { ident, ty } => (ident.span(), primitive_variant(*ty, ident.span())),
                Type::RustPrimitive { ident, ty } => (ident.span(), primitive_variant(JavaPrimitive::from(*ty), ident.span())),
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
            | Return::Result(ResultType::Option(class), _) => class.to_jni_class_path(),
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
            Return::Assertive(Type::RustPrimitive { ty, .. })
            | Return::Result(ResultType::Assertive(Type::RustPrimitive { ty, .. }), _)
                => ty.special_case_conversion(value),
            // Special cases for Java Objects
            Return::Assertive(Type::Object(class))
            | Return::Option(class)
            | Return::Result(ResultType::Assertive(Type::Object(class)), _)
            | Return::Result(ResultType::Option(class), _) =>
                if class.to_jni_class_path() == "java/lang/String" {
                    // Wrap java.lang.String in JString
                    Some(quote! { ::jni::objects::JString::from(#value) })
                } else {
                    None
                },
            _ => None
        }
    }
}
impl SigType for Return {
    fn sig_char(&self) -> Ident {
        match self {
            Self::Void(ident) | Self::Result(ResultType::Void(ident), _) => Ident::new("v", ident.span()),
            Self::Assertive(ty) | Self::Result(ResultType::Assertive(ty), _) => ty.sig_char(),
            Self::Option(class) | Self::Result(ResultType::Option(class), _) => class.sig_char(),
        }
    }
    fn sig_type(&self) -> LitStr {
        match self {
            Self::Void(ident)
            | Self::Result(ResultType::Void(ident), _) => LitStr::new("V", ident.span()),
            Self::Assertive(ty)
            | Self::Result(ResultType::Assertive(ty), _) => ty.sig_type(),
            Self::Option(class)
            | Self::Result(ResultType::Option(class), _) => class.sig_type(),
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
            let value = param.value();
            let var_name = Ident::new(&format!("__param_{i}"), value.span());
            // Parameters of type object must always be passed by reference.
            if param.is_primitive() {
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
            param.variant(Ident::new(&format!("__param_{i}"), param.value().span()))
        });
    quote! { &[ #( #params ),* ] }
}

/// Generate the signature string for a JNI call.
fn gen_signature<'a>(params: impl Iterator<Item = &'a Parameter>, return_type: &Return) -> LitStr {
    let mut buf = String::from("(");
    for param in params {
        // Array types in signature have an opening bracket prepended to the type
        if param.is_array() {
            buf.push('[');
        }
        buf.push_str(&param.ty().sig_type().value());
    }
    buf.push(')');
    buf.push_str(&return_type.sig_type().value());
    LitStr::new(&buf, Span::call_site())
}
