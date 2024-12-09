use std::fmt::Debug;
use either::Either;
use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned, ToTokens, TokenStreamExt};
use syn::{
    parenthesized, parse::{discouraged::Speculative, Parse, ParseStream, Parser}, punctuated::Punctuated, Ident, LitStr, Token
};
use utils::first_char_uppercase;
use crate::{
    types::{ArrayType, Class, InnerType, JavaPrimitive, RustPrimitive, SigType, SpecialCaseConversion, Type, NULL_KEYWORD},
    utils::{gen_signature, join_spans, Spanned}
};

/// Processes input for macro call [super::call!].
pub fn jni_call(call: MethodCall) -> TokenStream {
    let name = call.method_name.to_string();
    let signature = gen_signature(call.parameters.iter(), &call.return_type);
    let param_vars = gen_arg_vars_defs(call.parameters.iter());
    let arguments = gen_arguments(call.parameters.iter());

    // Build the macro function call
    let jni_method = if call.call_type.is_static() {
        quote!(call_static_method)
    } else {
        quote!(call_method)
    };
    let jni_call = quote! { {
        #param_vars
        env.#jni_method(__callee, #name, #signature, #arguments)
    } };

    call.return_type.convert_call_result(&name, jni_call, &call.call_type)
}

/// Processes input for macro call [super::new!].
pub fn jni_call_constructor(call: ConstructorCall) -> TokenStream {
    // The Class/ClassObject that the constructor was called for
    let callee = match &call.class {
        Either::Left(class) => class.to_jni_class_path().to_token_stream(),
        Either::Right(expr) => quote_spanned!(expr.span()=> (#expr).as_ref()),
    };
    let callee_var = quote_spanned!(callee.span()=> __callee);
    let call_target = match &call.class {
        Either::Left(_) => quote! { ::ez_jni::__throw::CallTarget::Class(#callee_var) },
        Either::Right(_) => quote! { ::ez_jni::__throw::CallTarget::ClassObject(#callee_var) },
    };
    let signature = gen_signature(call.parameters.iter(), &Return::new_void(Span::call_site()));
    let param_vars = gen_arg_vars_defs(call.parameters.iter());
    let arguments = gen_arguments(call.parameters.iter());
    let method_name = format!("<init>{}", signature.value());
    let call_failed_msg = format!("Failed to call constructor {}: {{err}}", signature.value());
    
    // The Initial JNI call, before any types or errors are checked
    let initial = quote! {
        use std::borrow::BorrowMut as _;
        use std::convert::AsRef as _;
        let __callee = #callee;
        #param_vars
        let __call = env.new_object(__callee, #signature, #arguments);
    };
    match call.err_type {
        Some(err) => quote! { {
            #initial
            ::ez_jni::__throw::catch::<#err>(env.borrow_mut())
                .map(|_| __call.unwrap_or_else(|err| panic!(#call_failed_msg)))
        } },
        None => quote! { {
            #initial
            ::ez_jni::__throw::panic_uncaught_exception(env.borrow_mut(), #call_target, #method_name);
            __call
                .unwrap_or_else(|err| panic!(#call_failed_msg))
        } }
    }
}

/// Define a JNI call to a Java method with parameters with expected types, and an expected return type.
///
/// See [`crate::call`] for an example.
pub struct MethodCall {
    pub call_type: CallType,
    pub method_name: Ident,
    pub parameters: Punctuated<Parameter, Token![,]>,
    pub return_type: Return,
}
impl Parse for MethodCall {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // This parse implementation is complicated to allow parsing arbitrary Expressions as the callee.
        use proc_macro2::{TokenTree, Delimiter, Spacing};

        let is_static = input.parse::<Token![static]>().is_ok();

        let mut callee_tokens = TokenStream::new();
        // Keep track of the 2 tokens directly preceding the `(...) ->` pattern, as those are the Dot and Method Ident.
        let mut method_dot = None;
        let mut method_ident = None;

        // Now we will iterate through the TokenStream until we find this pattern: `(...) ->`.
        // These tokens basically split the left and right sides of the function syntax.
        // We also know that the token before the `(...)` is the Method name,
        // So we need to keep track of at least 1 previous token.
        // We also need to keep track of the Dot preceding the Method name so it isn't parsed as part of the callee.
        let param_group = input.step(|cursor| {
            let mut current = *cursor;

            // Look for pattern `(...) ->` and aggregate all tokens that come before it
            while let Some((token, next)) = current.token_tree() {
                current = next;
            
                // Check (...). Btw, welcome to nesting hell.
                if let TokenTree::Group(group) = &token {
                    if group.delimiter() == Delimiter::Parenthesis {
                        // Check -
                        if let Some((punct, next)) = next.punct() {
                            if punct.as_char() == '-' && punct.spacing() == Spacing::Joint {
                                // Check >
                                if let Some((punct, next)) = next.punct() {
                                    if punct.as_char() == '>' && punct.spacing() == Spacing::Alone {
                                        // Found pattern `(...) ->`
                                        return Ok((group.clone(), next));
                                    }
                                }
                            }
                        }
                    }
                }

                // The next tokens don't form the `(...) ->` pattern.
                // Shift the tokens Dot and Ident tokens to the left.
                if let Some(ident_token) = (&mut method_ident).take() {
                    (&mut method_dot).take()
                        .map(|token| callee_tokens.append(token));
                    method_dot = Some(ident_token);
                }
                method_ident = Some(token)

                // This wasn't it, try next
            }

            Err(cursor.error("Invalid tokens; Did not find pattern `(...) ->`"))
        })?;

        // Error used if either method_dot or method_ident were None.
        let none_err = syn::Error::new(param_group.span(), "Expected Dot and Ident `.methodName`");

        let method_name = method_dot
            .ok_or_else(|| none_err.clone())
            .and_then(|token| match token {
                TokenTree::Punct(punct)
                    if punct.as_char() == '.'
                    && punct.spacing() == Spacing::Alone
                        => Ok(()),
                _ => Err(syn::Error::new(token.span(), "Expected Dot '.'"))
            })
            .and(method_ident.ok_or(none_err))
            .and_then(|token| match token {
                TokenTree::Ident(ident) => Ok(ident),
                _ => Err(syn::Error::new(token.span(), "Expected Ident"))
            })?;

        Ok(Self {
            call_type: CallType::parse_callee(callee_tokens, is_static)?,
            method_name,
            parameters: Parser::parse2(Punctuated::parse_terminated, param_group.stream())?,
            return_type: input.parse()?,
        })
    }
}

pub enum CallType {
    /// The call is for a `static` method.
    /// 
    /// This can be either [`Class`] tokens or an [`Expression`][syn::Expr] (enclosed in `parenthesis` or `braces`)
    /// that resolves to a [class object][jni::objects::JClass].
    /// 
    /// ```ignore
    /// call!(static java.lang.String.methodName ...);
    /// ```
    Static(Either<Class, TokenStream>),
    /// The call is for a Method of an existing Object, stored in a variable.
    /// If the object is more than an Ident, it must be enclosed in `parenthesis` or `braces`.
    /// e.g. `object.methodName(...)` or `(something.object).methodName(...)`.
    Object(TokenStream),
}
impl CallType {
    pub fn is_static(&self) -> bool {
        match self {
            Self::Static(_) => true,
            Self::Object(_) => false,
        }
    }

    /// Returns the [`Class`] or **Object expression** that the *method* (static or not) is being called on.
    pub fn callee(&self) -> Either<&Class, &TokenStream> {
        match self {
            Self::Static(Either::Left(class)) => Either::Left(class),
            Self::Static(Either::Right(obj))
            | Self::Object(obj) => Either::Right(obj),
        }
    }

    /// Parse [`Class`] or leave as Expression. Then create an instance of this struct.
    pub(self) fn parse_callee(callee: TokenStream, is_static: bool) -> syn::Result<Self> {
        // Parse speculatively without having to clone TokenStream
        Parser::parse2(|input: ParseStream| {
            let fork = input.fork();

            let result = fork.parse::<Class>()
                .and_then(|class| {
                    // Must have parsed all tokens, or it is considered error
                    if !fork.is_empty() {
                        Err(fork.error("Must parse all tokens"))
                    } else {
                        input.advance_to(&fork);
                        drop(fork);

                        Ok(class)
                    }
                });

            Ok(match result {
                Ok(class) => if is_static {
                    Self::Static(Either::Left(class))
                } else {
                    return Err(syn::Error::new(class.span(), "Can't call object method on Class. Try using 'static' at the beginning of the macro, or use the singleton! macro."))
                },
                // If Class could not be parsed, use verbatim TokenStream.
                Err(_) => if is_static {
                    Self::Static(Either::Right(input.parse()?))
                } else {
                    Self::Object(input.parse()?)
                },
            })
        }, callee)
    }
}

/// Define a JNI call to a Java Class' constructor.
/// 
/// See [`crate::new`] for an example.
pub struct ConstructorCall {
    pub class: Either<Class, TokenStream>,
    pub parameters: Punctuated<Parameter, Token![,]>,
    pub err_type: Option<syn::Path>
}
impl Parse for ConstructorCall {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // This is similar to MethodCall::parse() in that it will collect tokens to parse as the class until it finds a certain pattern.

        let mut class_tokens = TokenStream::new();
        
        // Iterate through the TokenStream until this pattern is found: `(...)` | `(...) throws`.
        let param_group = input.step(|cursor| {
            use proc_macro2::{TokenTree, Delimiter};

            let mut current = *cursor;

            // Look for the pattern while pushing tokens
            while let Some((token, next)) = current.token_tree() {
                current = next;

                // Check (...)
                if let TokenTree::Group(group) = &token {
                    if group.delimiter() == Delimiter::Parenthesis {
                        // Check it's the last token
                        if next.eof() {
                            // Found pattern `(...)`
                            return Ok((group.clone(), next));
                        }
                        // OR is followed by `throws`
                        else if let Some((ident, next)) = next.ident() {
                            if ident == "throws" {
                                // Found pattern `(...) throws`
                                return Ok((group.clone(), next))
                            }
                        }
                    }
                }

                // These tokens don't form the pattern
                class_tokens.append(token);
            }

            Err(syn::Error::new(current.span(), "Invalid tokens"))
        })?;

        // Parse speculatively without having to clone TokenStream
        let class =  Parser::parse2(|input: ParseStream| {
            let fork = input.fork();

            fork.parse::<Class>()
                .inspect(move |_| input.advance_to(&fork))
                .map(|class| Either::Left(class))
                .or_else(|err| {
                    input.parse::<TokenStream>()
                        .map(|tokens| Either::Right(tokens))
                        .map_err(|_| err)
                })
        }, class_tokens)?;

        Ok(Self {
            class,
            parameters: Parser::parse2(Punctuated::parse_terminated, param_group.stream())?,
            err_type: (!input.is_empty())
                .then(|| input.parse::<syn::Path>())
                .transpose()?
        })
    }
}

/// Parameter to a JNI method call, such as `java.lang.String(value)`.
/// Holds a [`Type`] that can be an [`ArrayType`], or a regular *single* [`InnerType`].
/// Can accept an **Array Literal** if the [`Type`] is array.
pub struct Parameter {
    ty: Type,
    value: ParamValue,
}
impl Parameter {
    /// Conver the parameter to a `Jvalue` enum variant that will be used in the JNI call parameter list.
    /// The variant will have one of the parameter variables as the inner value.
    pub fn jni_variant(&self, var_name: Ident) -> TokenStream {
        fn primitive_variant(ty: JavaPrimitive, span: Span) -> TokenStream {
            Ident::new(
                &if ty == JavaPrimitive::Boolean {
                    "Bool".to_string()
                } else {
                    first_char_uppercase(&ty.to_string())
                },
            span)
                .to_token_stream()
        }
        
        let (ty_span, ty_variant) = match &self.ty {
            Type::Assertive(ty) => match ty {
                InnerType::JavaPrimitive { ident, ty } => (ident.span(), primitive_variant(*ty, ident.span())),
                InnerType::RustPrimitive { ident, ty } => (ident.span(), primitive_variant(JavaPrimitive::from(*ty), ident.span())),
                InnerType::Object(class) => (class.span(), quote!(Object)),
            },
            Type::Array(array) => (array.span(), quote!(Object)),
            Type::Option { .. } => panic!("Unreachable code; Already checked that Option is not used in arguments")
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
        let ty = input.parse::<Type>()?;
        let value_tokens;
        parenthesized!(value_tokens in input);
        if value_tokens.is_empty() {
            return Err(value_tokens.error("Must provide a parameter value."));
        }

        let value = value_tokens.parse::<ParamValue>()?;

        // Detect if user uses Option (iteratively) and disallow it,
        // But only for the first layer (i.e. Object and top-level array)
        if let Type::Option { ident, .. } = &ty {
            return Err(syn::Error::new(
                ident.span(),
                "Can't use 'Option' in call! or new! arguments. Instead, use 'null' as the value."
            ))
        }

        // Check that the correct Value was passed in for the correct Type
        match &value {
            // null can be used for Object and Array
            ParamValue::Null(null) => match &ty {
                Type::Assertive(InnerType::JavaPrimitive { .. } | InnerType::RustPrimitive { .. })
                    => return Err(syn::Error::new(null.span(), "Can't use 'null' as value of primitive argument type.")),
                Type::Assertive(InnerType::Object(_))
                | Type::Option { .. }
                | Type::Array(_) => { },
            },
            _ => {}
        };

        // If user tries to pass in an expression that resolves to `JObject::null()`, tell them to use 'null' keyword.
        if let ParamValue::Value(value) = &value {
            let v_str = value.to_string().replace(|c: char| c.is_whitespace(), "");
            if v_str.ends_with("::null()")
            || v_str.ends_with("::null_mut()") {
                // If ty is any of these, it will not accept the expression of a null value, so use 'null' instead.
                let should_use_null = match &ty {
                    Type::Array(ArrayType { ty, .. }) => match ty.as_ref() {
                        Type::Assertive(InnerType::Object(class)) => class.to_jni_class_path() == "java/lang/String",
                        _ => false,
                    }
                    | Type::Assertive(InnerType::Object(class)) => class.to_jni_class_path() == "java/lang/String",
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
impl ToTokens for Parameter {
    /// Returns the expression that will be put inside the `JValue::Variant(expr)` in the parameter list, a.k.a the *parameter's value*.
    /// 
    /// The parsed value is converted to a valid Rust expression (with [`SpecialCaseConversion::convert_rust_to_java`]),
    /// which is then converted to a `JObject` or `jprimitive`.
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append_all(match &self.value {
            ParamValue::Null(null) => quote_spanned!(null.span()=> ::jni::objects::JObject::null()),
            ParamValue::Value(value) => self.ty.convert_rust_to_java(&value)
                .unwrap_or_else(|| value.clone())
        })
    }
}

pub enum ParamValue {
    /// The value is `JObject::null()`.
    /// 
    /// *value* [`Null`][ParamValue::Null] and *type* [`Type::is_primitive()`] are mutually exclusive.
    /// This is checked by [`Parameter::parse()`].
    Null(Ident),
    Value(TokenStream)
}
impl Spanned for ParamValue {
    fn span(&self) -> Span {
        match self {
            Self::Null(null) => null.span(),
            Self::Value(value) => value.span(),
        }
    }
}
impl Parse for ParamValue {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let fork = input.fork();
        
        Ok(match fork.parse::<Ident>() {
            // Check if argument value is 'null'.
            Ok(ident) if ident.to_string() == NULL_KEYWORD => {
                input.advance_to(&fork);
                Self::Null(ident)
            },
            // The value is the Verbatim TokenStream
            _ => Self::Value(input.parse()?),
        })
    }
}
impl Debug for ParamValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Null(null) => f.debug_tuple("ParamValue::Null")
                .field(&null.to_string())
                .finish(),
            Self::Value(value) => f.debug_tuple("ParamValue::Value")
                .field(&value.to_string())
                .finish()
        }
    }
}

/// The return type of a JNI method call.
///
/// The caller of the macro can assert that the JNI method call will result in one of the following:
/// 1. [`Type`] (or `void`) if the function being called can't return `NULL` or throw an `exception` (e.g. `bool` or `java.lang.String`).
/// 2. `Option<Class>` if the return type is an [`Object`][Type::Object] that could be **NULL**.
///    Java *does not allow* primitive types (i.e. not Object) to be **NULL**, so [Self::Option] can only be used with a [Class].
/// 3. `Result<Type | void | Option<Class>, E>` if the method call can throw an **Exception**,
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
#[derive(Debug)]
pub enum ReturnableType {
    Void(Ident),
    Type(Type)
}
impl Return {
    /// Create a new [`Return`] Type that is **Void**.
    pub fn new_void(span: Span) -> Self {
        Self::Assertive(ReturnableType::Void(Ident::new("void", span)))
    }
    /// Returns the inner type (as String).
    pub fn inner(&self) -> String {
        let ty = match self {
            Self::Assertive(ty) => ty,
            Self::Result { ty, .. } => ty
        };
        match ty {
            ReturnableType::Void(ident) => ident.to_string(),
            ReturnableType::Type(ty) => ty.to_string(),
        }
    }
    /// Converts to value of the JNI Call to a *Rust value*.
    /// 
    /// Also checks if the Call resulted in an *exception*.
    /// `panic!s` if there was an exception and the return type was not [`Result`].
    /// Otherwise, wraps the value in a [`Result`].
    pub fn convert_call_result(&self, method_name: &str, jni_call: TokenStream, call_type: &CallType) -> TokenStream {
        // Common checks done by all Return variants
        let common = {
            // Induce panic when fails to call method
            let call_failed_msg = match call_type.callee() {
                Either::Left(class) => format!("Failed to call static method {class}.{method_name}(): {{err}}"),
                Either::Right(_) => format!("Failed to call method {method_name}(): {{err}}"),
            };
            // Induce panic when the returned value is not the expected type
            let incorrect_type_msg = format!("Expected {method_name}() to return {}: {{err}}", self.inner());
            let sig_char = Ident::new(&self.sig_char().to_string(), self.span());
            // Apply special case conversion
            let conversion = match self {
                // Ignore bool conversion because JValue.z() returns Rust bool, not jboolean.
                Self::Assertive(ReturnableType::Type(Type::Assertive(InnerType::RustPrimitive { ty: RustPrimitive::Bool, .. })))
                | Self::Assertive(ReturnableType::Type(Type::Assertive(InnerType::JavaPrimitive { ty: JavaPrimitive::Boolean, .. })))
                | Self::Result { ty: ReturnableType::Type(Type::Assertive(InnerType::RustPrimitive { ty: RustPrimitive::Bool, .. })), .. }
                | Self::Result { ty: ReturnableType::Type(Type::Assertive(InnerType::JavaPrimitive { ty: JavaPrimitive::Boolean, .. })), .. }
                    => None,
                // Void has no conversion
                Self::Assertive(ReturnableType::Void(_))
                | Self::Result { ty: ReturnableType::Void(_), .. } => None,
                // Normal conversion
                Self::Assertive(ReturnableType::Type(ty))
                | Self::Result { ty: ReturnableType::Type(ty), .. } => ty.convert_java_to_rust(&quote_spanned!(jni_call.span()=> v))
                    .map(|conversion| quote! { .map(|v| #conversion) }),
            };

            quote! {
                .unwrap_or_else(|err| panic!(#call_failed_msg))
                .#sig_char()
                #conversion // Might be empty tokens
                .unwrap_or_else(|err| panic!(#incorrect_type_msg))
            }
        };

        // The class or object that the method is being called on.
        let callee = match call_type.callee() {
            Either::Left(class) => class.to_jni_class_path().to_token_stream(),
            Either::Right(expr) => quote!(&(#expr)),
        };
        let callee_var = quote_spanned!(callee.span()=> __callee);
        let target = match &call_type {
            CallType::Static(Either::Left(_)) => quote! { ::ez_jni::__throw::CallTarget::Class(#callee_var) },
            CallType::Static(Either::Right(_)) => quote! { ::ez_jni::__throw::CallTarget::ClassObject(#callee_var) },
            CallType::Object(_) => quote! { ::ez_jni::__throw::CallTarget::Object(#callee_var) },
        };

        match self {
            // For return types that are not Result
            Self::Assertive(_) => quote! { {
                use ::std::borrow::BorrowMut as _;
                let __callee = #callee;
                let __call = #jni_call;
                ::ez_jni::__throw::panic_uncaught_exception(env.borrow_mut(), #target, #method_name);
                __call #common
            } },
            // Move the result of the method call to a Result if the caller expects that the method could throw.
            Self::Result { err_ty, ..} => quote! { {
                use ::std::borrow::BorrowMut as _;
                let __callee = #callee;
                let __call = #jni_call;
                ::ez_jni::__throw::catch::<#err_ty>(env.borrow_mut())
                    .map(|_| __call #common)
            } }
        }
    }
}
impl SigType for Return {
    fn sig_char(&self) -> Ident {
        match self {
            Self::Assertive(ReturnableType::Void(ident))
            | Self::Result { ty: ReturnableType::Void(ident), .. } => Ident::new("v", ident.span()),
            Self::Assertive(ReturnableType::Type(ty))
            | Self::Result { ty: ReturnableType::Type(ty), .. } => ty.sig_char(),
        }
    }
    fn sig_type(&self) -> LitStr {
        match self {
            Self::Assertive(ReturnableType::Void(ident))
            | Self::Result { ty: ReturnableType::Void(ident), .. } => LitStr::new("V", ident.span()),
            Self::Assertive(ReturnableType::Type(ty))
            | Self::Result { ty: ReturnableType::Type(ty), .. } => ty.sig_type(),
        }
    }
}
impl Spanned for Return {
    fn span(&self) -> Span {
        #[inline] fn inner_ty_span(ty: &ReturnableType) -> Span {
            match ty {
                ReturnableType::Void(ident) => ident.span(),
                ReturnableType::Type(ty) => ty.span(),
            }
        }
        match self {
            Self::Assertive(ty) => inner_ty_span(ty),
            Self::Result { result, ty, err_ty } => join_spans([
                result.span(), inner_ty_span(ty), err_ty.span()
            ]),
        }
    }
}
impl Parse for Return {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let fork = input.fork();
        // Check if caller uses void or Result as the return type
        Ok(match fork.parse::<Ident>() {
            Ok(ident) => match ident.to_string().as_str() {
                "void" => {
                    input.advance_to(&fork);
                    Self::Assertive(ReturnableType::Void(ident))
                },
                "Result" => {
                    input.advance_to(&fork);
                    drop(fork);
                    // Parse generic arguements
                    input.parse::<Token![<]>().map_err(|err| {
                        input.error(format!("Result takes generic arguments; {err}"))
                    })?;
                    let ty = {
                        let fork = input.fork();
                        match fork.parse::<Ident>() {
                            // Check if is void, or a regular type
                            Ok(ident) if ident.to_string() == "void" => {
                                input.advance_to(&fork);
                                ReturnableType::Void(ident)
                            },
                            _ => ReturnableType::Type(input.parse()?)
                        }
                    };
                    input.parse::<Token![,]>().map_err(|err| {
                        input.error(format!("Result takes 2 generic arguments; {err}"))
                    })?;
                    let err_ty = input.parse::<syn::Path>()?;
                    input.parse::<Token![>]>()?;
                    Self::Result { result: ident, ty, err_ty }
                }
                // Is probably a primitive or Class
                _ => Self::Assertive(ReturnableType::Type(input.parse()?)),
            },
            // Is probably an Array
            Err(_) => Self::Assertive(ReturnableType::Type(input.parse()?)),
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
            let value = param.to_token_stream();
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
            param.jni_variant(Ident::new(&format!("__param_{i}"), param.span()))
        });
    quote! { &[ #( #params ),* ] }
}
