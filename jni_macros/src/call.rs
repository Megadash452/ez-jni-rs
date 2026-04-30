use std::fmt::Debug;
use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned, ToTokens, TokenStreamExt};
use syn::{
    parenthesized, parse::{discouraged::Speculative, Parse, ParseStream, Parser}, punctuated::Punctuated, Expr, Ident, LitStr, Token
};
use crate::{
    types::{Class, ClassRustType, InnerType, SigType, Type, NULL_KEYWORD},
    utils::{gen_signature, join_spans, ops_prelude, Spanned, StepResult, TokenTreeExt as _}
};

/// Processes input for macro call [super::call!].
pub fn jni_call(call: MethodCall) -> TokenStream {
    let ops_prelude = ops_prelude(call.mods.env);
    // The class or object that the method is being called on.
    let callee = match &call.call_type {
        CallType::Static(ClassRepr::String(class)) => {
            let class = class.to_jni_class_path();
            quote!(::ez_jni::utils::ClassRepr::String(#class))
        },
        CallType::Static(ClassRepr::Object(expr)) => quote!{ ::ez_jni::utils::ClassRepr::Object((#expr).borrow()) },
        CallType::Object(expr) => quote!{ (#expr).borrow() },
    };

    let name = call.method_name.to_string();
    let signature = gen_signature(call.parameters.iter(), &call.return_type);
    let arguments = gen_arguments(call.parameters.iter());

    // Convert from the returned Result to a value the caller expects.
    let return_conversion = call.mods.jni_error_handler.convert_call_return_value(&call.return_type, true);
    // TODO: make an error handler for #val_conversion of arguments, so that errors gets returned in the MethodCallError.

    // Build the macro function call

    let jni_method = if call.call_type.is_static() {
        quote! { ::ez_jni::utils::call_static_method }
    } else {
        quote! { ::ez_jni::utils::call_obj_method }
    };
    quote! { {
        #ops_prelude
        #[allow(noop_method_call)]
        #jni_method(#callee, #name, #signature, #arguments, env)
            #return_conversion
    } }
}

/// Processes input for macro call [super::new!].
pub fn jni_call_constructor(call: ConstructorCall) -> TokenStream {
    let ops_prelude = ops_prelude(call.mods.env);
    // The Class/ClassObject that the constructor was called for
    let callee = match &call.class {
        ClassRepr::String(class) => {
            let class = class.to_jni_class_path();
            quote!(::ez_jni::utils::ClassRepr::String(#class))
        },
        ClassRepr::Object(expr) => quote!{ ::ez_jni::utils::ClassRepr::Object((#expr).borrow()) },
    };

    let signature = gen_signature(call.parameters.iter(), &Return::new_void(Span::call_site()));
    let arguments = gen_arguments(call.parameters.iter());

    let return_type = match call.class {
        ClassRepr::String(class) => Type::Assertive(InnerType::Object(class)),
        ClassRepr::Object(expr) => Type::Assertive(InnerType::Object(Class::from_rust_type(ClassRustType::JObject, expr.span()))),
    };
    let return_type = match call.err_class {
        Some(err_class) => Return::Result { result: Ident::new("throws", err_class.span()), ty: ReturnableType::Type(return_type), err_class },
        None => Return::Assertive(ReturnableType::Type(return_type)),
    };
    let return_conversion = call.mods.jni_error_handler.convert_call_return_value(&return_type, false);

    quote! { {
        #ops_prelude
        #[allow(noop_method_call)]
        ::ez_jni::utils::create_object(#callee, #signature, #arguments, env)
            #return_conversion
    } }
}

/// Processes input for macro call [super::field!].
pub fn field(call: FieldCall) -> TokenStream {
    let ops_prelude = ops_prelude(call.mods.env);
    // The class or object that the method is being called on.
    let callee = match &call.call_type {
        CallType::Static(ClassRepr::String(class)) => {
            let class = class.to_jni_class_path();
            quote!(::ez_jni::utils::ClassRepr::String(#class))
        },
        CallType::Static(ClassRepr::Object(expr)) => quote!{ ::ez_jni::utils::ClassRepr::Object((#expr).borrow()) },
        CallType::Object(expr) => quote!{ (#expr).borrow() },
    };
    let name = call.field_name.to_string();
    let ty_sig = call.ty.sig_type();
    let error_handler = call.mods.jni_error_handler.handle_call_error();

    // Build the macro function call
    match call.set_val {
        Some(val) => {
            let jni_method = if call.call_type.is_static() {
                quote!(::ez_jni::utils::set_static_field)
            } else {
                quote!(::ez_jni::utils::set_obj_field)
            };
            // Convert Rust value to JValue for argument
            let val = call.ty.convert_rvalue_to_jvalue(&val.to_token_stream());
            // TODO: make an error handler for #val_conversion, so that error gets returned in the FieldError.
            quote! { {
                #ops_prelude
                #[allow(noop_method_call)]
                #jni_method(#callee, #name, #ty_sig, #val, env)
                    #error_handler
            } }
        },
        None => {
            let jni_method = if call.call_type.is_static() {
                quote!(::ez_jni::utils::get_static_field)
            } else {
                quote!(::ez_jni::utils::get_obj_field)
            };
            // Convert jvalue returned from call
            let conversion = call.ty.convert_jvalue_to_rvalue(&quote!(__val), &call.mods.jni_error_handler);
            quote! { {
                #ops_prelude
                #[allow(noop_method_call)]
                #jni_method(#callee, #name, #ty_sig, env)
                    .and_then(|__val| Ok(#conversion))
                    #error_handler
            } }
        }
    }
}

/// Processes input for macro call [super::class!].
pub fn get_class(mods: MacroCallModifiers, class: Class) -> TokenStream {
    let ops_prelude = ops_prelude(mods.env);
    let class = class.to_jni_class_path();
    let error_handler = mods.jni_error_handler.handle_call_error();

    quote! { {
        #ops_prelude
        ::ez_jni::utils::get_class(#class, env)
            #error_handler
    } }
}

/// Processes input for macro call [super::singleton!].
pub fn singleton_instance(mods: MacroCallModifiers, class: Class) -> TokenStream {
    jni_call(MethodCall {
        mods,
        call_type: CallType::Static(ClassRepr::String(class.clone())),
        method_name: Ident::new("getInstance", Span::call_site()),
        parameters: Punctuated::new(),
        return_type: Return::Assertive(ReturnableType::Type(Type::Assertive(InnerType::Object(class)))),
    })
}

/// Define a JNI call to a Java method with parameters with expected types, and an expected return type.
///
/// See [`crate::call`] for an example.
pub struct MethodCall {
    pub mods: MacroCallModifiers,
    pub call_type: CallType,
    pub method_name: Ident,
    pub parameters: Punctuated<Parameter, Token![,]>,
    pub return_type: Return,
}
impl Parse for MethodCall {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // This parse implementation is complicated to allow parsing arbitrary Expressions as the callee.
        use proc_macro2::{TokenTree, Delimiter, Spacing};

        let mods = input.parse()?;
        let is_static = input.parse::<Token![static]>().is_ok();

        // Search for pattern `(...) ->`
        let result = crate::utils::step_until_each(input, [
            |token| matches!(token, TokenTree::Group(group) if group.delimiter() == Delimiter::Parenthesis),
            |token| matches!(token, TokenTree::Punct(punct) if punct.as_char() == '-' && punct.spacing() == Spacing::Joint),
            |token| matches!(token, TokenTree::Punct(punct) if punct.as_char() == '>' && punct.spacing() == Spacing::Alone),
        ])
        .map_err(|err| syn::Error::new(err.span(), format!("{err}; Expected pattern `(...) ->`")))?;

        let param_group = result.pattern_tokens[0].clone().group()?;
        let mut callee_tokens = result.pre_tokens;

        let method_ident = callee_tokens.pop();
        let method_dot = callee_tokens.pop();

        // Error used if either method_dot or method_ident were None.
        let none_err = syn::Error::new(param_group.span(), "Expected Dot and Ident `.methodName`");

        // Get the method name from the stepped tokens, ensureing there is also the Dot.
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
            mods,
            call_type: CallType::parse_callee(callee_tokens.into_iter().collect(), is_static)?,
            method_name,
            parameters: Parser::parse2(Punctuated::parse_terminated, param_group.stream())?,
            return_type: input.parse()?,
        })
    }
}

/// A wrapper for a [`Class`] value that will be used in a **JNI Call**.
pub enum ClassRepr {
    /// The class is represented by a [`ClassPath`][Class] of tokens.
    /// The *ClassPath* will have to be looked up to get a *Class Object*
    String(Class),
    /// The class is represented by a [`Rust Expr`][Expr] that resolves to a [class object][jni::objects::JClass].
    /// 
    /// The class object has already been looked up and can be operated on with JNI.
    Object(Expr),
}

/// Wether a JNI call is being done for a **static** or **object** member.
pub enum CallType {
    /// The call is for a `static` method.
    /// 
    /// ```ignore
    /// call!(static java.lang.String.methodName ...);
    /// ```
    Static(ClassRepr),
    /// The call is for a Method of an existing Object, stored in a variable.
    /// If the object is more than an Ident, it must be enclosed in `parenthesis` or `braces`.
    /// e.g. `object.methodName(...)` or `(something.object).methodName(...)`.
    Object(Expr),
}
impl CallType {
    pub fn is_static(&self) -> bool {
        match self {
            Self::Static(_) => true,
            Self::Object(_) => false,
        }
    }

    /// Parse [`Class`] or leave as Expression. Then create an instance of this struct.
    /// 
    /// This function parses *ALL* tokens in the [`TokenStream`] and returns `Error` if this is not the case.
    pub(self) fn parse_callee(callee: TokenStream, is_static: bool) -> syn::Result<Self> {
        // Parse speculatively without having to clone TokenStream
        Parser::parse2(|input: ParseStream| {
            let fork = input.fork();

            let result = fork.parse::<Class>()
                .and_then(|class| {
                    // Must have parsed all tokens, or it is considered error
                    if !fork.is_empty() {
                        Err(syn::Error::new(fork.cursor().token_stream().span(), "Unparsed tokens"))
                    } else {
                        input.advance_to(&fork);
                        drop(fork);

                        Ok(class)
                    }
                });

            Ok(match result {
                Ok(class) => if is_static {
                    Self::Static(ClassRepr::String(class))
                } else {
                    return Err(syn::Error::new(class.span(), "Can't call object method on Class. Try using 'static' at the beginning of the macro, or use the singleton! macro."))
                },
                // If Class could not be parsed, use verbatim TokenStream.
                Err(err) => if is_static {
                    Self::Static(ClassRepr::Object(input.parse()
                        .map_err(|_| err)?
                    ))
                } else {
                    Self::Object(input.parse()
                        .map_err(|_| err)?
                    )
                },
            })
        }, callee)
    }
}
impl Debug for CallType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Static(ClassRepr::String(class)) => f.debug_tuple("CallType::Static").field(class).finish(),
            Self::Static(ClassRepr::Object(_)) => write!(f, "CallType::Static"),
            Self::Object(_) => write!(f, "CallType::Object"),
        }
    }
}

/// A JNI call to *access* or *set* the value of a **field**.
pub struct FieldCall {
    pub mods: MacroCallModifiers,
    pub call_type: CallType,
    pub field_name: Ident,
    pub ty: Type,
    /// If the field is being set, this is the value it will be set to.
    pub set_val: Option<Expr>,
}
impl Parse for FieldCall {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        use proc_macro2::{TokenTree, Spacing};

        let mods = input.parse()?;
        let is_static = input.parse::<Token![static]>().is_ok();
        
        // Search for pattern `.#Ident :`
        let StepResult { pre_tokens, mut pattern_tokens } = crate::utils::step_until_each(input, [
            |token| matches!(token, TokenTree::Punct(punct) if punct.as_char() == '.' && punct.spacing() == Spacing::Alone),
            |token| matches!(token, TokenTree::Ident(_)),
            |token| matches!(token, TokenTree::Punct(punct) if punct.as_char() == ':' && punct.spacing() == Spacing::Alone),
        ])
        .map_err(|err| syn::Error::new(err.span(), format!("{err}; Expected pattern `.#Ident :`")))?;

        Ok(Self {
            mods,
            call_type: CallType::parse_callee(pre_tokens.into_iter().collect(), is_static)?,
            field_name: pattern_tokens.tail.remove(0).ident()?,
            ty: input.parse()?,
            set_val: {
                // If there is an equal sign, this means the field is being set
                if input.parse::<Token![=]>().is_ok() {
                    Some(input.parse()?)
                } else {
                    None
                }
            }
        })
    }
}
impl Debug for FieldCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FieldCall")
            .field("mods", &self.mods)
            .field("call_type", &self.call_type)
            .field("field_name", &self.field_name)
            .field("ty", &self.ty)
            .field("set_val", &self.set_val.as_ref().map(|expr| expr.to_token_stream().to_string()))
            .finish()
    }
}

/// Define a JNI call to a Java Class' constructor.
/// 
/// See [`crate::new`] for an example.
pub struct ConstructorCall {
    pub mods: MacroCallModifiers,
    pub class: ClassRepr,
    pub parameters: Punctuated<Parameter, Token![,]>,
    pub err_class: Option<Class>
}
impl Parse for ConstructorCall {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // This is similar to MethodCall::parse() in that it will collect tokens to parse as the class until it finds a certain pattern.
        use proc_macro2::{TokenTree, Delimiter};

        let mods = input.parse()?;

        // Search for pattern `(...)` | `(...) throws`
        let StepResult { pre_tokens, pattern_tokens } = crate::utils::step_until(input, |first, next| {
            // Check (...)
            if let TokenTree::Group(group) = first {
                if group.delimiter() == Delimiter::Parenthesis {
                    // Check it's the last token
                    if next.eof() {
                        // Found pattern `(...)`
                        return Some(next);
                    }
                    // OR is followed by `throws`
                    else if let Some((ident, next)) = next.ident() {
                        if ident == "throws" {
                            // Found pattern `(...) throws`
                            return Some(next)
                        }
                    }
                }
            }

            None
        })
        .map_err(|err| syn::Error::new(err.span(), format!("{err}; Expected pattern `(...)/EOF` or `(...) throws`")))?;

        let param_tokens = pattern_tokens[0].clone().group()?.stream();

        // Parse speculatively without having to clone TokenStream
        let class =  Parser::parse2(|input: ParseStream| {
            let fork = input.fork();

            fork.parse::<Class>()
                .inspect(move |_| input.advance_to(&fork))
                .map(|class| ClassRepr::String(class))
                .or_else(|err| {
                    input.parse::<Expr>()
                        .map(|tokens| ClassRepr::Object(tokens))
                        .map_err(|_| err)
                })
        }, pre_tokens.into_iter().collect())?;

        Ok(Self {
            mods,
            class,
            parameters: Parser::parse2(Punctuated::parse_terminated, param_tokens)?,
            err_class: (!input.is_empty())
                .then(|| {
                    let class = input.parse::<Class>()?;
                    class.is_throwable()?;
                    Ok::<_, syn::Error>(class)
                })
                .transpose()?
        })
    }
}

/// Parameter to a JNI method call, such as `java.lang.String(value)`.
/// Holds a [`Type`] and a [`Value`][ParamValue].
/// Can accept an **Array Literal** if the [`Type`] is array.
#[derive(Debug, Clone)]
pub struct Parameter {
    ty: Type,
    value: ParamValue,
}
impl Spanned for Parameter {
    fn span(&self) -> Span {
        join_spans([self.ty.span(), self.value.span()])
    }
}
impl SigType for Parameter {
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
                format!("Can't use 'Option' in call! or new! arguments. Instead, use '{NULL_KEYWORD}' as the value.")
            ))
        }

        // Check that the correct Value was passed in for the correct Type
        match &value {
            // null can be used for Object and Array only.
            ParamValue::Null(null) => match &ty {
                Type::Assertive(InnerType::Primitive(_))
                | Type::Option { ty: InnerType::Primitive(_), .. }
                    => return Err(syn::Error::new(null.span(), format!("Can't use '{NULL_KEYWORD}' as value of primitive argument type."))),
                Type::Assertive(InnerType::Object(_) | InnerType::Array(_))
                | Type::Option {  .. } => { },
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
                    Type::Assertive(InnerType::Object(class)) => class.rust_type() == ClassRustType::String,
                    Type::Assertive(InnerType::Array(_)) => true,
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

#[derive(Debug, Clone)]
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

/// The return type of a JNI method call.
///
/// The caller of the macro can assert that the JNI method call will result in one of the following:
/// 1. [`Type`] (or `void`) if the function being called can't return `NULL` or throw an `exception` (e.g. `bool` or `java.lang.String`).
/// 2. `Option<Type>` if the return type is an [`Object`][Type::Object] that could be **NULL**.
/// 3. `Result<void | Type | Option<Type>, Class>` if the method call can throw an **Exception**,
///    where the `Err` type is a **Java Class** that extends `java.lang.Throwable`.
pub enum Return {
    /// The method being called can't throw (will `panic!` if it does).
    Assertive(ReturnableType),
    /// Holds the Ok Type (a Type or Option), and the Err Type.
    Result {
        result: Ident,
        ty: ReturnableType,
        err_class: Class
    },
}
#[derive(Debug)]
pub enum ReturnableType {
    Void(Ident),
    Type(Type)
}
#[allow(unused)]
impl Return {
    /// Create a new [`Return`] Type that is **Void**.
    pub fn new_void(span: Span) -> Self {
        Self::Assertive(ReturnableType::Void(Ident::new("void", span)))
    }
    /// Create a new [`Return`] Type that holds a regular [`Type`].
    pub fn new_type(ty: Type) -> Self {
        Self::Assertive(ReturnableType::Type(ty))
    }
}
impl SigType for Return {
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
            Self::Result { result, ty, err_class } => join_spans([
                result.span(), inner_ty_span(ty), err_class.span()
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

                    let err_class = input.parse::<Class>()?;
                    err_class.is_throwable()?;

                    input.parse::<Token![>]>()?;
                    Self::Result { result: ident, ty, err_class }
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
            Self::Result { result, ty, err_class } => f.debug_struct("Return::Result")
                .field("result", result)
                .field("ty", ty)
                .field("err_class", &err_class.to_jni_class_path())
                .finish()
        }
    }
}

/// (Optional) Modifier tokens for [`crate::call!`] and its adjacent macros.
/// 
/// # Parsing
/// Parses the beginnning tokens of the [`crate::call`] macros to find an
/// *[`Explicit`][Env::Explicit] [`JNIEnv`][jni::JNIEnv] declaration* and/or a `?` ([JniCallErrorHandler]).
/// 
/// Takes all tokens until it finds the symbol `=>`.
/// All tokens before that symbol are the expression that resolves to the [`JNIEnv`][jni::JNIEnv].
/// 
/// Returns `Err` if the tokens could't be parsed into an [`Explicit`][Env::Explicit] [`Expression`][syn::Expr] or a `?`.
/// 
/// ## Example
/// ```ignore
/// call!(env, ?=> myfn() -> void)
/// ```
#[derive(Debug, Default)]
pub struct MacroCallModifiers {
    pub jni_error_handler: JniCallErrorHandler,
    pub env: Env,
}
impl Parse for MacroCallModifiers {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        use proc_macro2::{TokenTree, Spacing};

        // Check whether the macro caller is going to declare modifiers for this call.
        let result = crate::utils::step_until_each(input, [
            |token| matches!(token, TokenTree::Punct(punct) if punct.as_char() == '=' && punct.spacing() == Spacing::Joint),
            |token| matches!(token, TokenTree::Punct(punct) if punct.as_char() == '>' && punct.spacing() == Spacing::Alone),
        ]);

        Ok(match result {
            Ok(pattern) => {
                enum ModPossibilities {
                    ErrorHandler(JniCallErrorHandler),
                    Env(Env),
                }
                impl Parse for ModPossibilities {
                    fn parse(input: ParseStream) -> syn::Result<Self> {
                        if input.peek(Token![?]) {
                            Ok(Self::ErrorHandler(input.parse()?))
                        } else {
                            Ok(Self::Env(input.parse()?))
                        }
                    }
                }

                let tokens = pattern.pre_tokens
                    .into_iter()
                    .collect::<TokenStream>();
                let span = tokens.span();
                let mods = syn::parse::Parser::parse2(|input: ParseStream| {
                    Punctuated::<ModPossibilities, Token![,]>::parse_terminated(input)
                }, tokens)?;

                let mut jni_error_handler = Option::<JniCallErrorHandler>::None;
                let mut env = Option::<Env>::None;

                for m in mods {
                    match m {
                        ModPossibilities::ErrorHandler(error_handler) => match jni_error_handler {
                            Some(_) => return Err(syn::Error::new(span, "ErrorHandler modifier is set twice")),
                            None => jni_error_handler = Some(error_handler),
                        },
                        ModPossibilities::Env(expr) => match env {
                            Some(_) => return Err(syn::Error::new(span, "JNIEnv modifier is set twice")),
                            None => env = Some(expr),
                        },
                    }
                }

                Self {
                    jni_error_handler: jni_error_handler.unwrap_or_default(),
                    env: env.unwrap_or_default(),
                }
            },
            // Arrow pattern not found.
            Err(_) => Self::default(),
        })
    }
}

/// Handles the declaration of the [`JNIEnv`][jni::JNIEnv] used in the macros.
/// 
/// This can take the form of either a **local variable** declared within the *macro block*,
/// or an **argument** that is passed externally.
/// Regardless of the form, a *local* `env: &mut JNIEnv<'_>` MUST be *in scope* in all generated code by the macros.
/// 
/// Parses an [`Expr`] and returns [`Explicit`][Env::Explicit], otherwise returns [`None`][Env::None] (does not return `Err`).
#[derive(Default)]
pub enum Env {
    /// The `env` is evaluated from a custom [`Expression`][Expr] passed in by the macro user.
    /// The caller macro still needs to create a local `env: &mut JNIEnv<'_>` to store the reference.
    Explicit(Expr),
    /// The `env` is in scope because it is an **argument**,
    /// so the caller macro doesn't need to generate `env` local.
    /// 
    /// This is only used in [`jni_fn`][crate::jni_fn::jni_fn()] and [`FromObject` derive][crate::object].
    /// This CANNOT be used in the other (function-like) macros.
    Argument,
    /// No `env` available.
    /// The caller macro has to create its own `env` local by calling [`get_env()`][ez_jni::utils::get_env()].
    #[default] None
}
impl Parse for Env {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.parse::<Expr>()
            .map(Self::Explicit)
            .or(Ok(Self::None))
    }
}
impl ToTokens for Env {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append_all(match self {
            Self::Explicit(expr) => quote! { let env: &mut ::jni::JNIEnv<'_> = #expr; },
            Self::Argument => quote!(),
            Self::None => quote! { let env: &mut ::jni::JNIEnv<'_> = ::ez_jni::utils::get_env(); },
        })
    }
}
impl Debug for Env {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Explicit(expr) => f.write_str(&format!("Env::Explicit({})", expr.to_token_stream().to_string())),
            Self::Argument => f.write_str("Env::Argument"),
            Self::None => write!(f, "Env::None")
        }
    }
}

/// Handles errors of JNI calls, whether the macro should `panic!` on error ([`Unwrap`][Self::Unwrap]), or wether it should *let the caller handle it* ([`Manual`][Self::Manual]).
/// 
/// This should generate different tokens depending on which part of the macro output is handling the error.
/// For instance, for **value conversions** in [`types` module][crate::types] simply `unwrap()` the result or *propagate it* (with `?`, use [`Self::handle_value_conversion_error`]).
/// When used with the *top-level macros* (i.e. [`call`][MethodCall], [`field`][FieldCall], [`new`][ConstructorCall], and [`class`][get_class]),
/// however, the result can be `unwraped`, but the error can't be propagated because the macro doesn't know in what scope it is being called in,
/// so it lets the caller handle the [`Err`].
/// On top of that, the ez_jni functions for [`call`][MethodCall] and [`new`][ConstructorCall] can return an [`Err`] of either `MethodCallEror` or `JavaException`,
/// so those need completely different *error handling* and *value conversion* tokens.
#[derive(Default)]
pub enum JniCallErrorHandler {
    Manual(Token![?]),
    #[default] Unwrap,
}
impl JniCallErrorHandler {
    /// Handle the [`Result`] of a call to any ez_jni function/method for converting a value to/from Java/Rust.
    /// 
    /// Generates tokens for `unwraping` or propagating (`?`) error to the macro call error handler (see [`Self::handle_call_error`] and [`Self::convert_call_return_value`])
    /// 
    /// The resulting tokens from this function *must* be used as a **method chain**.
    pub fn handle_value_conversion_error(&self) -> TokenStream {
        match self {
            Self::Unwrap => quote! { .unwrap_jni() },
            Self::Manual(q) => q.to_token_stream(),
        }
    }

    /// Handle the [`Result`] of a *top-level macro call*.
    /// 
    /// Generates `unwraping` tokens, or nothing if the error should be handled by the caller.
    /// 
    /// The resulting tokens from this function *must* be used as a **chain**.
    fn handle_call_error(&self) -> TokenStream {
        match self {
            Self::Unwrap => quote! { .unwrap_jni() },
            Self::Manual(_) => quote! { },
        }
    }

    /// Handle the [`Result`]s of [`call`][MethodCall] and [`new`][ConstructorCall].
    /// Both error types `MethodCallError` and `JavaException` have to be handled in product form of the [`Return`] and [`JniCallError`] states.
    /// 
    /// The argument **`convert_jvalue`** indicates whether the jni call's [`Ok`] value is a [`JValue`][jni::objects::JValueOwned] that must be converted to a *Rust value*.
    /// Otherwise, pass `false` if the returned value should *not* be converted.
    /// 
    /// Generates `unwraping` tokens, or tries to combine the two error types.
    /// 
    /// The resulting tokens from this function *must* be used as a **chain**.
    fn convert_call_return_value(&self, return_type: &Return, convert_jvalue: bool) -> TokenStream {
        // Convert from the returned Java value to Rust value
        let val_token = |span: Span| quote_spanned! {span=> __val };
        let value_conversion = if convert_jvalue {
            match return_type {
                // Normal conversion
                Return::Assertive(ReturnableType::Type(ty))
                | Return::Result { ty: ReturnableType::Type(ty), .. } => ty.convert_jvalue_to_rvalue(&val_token(ty.span()), self),
                // Void conversion
                Return::Assertive(ReturnableType::Void(ident))
                | Return::Result { ty: ReturnableType::Void(ident), .. } => Type::convert_void_to_unit(&val_token(ident.span()), self)
            }
        } else {
            val_token(Span::call_site())
        };

        match (self, return_type) {
            // Panic if the call returned MethodCallError or a JavaException.
            (JniCallErrorHandler::Unwrap, Return::Assertive(_)) => {
                let value_conversion = if convert_jvalue {
                    quote! { .map(|__val| #value_conversion) }
                } else {
                    quote! { }
                };
                quote! {
                    .unwrap_jni()
                    #value_conversion
                    .unwrap_jni()
                }
            },
            // Panic if the call returned MethodCallError, but return the JavaException in a Result.
            (JniCallErrorHandler::Unwrap, Return::Result { err_class, .. }) => {
                let value_conversion = if convert_jvalue {
                    quote! { .map(|__val| #value_conversion) }
                } else {
                    quote! { }
                };
                let err_class = err_class.to_jni_class_path();
                quote! {
                    .and_then(|__result| {
                        if let Err(__exception) = &__result {
                            ::ez_jni::utils::check_object_class(__exception.as_ref(), #err_class, env)?;
                        }
                        Ok(__result)
                    })
                    .unwrap_jni()
                    #value_conversion
                }
            },
            // Combine the MethodCallError and JavaException into the same result with MethodCallError.
            (JniCallErrorHandler::Manual(_), Return::Assertive(_)) => {
                quote! {
                    .and_then(|__result| match __result {
                        // Can't use map because the #value_conversion errors are being propagated.
                        Ok(__val) => Ok(#value_conversion),
                        Err(__exception) => Err(::ez_jni::error::MethodCallError::UnhandledException(__exception)),
                    })
                }
            },
            // Return the MethodCallError or JavaException in different Results.
            (JniCallErrorHandler::Manual(_), Return::Result { err_class, .. }) => {
                let err_class = err_class.to_jni_class_path();
                quote! {
                    .and_then(|__result| match __result {
                        Ok(__val) => Ok(Ok(#value_conversion)),
                        // Can't use map because the #value_conversion errors are being propagated.
                        Err(__exception) => {
                            ::ez_jni::utils::check_object_class(__exception.as_ref(), #err_class, env)?;
                            Ok(Err(__exception))
                        },
                    })
                }
            },
        }
    }
}
impl Parse for JniCallErrorHandler {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(match input.parse::<Token![?]>() {
            Ok(q) => Self::Manual(q),
            Err(_) => Self::Unwrap,
        })
    }
}
impl Debug for JniCallErrorHandler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Manual(_) => f.write_str("JniCallErrorHandler::Manual"),
            Self::Unwrap => f.write_str("JniCallErrorHandler::Unwrap"),
        }
    }
}

/// Generates the argument array that will be passed to the JNI call.
/// 
/// Each [`Parameter`] generates a [`JValue`][jni::objects::JValue] (specifically the *borrowed* version) using [`Type::convert_rvalue_to_jvalue()`].
fn gen_arguments<'a>(params: impl Iterator<Item = &'a Parameter>) -> TokenStream {
    let params = params
        .map(|param| {
            match &param.value {
                ParamValue::Null(null) => quote_spanned! {null.span()=> ::jni::objects::JValue::Object(&::jni::objects::JObject::null()) },
                ParamValue::Value(value) => param.ty.convert_rvalue_to_jvalue(value),
            }
        });
    quote! { &[ #( #params ),* ] }
}
