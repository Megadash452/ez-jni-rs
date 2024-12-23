use std::fmt::Debug;
use either::Either;
use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned, ToTokens, TokenStreamExt};
use syn::{
    parenthesized, parse::{discouraged::Speculative, Parse, ParseStream, Parser}, punctuated::Punctuated, Expr, Ident, LitStr, Token
};
use utils::first_char_uppercase;
use crate::{
    types::{ArrayType, Class, InnerType, JavaPrimitive, RustPrimitive, SigType, SpecialCaseConversion, Type, NULL_KEYWORD},
    utils::{gen_signature, join_spans, Spanned, StepResult, TokenTreeExt as _}
};

/// Processes input for macro call [super::call!].
pub fn jni_call(call: MethodCall) -> TokenStream {
    // The class or object that the method is being called on.
    let callee = match call.call_type.callee() {
        Either::Left(class) => class.to_jni_class_path().to_token_stream(),
        Either::Right(expr) => quote!(&(#expr)),
    };
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
    call.return_type.convert_java_to_rust(quote! { {
        #param_vars
        let __callee = #callee;
        env.#jni_method(__callee, #name, #signature, #arguments)
    } })
}

/// Processes input for macro call [super::new!].
pub fn jni_call_constructor(call: ConstructorCall) -> TokenStream {
    // The Class/ClassObject that the constructor was called for
    let callee = match &call.class {
        ClassRepr::String(class) => class.to_jni_class_path().to_token_stream(),
        ClassRepr::Object(expr) => quote_spanned!(expr.span()=> (#expr).borrow()),
    };
    let signature = gen_signature(call.parameters.iter(), &Return::new_void(Span::call_site()));
    let param_vars = gen_arg_vars_defs(call.parameters.iter());
    let arguments = gen_arguments(call.parameters.iter());
    // Handle call result depending on whether it is expected to throw or not
    let error_handler = match call.err_type {
        // Move the result of the method call to a Result if the caller expects that the method could throw.
        Some(err_ty) => quote! {
            .map_err(|err| ::ez_jni::__throw::handle_exception_conversion::<#err_ty>(err, env.borrow_mut()))
        },
        // For return types that are not Result
        None => quote! {
            .unwrap_or_else(|err| ::ez_jni::__throw::handle_jni_call_error(err, env.borrow_mut()))
        },
    };

    quote! { {
        use ::std::borrow::BorrowMut as _;
        use ::std::borrow::Borrow as _;
        #param_vars
        #[allow(noop_method_call)]
        let __callee = #callee;
        env.new_object(__callee, #signature, #arguments)
            #error_handler
    } }
}

/// Processes input for macro call [super::field!].
pub fn field(call: FieldCall) -> TokenStream {
    // The class or object that the method is being called on.
    let callee = match &call.call_type {
        CallType::Static(ClassRepr::String(class)) => {
            let class = class.to_jni_class_path();
            quote!(::ez_jni::utils::ClassRepr::String(#class))
        },
        CallType::Static(ClassRepr::Object(class)) => quote!(::ez_jni::utils::ClassRepr::Object(&(#class))),
        CallType::Object(expr) => quote!(&(#expr)),
    };
    let name = call.field_name.to_string();
    let ty = call.ty.sig_type();
    let ty_char = call.ty.sig_char();
    let conversion = call.ty.convert_java_to_rust(&quote_spanned!(call.ty.span()=> v))
        .map(|conversion| quote! { .map(|v| #conversion) });

    // Build the macro function call
    match call.set_val {
        Some(val) => {
            let jni_method = if call.call_type.is_static() {
                quote!(::ez_jni::utils::set_static_field)
            } else {
                quote!(::ez_jni::utils::set_obj_field)
            };
            let arg = jvalue_variant(&call.ty, quote_spanned!(val.span()=> __param_0));
            // The value being set is given to the JNI call as an argument. It must also be stored in a local var.
            let param_var = gen_arg_vars_defs(std::iter::once(&Parameter {
                ty: call.ty,
                value: ParamValue::Value(val.into_token_stream())
            }));

            quote! { {
                use ::std::borrow::BorrowMut as _;
                #param_var
                #jni_method(#callee, #name, #ty, #arg, env.borrow_mut())
            } }
        },
        None => {
            let jni_method = if call.call_type.is_static() {
                quote!(::ez_jni::utils::get_static_field)
            } else {
                quote!(::ez_jni::utils::get_obj_field)
            };

            quote! { {
                use ::std::borrow::BorrowMut as _;
                #jni_method(#callee, #name, #ty, env.borrow_mut())
                    .#ty_char()
                    #conversion // Might be empty tokens
                    .unwrap_or_else(|err| ::ez_jni::__throw::handle_jni_call_error(err, env.borrow_mut()))
            } }
        }
    }
}

/// Processes input for macro call [super::class!].
pub fn get_class(class: Class) -> TokenStream {
    let class = class.to_jni_class_path();
    quote! { {
        use ::std::borrow::BorrowMut as _;
        env.find_class(#class)
            .unwrap_or_else(|err| ::ez_jni::__throw::handle_jni_call_error(err, env.borrow_mut()))
    } }
}

/// Processes input for macro call [super::singleton!].
pub fn singleton_instance(class: Class) -> TokenStream {
    jni_call(MethodCall {
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

    /// Returns the [`Class`] or **Object expression** that the *method* (static or not) is being called on.
    pub fn callee(&self) -> Either<&Class, &Expr> {
        match self {
            Self::Static(ClassRepr::String(class)) => Either::Left(class),
            Self::Static(ClassRepr::Object(obj))
            | Self::Object(obj) => Either::Right(obj),
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

/// A JNI call to *access* or *set* the value fo a **field**.
pub struct FieldCall {
    pub call_type: CallType,
    pub field_name: Ident,
    pub ty: Type,
    /// If the field is being set, this is the value it will be set to.
    pub set_val: Option<Expr>,
}
impl Parse for FieldCall {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        use proc_macro2::{TokenTree, Spacing};

        let is_static = input.parse::<Token![static]>().is_ok();
        
        // Search for pattern `.#Ident :`
        let StepResult { pre_tokens, mut pattern_tokens } = crate::utils::step_until_each(input, [
            |token| matches!(token, TokenTree::Punct(punct) if punct.as_char() == '.' && punct.spacing() == Spacing::Alone),
            |token| matches!(token, TokenTree::Ident(_)),
            |token| matches!(token, TokenTree::Punct(punct) if punct.as_char() == ':' && punct.spacing() == Spacing::Alone),
        ])
        .map_err(|err| syn::Error::new(err.span(), format!("{err}; Expected pattern `.#Ident :`")))?;

        Ok(Self {
            call_type: CallType::parse_callee(pre_tokens.into_iter().collect(), is_static)?,
            field_name: pattern_tokens.remove(1).ident()?,
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

/// Define a JNI call to a Java Class' constructor.
/// 
/// See [`crate::new`] for an example.
pub struct ConstructorCall {
    pub class: ClassRepr,
    pub parameters: Punctuated<Parameter, Token![,]>,
    pub err_type: Option<syn::Path>
}
impl Parse for ConstructorCall {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // This is similar to MethodCall::parse() in that it will collect tokens to parse as the class until it finds a certain pattern.
        use proc_macro2::{TokenTree, Delimiter};

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
            class,
            parameters: Parser::parse2(Punctuated::parse_terminated, param_tokens)?,
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
    /// Converts to value of the JNI Call to a *Rust value*.
    /// 
    /// Also checks if the Call resulted in an *exception*.
    /// `panic!s` if there was an exception and the return type was not [`Result`].
    /// Otherwise, wraps the value in a [`Result`].
    /// 
    /// See also [SpecialCaseConversion::convert_java_to_rust()] for API doc.
    pub fn convert_java_to_rust(&self, jni_call: TokenStream) -> TokenStream {
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
            | Self::Result { ty: ReturnableType::Type(ty), .. } => ty.convert_java_to_rust(&quote_spanned!(ty.span()=> v))
                .map(|conversion| quote! { .map(|v| #conversion) }),
        };

        let sig_char = self.sig_char();

        // Handle call result depending on whether it is expected to throw or not
        match self {
            // For return types that are not Result
            Self::Assertive(_) => quote! { {
                use ::std::borrow::BorrowMut as _;
                #jni_call
                    .unwrap_or_else(|err| ::ez_jni::__throw::handle_jni_call_error(err, env.borrow_mut()))
                    .#sig_char()
                    #conversion // Might be empty tokens
                    .unwrap_or_else(|err| ::ez_jni::__throw::handle_jni_call_error(err, env.borrow_mut()))
            } },
            // Move the result of the method call to a Result if the caller expects that the method could throw.
            Self::Result { err_ty, ..} =>  quote! { {
                use ::std::borrow::BorrowMut as _;
                #jni_call
                    .map_err(|err| ::ez_jni::__throw::handle_exception_conversion::<#err_ty>(err, env.borrow_mut()))
                    .map(|v| v.#sig_char().unwrap_or_else(|err| ::ez_jni::__throw::handle_jni_call_error(err, env.borrow_mut())))
                    #conversion // Might be empty tokens
            } },
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
            jvalue_variant(&param.ty, Ident::new(&format!("__param_{i}"), param.span()).into_token_stream())
        });
    quote! { &[ #( #params ),* ] }
}

/// Conver the parameter to a `Jvalue` enum variant that will be used in the JNI call parameter list.
/// The variant will have one of the parameter variables as the inner value.
pub fn jvalue_variant(ty: &Type, val: TokenStream) -> TokenStream {
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
    
    let (ty_span, ty_variant) = match ty {
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
    tt.append_all(quote_spanned! {val.span()=> (#val) });
    tt
}
