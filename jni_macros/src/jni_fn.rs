use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned, ToTokens, TokenStreamExt};
use syn::{braced, ext::IdentExt as _, parenthesized, parse::{Parse, ParseStream}, punctuated::Punctuated, token::{Brace, Paren}, Attribute, GenericParam, Generics, Ident, Lifetime, Token};
use utils::java_method_to_symbol;
use crate::{
    call::{Env, JniCallErrorHandler},
    types::{Class, ClassRustType, Conversion as _, InnerType, Primitive, Return, ReturnType, Type},
    utils::{Spanned, gen_signature, join_spans, merge_errors, ops_prelude, take_class_attribute_required}
};

/// Processes the input for [`crate::jni_fns`].
/// Converts the parsed [`JniFn`] to a regular function used in Rust.
pub fn jni_fn(input: ParseStream) -> syn::Result<Vec<TokenStream>> {
    let mut inputs = Vec::new();
    // A (optional) class applied to all the jni_fn in this block
    let class = input.parse::<Class>().ok();
    if class.is_some() {
        input.parse::<Token![=>]>()?;
    }

    // Parse multiple jni_fn
    while !input.is_empty() {
        inputs.push(input.parse::<JniFn>()?.into_token_stream(class.as_ref())?);
    }

    // Convert all JniFn to ItemFn 
    Ok(inputs)
}

// TODO: Allow generics in the arguments and return type if they are a Java Class

/// A rust function that uses Java types (or some Rust types) and is called by Java Code.
/// 
/// A [`JniFn`] is one that MUST be exported by the user library.
pub struct JniFn {
    pub attrs: Vec<Attribute>,
    pub pub_token: Token![pub],
    pub static_token: Option<Token![static]>,
    pub fn_token: Token![fn],
    pub name: Ident,
    pub lt_token: Token![<],
    pub lifetime: Lifetime,
    pub gt_token: Token![>],
    pub paren_token: Paren,
    pub inputs: Punctuated<JniFnArg, Token![,]>,
    // pub variadic: Option<Variadic>, Should this be allowed?
    pub output: Return,
    pub brace_token: Brace,
    pub content: TokenStream,
}
impl JniFn {
    const LIFETIME_NAME: &str = "local";
    const USER_FUNCTION_NAME: &str = "f";

    /// Fulfills the same role as [`ToTokens::to_tokens`],
    /// but [`JniFn`] mut have the [`Class`] that it belongs to either in the [`Attribute`]s,
    /// or provided as the argument to this function.
    pub fn into_token_stream(mut self, class: Option<&Class>) -> syn::Result<TokenStream> {
        let mut tt = TokenStream::new();
        let tokens = &mut tt;

        // The real name of the function as it will be used by Java
        let name = {
            let result = take_class_attribute_required(&mut self.attrs, self.name.span());
            // Get the class either from the Attributes or the argument
            let class = match result.as_ref() {
                Ok(c) => match class {
                    // class argument should be None if there is a class Attribute
                    Some(_) => return Err(syn::Error::new(c.span(), "Do not give functions a `class` attribute if the Class was provided with the macro invocation.")),
                    None => &c,
                },
                Err(err) => class.ok_or(err.clone())?,
            };
    
            Ident::new(&java_method_to_symbol(&class.to_string(), &self.name.to_string()), self.name.span())
        };

        // Build a java method signature, something like (Ljava.lang.String;)I
        let method_sig = gen_signature(self.inputs.iter().map(|i| &i.ty), &self.output).value();

        let pub_token = &self.pub_token;
        let fn_token = &self.fn_token;
        let lt_token = &self.lt_token;
        let lifetime = &self.lifetime;
        let gt_token = &self.gt_token;

        tokens.append_all(&self.attrs);
        tokens.append_all(quote! {
            #[doc = ""]
            #[doc = #method_sig]
            #[unsafe(no_mangle)]
            #pub_token extern "system" #fn_token #name #lt_token #lifetime #gt_token
        });
        // Parameters
        self.paren_token.surround(tokens, |tokens| {
            let receiver = self.receiver_param();
            tokens.append_all(quote_spanned!(self.paren_token.span.span()=>
                env: ::jni::JNIEnv<'local>, #receiver,
            ));
            self.inputs.to_tokens(tokens);
        });
        self.output.to_jni_fn_tokens(tokens);
        self.brace_token.surround(tokens, |tokens| {
            tokens.append_all(self.user_function());
            let ops_prelude = ops_prelude(Env::Argument);
            // Create variables with the converted value, where the variable name shadows the argument of the jni_fn
            let args_conversions = self.inputs.iter()
                .map(|arg| arg.create_rust_val())
                .collect::<TokenStream>();
            // Generates teh token that call the inner user function. The return value of this called is converted in the next local variable.
            let user_fn_call = {
                let user_function_ident = Ident::new(Self::USER_FUNCTION_NAME, join_spans([self.name.span(), self.inputs.span()]));

                let receiver = match &self.static_token {
                    Some(ident) => Ident::new("class", ident.span),
                    None => Ident::new("this", self.name.span()),
                };

                // Create list of argument names to pass to the user_function (the arguments are already converted to the Rust types at this point).
                let arg_names = self.inputs.pairs()
                    .map(|pair| {
                        let (arg, punct) = pair.into_tuple();
                        syn::punctuated::Pair::new(&arg.name, punct)
                    })
                    .collect::<Punctuated<_, Token![,]>>();

                quote! { #user_function_ident(#receiver, #arg_names) }
            };
            let call_and_return_conversion = self.output.convert_rust_to_java_sys(user_fn_call);

            // Use the _map version of run_with_jnienv() if the return type needs conversion to Java
            tokens.append_all(quote_spanned! {self.content.span()=>
                unsafe { ::ez_jni::__throw::run_with_jnienv(env, #[allow(noop_method_call)] move | #[allow(unused_variables)] env | {
                    #ops_prelude
                    #args_conversions
                    #call_and_return_conversion
                }) }
            });
        });

        Ok(tt)
    }

    /// Generates the **function** that executes the code that the *user provided* to the macro.
    /// 
    /// The generated function will take the *same parameters* and the *same return type* that the user provided.
    fn user_function(&self) -> TokenStream {
        let Self { fn_token, name, lt_token, lifetime, gt_token, paren_token, inputs, output, brace_token, content, .. } = self;

        let name = Ident::new(Self::USER_FUNCTION_NAME, name.span());
        
        let mut parameters = TokenStream::new();
        paren_token.surround(&mut parameters, |tokens| {
            // Add receiver to parameters
            tokens.append_all(self.receiver_param());
            tokens.append(proc_macro2::TokenTree::Punct(proc_macro2::Punct::new(',', proc_macro2::Spacing::Alone)));
            
            for (param, comma) in inputs.pairs().map(|pair| pair.into_tuple()) {
                tokens.append_all(&param.attrs);
                param.mutability.to_tokens(tokens);
                param.name.to_tokens(tokens);
                param.colon_token.to_tokens(tokens);
                tokens.append_all(param.ty.type_tokens(false, false, Some(lifetime.clone())));
                comma.to_tokens(tokens);
            }
        });

        let rust_type_tokens = |ty: &ReturnType| -> TokenStream {
            match ty {
                ReturnType::JavaVoid(ident) => ident.to_token_stream(),
                ReturnType::RustUnit(paren) => {
                    let mut tokens = TokenStream::new();
                    paren.surround(&mut tokens, |_| {});
                    tokens
                },
                ReturnType::Type(ty) => ty.type_tokens(false, false, Some(lifetime.clone())),
            }
        };
        let output = match output {
            Return::Empty => quote!(),
            Return::Assertive { arrow, ty } => {
                let rust_ty = rust_type_tokens(ty);
                quote! { #arrow #rust_ty }
            },
            Return::Result { arrow, result, ty, err_class } => {
                let rust_ty = rust_type_tokens(ty);
                let err_type = Class::from_rust_type(ClassRustType::JThrowable, err_class.span())
                    .type_tokens(false, false, Some(lifetime.clone()));
                quote! { #arrow #result<#rust_ty, #err_type> }
            }
        };

        let mut body = TokenStream::new();
        brace_token.surround(&mut body, |tokens| content.to_tokens(tokens));
        quote! { #fn_token #name #lt_token #lifetime #gt_token #parameters #output #body }
    }

    /// Generates a **parameter** that is used as the receiver of the function.
    /// This is the same funcitonality as `self` but with Java names.
    /// 
    /// *Static* methods receive [`Class`][jni::objects::JClass], and *object* methods receive [`Object`][jni::objects::JObject].
    fn receiver_param(&self) -> TokenStream {
        let param = match &self.static_token {
            Some(static_token) => quote_spanned! {static_token.span()=> class: ::jni::objects::JClass<'local> },
            None => quote! { this: ::jni::objects::JObject<'local> }
        };
        quote! { #[allow(unused_variables)] #param }
    }
}
impl Parse for JniFn {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // -- Parse input function. Collect multiple errors
        let mut errors = Vec::new();

        let attrs = input.call(Attribute::parse_outer)
            .map_err(|err| errors.push(err))
            .ok();

        // Parse `pub`
        let pub_token = input.parse::<Token![pub]>()
            .map_err(|err| errors.push(
                syn::Error::new(err.span(), format!("{err}; jn_fn must have `pub` because they must be exported by the library"))
            ))
            .ok();
        // Parse `static`
        let static_token = input.parse::<Token![static]>()
            .ok();
        // Parse `fn`
        let fn_token = input.parse::<Token![fn]>()
            .map_err(|err| errors.push(err))
            .ok();

        // Parse function name
        let name = input.parse::<Ident>()
            .map_err(|err| errors.push(syn::Error::new(err.span(), format!("{err}; Expected function name"))))
            .ok();

        // Parse all generics, but will only accept one lifetime
        let generics = input.parse::<Generics>()
            .map_err(|err| errors.push(err))
            .ok()
            .and_then(|generics| {
                let lt_token = match generics.lt_token {
                    Some(lt_token) => Some(lt_token),
                    None => {
                        errors.push(syn::Error::new(Span::call_site(), format!("Must have a generic lifetime")));
                        None
                    },
                }?;
                Some((lt_token, generics.params, generics.gt_token?))
            });

        // Parse arguments with Java types
        let (paren_token, inputs) = {
            let inner;
            (
                parenthesized!(inner in input),
                Punctuated::<JniFnArg, Token![,]>::parse_terminated(&inner)
                    .map_err(|err| errors.push(err))
                    .ok()
            )
        };

        // Parse return arrow `->` and return type (is void if there is none)
        let output = input.parse::<Return>()
            .map_err(|err| errors.push(err))
            .ok();

        // Parse the content of the function inside the braces `{ ... }`
        let (brace_token, content) = {
            let inner;
            (braced!(inner in input), inner.parse()?)
        };

        merge_errors(errors)?;
        // If there were any errors, the function returned on the line above, so reaching here means all tokens were parsed successfully.
        let attrs = attrs.unwrap();
        let pub_token = pub_token.unwrap();
        let fn_token = fn_token.unwrap();
        let name = name.unwrap();
        let (lt_token, generics, gt_token) = generics.unwrap();
        let inputs = inputs.unwrap();
        let output = output.unwrap();

        // -- Perform checks on the successfully parsed function
        errors = Vec::new();
        let lifetime_error_msg = format!("jni_fn must have one and only one lifetime, named \"{}\"", Self::LIFETIME_NAME);
        let mut iter = generics.into_iter();

        // Parsed all generics, but will only accept one lifetime, and no other generic types
        let lifetime = match iter.next() {
            Some(GenericParam::Lifetime(lifetime)) =>
                if lifetime.lifetime.ident.to_string() != Self::LIFETIME_NAME {
                    errors.push(syn::Error::new(lifetime.span(), &lifetime_error_msg));
                    None
                } else {
                    Some(lifetime)
                },
            Some(generic) => {
                errors.push(syn::Error::new(generic.span(), "jni_fn can't have generic constants or types"));
                None
            },
            None => {
                errors.push(syn::Error::new(name.span(), &lifetime_error_msg));
                None
            },
        };
        // Only allow 1 lifetime
        if let Some(lifetime) = iter.next() {
            errors.push(syn::Error::new(lifetime.span(), &lifetime_error_msg))
        }

        // Check that the function doesn't have arguments with these names because those are implicitly added
        for arg_name in ["env", "class", "this"] {
            if let Some(arg) = inputs.iter().find(|arg| arg.name == arg_name) {
                errors.push(syn::Error::new(
                    arg.span(),
                    format!("Function can't have an argument named \"{arg_name}\"; this is added implicitly")
                ))
            }
        }

        merge_errors(errors)?;
        let lifetime = lifetime.unwrap();

        Ok(Self { attrs, pub_token, static_token, fn_token, name, lt_token, lifetime: lifetime.lifetime, gt_token, paren_token, inputs, output, brace_token, content })
    }
}

pub struct JniFnArg {
    pub attrs: Vec<Attribute>,
    pub mutability: Option<Token![mut]>,
    pub name: Ident,
    pub colon_token: Token![:],
    pub ty: Type
}
impl JniFnArg {
    /// Outputs a statement (*local variable definition*) that converts the argument to a Rust value.
    /// 
    /// The argument is shadowed by the local variable with the same name.
    fn create_rust_val(&self) -> TokenStream {
        let attrs = &self.attrs;
        let mutability = &self.mutability;
        let name = &self.name;
        // Skip parameter when name is ellided
        if name.to_string() == "_" {
            return TokenStream::new()
        }
        // Variants of JObject are used as the native function parameter types (only at top-level of the type),
        // so conversion (e.g. JString) is not necessary.
        if matches!(&self.ty, Type::Assertive(InnerType::Object(class)) if class.is_object_ref()) {
            return TokenStream::new();
        }
        self.ty.convert_java_to_rust(&name.to_token_stream(), &JniCallErrorHandler::Unwrap)
            .map(|conversion| quote! { #(#attrs)* let #mutability #name = #conversion; })
            // Output nothing if there is no conversion to be done.
            .unwrap_or_default()
    }
}
impl Parse for JniFnArg {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            attrs: Attribute::parse_outer(input)?,
            mutability: input.parse()?,
            name: input.call(Ident::parse_any)?,
            colon_token: input.parse()?,
            ty: input.parse()?
        })
    }
}
impl ToTokens for JniFnArg {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append_all(&self.attrs);
        // self.mutability.to_tokens(tokens); // Ignore mutability in the jni_fn. It is used in the user_function
        self.name.to_tokens(tokens);
        self.colon_token.to_tokens(tokens);
        
        // Convert the `Type` to a `jni::objects` type (with 'local lifetime).
        // These are the argument types that will be used in the Java native method.
        tokens.append_all(match &self.ty {
            Type::Assertive(InnerType::Primitive(prim)) => {
                let j_type = Ident::new(&format!("j{}", prim.java_prim()), prim.span());
                quote_spanned! {prim.span()=> ::jni::sys::#j_type}
            },
            Type::Assertive(InnerType::Object(class))
            | Type::Option { ty: InnerType::Object(class), .. } => match class.rust_type() {
                ClassRustType::Primitive(_)
                | ClassRustType::JObject  => quote_spanned! {self.ty.span()=> ::jni::objects::JObject<'local> },
                ClassRustType::JClass     => quote_spanned! {self.ty.span()=> ::jni::objects::JClass<'local> },
                ClassRustType::JThrowable => quote_spanned! {self.ty.span()=> ::jni::objects::JThrowable<'local> },
                ClassRustType::JString
                | ClassRustType::String   => quote_spanned! {self.ty.span()=> ::jni::objects::JString<'local> },
            },
            Type::Assertive(InnerType::Array(_))
            | Type::Option { .. } => quote_spanned! {self.ty.span()=> ::jni::objects::JObject<'local> },
        })
    }
}

impl Return {
    /// Generates the code that converts the value returned by the *main body* (inner function) of the jni_fn to one of the [`jni::sys`] types.
    /// 
    /// See [`Type::convert_rust_to_java()`].
    /// Returns the *unmodified* passed **`value`** in if no conversion is necessary.
    /// 
    /// This should only be used with the [`jni_fn`] macro.
    fn convert_rust_to_java_sys(&self, value: TokenStream) -> TokenStream {
        fn ok_conversion(ty: &ReturnType, val: &TokenStream) -> Option<TokenStream> {
            match ty {
                ReturnType::JavaVoid(_) | ReturnType::RustUnit(_) => None,
                ReturnType::Type(ty) => match ty {
                    Type::Assertive(InnerType::Primitive(prim)) => prim.convert_rust_to_java(val),
                    // Always convert Object to jni::sys::jobject
                    Type::Assertive(InnerType::Object(_) | InnerType::Array(_))
                    | Type::Option { .. } => Some(match ty.convert_rust_to_java(val) {
                        Some(conversion) => quote_spanned!(conversion.span()=> #conversion.as_raw()),
                        None => quote_spanned!(val.span()=> #val.as_raw())
                    }),
                },
            }
        }

        match self {
            Self::Empty
            | Self::Assertive { ty: ReturnType::JavaVoid(_) | ReturnType::RustUnit(_), .. } => value,
            Self::Assertive { ty, .. } => ok_conversion(ty, &value).unwrap_or(value),
            Self::Result { ty, err_class, .. } => {
                let exception_class = err_class.to_jni_class_path();

                let val_token = quote_spanned! {ty.span()=> __val };
                let exception_token = quote_spanned! {exception_class.span()=> __exception };
                let ok_conversion = match ok_conversion(ty, &val_token) {
                    Some(conversion) => quote_spanned! {self.span()=> .map(|#val_token| #conversion) },
                    None => quote! { },
                };

                quote_spanned! {self.span()=>
                    #value
                        #ok_conversion
                        .unwrap_or_else(|#exception_token| {
                            ::ez_jni::utils::check_object_class(&#exception_token, #exception_class, env).unwrap_jni();
                            ::ez_jni::__throw::panic_throwable(&#exception_token)
                        })
                }
            },
        }
    }

    /// Appends to the [`TokenStream`] the correct type in [`jni::sys`] that corresponds to the [`ReturnType`].
    /// 
    /// Fulfills the purpose of [`ToTokens`] for [`Return`].
    /// This should only be used with the [`jni_fn`] macro.
    fn to_jni_fn_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Empty
            | Self::Assertive { ty: ReturnType::JavaVoid(_) | ReturnType::RustUnit(_), .. }
            | Self::Result { ty: ReturnType::JavaVoid(_) | ReturnType::RustUnit(_), .. } => { },
            Self::Assertive { arrow, ty: ReturnType::Type(output) }
            | Self::Result { arrow, ty: ReturnType::Type(output), .. } => {
                arrow.to_tokens(tokens);

                /// Convert the `Type` to a `jni::sys` type.
                /// Used for *raw primitives* and *raw primitive arrays*.
                fn primitive(prim: &Primitive, is_array: bool) -> TokenStream {
                    let j_type = Ident::new(&format!("j{}{}", prim.java_prim(),  if is_array { "Array" } else { "" }), prim.span());
                    quote_spanned! {prim.span()=> ::jni::sys::#j_type}
                }
                tokens.append_all(match output {
                    Type::Assertive(InnerType::Primitive(prim)) => primitive(prim, false),
                    Type::Assertive(InnerType::Object(class))
                    | Type::Option { ty: InnerType::Object(class), .. } => match class.rust_type() {
                        ClassRustType::Primitive(_)
                        | ClassRustType::JObject  => quote_spanned! {output.span()=> ::jni::sys::jobject },
                        ClassRustType::JClass     => quote_spanned! {output.span()=> ::jni::sys::jclass },
                        ClassRustType::JThrowable => quote_spanned! {output.span()=> ::jni::sys::jthrowable },
                        ClassRustType::JString
                        | ClassRustType::String   => quote_spanned! {output.span()=> ::jni::sys::jstring },
                    },
                    Type::Option { .. } => quote_spanned! {output.span()=> ::jni::sys::jobject },
                    Type::Assertive(InnerType::Array(array)) => match &*array.ty {
                        Type::Assertive(InnerType::Primitive(prim)) => primitive(prim, true),
                        Type::Assertive(InnerType::Object(_) | InnerType::Array(_))
                        | Type::Option { .. } => quote_spanned! {output.span()=> ::jni::sys::jobjectArray },
                    },
                });
            },
        }
    }
}
