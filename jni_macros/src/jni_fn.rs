use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned, ToTokens, TokenStreamExt};
use syn::{braced, ext::IdentExt as _, parenthesized, parse::{Parse, ParseStream}, punctuated::Punctuated, token::{Brace, Paren}, Attribute, GenericParam, Generics, Ident, Lifetime, LitStr, Token};
use utils::java_method_to_symbol;
use crate::{types::{Class, ClassRustType, Conversion as _, InnerType, JavaPrimitive, RustPrimitive, SigType, Type}, utils::{gen_signature, merge_errors, take_class_attribute_required, Spanned}};

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
    pub output: JniReturn,
    pub brace_token: Brace,
    pub content: TokenStream,
}
impl JniFn {
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
            let receiver = self.receiver();
            tokens.append_all(quote_spanned!(self.paren_token.span.span()=>
                env: ::jni::JNIEnv<'local>, #receiver,
            ));
            self.inputs.to_tokens(tokens);
        });
        self.output.to_tokens(tokens);
        self.brace_token.surround(tokens, |tokens| {
            tokens.append_all(self.user_function());
            let run = self.convert_arguments();

            // Use the _map version of run_with_jnienv() if the return type needs conversion to Java
            tokens.append_all(match self.output.convert_rust_to_java_sys(&quote_spanned!(self.output.span()=> r)) {
                Some(conversion) => quote_spanned! {self.content.span()=>
                    ::ez_jni::__throw::run_with_jnienv_map(env, #run, |r, #[allow(unused_variables)] env| #conversion)
                },
                None => quote_spanned! {self.content.span()=>
                    ::ez_jni::__throw::run_with_jnienv(env, #run)
                }
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
            tokens.append_all(self.receiver());
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

        let output = match output {
            JniReturn::Void => quote!(),
            JniReturn::Type { arrow_token, ty } => {
                let rust_ty = ty.type_tokens(false, false, Some(lifetime.clone()));
                quote! { #arrow_token #rust_ty }
            }
        };

        let mut body = TokenStream::new();
        brace_token.surround(&mut body, |tokens| content.to_tokens(tokens));
        quote! { #fn_token #name #lt_token #lifetime #gt_token #parameters #output #body }
    }

    /// Generates the **closure** that converts the *Java arguments* passed to the jni_fn to *Rust values* to be passed to the [`user_function`][Self::user_function()].
    /// 
    /// The generated closure is also what calls the [`user_function`][Self::user_function()] at the end and passes the arguments to it.
    fn convert_arguments<'a>(&self) -> TokenStream {
        let user_function_ident = Ident::new(Self::USER_FUNCTION_NAME, Span::call_site());

        let receiver = Ident::new(match &self.static_token {
            Some(_) => "class",
            None => "this"
        }, Span::call_site());

        // Create list of argument names to pass to the user_function
        let arg_names = self.inputs.pairs()
            .map(|pair| {
                let (arg, punct) = pair.into_tuple();
                syn::punctuated::Pair::new(&arg.name, punct)
            })
            .collect::<Punctuated<_, Token![,]>>();
        
        // Create variables with the converted value, where the variable name shadows the argument of the jni_fn
        let conversions = self.inputs.iter()
            .map(|arg| arg.create_rust_val())
            .collect::<TokenStream>();

        quote! {
            /* #[allow(noop_method_call)] */ move | #[allow(unused_variables)] env | {
                #[allow(unused_imports)] use ::std::borrow::Borrow;
                #[allow(unused_imports)] use ::ez_jni::utils::ResultExt as _;
                // #[allow(unused_variables)]
                // let env: &mut ::jni::JNIEnv = #env;
                #conversions
                #user_function_ident(#receiver, #arg_names)
            }
        }
    }

    /// Generates a **parameter** that is used as the receiver of the function.
    /// This is the same funcitonality as `self` but with Java names.
    /// 
    /// *Static* methods receive [`Class`][jni::objects::JClass], and *object* methods receive [`Object`][jni::objects::JObject].
    fn receiver(&self) -> TokenStream {
        let param = match &self.static_token {
            Some(static_token) => quote_spanned! {static_token.span()=> class: ::jni::objects::JClass<'local> },
            None => quote! { this: ::jni::objects::JObject<'local> }
        };
        quote! { #[allow(unused_variables)] #param }
    }
}
impl Parse for JniFn {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        static LIFETIME_ERROR: &str = "jni_fn must have one and only one lifetime, named \"local\"";

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
        let output = input.parse::<JniReturn>()
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

        let mut iter = generics.into_iter();

        // Parsed all generics, but will only accept one lifetime, and no other generic types
        let lifetime = match iter.next() {
            Some(GenericParam::Lifetime(lifetime)) =>
                if lifetime.lifetime.ident.to_string() != "local" {
                    errors.push(syn::Error::new(lifetime.span(), LIFETIME_ERROR));
                    None
                } else {
                    Some(lifetime)
                },
            Some(generic) => {
                errors.push(syn::Error::new(generic.span(), "jni_fn can't have generic constants or types"));
                None
            },
            None => {
                errors.push(syn::Error::new(name.span(), LIFETIME_ERROR));
                None
            },
        };
        // Only allow 1 lifetime
        if let Some(lifetime) = iter.next() {
            errors.push(syn::Error::new(lifetime.span(), LIFETIME_ERROR))
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
        // Output nothing if parameter name is ellided
        if name.to_string() == "_" {
            return TokenStream::new()
        }
        self.ty.convert_java_to_rust(&quote!(#name))
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
        fn primitive(j_prim: JavaPrimitive, span: Span) -> TokenStream {
            let j_type = Ident::new(&format!("j{j_prim}"), span);
            quote_spanned! {span=> ::jni::sys::#j_type}
        }
        tokens.append_all(match &self.ty {
            Type::Assertive(InnerType::RustPrimitive { ident, ty }) => primitive(JavaPrimitive::from(*ty), ident.span()),
            Type::Assertive(InnerType::JavaPrimitive { ident, ty }) => primitive(*ty, ident.span()),
            Type::Assertive(InnerType::Object(class))
            | Type::Option { ty: InnerType::Object(class), .. } => match class.rust_type() {
                ClassRustType::JObject    => quote_spanned! {self.ty.span()=> ::jni::objects::JObject<'local> },
                ClassRustType::JClass     => quote_spanned! {self.ty.span()=> ::jni::objects::JClass<'local> },
                ClassRustType::JThrowable => quote_spanned! {self.ty.span()=> ::jni::objects::JThrowable<'local> },
                ClassRustType::String | ClassRustType::JString => quote_spanned! {self.ty.span()=> ::jni::objects::JString<'local> },
            },
            Type::Assertive(InnerType::Array(_))
            | Type::Option { .. } => quote_spanned! {self.ty.span()=> ::jni::objects::JObject<'local> },
        })
    }
}

pub enum JniReturn {
    Void,
    Type {
        arrow_token: Token![->],
        ty: Type
    }
}
impl JniReturn {
    /// Convert the value returned by the *main body* of the jni_fn to one of the [`jni::sys`] types.
    /// 
    /// See [`Type::convert_rust_to_java()`].
    /// Returns [`None`] if no conversion is necessary.
    pub fn convert_rust_to_java_sys(&self, value: &TokenStream) -> Option<TokenStream> {
        match self {
            Self::Void => None,
            Self::Type { ty, .. } => match ty {
                Type::Assertive(InnerType::RustPrimitive { ty: prim, .. }) => prim.convert_rust_to_java(value),
                Type::Assertive(InnerType::JavaPrimitive { ty: prim, .. }) => RustPrimitive::from(*prim).convert_rust_to_java(value),
                // Always convert Object to jni::sys::jobject
                Type::Assertive(InnerType::Object(_) | InnerType::Array(_))
                | Type::Option { .. } => Some(match ty.convert_rust_to_java(value) {
                    Some(conversion) => quote_spanned!(conversion.span()=> #conversion.as_raw()),
                    None => quote_spanned!(value.span()=> #value.as_raw())
                })
            },
        }
    }
}
impl SigType for JniReturn {
    fn sig_char(&self) -> Ident {
        match self {
            Self::Void => Ident::new("v", Span::call_site()),
            Self::Type { ty, .. } => ty.sig_char()
        }
    }
    fn sig_type(&self) -> LitStr {
        match self {
            Self::Void => LitStr::new("V", Span::call_site()),
            Self::Type { ty, .. } => ty.sig_type()
        }
    }
}
impl Parse for JniReturn {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // Parse return arrow `->` and return type (is void if there is none)
        Ok(if let Ok(arrow_token) = input.parse::<Token![->]>() {
            Self::Type { arrow_token, ty: input.parse()? }
        } else {
            Self::Void
        })
    }
}
impl ToTokens for JniReturn {
    /// Appends the [`jni::sys`] type tokens to the [`TokenStream`].
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Void => { }, // Void return has no tokens
            Self::Type { arrow_token, ty: output } => {
                arrow_token.to_tokens(tokens);
                // Convert the `Type` to a `jni::sys` type.
                fn primitive(j_prim: JavaPrimitive, is_array: bool, span: Span) -> TokenStream {
                    let j_type = Ident::new(&format!("j{j_prim}{}", if is_array { "Array" } else { "" }), span);
                    quote_spanned! {span=> ::jni::sys::#j_type}
                }
                tokens.append_all(match output {
                    Type::Assertive(InnerType::RustPrimitive { ident, ty }) => primitive(JavaPrimitive::from(*ty), false, ident.span()),
                    Type::Assertive(InnerType::JavaPrimitive { ident, ty }) => primitive(*ty, false, ident.span()),
                    Type::Assertive(InnerType::Object(class))
                    | Type::Option { ty: InnerType::Object(class), .. } => match class.rust_type() {
                        ClassRustType::JObject    => quote_spanned! {output.span()=> ::jni::sys::jobject },
                        ClassRustType::JClass     => quote_spanned! {output.span()=> ::jni::sys::jclass },
                        ClassRustType::JThrowable => quote_spanned! {output.span()=> ::jni::sys::jthrowable },
                        ClassRustType::String | ClassRustType::JString => quote_spanned! {output.span()=> ::jni::sys::jstring },
                    },
                    Type::Option { .. } => quote_spanned! {output.span()=> ::jni::sys::jobject },
                    Type::Assertive(InnerType::Array(array)) => match &*array.ty {
                        Type::Assertive(InnerType::RustPrimitive { ident, ty }) => primitive(JavaPrimitive::from(*ty), true, ident.span()),
                        Type::Assertive(InnerType::JavaPrimitive { ident, ty }) => primitive(*ty, true, ident.span()),
                        Type::Assertive(InnerType::Object(_) | InnerType::Array(_))
                        | Type::Option { .. } => quote_spanned! {output.span()=> ::jni::sys::jobjectArray },
                    },
                });
            },
        }
    }
}
