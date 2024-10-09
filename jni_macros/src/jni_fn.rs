use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned, ToTokens, TokenStreamExt};
use syn::{braced, parenthesized, parse::{Parse, ParseStream}, punctuated::Punctuated, spanned::Spanned, Attribute, GenericParam, Generics, Ident, ItemFn, LifetimeParam, LitStr, Token};
use crate::{
    utils::{gen_signature, get_class_attribute_required, merge_errors},
    types::{ClassPath, RustPrimitive, SigType, InnerType}
};

/// Processes the input for [`crate::jni_fns`].
/// Converts the parsed [`JniFn`] to a regular function used in Rust.
pub fn jni_fn(input: ParseStream) -> syn::Result<Vec<ItemFn>> {
    let mut inputs = Vec::new();
    let mut errors = Vec::new();

    // Parse multiple jni_fn
    while !input.is_empty() {
        match input.parse::<JniFn>() {
            Ok(f) => inputs.push(f),
            Err(error) => errors.push(error),
        }
    }

    merge_errors(errors)?;

    // Convert all JniFn to ItemFn 
    Ok(inputs.into_iter()
        .map(|f| f.to_rust_fn())
        .collect()
    )
}

// TODO: Allow generics in the arguments and return type if they are a Java Class

// TODO: support arrays

/// A rust function that uses Java types (or some Rust types) and is called by Java Code.
/// 
/// A [`JniFn`] is one that MUST be exported by the user library.
pub struct JniFn {
    pub attrs: Vec<Attribute>,
    /// The Java Class the function is a method of.
    /// This takes the form of an attribute that is removed after parsing.
    class: ClassPath,
    pub name: Ident,
    pub lifetime: LifetimeParam,
    pub inputs: Punctuated<JniFnArg, Token![,]>,
    // pub variadic: Option<Variadic>, Should this be allowed?
    pub output: JniReturn,
    pub content: TokenStream,
}
impl JniFn {
    pub fn to_rust_fn(&self) -> ItemFn {
        let name = {
            let class = self.class.to_string()
                .replace('.', "_");
            let name = self.name.to_string().replace('_', "_1");
    
            Ident::new(&format!("Java_{class}_{name}"), self.name.span())
        };

        // Build a java method signature, something like (Ljava.lang.String;)I
        let method_sig = gen_signature(self.inputs.iter().map(|i| &i.ty), &self.output).value();

        let attrs = &self.attrs;
        let lifetime = &self.lifetime;
        let inputs = &self.inputs;
        let output = &self.output;
        let content = &self.content;

        syn::parse_quote! {
            #(#attrs)*
            #[doc = ""]
            #[doc = #method_sig]
            #[no_mangle]
            pub extern "system" fn #name<#lifetime>(mut env: ::jni::JNIEnv<'local>, _class: ::jni::objects::JClass<'local>, #inputs) #output {
                ::ez_jni::__throw::catch_throw(&mut env, move |env| { #content })
            }
        }
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
        let _ = input.parse::<Token![pub]>()
            .map_err(|err| errors.push(
                syn::Error::new(err.span(), format!("{err}; jn_fn must have `pub` because they must be exported by the library"))
            ));

        // Parse `fn`
        input.parse::<Token![fn]>()?;

        // Parse function name
        let name = input.parse::<Ident>()
            .map_err(|err|
                syn::Error::new(err.span(), format!("{err}; Expected function name"))
            )?;

        // Parsed all generics, but will only accept one lifetime
        let generics = input.parse::<Generics>()
            .map_err(|err| errors.push(
                syn::Error::new(err.span(), format!("{err}; {LIFETIME_ERROR}"))
            ))
            .ok();

        // Parse arguments with Java types
        let inputs = {
            let inner;
            parenthesized!(inner in input);

            Punctuated::<JniFnArg, Token![,]>::parse_terminated(&inner)
                .map_err(|err| errors.push(err))
                .ok()
        };

        // Parse return arrow `->` and return type (is void if there is none)
        let output = input.parse::<JniReturn>()
            .map_err(|err| errors.push(err))
            .ok();

        // Parse the content of the function inside the braces `{ ... }`
        let content = {
            let inner;
            braced!(inner in input);
            inner.parse()?
        };

        merge_errors(errors)?;

        // If there were any errors, the function did eraly return, so all tokens were parsed successfully.
        let mut attrs = attrs.unwrap();
        let inputs = inputs.unwrap();
        let output = output.unwrap();

        // -- Perform checks on the successfully parsed function
        errors = Vec::new();

        // jni_fn must have a `class` attribute
        let class = get_class_attribute_required(&mut attrs, name.span())
            .map_err(|err| errors.push(err))
            .ok();

        let generics = generics.unwrap();
        let mut iter = generics.params.iter();

        // Parsed all generics, but will only accept one lifetime, and no other generic types
        let lifetime = match iter.next() {
            Some(GenericParam::Lifetime(lifetime)) =>
                if lifetime.lifetime.ident.to_string() != "local" {
                    errors.push(syn::Error::new(lifetime.span(), LIFETIME_ERROR));
                    None
                } else {
                    Some(lifetime.clone())
                },
            Some(generic) => {
                errors.push(syn::Error::new(generic.span(), "jni_fn can't have generic constants or types"));
                None
            },
            None => {
                errors.push(syn::Error::new(generics.span(), LIFETIME_ERROR));
                None
            }
        };

        // Only allow 1 lifetime
        if let Some(lifetime) = iter.next() {
            errors.push(syn::Error::new(lifetime.span(), LIFETIME_ERROR))
        }

        // Check that the function doesn't have arguments named "env" or "_class" because those are implicitly added
        for arg_name in ["env", "_class"] {
            if let Some(arg) = inputs.iter().find(|arg| arg.name.to_string() == arg_name) {
                errors.push(syn::Error::new(
                    arg.span(),
                    format!("Function can't have an argument named \"{arg_name}\"; this is added implicitly")
                ))
            }
        }

        merge_errors(errors)?;

        let class = class.unwrap();
        let lifetime = lifetime.unwrap();

        Ok(Self { attrs, class, name, lifetime, inputs, output, content })
    }
}
impl ToTokens for JniFn {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append_all(self.to_rust_fn().into_token_stream())
    }
}

pub struct JniFnArg {
    pub attrs: Vec<Attribute>,
    pub name: Ident,
    pub ty: InnerType
}
impl Parse for JniFnArg {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            attrs: Attribute::parse_outer(input)?,
            name: input.parse()?,
            ty: {
                input.parse::<Token![:]>()?;
                input.parse()?
            }
        })
    }
}
impl ToTokens for JniFnArg {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append_all(&self.attrs);
        tokens.append(self.name.clone());
        tokens.append_all(quote!(:));
        
        // Convert Class to JObject (java.lang.String to JString) and leave primitives alone
        tokens.append_all(match &self.ty {
            InnerType::RustPrimitive { ident, ty } => Ident::new(&ty.to_string(), ident.span()).into_token_stream(),
            InnerType::JavaPrimitive { ident, ty } => Ident::new(&RustPrimitive::from(*ty).to_string(), ident.span()).into_token_stream(),
            InnerType::Object(class) =>
                if class.to_string() == "java.lang.String" {
                    quote_spanned! {class.span()=> ::jni::objects::JString<'local>}
                } else {
                    quote_spanned! {class.span()=> ::jni::objects::JObject<'local>}
                }
        })
    }
}

pub enum JniReturn {
    Void,
    Type(InnerType)
}
impl SigType for JniReturn {
    fn sig_char(&self) -> Ident {
        match self {
            Self::Void => Ident::new("v", Span::call_site()),
            Self::Type(ty) => ty.sig_char()
        }
    }
    fn sig_type(&self) -> LitStr {
        match self {
            Self::Void => LitStr::new("V", Span::call_site()),
            Self::Type(ty) => ty.sig_type()
        }
    }
}
impl Parse for JniReturn {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // Parse return arrow `->` and return type (is void if there is none)
        Ok(if input.parse::<Token![->]>().is_ok() {
            Self::Type(input.parse()?)
        } else {
            Self::Void
        })
    }
}
impl ToTokens for JniReturn {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Void => { }, // Void return has no tokens
            Self::Type(output) => {
                // Convert Class to *jobject (or *jstring) and leave primitives alone
                let ty = match output {
                    InnerType::RustPrimitive { ident, ty } => Ident::new(&ty.to_string(), ident.span()).into_token_stream(),
                    InnerType::JavaPrimitive { ident, ty } => Ident::new(&RustPrimitive::from(*ty).to_string(), ident.span()).into_token_stream(),
                    InnerType::Object(class) =>
                        if class.to_string() == "java.lang.String" {
                            quote_spanned! {class.span()=> ::jni::sys::jstring}
                        } else {
                            quote_spanned! {class.span()=> ::jni::sys::jobject}
                        }
                };
                tokens.append_all(quote_spanned!(output.span()=> -> #ty))
            },
        }
    }
}