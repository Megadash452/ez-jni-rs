use proc_macro2::TokenStream;
use quote::{quote, quote_spanned, ToTokens, TokenStreamExt};
use syn::{braced, parenthesized, parse::Parse, punctuated::Punctuated, spanned::Spanned, Attribute, GenericParam, Generics, Ident, ItemFn, LifetimeParam, Token};
use crate::utils::{merge_errors, RustPrimitive, Type};

/// Processes the input for [`crate::jni_fn`].
/// Converts the parsed [`JniFn`] to a regular function used in Rust.
pub fn jni_fn(input: JniFn, package: &str, class_path: &str) -> ItemFn {
    let name = {
        // Process package data to use underscores (_)
        let full_class = format!("{package}_{class_path}")
            .replace(['.', '/'], "_");
        let name = input.name.to_string().replace('_', "_1");

        Ident::new(&format!("Java_{full_class}_{name}"), input.name.span())
    };

    let output = match &input.output {
        Some(output) => {
            // Convert Class to *jobject (or *jstring) and leave primitives alone
            let ty = match output {
                Type::RustPrimitive { ident, ty } => Ident::new(&ty.to_string(), ident.span()).into_token_stream(),
                Type::JavaPrimitive { ident, ty } => Ident::new(&RustPrimitive::from(*ty).to_string(), ident.span()).into_token_stream(),
                Type::Object(class) =>
                    if class.to_string() == "java.lang.String" {
                        quote_spanned! {class.span()=> ::jni::sys::jstring}
                    } else {
                        quote_spanned! {class.span()=> ::jni::sys::jobject}
                    }
            };
            quote!(-> #ty)
        },
        None => quote!()
    };

    let attrs = &input.attrs;
    let lifetime = &input.lifetime;
    let inputs = &input.inputs;
    let content = &input.content;

    syn::parse_quote! {
        #[no_mangle]
        #(#attrs)*
        pub extern "system" fn #name<#lifetime>(mut _env: ::jni::JNIEnv<'local>, _class: ::jni::objects::JClass<'local>, #inputs) #output {
            ::ez_jni::__throw::catch_throw(&mut _env, move |env| { #content })
        }
    }
}

// TODO: Allow generics in the arguments and return type if they are a Java Class

/// A rust function that uses Java types (or some Rust types) and is called by Java Code.
/// 
/// A [`JniFn`] is one that MUST be exported by the user library.
pub struct JniFn {
    pub attrs: Vec<Attribute>,
    pub name: Ident,
    pub lifetime: LifetimeParam,
    pub inputs: Punctuated<JniFnArg, Token![,]>,
    // pub variadic: Option<Variadic>, Should this be allowed?
    pub output: Option<Type>,
    pub content: TokenStream,
}
impl Parse for JniFn {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
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
        let name = input.parse()
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
        let output = if input.parse::<Token![->]>().is_ok() {
            Some(input.parse::<Type>()?)
        } else {
            None
        };

        // Parse the content of the function inside the braces `{ ... }`
        let content = {
            let inner;
            braced!(inner in input);
            inner.parse()?
        };

        merge_errors(errors)?;

        // If there were any errors, the function did eraly return, so all tokens were parsed successfully.
        let attrs = attrs.unwrap();
        let inputs = inputs.unwrap();

        // -- Perform checks on the successfully parsed function
        errors = Vec::new();

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

        Ok(Self { attrs, name, lifetime: lifetime.unwrap(), inputs, output, content })
    }
}

pub struct JniFnArg {
    pub attrs: Vec<Attribute>,
    pub name: Ident,
    pub ty: Type
}
impl Parse for JniFnArg {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
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
        // // Add a doc attribute to preserve the Class (if any) of the argument
        // if let Type::Object(class) = &self.ty {
        //     let class = class.to_string();
        //     tokens.append_all(quote!(#[doc = #class]));
        // }
        tokens.append(self.name.clone());
        tokens.append_all(quote!(:));
        
        // Convert Class to JObject (java.lang.String to JString) and leave primitives alone
        tokens.append_all(match &self.ty {
            Type::RustPrimitive { ident, ty } => Ident::new(&ty.to_string(), ident.span()).into_token_stream(),
            Type::JavaPrimitive { ident, ty } => Ident::new(&RustPrimitive::from(*ty).to_string(), ident.span()).into_token_stream(),
            Type::Object(class) =>
                if class.to_string() == "java.lang.String" {
                    quote_spanned! {class.span()=> ::jni::objects::JString<'local>}
                } else {
                    quote_spanned! {class.span()=> ::jni::objects::JObject<'local>}
                }
        })
    }
}