use std::sync::RwLock;
use proc_macro2::Span;
use quote::{quote, quote_spanned, ToTokens};
use syn::{spanned::Spanned, GenericParam, Ident, ItemFn, LitStr};
use proc_macro::TokenStream;

static PACKAGE_NAME: RwLock<Option<String>> = RwLock::new(None);

/// Defines the name of the package name to use in the names of exported JNI functions.
/// 
/// Must be all lowercase and have no hyphens.
/// 
/// Example: `package!("me.author.packagename")`
#[proc_macro]
pub fn package(input: TokenStream) -> TokenStream {
    let package_name = syn::parse_macro_input!(input as LitStr).value();
    *PACKAGE_NAME.write().unwrap() = Some(package_name);
    return TokenStream::new()
}

/// Changes a function's signature so that it can be called from external Java code.
/// Requires that the function be defined with `pub` visibility, no generic types, no lifetime named "local", and no arguments named "env" or "_class".
/// 
/// Also takes the name of a Java Class or extra package data where this function is defined in the Java side.
/// 
/// ### Example
/// ```
/// package!("me.author.packagename")
/// 
/// #[jni_fn("MyClass")]
/// pub fn hello_world(s: JString) {
///     // body
/// }
/// ``````
/// expands to
/// 
/// ```
/// #[no_mangle]
/// pub extern "system" fn Java_me_author_packagename_myClass_hello_1world<'local>(
///     mut env: ::jni::JNIEnv<'local>, _class: ::jni::objects::JClass<'local>,
///     s: JString
/// ) {
///     // body
/// }
/// ```
#[proc_macro_attribute]
pub fn jni_fn(attr_args: TokenStream, input: TokenStream) -> TokenStream {
    let mut input = syn::parse_macro_input!(input as ItemFn);
    let extra_package_data = if attr_args.is_empty() { None } else {
        Some(syn::parse_macro_input!(attr_args as LitStr).value())
    };

    // Function must have 'pub' visibility
    match input.vis {
        syn::Visibility::Public(_) => {},
        _ => return error_spanned(input.sig.span(), "Function must have 'pub' visibility").into()
    }

    // Function can't have a lifetime named "local"
    if let Some(lifetime) = input.sig.generics.params.iter()
        .filter_map(|g| match g {
            GenericParam::Lifetime(lifetime) => Some(lifetime), _ => None
        })
        .find(|&lifetime| lifetime.lifetime.ident.to_string() == "local")
    {
        return error_spanned(lifetime.lifetime.span(), "Function can't have a lifetime named \"local\"").into()
    }

    // Function can't have generic types
    let generic_types = input.sig.generics.params.iter()
        .filter(|g| match g {
            GenericParam::Type(_) | GenericParam::Const(_) => true,
            _ => false
        })
        .map(|g| error_spanned(g.span(), "Function can't have generic types"))
        .fold(quote!{}, |acc, next| quote! { #acc #next }.into());
    if !generic_types.is_empty() {
        return generic_types.into()
    }

    // Function can't have arguments named "env" or "_class"
    let bad_arguments = input.sig.inputs.iter()
        .filter_map(|arg| match arg {
            syn::FnArg::Typed(arg) => Some(arg),
            _ => None
        })
        .filter(|&arg| ["env", "_class"].contains(&arg.pat.to_token_stream().to_string().as_str()))
        .map(|arg| error_spanned(arg.span(), format!("Function can't have an argument named \"{}\"", arg.pat.to_token_stream())))
        .fold(quote!{}, |acc, next| quote! { #acc #next }.into());
    if !bad_arguments.is_empty() {
        return bad_arguments.into()
    }

    // Change name of function
    let class_path = match &*PACKAGE_NAME.read().unwrap() {
        // Process package data to use underscores (_)
        Some(package_name) =>
            format!("{package_name}.{}", extra_package_data.unwrap_or("".to_string()))
                .replace(['.', '/'], "_"),
        None => return error("Macro jni_macros::package! has not been called.").into()
    };
    let name = input.sig.ident.to_string().replace('_', "_1");
    input.sig.ident = Ident::new(&format!("Java_{class_path}_{name}"), input.sig.ident.span());
    // Convert to system ABI
    input.attrs.push(syn::parse_quote!(#[no_mangle]));
    input.sig.abi = Some(syn::parse_quote!(extern "system"));
    // Add 'local lifetime
    input.sig.generics.params.push(GenericParam::Lifetime(syn::parse_quote!('local)));
    // Add env and _class arguments
    input.sig.inputs.push(syn::FnArg::Typed(syn::parse_quote!(mut env: ::jni::JNIEnv<'local>)));
    input.sig.inputs.push(syn::FnArg::Typed(syn::parse_quote!(_class: ::jni::objects::JClass<'local>)));

    quote! { #input }.into()

    // let fn_name = format!("Java__DavSyncRs_");
    // let fn_generics = input.sig.generics.params.into_iter();
    // let fn_args = input.sig.inputs.into_iter();
    // let fn_return = input.sig.output;
    // let fn_where_clause = input.sig.generics.where_clause;
    // let fn_body = input.block;
    // quote! {
    //     #[no_mangle]
    //     pub extern "system" fn #fn_name<'local, #(#fn_generics),*>(mut env: JNIEnv<'local>, _class: JClass<'local>, #(#fn_args),*) -> #fn_return
    //     #fn_where_clause
    //     #fn_body
    // }.into()
}

fn error(err: impl AsRef<str>) -> proc_macro2::TokenStream {
    let err = err.as_ref();
    quote! { compile_error!(#err); }
}
fn error_spanned(span: Span, err: impl AsRef<str>) -> proc_macro2::TokenStream {
    let err = err.as_ref();
    quote_spanned! {span=>
        compile_error!(#err);
    }
}