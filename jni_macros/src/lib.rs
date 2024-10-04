mod jni_fn;
mod call;
mod object;
mod utils;

use call::{ConstructorCall, MethodCall};
use either::Either;
use jni_fn::JniFn;
use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use std::sync::RwLock;
use syn::LitStr;
use utils::{error, item_from_derive_input};

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
    return TokenStream::new();
}

/// Changes a function's signature so that it can be called from external Java code.
/// Requires that the function be defined with `pub` visibility, exactly *one lifetime* (named "local"), no generic constants or types, and no arguments named "env" or "_class".
///
/// Also takes the name of a Java Class or extra package data where this function is defined in the Java side.
///
/// Also puts the *block* of the function inside a function that catches `panics!` and throws a Java `Exception` with the panic message.
/// The *return value* should only be a type with *integer representation*, such as a pointer (*const T or *mut T), an enum using *repr(T)*, i32, bool, etc.
///
/// ### Example
/// ```
/// # use jni_macros::{package, jni_fn};
/// # use jni::objects::JString;
/// package!("me.author.packagename");
///
/// #[jni_fn("MyClass")]
/// pub fn hello_world<'local>(s: JString<'local>) {
///     // body
/// }
/// ```
/// expands to
///
/// ```
/// # use jni::objects::JString;
/// #[no_mangle]
/// pub extern "system" fn Java_me_author_packagename_myClass_hello_1world<'local>(
///     mut env: ::jni::JNIEnv<'local>, _class: ::jni::objects::JClass<'local>,
///     s: JString<'local>
/// ) {
///     ::ez_jni::__throw::catch_throw(&mut env, move |env| {
///         // body
///     })
/// }
/// ```
#[proc_macro_attribute]
pub fn jni_fn(attr_args: TokenStream, input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as JniFn);
    // Contains extra data of the Class path that is combined with PACKAGE_NAME
    let extra_package_data = if attr_args.is_empty() {
        String::new()
    } else {
        syn::parse_macro_input!(attr_args as LitStr).value()
    };
    
    let package = PACKAGE_NAME.read().unwrap();
    let package = match package.as_ref() {
        // Process package data to use underscores (_)
        Some(package_name) => package_name,
        None => return error("Macro jni_macros::package! has not been called.").into(),
    };

    jni_fn::jni_fn(input, package, &extra_package_data)
        .into_token_stream()
        .into()
}

/// A macro that helps make JNI Method calls less verbose and easier to use in Rust.
///
/// Can be used to call **static methods** on Java classes:
/// ```text
/// call!(static me.author.ClassName.methodName(int(arg1), java.lang.String(arg2)) -> int)
///                Primitive type parameter --->\_______/  \____________________/     \_/
///                  Object type parameter --------------------------^                 |
///                     Return type        --------------------------------------------^
/// ```
/// Or to call **object methods**:
/// ```no_run
/// # use jni_macros::call;
/// call!(object.methodName() -> void);
/// ```
///
/// # Syntax
///
/// To use the **static method** call, prepend the call with `static`, then the path to the *class* and *method*,
/// ```no_run
/// # use jni_macros::call;
/// call!(static me.author.ClassName.methodName() -> void);
/// ```
///
/// To use an **object method** call, simply put a *variable name* that is of type `JObject` (or put an *expression* that resolves to a `JObject` in parentheses).
/// Example:
/// ```no_run
/// # use jni_macros::call;
/// call!(my_object.myMethod() -> void);
/// call!((getObject()).myMethod() -> void);
/// ```
///
/// ## Parameters
///
/// The parameters of the method call are placed inside perentheses after the method name,
/// and can be *primitive values*, *object values*, or *arrays of either* (type wrapped in brackets).
///
/// All parameters have a **type**, and a **value** (wrapped in parenthesis).
/// The value goes in parenthesis after the parameter type, and is any expression that resolves to the right type.
/// For *arrays*, the value could be one of the `JPrimitiveArray`s or `JObjectArray`, or an array literal of either.
///
/// ```ignore
/// int(2 + 2) // primitive
/// me.author.ClassName(value) // object
/// [bool]([true, false]) // primitive array
/// [java.lang.String](value) // object array
/// ```
///
/// ## Return
///
/// The parameters are followed by a *return arrow* `->` and the *return type*.
/// The return type may be an assertive *void, primitive or Class*, an [`Option`] of a nullable Class, or a [`Result`] of one of the previous choices.
///
/// - Use the **assertive type** when the Java method being called *can't return `NULL`* or throw an *exception*,
///   such as when it is marked with `@NonNull`.
/// - Use **`Option<Type>`** when the method *can return a `NULL`* value.
/// - Use **`Result<Type, E>`** when the method can throw an *exception*, e.g. `void method() throws Exception { ... }`, and the return value *can't be `NULL`*.
/// - Use **`Result<Option<Type>, E>`** when the method can throw, and the return value *can be `NULL`*.
///
/// Here are some examples of return types:
/// ```ignore
/// -> int OR java.lang.String
/// -> Option<java.lang.String>
/// -> Result<int, String> OR Result<java.lang.String, String>
/// -> Result<Option<java.lang.String>, String>
/// -> Result<int, MyErrorType>
/// ```
/// Note that `Option` can't be used with *primitive types* because those can't be `NULL` in Java.
///
/// ### Exceptions
///
/// The **`E`** in the `Result` type can be any Rust type that *implements [`FromException`][ez_jni::FromException]*
/// (usually an *Error Enum*).
///
/// If the call to [`from_exception`][ez_jni::FromException::from_exception] fails,
/// the Exception will not be caught and the program will `panic!`.
/// This is similar to how in Java, if the exception is not of any type of the *catch blocks*, the exception will not be caught.
///
/// When `E` is [`String`], it will catch any Exception.
#[proc_macro]
pub fn call(input: TokenStream) -> TokenStream {
    let call = syn::parse_macro_input!(input as MethodCall);
    call::jni_call(call).into()
}

/// Call a Java Class' constructor.
/// 
/// Has similar syntax as [*calling a static method*][crate::call!], but there is no *method name* or *return value*.
/// 
/// ```no_run
/// # use jni_macros::new;
/// new!(me.author.ClassName(int(arg1), java.lang.String(arg2)))
/// ```
/// 
/// ### Exceptions
/// 
/// The constructor can be followed by **`throws`** with a Rust type that *implements [`FromException`]*.
/// This will make the constructor call return a `Result<JObject, E>` instead,
/// and the exception will be caught if it occurs.
/// 
/// ```no_run
/// # use jni_macros::new;
/// new!(me.author.ClassName() throws String)
/// ```
#[proc_macro]
pub fn new(input: TokenStream) -> TokenStream {
    let call = syn::parse_macro_input!(input as ConstructorCall);
    call::jni_call_constructor(call).into()
}

/// See [`ez_jni::FromObject`].
#[proc_macro_derive(FromObject, attributes(class, field))]
pub fn from_object(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);
    match item_from_derive_input(input) {
        Either::Left(st) => object::from_object_st(st)
            .unwrap_or_else(|err| err.to_compile_error()),
        Either::Right(enm) => object::from_object_enum(enm)
            .unwrap_or_else(|err| err.to_compile_error()),
    }.into()
}

/// See [`ez_jni::FromException`].
#[proc_macro_derive(FromException, attributes(class, field))]
pub fn from_exception(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);
    match item_from_derive_input(input) {
        Either::Left(st) => object::from_exception_struct(st)
            .unwrap_or_else(|err| err.to_compile_error()),
        Either::Right(enm) => object::from_exception_enum(enm)
            .unwrap_or_else(|err| err.to_compile_error()),
    }.into()
}

/// Print output. See [`std::println!`].
/// 
/// In Android, printing to `STDOUT` does not work because apparently it redirects to `/dev/null`.
/// This macro will instead crate a String and send it to `android.util.Log`.
/// 
/// Use this macro instead of [`std::println!`] everywhere.
/// 
/// See also [`eprintln()`].
#[proc_macro]
pub fn println(input: TokenStream) -> TokenStream {
    let input = proc_macro2::TokenStream::from(input);
    let string = if input.is_empty() {
        quote!("".to_string())
    } else {
        input.clone()
    };
    
    quote!{
        if #[cfg(target_os = "android")] {
            ::ez_jni::utils::__println(format!(#input), env)
        } else {
            ::std::println!(#string)
        }
    }.into()
}

/// Print error. See [`std::eprintln!`].
/// 
/// In Android, printing to `STDERR` does not work because apparently it redirects to `/dev/null`.
/// This macro will instead crate a String and send it to `android.util.Log`.
/// 
/// Use this macro instead of [`std::eprintln!`] everywhere.
/// 
/// See also [println()].
#[proc_macro]
pub fn eprintln(input: TokenStream) -> TokenStream {
    let input = proc_macro2::TokenStream::from(input);
    let string = if input.is_empty() {
        quote!("".to_string())
    } else {
        input.clone()
    };
    
    quote!{ ::cfg_if::cfg_if! {
        if #[cfg(target_os = "android")] {
            ::ez_jni::utils::__eprintln(format!(#input), env)
        } else {
            ::std::eprintln!(#string)
        }
    } }.into()
}
