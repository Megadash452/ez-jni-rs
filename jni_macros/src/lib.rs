mod jni_fn;
mod call;
mod object;
mod utils;
mod types;

use call::{ConstructorCall, MethodCall};
use either::Either;
use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use utils::item_from_derive_input;

/// Converts a Rust function to one that can be called from external Java code.
/// 
/// The *arguments* and *return type* must be **Java Types** (or Rust primitives like u8).
/// 
/// You can also do multiple function definitions in one macro call.
/// 
/// ### Requirements
/// 
/// Requires that the function
/// 1. be defined with `pub` visibility,
/// 2. have exactly *one lifetime* (named `local`),
/// 3. no generic constants or types,
/// 4. and no arguments named `env` or `_class`.
///
/// The function must also have a `class` attribute with the *full name* of a Java Class (e.g. `java.lang.String`).
///
/// ### Panic catching
/// 
/// The *block* of the function will be wrapped with a *[special function](https://docs.rs/ez_jni/latest/ez_jni/__throw/fn.catch_throw.html)*
/// that catches `panics!` and throws them as Java `Exception`s with the panic message.
/// 
/// When a panic is caught and the exception is *thrown*,
/// the function will return a *[zeroed](std::mem::zeroed)* representation of the return type.
/// 
/// ### Mark of the sig
/// 
/// A doc-comment will be appended to the function definition with the *Java method signature* that must be used to call the function.
/// This is done in case some external library wants to parse and collect functions exported by a jni lib.
/// 
/// ### Example
/// ```
/// # use ez_jni_macros::jni_fn;
/// jni_fn! {
///     #[class(me.author.MyClass)]
///     pub fn hello_world<'local>(s: java.lang.String) -> int {
///         3
///     }
/// }
/// ```
/// expands to
///
/// ```
/// /// (Ljava/lang/String;)I;
/// #[no_mangle]
/// pub extern "system" fn Java_me_author_MyClass_hello_1world<'local>(
///     mut env: ::jni::JNIEnv<'local>, _class: ::jni::objects::JClass<'local>,
///     s: ::jni::objects::JString<'local>,
/// ) -> i32 {
///     ::ez_jni::__throw::catch_throw(&mut env, move |env| {
///         3
///     })
/// }
/// ```
#[proc_macro]
pub fn jni_fn(input: TokenStream) -> TokenStream {
    match syn::parse::Parser::parse(jni_fn::jni_fn, input) {
        Ok(output) => output
            .into_iter()
            .map(|f| f.into_token_stream())
            .collect::<proc_macro2::TokenStream>()
            .into(),
        Err(error) => error.to_compile_error().into()
    }
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
/// ```ignore
/// call!(object.methodName() -> void);
/// ```
///
/// # Syntax
///
/// To use the **static method** call, prepend the call with `static`, then the path to the *class* and *method*,
/// ```ignore
/// call!(static me.author.ClassName.methodName() -> void);
/// ```
///
/// To use an **object method** call, simply put a *variable name* that is of type `JObject` (or put an *expression* that resolves to a `JObject` in parentheses).
/// Example:
/// ```ignore
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
/// The **`E`** in the `Result` type can be any Rust type that *implements [`FromException`](https://docs.rs/ez_jni/latest/ez_jni/trait.FromException.html)*
/// (usually an *Error Enum*).
///
/// If the call to [`from_exception`](https://docs.rs/ez_jni/latest/ez_jni/trait.FromException.html#method.from_exception) fails,
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
/// ```ignore
/// new!(me.author.ClassName(int(arg1), java.lang.String(arg2)))
/// ```
/// 
/// ### Exceptions
/// 
/// The constructor can be followed by **`throws`** with a Rust type that *implements [`FromException`]*.
/// This will make the constructor call return a `Result<JObject, E>` instead,
/// and the exception will be caught if it occurs.
/// 
/// ```ignore
/// new!(me.author.ClassName() throws String)
/// ```
#[proc_macro]
pub fn new(input: TokenStream) -> TokenStream {
    let call = syn::parse_macro_input!(input as ConstructorCall);
    call::jni_call_constructor(call).into()
}

/// See [`ez_jni::FromObject`](https://docs.rs/ez_jni/latest/ez_jni/trait.FromObject.html).
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

/// See [`ez_jni::FromException`](https://docs.rs/ez_jni/latest/ez_jni/trait.FromException.html).
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

/// Print output. See [`std::println!`](https://doc.rust-lang.org/std/macro.println.html).
/// 
/// In Android, printing to `STDOUT` does not work because apparently it redirects to `/dev/null`.
/// This macro will instead crate a String and send it to `android.util.Log`.
/// 
/// Requires the [`env`][jni::JNIEnv] argument be present in the calling function.
/// 
/// Use this macro instead of [`std::println!`](https://doc.rust-lang.org/std/macro.println.html) everywhere.
/// 
/// See also [`eprintln!`].
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

/// Print error. See [`std::eprintln!`](https://doc.rust-lang.org/std/macro.eprintln.html).
/// 
/// In Android, printing to `STDERR` does not work because apparently it redirects to `/dev/null`.
/// This macro will instead crate a String and send it to `android.util.Log`.
/// 
/// Requires the [`env`][jni::JNIEnv] argument be present in the calling function.
/// 
/// Use this macro instead of [`std::eprintln!`](https://doc.rust-lang.org/std/macro.eprintln.html) everywhere.
/// 
/// See also [`println!`].
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
