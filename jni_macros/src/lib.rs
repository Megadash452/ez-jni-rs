mod jni_fn;
mod call;
mod object;
mod utils;
mod types;

use call::{ConstructorCall, MethodCall};
use either::Either;
use proc_macro::TokenStream;
use quote::quote;
use utils::item_from_derive_input;

/// Define a function in Rust that can be called from external Java code.
/// 
/// The function can also be defined with `static` to match the declaration of the *native method* in the Java side.
/// This means that the function will be called with a *Class* instead of an *Object* as the *receiver*.
/// 
/// You can also do multiple function definitions in one macro call.
/// 
/// ### Requirements
/// 
/// Requires that the function
/// 1. be defined with `pub` visibility,
/// 2. have exactly *one lifetime* (named `local`),
/// 3. no generic constants or types,
/// 4. and no arguments named `env` or `this` (or `class` if it is *static*.).
///
/// All *jni_fn*s must belong to a *Java Class* where the native function is declared.
/// The class must be the full class path (e.g. `java.lang.String`).
/// This can be done with a `class` attribute on each function,
/// or with the Class at the start of the macro input,
/// which will apply to all functions in that macro invocation.
/// 
/// ### Argument and Return Types
/// 
/// The **arguments** and **return** can have any Type that is also used for [`ez_jni::call!`](https://docs.rs/ez_jni/latest/ez_jni/macro.call.html#types).
/// 
/// The implicit argument `this` has type [`JObject`][jni::objects::JObject],
/// or `class` (if it is *static*) has type [`JClass`][jni::objects::JClass],
/// although both types are essentially the same.
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
/// jni_fn! { me.author.MyClass =>
///     // or with attribute: #[class(me.author.MyClass)]
///     pub static fn hello_world<'local>(s: java.lang.String) -> int {
///         3
///     }
/// }
/// ```
/// expands to
///
/// ```
/// # use jni::{JNIEnv, objects::{JClass, JString}};
/// /// (Ljava/lang/String;)I
/// #[no_mangle]
/// pub extern "system" fn Java_me_author_MyClass_hello_1world<'local>(
///     mut env: JNIEnv<'local>, #[allow(unused_variables)] class: JClass<'local>,
///     s: JString<'local>,
/// ) -> jni::sys::jint {
///     ez_jni::__throw::catch_throw(&mut env, move |#[allow(unused_variables)] env| -> i32 {
///         let s = ez_jni::utils::get_string(s, env);
///         3
///     })
/// }
/// ```
#[proc_macro]
pub fn jni_fn(input: TokenStream) -> TokenStream {
    match syn::parse::Parser::parse(jni_fn::jni_fn, input) {
        Ok(output) => output
            .into_iter()
            .collect::<proc_macro2::TokenStream>()
            .into(),
        Err(error) => error.to_compile_error().into()
    }
}

/// A macro that helps make JNI Method calls less verbose and easier to use in Rust.
///
/// # Syntax
/// ```text
/// call!(static me.author.ClassName.methodName(int(arg1), java.lang.String(arg2)) -> int)
///                Primitive type parameter --->\_______/  \____________________/     \_/
///                  Object type parameter --------------------------^                 |
///                     Return type        --------------------------------------------^
/// ```
///
/// ## Method Types
/// 
/// This macro can handle calling **static** or **object** methods.
/// 
/// To use call a **static method**, start with the `static` keyword, then the *fully-qualified class name*, and the *method name*.
/// ```ignore
/// call!(static me.author.ClassName.methodName() -> void);
/// ```
///
/// To use call method on an Object (a.k.a. **object method**), simply put a *variable name* that is of type `JObject` (or put an *expression* that resolves to a `JObject` in parentheses).
/// Example:
/// ```ignore
/// call!(my_object.myMethod() -> void);
/// call!((getObject()).myMethod() -> void);
/// ```
/// 
/// ## Types
/// 
/// The function call has *argument* and *return* **types**.
/// 
/// A type can be a **[Java Primitive](https://docs.oracle.com/javase/tutorial/java/nutsandbolts/datatypes.html)**,
/// a **[Rust Primitive](std::primitive)** (which is transmuted to the corresponding Java Primitive),
/// a **Java Class**,
/// or an **Array** of one of the previous types (the type wrapped in *brackets* `[]`).
/// 
/// *Classes* and *Arrays* can be wrapped with [`Option`], but not *primitives*.
/// This makes it so that *arguments* can be passed without needing a manual conversion from the caller,
/// and to allow **null** values for the *return* in Rust.
/// This can be used when the Type in the Java function declaration has the `@Nullable` annotation,
/// or the function documents that it can accept or return a `null` value.
/// 
/// Arrays can be **multi-dimensional** (with unlimited dimensions).
/// Inner types of the array can also be wrapped with [`Option`] (except primitives, of course).
/// 
/// For the class `java.lang.String`, use the Rust type [`String`] instead.
/// 
/// [`String`], **Array**, and [`Option`] values will be automatically converted between the 2 languages when making calls.
/// 
/// Here are *some* examples of valid Types:
/// ```ignore
/// int              // primitive
/// i32              // Rust primitive
/// java.lang.Object // object
/// String           // string
/// [int]            // primitive array
/// [String]         // object array
/// [[int]]          // multidimensional array
/// Option<String>   // nullable object
/// Option<[Option<String>]> // nullable array of nullable objects
/// ```
/// 
/// In the sections below, the use of `T` or `Type` means that it can accept any of the types declared above.
///
/// ## Arguments
///
/// The arguments of the method call are placed inside perentheses after the method name.
/// The syntax is simple: `type(value)`.
///
/// All arguments have a **type** (see the [types section](https://docs.rs/ez_jni/latest/ez_jni/macro.call.html#types)),
/// followed by a **value** (wrapped in parenthesis).
/// 
/// **Array** arguments' **values** can be any Rust Type that is `AsRef<[T]>`,
/// i.e. the value can be read as a *slice* of said type.
/// e.g. [slice](https://doc.rust-lang.org/std/primitive.slice.html)s, [`Vec`]s, boxed slices, etc.
/// 
/// Because [`String`] arguments only accept Rust strings (which can't be **null**),
/// the macro creates a *custom keyword* `null` for the argument values.
/// This is only usable for *Objects*.
/// 
/// Example of `null` argument value:
/// ```ignore
/// String(null)
/// ```
///
/// ## Return
///
/// The arguments are followed by a *return arrow* `->` and the **return type**.
/// The return type may be `void`, one of the [`Types`](https://docs.rs/ez_jni/latest/ez_jni/macro.call.html#types) above,
/// or a [`Result<T, E>`] of any of the previous choices.
///
/// The real type (`T`) can be wrapped in [`Result`] when the method can return an **Exception**.
/// Use this when the method is marked with `throws Exception`.
/// If the method throws, but [`Result`] was not used, a `panic!` will occur.
/// 
/// ### Exceptions
///
/// The **`E`** in the [`Result`] type can be any Rust type that *implements [`FromException`](https://docs.rs/ez_jni/latest/ez_jni/trait.FromException.html)*
/// (usually an *Error Enum*).
///
/// If the call to [`from_exception`](https://docs.rs/ez_jni/latest/ez_jni/trait.FromException.html#method.from_exception) fails,
/// the Exception will not be caught and the program will `panic!`.
/// This is similar to how in Java, if the exception is not of any type of the *catch blocks*, the exception will not be caught.
///
/// When `E` is [`String`] or , it will catch any Exception.
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
    let trait_ = syn::parse_quote!(::ez_jni::FromObject);
    let method = syn::parse_quote!(from_object);
    let obj_ty = syn::parse_quote!(::jni::objects::JObject);

    match item_from_derive_input(input) {
        Either::Left(st) => object::derive_struct(st, trait_, method, obj_ty)
            .unwrap_or_else(|err| err.to_compile_error()),
        Either::Right(enm) => object::derive_enum(enm, trait_, method, obj_ty)
            .unwrap_or_else(|err| err.to_compile_error()),
    }.into()
}

/// See [`ez_jni::FromException`](https://docs.rs/ez_jni/latest/ez_jni/trait.FromException.html).
#[proc_macro_derive(FromException, attributes(class, field))]
pub fn from_exception(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);
    let trait_ = syn::parse_quote!(::ez_jni::FromException);
    let method = syn::parse_quote!(from_exception);
    let obj_ty = syn::parse_quote!(::jni::objects::JThrowable);

    match item_from_derive_input(input) {
        Either::Left(st) => object::derive_struct(st, trait_, method, obj_ty)
            .unwrap_or_else(|err| err.to_compile_error()),
        Either::Right(enm) => object::derive_enum(enm, trait_, method, obj_ty)
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
    
    quote!{ ::cfg_if::cfg_if! {
        if #[cfg(target_os = "android")] {
            ::ez_jni::utils::__println(format!(#input), env)
        } else {
            ::std::println!(#string)
        }
    } }.into()
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
