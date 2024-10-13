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
/// a **[Rust Primitive](std::primitive)**,
/// a **Java Class**,
/// or an **Array** of one of the previous types (the type wrapped in *brackets* `[]`).
/// 
/// For the class `java.lang.String`, use the Rust type [`String`] instead.
/// The values will be automatically converted by the macro between Rust and Java
/// (depending on whether the type is for an *argument* or *return*).
/// 
/// In the sections below, the use of `T` or `Type` means that it can accept any of the types declared above.
///
/// ## Arguments
///
/// The arguments of the method call are placed inside perentheses after the method name,
/// and can be *primitive values*, *object values*, or *arrays of either* (type wrapped in brackets).
///
/// All arguments have a **type**, and a **value** (wrapped in parenthesis).
/// The value goes in parenthesis after the argument type, and is any expression that resolves to *primitive* or *object* of the respective type.
/// 
/// **Array** arguments' **values** can be any Rust Type that is `AsRef<[T]>`,
/// i.e. the value can be read as a *slice* of said type.
/// e.g. [slice](https://doc.rust-lang.org/std/primitive.slice.html)s, [`Vec`]s, boxed slices, etc.
/// 
/// Because `String` arguments only accept Rust strings (which can't be **null**),
/// the macro creates a *custom keyword* `null` for the argument values.
/// This is only usable for *Objects*.
/// For *String arrays* the value can also be `&[Option<String>]`.
///
/// Here are some examples of an argument:
/// ```ignore
/// int(2 + 2)                   // primitive
/// me.author.ClassName(value)   // object
/// String("Hello, World!")      // string
/// [bool]([true, false])        // primitive array
/// [java.lang.Object](null)     // object array (with null)
/// [String](["Hello", "World"]) // string array
/// [String](["Hello", null])    // string array (with null)
/// ```
///
/// ## Return
///
/// The arguments are followed by a *return arrow* `->` and the **return type**.
/// The return type may be *void*, one of the [`Types`](https://docs.rs/ez_jni/latest/ez_jni/macro.call.html#types) above,
/// an [`Option`] of a Class, or a [`Result<T, E>`] of any of the previous choices.
///
/// - Use the **assertive type** (`T` by itself) when the Java method being called *can't return `NULL`* or throw an *exception*,
///   such as when it is marked with `@NonNull`.
/// - Use **`Option<T>`** when the method *can return a `NULL`* value, but can't throw an *exception*.
/// - Use **`Result<T, E>`** when the method can throw an *exception*, e.g. `void method() throws Exception { ... }`, and the return value *can't be `NULL`*.
/// - Use **`Result<Option<T>, E>`** when the method can throw an *exception*, and the return value *can be `NULL`*.
/// 
/// Here are some examples of return types:
/// ```ignore
/// -> int
/// -> java.lang.String
/// -> [int]
/// -> [String]
/// -> Option<java.lang.String>
/// -> Result<int, String>
/// -> Result<Option<String>, MyErrorType>
/// ```
/// Note that `Option` can't be used with *primitive types* because those can't be `NULL` in Java.
/// However, it can be used with an *Array of primitives* because Java Arrays are *objects*.
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
