mod jni_fn;
mod call;
mod object;
mod utils;
mod types;
mod private;

use call::{ConstructorCall, FieldCall, MethodCall};
use either::Either;
use proc_macro::TokenStream;
use quote::quote;
use syn::{parse::Parser, LitStr, Token};
use utils::{item_from_derive_input, SynResultExt as _};

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
/// 3. have no generic constants or types,
/// 4. and have no argument named `this` (or `class` if it is *static*.).
///
/// All *jni_fn*s must belong to a *Java Class* where the native function is declared.
/// The class must be the full class path (e.g. `java.lang.String`).
/// This can be specified with a `class` attribute on each function,
/// or with the Class at the start of the macro input,
/// which will apply to all functions in that macro invocation.
/// 
/// ### Argument and Return Types
/// 
/// The **arguments** and **return** can have any Type that is also used for [`call!`][call!#types].
/// 
/// The implicit argument `this` has type [`JObject`][jni::objects::JObject],
/// or `class` (if it is *static*) has type [`JClass`][jni::objects::JClass],
/// although both types are essentially the same.
///
/// ### Panic catching
/// 
/// The *block* of the function will be wrapped with a *special panic catcher*
/// that catches `panics!` and throws them as an **Exception** of [`me.marti.ezjni.RustPanic`](https://github.com/Megadash452/ez-jni-rs/blob/main/src/me/marti/ezjni/RustPanic.java) with the panic message.
/// 
/// When a panic is caught and the exception is *thrown*,
/// the function will return a *[zeroed][std::mem::zeroed]* representation of the return type.
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
///     pub static fn hello_world<'local>(s: String) -> int {
///         3
///     }
/// }
/// ```
/// roughly expands to
///
/// ```
/// # use jni::{JNIEnv, objects::{JClass, JObject}};
/// # use ez_jni::FromObject;
/// ///
/// /// (Ljava/lang/String;)I
/// #[unsafe(no_mangle)]
/// pub extern "system" fn Java_me_author_MyClass_hello_1world<'local>(
///     env: JNIEnv<'local>, class: JClass<'local>, s: JObject<'local>,
/// ) -> jni::sys::jint {
///     fn f<'local>(class: JClass<'local>, s: String) -> i32 {
///         3
///     }
///     unsafe { ez_jni::__throw::run_with_jnienv(env, move |env| {
///         let s = <String as FromObject>::from_object_env(&s, env).unwrap();
///         f(class, s)
///     }) }
/// }
/// ```
/// > **Note on `unsafe(no_mangle)`**: Since Rust 2024 edition,
/// > `no_mangle` (among others) requires being wrapped in `unsafe` to acknowledge that it may cause UB in some cases
/// > (see [the rfc](https://rust-lang.github.io/rfcs/3325-unsafe-attributes.html)).
/// > However, this is very unlikely to happen in the case of these functions because they have complicated names.
/// > Also, ez_jni is not designed for linking Java to native Rust with multiple sources.
#[proc_macro]
pub fn jni_fn(input: TokenStream) -> TokenStream {
    parse(input, jni_fn::jni_fn)
        .map(|output| output
            .into_iter()
            .collect::<proc_macro2::TokenStream>()
        )
        .unwrap_tokens()
}

/// A macro that helps make JNI Method calls less verbose and easier to use in Rust.
///
/// # Syntax
/// ```text
/// call!(static me.author.ClassName.methodName(int(arg1), String(arg2), java.lang.Int(arg3)) -> int)
///                Primitive type parameter --->\_______/                \_________________/     \_/
///                  Object type parameter ---------------------------------------^               |
///                     Return type        -------------------------------------------------------^
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
/// You can also pass a [`Class object`][jni::objects::JClass] instead of the full ClassPath,
/// as if it was an *object method*.
/// ```ignore
/// let class = env.get_object_class(obj).unwrap();
/// call!(static class.methodName() -> void);
/// ```
/// 
/// ## Explicit `JNIEnv`
/// 
/// The macro implicitly gets the [`JNIEnv`][jni::JNIEnv] from a thread-local stack.
/// If a different value is preffered, it can be specified at the beginning:
/// ```ignore
/// call!(env=> ...)
/// ```
/// where `env` can be any expression that resolves to a [`&mut JNIEnv`][jni::JNIEnv].
/// 
/// ## Types
/// 
/// The function call has *argument* and *return* **types**.
/// 
/// A type can be a **[Java Primitive](https://docs.oracle.com/javase/tutorial/java/nutsandbolts/datatypes.html)**,
/// a **[Rust Primitive][std::primitive]** (which is transmuted to the corresponding Java Primitive),
/// a **Java Class**,
/// or an **Array** of one of the previous types (the type wrapped in *brackets* `[]`).
/// 
/// All of the above types can be wrapped in [`Option`].
/// This allows the value to be `null` without causing an error.
/// *Primitives* can't be `null` in Java, only *Objects* can.
/// The macro solves this problem by using a **Primitive Class** (e.g. `java.lang.Boolean`)
/// instead of the *primitive* itself in the call.
/// This can be used when the Type in the Java function declaration has the `@Nullable` annotation,
/// or the function documents that it can accept or return a `null` value.
/// 
/// Arrays can be **multi-dimensional** (with unlimited dimensions).
/// Inner types of the array can also be wrapped with [`Option`].
/// 
/// > **Note**: When returning an **Array**, the actual rust type is `ObjectArray`.
///   See the [return section][call!#return] for more details.
/// 
/// For the class `java.lang.String`, use the Rust type [`String`] instead.
/// Using `java.lang.String` will use the Rust type [`JString`][jni::objects::JString].
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
/// Option<[Option<int>]> // nullable array of nullable objects
/// ```
/// 
/// <br><br>
/// > In the sections below, the use of `T` or `Type` means that it can accept any of the types declared above.
///
/// ## Arguments
///
/// The arguments of the method call are placed inside perentheses after the method name.
/// The syntax is simple: `type(value)`.
///
/// All arguments have a **type** (see the [types section][call!#types]),
/// followed by a **value** (wrapped in parenthesis).
/// 
/// Argument **values** are converted to a [**Java Value**][ez_jni::ToJValue] so that they can be passed to the *JNI call*.
/// 
/// [`Option`] is not allowed in arguments, except if it is within an **Array** (e.g. `[Option<String>]`).
/// This is to have better *readability* of the types in the arguments.
/// If you have an [`Option`] to pass as an argument (e.g. `Option<String>`),
/// manually convert it [to an object][ez_jni::ToObject] or use the [null keyword][call!#null-keyword].
// TODO: deprecate the use of option and nul keyword
/// 
/// #### Null keyword
/// 
/// Because [`String`] arguments only accept Rust strings (which can't be **null**),
/// the macro creates a *custom keyword* `null` for the argument values.
/// This is only usable for *Object* types and can't be used within array literals.
/// 
/// Example of `null` argument value:
/// ```ignore
/// String(null)
/// ```
///
/// ## Return
///
/// The arguments are followed by a *return arrow* `->` and the **return type**.
/// The return type may be `void`, one of the [`Types`](call!#types) above,
/// or a [`Result<T, CLASS>`] (where `T` is `void` or a [`Type`](call!#types),
/// and `CLASS` is any **Java Class** that extends [`Throwable`](https://docs.oracle.com/javase/8/docs/api/java/lang/Throwable.html)).
/// 
/// When a call returns an **Object Array** (e.g. `[java.lang.String]`),
/// the array is represented by [`ObjectArray`](https://docs.rs/ez_jni/latest/ez_jni/struct.ObjectArray.html).
/// This type is very much like a Rust array type (i.e. `Box<[T]>`, `&[T]`, etc),
/// but the array can only store *Object References*, along with an **element class**,
/// which is the **base class** that all Objects in the array must be an *instance of*.
/// 
/// ### Exceptions
///
/// Normally, when a call ***throws*** an `Exception`, the macro will cause a `panic!` with that `Exception`.
/// But when a *call* is marked to return a [`Result<T, CLASS>`] and the call *throws*,
/// the `Exception` will be caught and returned in the `Err` variant as a [`JavaException`](https://docs.rs/ez_jni/latest/ez_jni/struct.JavaException.html).
///
/// If the object was not of the *expected Class*, the `Exception` will *not be caught* and the program will `panic!`.
/// This is similar to how in Java, if the exception is not of any type of the *catch blocks*, the exception will not be caught.
/// You can use the *Class shorthands* for `Exception` or `Throwable` to skip the exception Class check.
/// 
/// ```ignore
/// call!(obj.method() -> Result<int, Exception>)
/// ```
#[proc_macro]
pub fn call(input: TokenStream) -> TokenStream {
    let call = syn::parse_macro_input!(input as MethodCall);
    call::jni_call(call).into()
}

/// Call a Java Class' constructor.
/// 
/// Has similar syntax as [*calling a static method*][call!#method-types], but there is no *method name* or *return value*.
/// 
/// ```ignore
/// new!(me.author.ClassName(int(arg1), String(arg2), java.lang.Int(arg3)))
/// ```
/// 
/// Can also take a custom [`JNIEnv`][jni::JNIEnv], like in [`call!`](call!#explicit-jnienv).
/// 
/// ### Exceptions
/// 
/// The constructor can be followed by **`throws`** and a **Java Class**.
/// This will make the constructor call return a [`Result<JObject, JavaException>`] instead,
/// and the exception will be caught if it occurs.
/// This is the same behavior as in [`call!`](call!#exceptions).
/// 
/// ```ignore
/// new!(me.author.ClassName() throws Exception)
/// ```
/// 
/// See [`JavaException`][https://docs.rs/ez_jni/latest/ez_jni/struct.JavaException.html].
#[proc_macro]
pub fn new(input: TokenStream) -> TokenStream {
    let call = syn::parse_macro_input!(input as ConstructorCall);
    call::jni_call_constructor(call).into()
}

/// Access or Set the value of a Java Field.
/// 
/// This macro will try to access the *Java field* directly.
/// If the field coudln't be accessed (because it doesn't exist, it's private, etc.),
/// then this macro will call a *Getter method* with that name.
/// This is similar to Kotlin does [properties for Java classses](https://kotlinlang.org/docs/java-interop.html#getters-and-setters).
/// 
/// # Syntax
/// 
/// ```text
/// field!(static me.author.ClassName.fieldName: String)
///     Callee -->\_________________/            \____/
///     Field type ---------------------------------^
/// ```
/// 
/// Use `static` if the field is a static field of a *Class*,
/// or ommit it if the field is of an *Object*.
/// 
/// The **callee** could be *Class Path* or *Object*,
/// following the same syntax as in [`call!`](call!#method-types).
/// 
/// The **type** follows the same syntax as in [`call!`](call!#types).
/// 
/// Can also take a custom [`JNIEnv`][jni::JNIEnv], like in [`call!`](call!#explicit-jnienv).
#[proc_macro]
pub fn field(input: TokenStream) -> TokenStream {
    let call = syn::parse_macro_input!(input as FieldCall);
    call::field(call).into()
}

/// Get the **class Object** for some Class.
/// The returned object has type [`java.lang.Class`](https://docs.oracle.com/javase/8/docs/api/java/lang/Class.html)
/// and is wrapped with [`JClass`][jni::objects::JClass].
/// 
/// Takes the *fully-qualified* **Class** as input.
/// 
/// ```ignore
/// let class: JClass = class!(me.author.Class);
/// ```
/// 
/// Can also take a custom [`JNIEnv`][jni::JNIEnv], like in [`call!`](call!#explicit-jnienv).
/// 
/// This is essentially just a shortcut to [`JNIEnv::find_class()`][jni::JNIEnv::find_class()].
#[proc_macro]
pub fn class(input: TokenStream) -> TokenStream {
    parse(input, |input| { Ok((
        input.parse()?, input.parse()?
    )) })
        .map(|(env, class)| call::get_class(env, class))
        .unwrap_tokens()
}

/// Get the *instance Object* of a **Singleton Class** by calling the `getInstance()` *static method* on the Class.
/// 
/// Takes the *fully-qualified* **Class** as input.
/// 
/// Can also take a custom [`JNIEnv`][jni::JNIEnv], like in [`call!`](call!#explicit-jnienv).
/// 
/// ```ignore
/// singleton!(me.author.Singleton);
/// singleton!(env=> me.author.Singleton);
/// ```
/// 
/// This is essentially just a shortcut to [`call!`] `Class.getInstance() -> Class`.
#[proc_macro]
pub fn singleton(input: TokenStream) -> TokenStream {
    parse(input, |input| { Ok((
        input.parse()?, input.parse()?
    )) })
        .map(|(env, class)| call::singleton_instance(env, class))
        .unwrap_tokens()
}

/// See [`ez_jni::FromObject`](https://docs.rs/ez_jni/latest/ez_jni/trait.FromObject.html).
#[proc_macro_derive(FromObject, attributes(class, field))]
pub fn from_object(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);

    match item_from_derive_input(input) {
        Either::Left(st) => object::derive_struct(st),
        Either::Right(enm) => object::derive_enum(enm),
    }.unwrap_tokens()
}

/// Print output. See [`std::println!`].
/// 
/// In Android, printing to `STDOUT` does not work because apparently it redirects to `/dev/null`.
/// This macro will instead crate a String and send it to `android.util.Log`.
/// 
/// The caller must use this macro from a JNI context.
/// That is, this must be called from a [`jni_fn!`],
/// or a function called from a [`jni_fn!`].
/// 
/// Use this macro instead of [`std::println!`] everywhere.
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
    
    quote!{ ::ez_jni::utils::cfg_if::cfg_if! {
        if #[cfg(target_os = "android")] {
            ::ez_jni::utils::__println(format!(#input))
        } else {
            ::std::println!(#string)
        }
    } }.into()
}

/// Print error. See [`std::eprintln!`].
/// 
/// In Android, printing to `STDERR` does not work because apparently it redirects to `/dev/null`.
/// This macro will instead crate a String and send it to `android.util.Log`.
/// 
/// The caller must use this macro from a JNI context.
/// That is, this must be called from a [`jni_fn!`],
/// or a function called from a [`jni_fn!`].
/// 
/// Use this macro instead of [`std::eprintln!`] everywhere.
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
    
    quote!{ ::ez_jni::utils::cfg_if::cfg_if! {
        if #[cfg(target_os = "android")] {
            ::ez_jni::utils::__eprintln(format!(#input))
        } else {
            ::std::eprintln!(#string)
        }
    } }.into()
}

/// Parse anything :/
fn parse<T>(input: proc_macro::TokenStream, parser: impl FnOnce(syn::parse::ParseStream) -> syn::Result<T>) -> syn::Result<T> {
    Parser::parse(parser, input)
}

// -- Private: Macros that can only be used by the `ez_jni` crate.

/// Compile a `Java File` into a binary `Class file`.
/// 
/// The input is the path to the *java source root*,
/// and the *Java Class Path*,
/// which should reflect the path in the *java source*.
/// 
/// Outputs the resulting binary `Class file`.
/// Since this can only output the binary of *one* file,
/// Java files with *nested classes* are not supported.
/// 
/// ### Example
/// ```
/// static BYTE_CODE: &[u8] = ez_jni_macros::compile_java_class!("./src/", "me/marti/ezjni/RustPanic");
/// ```
/// 
/// This is only used internally used by `ez_jni::__throw::throw_panic()`.
#[doc(hidden)]
#[proc_macro]
pub fn compile_java_class(input: TokenStream) -> TokenStream {
    // let java_file_path = syn::parse_macro_input!(input as LitStr);
    let (java_root, class_path) = match Parser::parse(|input: syn::parse::ParseStream| {
        let java_root = input.parse::<LitStr>()?;
        input.parse::<Token![,]>()?;
        let class_path = input.parse::<LitStr>()?;
        Ok((java_root, class_path))
    }, input) {
        Ok(pair) => pair,
        Err(err) => return err.to_compile_error().into(),
    };

    private::compile_java_class(java_root.value(), &class_path.value()).unwrap_tokens()
}
