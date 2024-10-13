//! # EZ JNI
//! 
//! This library helps with creating Rust libraries or programs that can *use* Java code or be *used by* Java code.
//! 
//! If you are not familiar with the *Java Native Interface*, take a look at the [JNI bindings](https://crates.io/crates/jni).
//! 
//! ## Using Java from Rust
//! 
//! The [`call!`] macro is used to call Java methods.
//! Specify the **name** of the method, as well as the *Class* if it is a `static method`, or the object (within parenthesis) the method belongs to.
//! Then pass in **parameters** by specifying the *Java Type*, and the value (within parenthesis).
//! The call will handle `null` return values and thrown `exceptions` if you use `Option` or `Result` (respectively) as the return type.
//! 
//! The [`new!`] macro can create new Object by calling the constructor.
//! Specify the *Class* of the object you want to construct, and pass specify the **parameters**.
//! You can also handle `exceptions` by using `throws` after the parameters and specifying an **error type**.
//! The syntax is very similar to [`call!`].
//! 
//! ## Using Rust from Java
//! 
//! A Rust or C function that can be called from Java must be named in a specific way
//! and have a similar signature as the method on the Java side
//! (see [the JNI doc](https://docs.rs/jni/0.21.1/jni/#getting-started) for more details).
//! The [`jni_fn!`] macro takes a Rust function (with *Java Types* instead of Rust types)
//! and makes it into one that can be called from Java.
//! 
//! These `jni_fns` are exported in the binary and serve as entrypoints to Rust from Java.
//! The idea is similar to how Rust library crates have functions and types that are exported and used by other Rust packages.
//! 
//! ## Android and printing
//! 
//! Using the regular [`std::print`] (or any of its variants) will not work in Android
//! because it seems that **stdout** and **stderr** redirect to `/dev/null` (for some reason).
//! To get around this, replace those macros with the ones in this crate,
//! which will call the right method to print in Android.
//! 
//! ```
//! use ez_jni::{println, eprintln};
//! ```
//! 
//! The downside is that the macro requires [`env`](jni::JNIEnv) to be a variable/argument that the macro can access,
//! so you will only be able to print from functions that have the `env` passed into them.

#[doc(hidden)]
/// Used only by [`ez_jni_macros`]
pub mod __throw;
#[macro_use]
pub mod utils;
mod object;
extern crate self as ez_jni;

pub use ez_jni_macros::*;
pub use object::*;
