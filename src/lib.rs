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
pub mod __throw;
#[macro_use]
pub mod utils;
mod object;
mod jvalue;
mod hints;
mod types;
extern crate self as ez_jni;

pub use object::*;
pub use jvalue::*;
pub use types::*;
pub use ez_jni_macros::{jni_fn, call, new, field, class, singleton, FromObject, println, eprintln};
pub(crate) use ez_jni_macros::compile_java_class;

use std::{cell::RefCell, collections::LinkedList};
use jni::JNIEnv;

thread_local! {
    /// Holds a stack of [`JNIEnv`] that was aquired from native function call (a.k.a jni_fn).
    /// The [`JNIEnv`]s are unique to each thread, so we don't need to worry about mutable access with race conditions.
    /// 
    /// Every time a jni_fn is called, the first instruction is to push the [`JNIEnv`] to this stack,
    /// and the last instruction is to pop the stack.
    /// This should be safe in theory, but in case there is any Undefined Behavior I didn't think of,
    /// the jni_fn checks that the pointer of the [`JNIEnv`] it is about to pop to make sure it is the same that it pushed.
    /// 
    /// ## Lifetime
    /// 
    /// The [`JNIEnv`]s stored here have *'static* lifetime, but this is a lie because the object will be dropped when the jni_fn that pushed it pops it.
    /// 
    /// ## Why Linked List?
    /// 
    /// If this stack were implemented using a [`Vec`] (or similar structs) the [`JNIEnv`] values would moved when the list allocates more space.
    /// This would not happen using a Linked List; all the values will remain in the same memory until the value is removed.
    /// 
    /// This is necessary because the macros unsefely borrow the top [`JNIEnv`] in the stack,
    /// assuming the value will remain valid until they release it.
    /// However, if a [`JNIEnv`] is borrowed from the [`Vec`], and a Java function is called, and that function then calls a Rust function in the same thread,
    /// the new [`JNIEnv`] will be pushed to the [`Vec`] and it may have to allocate and move the values, making the first borrow become **invalid**.
    /// 
    /// This *global variable* may only be directly used by [`__throw::jni_fn::catch_throw_main`].
    /// Instead, use [`get_env()`][crate::utils::get_env()].
    static LOCAL_JNIENV_STACK: RefCell<LinkedList<JNIEnv<'static>>> = const { RefCell::new(LinkedList::new()) };
}
