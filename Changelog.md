<!-- Check the total difference between 2 commits with this command: `git difftool older_commit^..newer_commit` -->

# Changelog for ez_jni / ez-jni-rs

# 0.7.1

Fixed print functions to use `String` instead of `java.lang.String`.

# 0.7.0

## API Changes

* #### Changed
  * Macros no longer assume that `JNIEnv` exists as a local variable named `env`.
    Instead, the `JNIEnv` is taken from a `ThreadLocal` stack.
  * Class `java.lang.String` no longer converts to/from a *Rust* **String**; it stays as an **Object**.
  * `FromObect for &JObject` exists in favor of `FromObject for JObject`.
* #### Added
  * Caller of the macros can explicitly pass a custom `JNIEnv`. E.g. `macro!(env=> ___)`.
    * `from_object()` no longer takes a `JNIEnv`.
      Created `from_object_env()` to pass the custom `JNIEnv`.
  * Created `FromJValue` and `TojValue` traits for more direct conversion between *Java* and *Rust* values.
  * Created `get_env()` to obtain the `JNIEnv` in a JNI context.
* #### Fixed
  * Hint messages have better formatting.

## Internal Changes
<details>
  <summary>Internal Changes</summary>

  * Upgraded to Rust 2024 edition.
  * Using [indoc](https://crates.io/crates/indoc) crate for `compile_fail` tests.
  * `FromObject` no longer creates new *local reference* for `JObject`.
  * `FromObject/ToObject` use more flexible lifetimes.
  * Fragmented `src/utils.rs`.
  * Macros no longer create *named* local variables for JNI call **arguments**.
    The arguments are inlined into the call.
  * Macros use `ToJValue` and `FromJValue` for conversions of most **arguments** and **return** values respectively (except *multi-dimensional arrays*).
  * `jni_fn` puts function body in a *nested function* and converts arguments from *Java* to *Rust* in a separate closure.
  * `__throw::catch_throw()` was renamed to `run_with_jnienv` and it takes **ownership** of the `JNIEnv` instead of borrowing it.
  * Added `LOCAL_JNIENV_STACK` *thread-local* variable that stores a `JNIEnv` for each call of a **jni_fn** (calling *Rust from Java*).
  * `FromObject/FromException` derive macro ***always*** uses *getter method* as fallback if field is not found.
  * Added `Env` object in `jni_macros/src/call.rs`.
  * Added `convert_rvalue_to_jvalue()` for call **arguments** and `convert_jvalue_to_rvalue()` for call **return** value in `jni_macros/src/types.rs`.
  * Added `ClassRustType` to identify which *Rust type* a `Class` would convert to in `jni_macros/src/types.rs`.
  * Tests `call.rs` and `object.rs` use `run_with_jnienv()`.

</details>

---

# 0.6.1 - 0.6.3

* #### Fixed
  * Crate no longer compiles any *Java Classes* if it is built for *docs.rs*.
  * Modified Cargo manifest to show `License` in *crates.io* and fix *docs.rs* error.

# 0.6.0

## API Changes

* #### Added
  * New `field!`, `class!`, and `singleton!` macros.
  * Class can be a *Class Path* or a *Class Object* in `call!` and `new!` macros.
  * Can inject **Rust Backtrace** into **Java Stacktrace** when a `panic!` occurs in a `jni_fn`.
* #### Changed
  * *Callee* object can be any ungrouped expression in `call!` and `new!` macros.
    E.g. `call!(r.u.s.t.myMethod() -> void)` and `new!(ru.st())`.

## Internal Changes
<details>
  <summary>Internal Changes</summary>

  * Added sub-crate `ez_jni_general_utils`.
  * Added `get/set_field()` and variations in `src/utils.rs`.
  * Macros in `call.rs` create a *callee* value (&str or JObject).
  * `MethodCall` parses for function defintion pattern `(...) -> _`, allowing the *callee* object to not be grouped.
  * `ConstructorCall` parses for group pattern `(...)`, allowing any Rust expression as the *class*.
  * `Return` no longer handles the whole call tokens in `call.rs`, now it just converts the return type.
  * Added `CallRepr` to hold a Class as either a `String` or as an `Object` in `call.rs` and `utils.rs`.
  * Added `CallType` for parsing *static* or *object calls* in `call.rs`.
    * Removed `StaticMethod` and `ObjectMethod` in favor of `CallType` in `call.rs`.
    * Removed `Class::parse_with_trailing_method()` in `types.rs`.
  * Added `compile_java_class!` macro to store a compiled Java Class data in a binary **static variable**. 
  * Added `TokenTreeExt` in `jni_macros/src/utils/mod.rs`.
  * Added `step_until()` and variations in `jni_macros/src/utils/step.rs`.
  * Split module `__throw`.
  * Added `handle_exception_conversion()` to convert an *Object* to a *Rust type* in `__throw/call.rs`.
  * Removed `catch()` in `__throw.rs`.
  * Removed `panic_uncaught_exception()` in favor of `handle_jni_call_error()` in `__throw/call.rs`.
    * `panic_exception()` takes most of what `panic_uncaught_exception()` did in `__throw.rs`.
    * `panic_exception()` panics with the *exception message* instead of the *exception object* if this crate's panic hook was not set.
  * Split `catch_throw()` and `catch_throw_map()` to `catch_throw_main()` since they share most of the same code in `__throw/jni_fn.rs`.

</details>

---