//! Contains helper functions for the macros in `/jni_macros/src/call.rs`.
use std::{borrow::Borrow, ops::Deref};

use jni::{
    errors::{Error as JNIError, Result as JNIResult},
    objects::{JClass, JObject, JValue, JValueOwned},
    JNIEnv
};
use utils::first_char_uppercase;
use crate::{FromObject, JavaException, __throw::{catch_exception, try_catch}, utils::ResultExt};
use super::{JNI_CALL_GHOST_EXCEPTION, MethodNotFound, FieldNotFound};


/// A wrapper for a `Class` value that will be used in a **JNI Call**.
#[derive(Clone, Copy)]
pub enum ClassRepr<'a, 'obj> {
    /// The class is represented by a **ClassPath** in [`String`][str] form.
    /// An actual *Class Object* will be looked up in the *JNI Call*.
    String(&'a str),
    /// The class is represented by a *Class Object* that has already been looked up
    /// and can be operated on with JNI.
    Object(&'a JClass<'obj>),
}

/// A [`Cow`][std::borrow::Cow] specifically designed for [`JObject`].
enum CowObject<'a, T> {
    Borrowed(&'a T),
    Owned(T)
}
impl<T> Borrow<T> for CowObject<'_, T> {
    fn borrow(&self) -> &T {
        match self {
            Self::Borrowed(t) => t,
            Self::Owned(t) => &t
        }
    }
}
impl<T> AsRef<T> for CowObject<'_, T> {
    fn as_ref(&self) -> &T {
        match self {
            Self::Borrowed(t) => t,
            Self::Owned(t) => &t
        }
    }
}
impl<T> Deref for CowObject<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::Borrowed(t) => t,
            Self::Owned(t) => &t
        }
    }
}
impl<T> From<T> for CowObject<'_, T> {
    #[inline(always)]
    fn from(obj: T) -> Self {
        Self::Owned(obj)
    }
}
impl<'a, T> From<&'a T> for CowObject<'a, T> {
    #[inline(always)]
    fn from(obj: &'a T) -> Self {
        Self::Borrowed(obj)
    }
}

/// Call a Java Object's method (`non-static`), given its **name**, **signature** and **arguments**.
/// 
/// The **signature** must be a [*method type signature*](https://docs.oracle.com/javase/8/docs/technotes/guides/jni/spec/types.html#type_signatures) as described in the JNI spec.
/// The **signature** defines the type of the **arguments** and **return** values.
/// 
/// This functions returns the *value returned* by the method, or an [`Exception`][JavaException] if the method *throws*.
/// If an error other than an [`Exception`][JavaException] occurs (or is [`MethodNotFound`](https://docs.oracle.com/javaee/7/api/javax/el/MethodNotFoundException.html)),
/// the function will `panic!`.
pub fn call_obj_method<'local>(object: &JObject<'_>, name: &'static str, sig: &str, args: &[JValue], env: &mut JNIEnv<'local>) -> Result<JValueOwned<'local>, JavaException> {
    env.call_method(object, name, sig, args)
        .map_err(|error| handle_call_error(error, Callee::Object(object), name, sig, env))
}
/// Same as [`call_obj_method()`] but for `static` methods.
pub fn call_static_method<'local>(class: ClassRepr<'_, '_>, name: &'static str, sig: &str, args: &[JValue], env: &mut JNIEnv<'local>) -> Result<JValueOwned<'local>, JavaException> {
    match class {
        ClassRepr::String(class_path) => {
            let class = env.find_class(class_path)
                .unwrap_or_else(|err| crate::__throw::handle_jni_call_error(err, env));
            env.call_static_method(class, name, sig, args)
        },
        ClassRepr::Object(class) => env.call_static_method(class, name, sig, args),
    }.map_err(|error| handle_call_error(error, class.into(), name, sig, env))
}

/// Creates a new [`Object`][JObject] by calling a *constructor* of a *class*.
/// 
/// This is to [`call_static_method()`], but does not take a *method name* and uses a different **JNI Call** under the hood.
pub fn create_object<'local>(class: ClassRepr<'_, '_>, sig: &str, args: &[JValue], env: &mut JNIEnv<'local>) -> Result<JObject<'local>, JavaException> {
    match class {
        ClassRepr::String(class) => env.new_object(class, sig, args),
        ClassRepr::Object(class) => env.new_object(class, sig, args),
    }.map_err(|error| handle_call_error(error, class.into(), "<init>", sig, env))
}

pub(super) enum Callee<'a, 'obj> {
    ClassPath(&'a str),
    Class(&'a JClass<'obj>),
    Object(&'a JObject<'obj>)
}
impl<'a, 'obj> Callee<'a, 'obj> {
    fn is_static(&self) -> bool {
        match self {
            Self::ClassPath(_) => true,
            Self::Class(_) => true,
            Self::Object(_) => false,
        }
    }

    /// Get a **Class Object** from the *value*.
    /// 
    /// `panic!`s if there is an error.
    fn get_class<'local>(&self, env: &mut JNIEnv<'local>) -> CowObject<'a, JClass<'obj>>
    where 'local: 'obj {
        match self {
            Self::ClassPath(class) => CowObject::Owned(
                env.find_class(class)
                    .unwrap_or_else(|err| crate::__throw::handle_jni_call_error(err, env))
            ),
            Self::Class(class) => CowObject::Borrowed(*class),
            Self::Object(object) => CowObject::Owned(
                env.get_object_class(object)
                    .unwrap_or_else(|err| crate::__throw::handle_jni_call_error(err, env))
            ),
        }
    }
}
impl<'a, 'local> From<ClassRepr<'a, 'local>> for Callee<'a, 'local> {
    fn from(class: ClassRepr<'a, 'local>) -> Self {
        match class {
            ClassRepr::String(class) => Self::ClassPath(class),
            ClassRepr::Object(class) => Self::Class(class)
        }
    }
}

/// Handles a [`JNIError`] resulting from a **call** function by `panic!ing` or converting it to an [`Exception`][JavaException].
/// `panic!s` if the [`Exception`][JavaException] is [`MethodNotFound`](https://docs.oracle.com/javaee/7/api/javax/el/MethodNotFoundException.html).
/// This function is designed to `panic!` if the *JNI call* itself fails,
/// but not if the code inside the *java method* fails.
/// 
/// If built in **debug** mode, may also output a hint on why the call *failed* and suggestions to fix it.
#[track_caller]
fn handle_call_error<'local>(
    error: JNIError,
    callee: Callee<'_, '_>,
    method_name: &'static str,
    method_sig: &str,
    env: &mut JNIEnv<'local>
) -> JavaException {
    #[cfg(debug_assertions)]
    let check_method_existence = |env: &mut JNIEnv<'local>| {
        let class = callee.get_class(env);
        crate::hints::print_method_existence_report(&class, method_name, method_sig, callee.is_static(), env);
    };

    match error {
        JNIError::MethodNotFound { .. } => {
            cfg_if::cfg_if! {
                if #[cfg(debug_assertions)] {
                    check_method_existence(env);
                }
            }
            crate::__throw::handle_jni_call_error(error, env)
        },
        JNIError::JavaException => {
            let ex = catch_exception(env)
                .expect(JNI_CALL_GHOST_EXCEPTION);
            let exception = JavaException::from_object_env(&ex, env).unwrap_display();

            // Only panic if Exception is MethodNotFound
            if let Ok(MethodNotFound) = MethodNotFound::from_object_env(&ex, env) {
                cfg_if::cfg_if! {
                    if #[cfg(debug_assertions)] {
                        check_method_existence(env);
                    }
                }
                // Panic with the Exception as the payload
                crate::__throw::panic_exception(exception)
            }

            exception
        },
        err => crate::__throw::handle_jni_call_error(err, env)
    }
}

/// Generates the **name** and **signature** of the *Getter Method* that is called if a Field was not found.
pub(super) fn getter_name_and_sig(field_name: &'static str, ty: &str) -> (String, String) { (
    format!("get{}", first_char_uppercase(field_name)),
    format!("(){ty}")
) }
/// Generates the **name** and **signature** of the *Setter Method* that is called if a Field was not found.
fn setter_name_and_sig(field_name: &'static str, ty: &str) -> (String, String) { (
    format!("set{}", first_char_uppercase(field_name)),
    format!("({ty})V")
) }

/// Access a **field** of a *Java Object*, given the **name** of the field and its **type**.
/// 
/// The **type** must be a [*type signature*](https://docs.oracle.com/javase/8/docs/technotes/guides/jni/spec/types.html#type_signatures) as described in the JNI spec. 
/// 
/// If the field does not exists this will call a *getter method* with `get` prepended to the field's name.
/// E.g. if the field `java.lang.String message` did not exist,
/// then `java.lang.String getMessage()` will be called.
/// 
/// This function already handles any errors returned by the *JNI Call* with a `panic!`.
pub fn get_obj_field<'local>(object: &JObject<'_>, name: &'static str, ty: &'static str, env: &mut JNIEnv<'local>) -> JValueOwned<'local> {
    field_helper(Callee::Object(object), name, ty,
        |env| env.get_field(object, name, ty),
        |env| {
            let (name, sig) = getter_name_and_sig(name, ty);
            env.call_method(object, name, sig, &[])
        },
    env)
        .unwrap_or_else(|err| crate::__throw::handle_jni_call_error(err, env))
}
/// Like [`get_obj_field()`], but gets the value of a `static` field of a **Class**.
pub fn get_static_field<'local>(class: ClassRepr<'_, '_>, name: &'static str, ty: &'static str, env: &mut JNIEnv<'local>) -> JValueOwned<'local> {
    field_helper(Callee::from(class), name, ty,
        |env| match class {
            ClassRepr::String(class) => env.get_static_field(class, name, ty),
            ClassRepr::Object(class) => env.get_static_field(class, name, ty),
        },
        |env| {
            let (name, sig) = getter_name_and_sig(name, ty);
            match class {
                ClassRepr::String(class) => env.call_static_method(class, name, sig, &[]),
                ClassRepr::Object(class) => env.call_static_method(class, name, sig, &[]),
            }
        },
    env)
        .unwrap_or_else(|err| crate::__throw::handle_jni_call_error(err, env))
}

/// Sets the value of a **field** of a *Java Object*, given the **name** of the field and its **type**.
/// 
/// The **type** must be a [*type signature*](https://docs.oracle.com/javase/8/docs/technotes/guides/jni/spec/types.html#type_signatures) as described in the JNI spec. 
/// 
/// If the field does not exists this will call a *setter method* with `set` prepended to the field's name.
/// E.g. if the field `java.lang.String message` did not exist,
/// then `java.lang.String setMessage(val)` will be called.
/// 
/// This function already handles any errors returned by the *JNI Call* with a `panic!`.
pub fn set_obj_field(object: &JObject<'_>, name: &'static str, ty: &'static str, val: JValue<'_, '_>, env: &mut JNIEnv<'_>) {
    field_helper(Callee::Object(object), name, ty,
        |env| {
            env.set_field(object, name, ty, val)?;
            Ok(JValueOwned::Void)
        },
        |env| {
            let (name, sig) = setter_name_and_sig(name, ty);
            env.call_method(object, name, sig, &[val])
        },
    env)
        .and_then(|val| val.v())
        .unwrap_or_else(|err| crate::__throw::handle_jni_call_error(err, env))
}
/// Like [`set_obj_field()`], but sets the value of a `static` field of a **Class**.
pub fn set_static_field<'local>(class: ClassRepr<'_, '_>, name: &'static str, ty: &'static str, val: JValue<'_, '_>, env: &mut JNIEnv<'local>) {
    field_helper(Callee::from(class), name, ty,
        |env| {
            match class {
                ClassRepr::String(class) => {
                    // Lookup the class object, which is necessary for both operation on the Field AND looking up the Field (apparently).
                    let class = env.find_class(class)
                        .unwrap_or_else(|err| crate::__throw::handle_jni_call_error(err, env));
                    env.set_static_field(&class, (&class, name, ty), val)
                },
                ClassRepr::Object(class) => env.set_static_field(class, (class, name, ty), val),
            }?;
            Ok(JValueOwned::Void)
        },
        |env| {
            let (name, sig) = setter_name_and_sig(name, ty);
            match class {
                ClassRepr::String(class) => env.call_static_method(class, name, sig, &[val]),
                ClassRepr::Object(class) => env.call_static_method(class, name, sig, &[val]),
            }
        },
    env)
        .and_then(|val| val.v())
        .unwrap_or_else(|err| crate::__throw::handle_jni_call_error(err, env))
}

/// Performs a *Field operation* (i.e. accessing or setting value).
/// If the *Field operation* fails,
/// then this will do a *Method call* that would be equivalent to the *Field operation*.
/// 
/// If that also fails, The function will print some hints to the user about why a Field or Method was not found.
/// However, this only happens when building in DEBUG mode because this involves *A LOT* of Java calls.
pub(super) fn field_helper<'local>(
    callee: Callee<'_, '_>,
    name: &'static str,
    ty: &str,
    field_op: impl FnOnce(&mut JNIEnv<'local>) -> JNIResult<JValueOwned<'local>>,
    method_op: impl FnOnce(&mut JNIEnv<'local>) -> JNIResult<JValueOwned<'local>>,
    env: &mut JNIEnv<'local>
) -> JNIResult<JValueOwned<'local>> {
    #![allow(unused_variables)]
    // This closure is ran if the Field was not found in field_op.
    // method_op will be called, and if a Method for this field was still not found,
    // another function will be called to check why a Field or Method was not found (typo, private, etc.).
    //
    // This only happens in DEBUG builds.
    #[cfg(debug_assertions)]
    let call_method_op = |env: &mut JNIEnv<'local>| {
        method_op(env)
            // Inspect call failure: Check Field and Getter Method failure
            .inspect_err(|err| {
                let class = callee.get_class(env);

                if let JNIError::MethodNotFound { .. } = err {
                    crate::hints::print_field_existence_report(&class, name, ty, callee.is_static(), env);
                } else if let JNIError::JavaException = err {
                    let exception = catch_exception(env)
                        .expect(JNI_CALL_GHOST_EXCEPTION);
                    if MethodNotFound::from_object_env(&exception, env).is_ok() {
                        crate::hints::print_field_existence_report(&class, name, ty, callee.is_static(), env);
                    }
                    // Rethrow the exception because this function will return the JNIError as it was recevied.
                    env.throw(exception)
                        .unwrap_or_else(|err| crate::__throw::handle_jni_call_error(err, env));
                }
            })
    };

    field_op(env)
        .or_else(|err| match err {
            JNIError::FieldNotFound { .. } => { cfg_if::cfg_if! {
                // Field does not exist, try calling a Getter
                if #[cfg(debug_assertions)] {
                    call_method_op(env)
                } else {
                    method_op(env)
                }
            } },
            JNIError::JavaException => {
                if let Some(FieldNotFound) = try_catch(env) {
                    // Try calling Getter/Setter if Field was not found
                    cfg_if::cfg_if! {
                        if #[cfg(debug_assertions)] {
                            call_method_op(env)
                        } else {
                            method_op(env)
                        }
                    }
                } else {
                    // Other unexpected exception
                    Err(JNIError::JavaException)
                }
            },
            err => Err(err)
        })
}
