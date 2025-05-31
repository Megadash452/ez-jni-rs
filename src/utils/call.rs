//! Contains helper functions for the macros in `/jni_macros/src/call.rs`.
use jni::{
    errors::{Error as JNIError, Result as JNIResult},
    objects::{JClass, JObject, JThrowable, JValue, JValueOwned},
    JNIEnv
};
use utils::first_char_uppercase;
use crate::{__throw::try_catch, FromException as _};
use super::{JNI_CALL_GHOST_EXCEPTION, MethodNotFound, FieldNotFound};


/// A wrapper for a `Class` value that will be used in a **JNI Call**.
#[derive(Clone, Copy)]
pub enum ClassRepr<'a, 'local> {
    /// The class is represented by a **ClassPath** in [`String`][str] form.
    /// An actual *Class Object* will be looked up in the *JNI Call*.
    String(&'static str /* FIXME: Maybe use 'a here? */),
    /// The class is represented by a *Class Object* that has already been looked up
    /// and can be operated on with JNI.
    Object(&'a JClass<'local>),
}
impl ClassRepr<'_, '_> {
    fn get_class<'local>(self, env: &mut JNIEnv<'local>) -> JNIResult<JClass<'local>> {
        match self {
            Self::String(class) => env.find_class(class),
            // FIXME: try to not use new_local_ref() here
            Self::Object(class) => env.new_local_ref(class)
                .map(|obj| JClass::from(obj)),
        }
    }
}

/// Call a Java Object's method (`non-static`), given its **name**, **signature** and **arguments**.
/// 
/// The **signature** must be a [*method type signature*](https://docs.oracle.com/javase/8/docs/technotes/guides/jni/spec/types.html#type_signatures) as described in the JNI spec.
/// The **signature** defines the type of the **arguments** and **return** values.
/// 
/// This functions returns the *value returned* by the method, or an [`Exception`][JThrowable] if the method *throws*.
/// If an error other than an [`Exception`][JThrowable] occurs, the function will `panic!`.
pub fn call_obj_method<'local>(object: &JObject<'_>, name: &'static str, sig: &'static str, args: &[JValue], env: &mut JNIEnv<'local>) -> Result<JValueOwned<'local>, JThrowable<'local>> {
    handle_call_error(env.call_method(object, name, sig, args), Callee::Object(object), name, sig, env)
}
/// Same as [`call_obj_method()`] but for `static` methods.
pub fn call_static_method<'local>(class: ClassRepr<'_, '_>, name: &'static str, sig: &'static str, args: &[JValue], env: &mut JNIEnv<'local>) -> Result<JValueOwned<'local>, JThrowable<'local>> {
    handle_call_error(match class {
        ClassRepr::String(class_path) => {
            let class = env.find_class(class_path)
                .unwrap_or_else(|err| crate::__throw::handle_jni_call_error(err, env));
            env.call_static_method(class, name, sig, args)
        },
        ClassRepr::Object(class) => env.call_static_method(class, name, sig, args),
    }, class.into(), name, sig, env)
}

/// Creates a new [`Object`][JObject] by calling a *constructor* of a *class*.
/// 
/// This is to [`call_static_method()`], but does not take a *method name* and uses a different **JNI Call** under the hood.
pub fn create_object<'local>(class: ClassRepr<'_, '_>, sig: &'static str, args: &[JValue], env: &mut JNIEnv<'local>) -> Result<JObject<'local>, JThrowable<'local>> {
    handle_call_error(match class {
        ClassRepr::String(class) => env.new_object(class, sig, args),
        ClassRepr::Object(class) => env.new_object(class, sig, args),
    }, class.into(), "<init>", sig, env)
}

enum Callee<'a, 'local> {
    ClassPath(&'static str),
    Class(&'a JClass<'local>),
    Object(&'a JObject<'local>)
}
impl<'a, 'local> From<ClassRepr<'a, 'local>> for Callee<'a, 'local> {
    fn from(class: ClassRepr<'a, 'local>) -> Self {
        match class {
            ClassRepr::String(class) => Self::ClassPath(class),
            ClassRepr::Object(class) => Self::Class(class)
        }
    }
}

/// Handles a [`JNIError`] resulting from a **call** function by `panic!ing` or converting it to an [`Exception`][JThrowable].
/// `panic!s` if the [`Exception`][JThrowable] is [`MethodNotFound`](https://docs.oracle.com/javaee/7/api/javax/el/MethodNotFoundException.html).
/// 
/// If built in **debug** mode, may also output a hint on why the call *failed* and suggestions to fix it.
fn handle_call_error<'local, T>(
    call_result: JNIResult<T>,
    callee: Callee<'_, '_>,
    method_name: &'static str,
    method_sig: &'static str,
    env: &mut JNIEnv<'local>
) -> Result<T, JThrowable<'local>> {
    #[cfg(debug_assertions)]
    let check_method_existence = |env: &mut JNIEnv<'local>| {
        let class = match callee {
            Callee::ClassPath(class) => env.find_class(class),
            Callee::Class(class) => env.new_local_ref(class).map(|class| JClass::from(class)),
            Callee::Object(object) => env.get_object_class(object),
        }.unwrap_or_else(|err| crate::__throw::handle_jni_call_error(err, env));
        crate::hints::check_method_existence(class, method_name, method_sig, env);
    };

    call_result.map_err(|err| match err {
        JNIError::MethodNotFound { .. } => {
            cfg_if::cfg_if! {
                if #[cfg(debug_assertions)] {
                    check_method_existence(env);
                }
            }
            crate::__throw::handle_jni_call_error(err, env)
        },
        JNIError::JavaException => {
            let exception = env.exception_occurred()
                .expect(JNI_CALL_GHOST_EXCEPTION);
            env.exception_clear().unwrap();

            // If Exception is MethodNotFound, panic instead.
            if let Ok(MethodNotFound) = MethodNotFound::from_exception(&exception, env) {
                cfg_if::cfg_if! {
                    if #[cfg(debug_assertions)] {
                        check_method_existence(env);
                    }
                }
                env.throw(exception)
                    .unwrap_or_else(|err| crate::__throw::handle_jni_call_error(err, env));
                crate::__throw::handle_jni_call_error(err, env)
            }

            exception
        },
        err => crate::__throw::handle_jni_call_error(err, env)
    })
}

/// Generates the **name** and **signature** of the *Getter Method* that is called if a Field was not found.
pub(super) fn getter_name_and_sig(field_name: &'static str, ty: &'static str) -> (String, String) { (
    format!("get{}", first_char_uppercase(field_name)),
    format!("(){ty}")
) }
/// Generates the **name** and **signature** of the *Setter Method* that is called if a Field was not found.
fn setter_name_and_sig(field_name: &'static str, ty: &'static str) -> (String, String) { (
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
    field_helper(name, ty,
        |env| env.get_field(object, name, ty),
        |env| {
            let (name, sig) = getter_name_and_sig(name, ty);
            env.call_method(object, name, sig, &[])
        },
        |env| env.get_object_class(object),
    env)
        .unwrap_or_else(|err| crate::__throw::handle_jni_call_error(err, env))
}
/// Like [`get_obj_field()`], but gets the value of a `static` field of a **Class**.
pub fn get_static_field<'local>(class: ClassRepr<'_, '_>, name: &'static str, ty: &'static str, env: &mut JNIEnv<'local>) -> JValueOwned<'local> {
    field_helper(name, ty,
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
        |env| class.get_class(env),
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
    field_helper(name, ty,
        |env| {
            env.set_field(object, name, ty, val)?;
            Ok(JValueOwned::Void)
        },
        |env| {
            let (name, sig) = setter_name_and_sig(name, ty);
            env.call_method(object, name, sig, &[val])
        },
        |env| env.get_object_class(object),
    env)
        .and_then(|val| val.v())
        .unwrap_or_else(|err| crate::__throw::handle_jni_call_error(err, env))
}
/// Like [`set_obj_field()`], but sets the value of a `static` field of a **Class**.
pub fn set_static_field<'local>(class: ClassRepr<'_, '_>, name: &'static str, ty: &'static str, val: JValue<'_, '_>, env: &mut JNIEnv<'local>) {
    field_helper(name, ty,
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
        |env| class.get_class(env),
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
    name: &'static str,
    ty: &'static str,
    field_op: impl FnOnce(&mut JNIEnv<'local>) -> JNIResult<JValueOwned<'local>>,
    method_op: impl FnOnce(&mut JNIEnv<'local>) -> JNIResult<JValueOwned<'local>>,
    get_class: impl FnOnce(&mut JNIEnv<'local>) -> JNIResult<JClass<'local>>,
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
            // Group the Error with the Class
            .map_err(|err| (
                err,
                get_class(env)
                    .unwrap_or_else(|err| crate::__throw::handle_jni_call_error(err, env))
            ))
            // Check Field and Getter Method failure
            .map_err(|(err, class)| {
                match &err {
                    JNIError::MethodNotFound { .. } => {
                        crate::hints::check_field_existence(class, name, ty, env)
                            .unwrap_or_else(|err| crate::__throw::handle_jni_call_error(err, env));
                    },
                    JNIError::JavaException => {
                        if let Some(MethodNotFound) = try_catch(env) {
                            crate::hints::check_field_existence(class, name, ty, env)
                                .unwrap_or_else(|err| crate::__throw::handle_jni_call_error(err, env));
                            // FIXME: need to rethrow the exception
                        }
                    },
                    err => { },
                };
                err
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
