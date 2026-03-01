//! Contains helper functions for the macros in `/jni_macros/src/call.rs`.
use std::{borrow::Borrow, ops::Deref};
use ez_jni_macros::call;
use jni::{
    objects::{JClass, JObject, JValue, JValueOwned},
    JNIEnv
};
use utils::first_char_uppercase;
use crate::{__throw::is_error, Class, FromJValue, JavaException, error::{ClassNotFoundError, FieldError, FieldNotFoundError, JniError, MethodCallError, MethodNotFoundError}, utils::{JniResultExt as _, ResultExt as _}};

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
#[cfg_attr(not(debug_assertions), allow(unused))]
enum CowObject<'a, T> {
    Borrowed(&'a T),
    Owned(T)
}
impl<T> Borrow<T> for CowObject<'_, T> {
    #[inline(always)]
    fn borrow(&self) -> &T {
        match self {
            Self::Borrowed(t) => t,
            Self::Owned(t) => &t
        }
    }
}
impl<T> AsRef<T> for CowObject<'_, T> {
    #[inline(always)]
    fn as_ref(&self) -> &T {
        match self {
            Self::Borrowed(t) => t,
            Self::Owned(t) => &t
        }
    }
}
impl<T> Deref for CowObject<'_, T> {
    type Target = T;

    #[inline(always)]
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
pub fn call_obj_method<'local>(object: &JObject<'_>, name: &'static str, sig: &str, args: &[JValue], env: &mut JNIEnv<'local>) -> Result<Result<JValueOwned<'local>, JavaException>, MethodCallError> {
    call_helper(object.into(), name, sig, |env| {
        env.call_method(object, name, sig, args)
    }, env)
}
/// Same as [`call_obj_method()`] but for `static` methods.
pub fn call_static_method<'local>(class: ClassRepr<'_, '_>, name: &'static str, sig: &str, args: &[JValue], env: &mut JNIEnv<'local>) -> Result<Result<JValueOwned<'local>, JavaException>, MethodCallError> {
    call_helper(class.into(), name, sig, |env| {
        match class {
            ClassRepr::String(class_path) => {
                let class = env.find_class(class_path)?;
                env.call_static_method(class, name, sig, args)
            },
            ClassRepr::Object(class) => env.call_static_method(class, name, sig, args),
        }
    }, env)
}

/// Creates a new [`Object`][JObject] by calling a *constructor* of a *class*.
/// 
/// This is to [`call_static_method()`], but does not take a *method name* and uses a different **JNI Call** under the hood.
pub fn create_object<'local>(class: ClassRepr<'_, '_>, sig: &str, args: &[JValue], env: &mut JNIEnv<'local>) -> Result<Result<JObject<'local>, JavaException>, MethodCallError> {
    call_helper(class.into(), "<init>", sig, |env| {
        match class {
            ClassRepr::String(class) => env.new_object(class, sig, args),
            ClassRepr::Object(class) => env.new_object(class, sig, args),
        }
    }, env)
}

#[cfg_attr(not(debug_assertions), allow(unused))]
pub(super) enum Callee<'a, 'obj> {
    ClassPath(&'a str),
    Class(&'a JClass<'obj>),
    Object(&'a JObject<'obj>)
}
#[cfg_attr(not(debug_assertions), allow(unused))]
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
    fn get_class<'local>(&self, env: &mut JNIEnv<'local>) -> Result<CowObject<'a, JClass<'obj>>, JniError>
    where 'local: 'obj {
        match self {
            Self::ClassPath(class) => Ok(CowObject::Owned(env.find_class(class).catch(env)?)),
            Self::Class(class) => Ok(CowObject::Borrowed(*class)),
            Self::Object(object) => Ok(CowObject::Owned(env.get_object_class(object).catch(env)?)),
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
impl<'a, 'local> From<&'a JObject<'local>> for Callee<'a, 'local> {
    #[inline(always)]
    fn from(object: &'a JObject<'local>) -> Self {
        Self::Object(object)
    }
}

/// Handles a [`JNIError`] resulting from a **call** function by `panic!ing` or converting it to an [`Exception`][JavaException].
/// `panic!s` if the [`Exception`][JavaException] is [`MethodNotFound`](https://docs.oracle.com/javaee/7/api/javax/el/MethodNotFoundException.html).
/// This function is designed to `panic!` if the *JNI call* itself fails,
/// but not if the code inside the *java method* fails.
/// 
/// If built in **debug** mode, may also output a hint on why the call *failed* and suggestions to fix it.
#[cfg_attr(not(debug_assertions), allow(unused_variables))]
#[track_caller]
fn call_helper<'local, T>(
    callee: Callee<'_, '_>,
    method_name: &'static str,
    method_sig: &str,
    call: impl FnOnce(&mut JNIEnv<'local>) -> jni::errors::Result<T>,
    env: &mut JNIEnv<'local>
) -> Result<Result<T, JavaException>, MethodCallError> {
    match callee {
        Callee::Class(class) if class.is_null() => return Err(MethodCallError::Null),
        Callee::Object(obj) if obj.is_null() => return Err(MethodCallError::Null),
        _ => { },
    }

    let callee_class = callee.get_class(env)?.as_ref();
    let target_class = call!(env=> callee_class.getName() -> String);

    let result = call(env)
        .catch(env)
        .map(|t| Ok(t))
        .or_else(|err| match err {
            JniError::Jni(jni::errors::Error::MethodNotFound { name, sig })
                => Err(MethodCallError::MethodNotFound(MethodNotFoundError::from_jni(target_class, name, &sig))),
            JniError::Jni(error) => Err(MethodCallError::Unknown { error: JniError::Jni(error) }),
            JniError::Exception(exception) => {
                if exception.is_instance_of(MethodNotFoundError::ERROR_CLASS)
                || exception.is_instance_of(MethodNotFoundError::EXCEPTION_CLASS) {
                    Err(MethodCallError::MethodNotFound(MethodNotFoundError::from_exception(exception)))
                } else if exception.is_instance_of(ClassNotFoundError::class()) {
                    Err(MethodCallError::ClassNotFound(ClassNotFoundError::from_exception(exception)))
                } else if is_error(&exception, env) {
                    // Classes that represent a JVM Error should be returned as a JNI MethodCallError.
                    Err(MethodCallError::Unknown { error: JniError::Exception(exception) })
                } else {
                    // All other Exception classes are returned directly,
                    // treating it like the JNI call succeeded, but the method itself did not.
                    Ok(Err(exception))
                }
            },
        });

    // Check hints of why the method was not found (typo, private, etc.).
    //
    // This only happens in DEBUG builds.
    #[cfg(debug_assertions)] {
        (&result).inspect_err(|error| match error {
            MethodCallError::MethodNotFound { .. } => crate::hints::print_method_existence_report(callee_class, method_name, method_sig, callee.is_static(), env),
            _ => { },
        })
    };

    result
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
pub fn get_obj_field<'local>(object: &JObject<'_>, name: &'static str, ty: &'static str, env: &mut JNIEnv<'local>) -> Result<JValueOwned<'local>, FieldError> {
    field_helper(Callee::Object(object), name, ty,
        |env| env.get_field(object, name, ty),
        |env| {
            let (name, sig) = getter_name_and_sig(name, ty);
            env.call_method(object, name, sig, &[])
        },
    env)
}
/// Like [`get_obj_field()`], but gets the value of a `static` field of a **Class**.
pub fn get_static_field<'local>(class: ClassRepr<'_, '_>, name: &'static str, ty: &'static str, env: &mut JNIEnv<'local>) -> Result<JValueOwned<'local>, FieldError> {
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
pub fn set_obj_field(object: &JObject<'_>, name: &'static str, ty: &'static str, val: JValue<'_, '_>, env: &mut JNIEnv<'_>) -> Result<(), FieldError> {
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
        .map(|val| <() as FromJValue>::from_jvalue(val.borrow()).unwrap_display())
}
/// Like [`set_obj_field()`], but sets the value of a `static` field of a **Class**.
pub fn set_static_field<'local>(class: ClassRepr<'_, '_>, name: &'static str, ty: &'static str, val: JValue<'_, '_>, env: &mut JNIEnv<'local>) -> Result<(), FieldError> {
    field_helper(Callee::from(class), name, ty,
        |env| {
            match class {
                ClassRepr::String(class) => {
                    // Lookup the class object, which is necessary for both operation on the Field AND looking up the Field (apparently).
                    let class = env.find_class(class)?;
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
        .map(|val| <() as FromJValue>::from_jvalue(val.borrow()).unwrap_display())
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
    field_op: impl FnOnce(&mut JNIEnv<'local>) -> jni::errors::Result<JValueOwned<'local>>,
    // TODO: make optional?
    method_op: impl FnOnce(&mut JNIEnv<'local>) -> jni::errors::Result<JValueOwned<'local>>,
    env: &mut JNIEnv<'local>
) -> Result<JValueOwned<'local>, FieldError> {
    match callee {
        Callee::Class(class) if class.is_null() => return Err(FieldError::Null),
        Callee::Object(obj) if obj.is_null() => return Err(FieldError::Null),
        _ => { },
    }

    let callee_class = callee.get_class(env)?.as_ref();
    let target_class = call!(env=> callee_class.getName() -> String);

    let result = field_op(env)
        .catch(env)
        .or_else(|err| {
            // Only call method_op() if the error was FieldNotFound
            let field_access_error = match err {
                JniError::Jni(jni::errors::Error::FieldNotFound { name, sig })
                    => FieldNotFoundError::from_jni(target_class, name, &sig),
                JniError::Exception(exception)
                if exception.is_instance_of(FieldNotFoundError::ERROR_CLASS)
                || exception.is_instance_of(FieldNotFoundError::EXCEPTION_CLASS)
                    => FieldNotFoundError::from_exception(exception),
                err => return Err((err, None)),
            };

            method_op(env)
                .catch(env)
                // Bundle the FieldNotFoundError from field_op() with the error from method_op().
                .map_err(|err| (err, Some(field_access_error)))
        })
        // Previous returns the Error of field_op() if 2nd arg is None, and the Error of mathod_op() if 2nd arg is Some.
        .map_err(|(error, field_error)| match error {
            JniError::Jni(jni::errors::Error::MethodNotFound { name, sig })
            if field_error.is_some() => FieldError::MethodNotFound {
                cause: field_error,
                error: MethodNotFoundError::from_jni(target_class, name, &sig),
            },
            JniError::Exception(exception)
            if exception.is_instance_of(MethodNotFoundError::ERROR_CLASS)
            || exception.is_instance_of(MethodNotFoundError::EXCEPTION_CLASS)
            && field_error.is_some() => FieldError::MethodNotFound {
                cause: field_error,
                error: MethodNotFoundError::from_exception(exception),
            },
            JniError::Exception(exception)
            if exception.is_instance_of(ClassNotFoundError::class()) => {
                FieldError::ClassNotFound(ClassNotFoundError::from_exception(exception))
            },
            // If the error is FieldNotFound or MethodNotFound here it is not because field_op() or method_op() themselves failed,
            // but because some other method called by those caused it to fail.
            error => FieldError::Unknown { error },
        });

    // Check hints of why the field or method were not found (typo, private, etc.).
    //
    // This only happens in DEBUG builds.
    #[cfg(debug_assertions)] {
        (&result).inspect_err(|error| match error {
            FieldError::FieldNotFound(_)
            | FieldError::MethodNotFound { .. } => crate::hints::print_field_existence_report(callee_class, name, ty, callee.is_static(), env),
            _ => { },
        })
    };

    result
}
