//! These functions are used in [`call!`][ez_jni_macros::call!] and *similar macros*.
use std::{fmt::{Debug, Display}, sync::OnceLock};
use jni::{objects::GlobalRef};
use super::*;
use crate::{JavaException, utils::{JNI_CALL_GHOST_EXCEPTION, JniResultExt as _}};

/// Panics with a *Java Exception* instead of *Rust String message*.
#[track_caller]
pub fn panic_exception(ex: JavaException) -> ! {
    // The panic payload will be thrown to Java,
    // so the payload should be the exception itself.
    // DO NOT inject bactrace here. That is done in throw_panic().
    ::std::panic::panic_any::<GlobalRef>(ex.into())
}

/// Like [`panic_exception()`], but can take any [`Throwable`][JThrowable] Object.
/// 
/// This function `panic!s` if the **object** could not be thrown.
#[track_caller]
#[inline(always)]
pub fn panic_throwable(object: &JThrowable<'_>) -> ! {
    // NOTE: Unwrap Debug instead of JNI so that a potential Exception is not thrown and confuses the catcher.
    let env = crate::utils::get_env();
    let is_throwable = env.with_local_frame(0, |env| {
        // Keep reference of the Class Object to avoid potentially expensive lookup.
        static THROWABLE_CLASS: OnceLock<GlobalRef> = OnceLock::new();
        let class = THROWABLE_CLASS.get_or_init(|| {
            let class = env.find_class("java/lang/Throwable")
                .catch(env)
                .unwrap();
            env.new_global_ref(class)
                .catch(env)
                .unwrap()
        });

        env.is_instance_of(object, class)
    }).catch(env).unwrap();

    if !is_throwable {
        panic!("{}", crate::error::FromObjectError::ClassMismatch {
            obj_class: env.with_local_frame(0, |env| {
                env.get_object_class(object)
                    .map(|class| {
                        let val = crate::utils::call_obj_method(&class, "getName", "()Ljava/lang/String;", &[], env)
                            .unwrap_or_else(|err| panic!("Error calling java.lang.Class.getName(): {err}"))
                            .unwrap_or_else(|exception| panic!("Did not expect java.lang.Class.getName() to throw an Exception: {exception}"));
                        <String as crate::FromJValue>::from_jvalue_env(val.borrow(), env)
                            .unwrap_or_else(|err| panic!("Error reading value returned by java.lang.Class.getName(): {err}"))
                    })
            }).catch(env).unwrap(),
            target_class: Some("java.lang.Throwable".to_string()),
        })
    }

    std::panic::panic_any(env.new_global_ref(object).catch(env).unwrap())
}

/// Same as [JniError::panic], but has less operations.
#[track_caller]
#[doc(hidden)]
pub(crate) fn __panic_jni_error(error: jni::errors::Error) -> ! {
    match error {
        jni::errors::Error::JavaException => {
            let env = crate::utils::get_env();

            let object = JThrowable::from(env.with_local_frame_returning_local(0, |env| Ok({
                catch_throwable(env)
                    .expect(JNI_CALL_GHOST_EXCEPTION)
                    .into()
            })).catch(env).unwrap());
            // SAFETY: catch_throwable() always returns an instance of Throwable.
            panic_throwable(&object)
        },
        error => panic!("{error}")
    }
}

/// Encapsulates a [`JNI Error`][jni::errors::Error] in a similar type that stores the `Exception` variant with the [`Exception Object`][crate::JavaException].
pub enum JniError {
    Exception(JavaException),
    Jni(jni::errors::Error),
}
impl JniError {
    pub(crate) fn new(error: jni::errors::Error, env: &mut JNIEnv<'_>) -> Self {
        match error {
            jni::errors::Error::JavaException => Self::Exception({
                let object = catch_throwable(env)
                    .expect(JNI_CALL_GHOST_EXCEPTION);
                JavaException::from_throwable(&object, env)
            }),
            error => Self::Jni(error)
        }
    }

    pub fn into_exception(self) -> Option<JavaException> {
        match self {
            JniError::Exception(ex) => Some(ex),
            JniError::Jni(_) => None,
        }
    }

    /// Handles the error returned by any [*JNI Call*](https://docs.rs/jni/0.21.1/jni/struct.JNIEnv.html#implementations) by `panic!king` with the error.
    /// 
    /// The error most likely is an `Exception`.
    /// If that's the case, this will `panic!` with the Exception object and will be *rethrown* to Java.
    /// Otherwise this panic with the [inner JNI Error][jni::errors::Error]'s message.
    #[track_caller]
    pub fn panic(self) -> ! {
        match self {
            Self::Exception(ex) => panic_exception(ex),
            Self::Jni(err) => panic!("{err}"),
        }
    }
}
impl std::error::Error for JniError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(match self {
            Self::Exception(ex) => ex,
            Self::Jni(error) => error,
        })
    }
}
impl Display for JniError {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Exception(ex) => <JavaException as Display>::fmt(ex, f),
            Self::Jni(err) => <jni::errors::Error as Display>::fmt(err, f),
        }
    }
}
impl Debug for JniError {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Exception(ex) => <JavaException as Debug>::fmt(ex, f),
            Self::Jni(err) => <jni::errors::Error as Debug>::fmt(err, f),
        }
    }
}

/// Checks if an `Exception` was *thrown* after calling a Java Method, and returns said `Exception`.
/// The `Exception` will be cleared so that it will be possible to do other jni_calls to handle the exception (e.g. rethrowing it).
/// 
/// Note: This function will only return `java.lang.Exception` and `panic!` if the Object is `java.lang.Error`.
/// This is according to the [Java documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/Error.html), that `java.lang.Error` should *never* be caught.
pub fn catch_exception(env: &mut JNIEnv<'_>) -> Option<JavaException> {
    let ex = catch_throwable(env)?;
    
    // Check if Object is `java.lang.Error` and `panic!`.
    if is_error(&ex, env) {
        panic_throwable(&ex)
    } else {
        Some(JavaException::from_throwable(&ex, env))
    }
}

/// Same as [`catch_exception()`], but can catch any `Throwable`, including `java.lang.Error`.
/// 
/// The returned Object will NEVER be `NULL`.
pub fn catch_throwable<'local>(env: &mut JNIEnv<'local>) -> Option<JThrowable<'local>> {
    // Don't use unwrap_jni() here, it will cause infinite recursion.
    let result = env.exception_occurred();
    // NOTICE: Can't call any function (including print) between the time when the exception is thrown and when `JNIEnv::exception_clear()` is called.
    // This means that if a method call could throw, the checks (call, type, and null) should be done AFTER the exception is caught.
    env.exception_clear().unwrap();
    let ex = match result {
        Ok(ex) => ex,
        Err(jni::errors::Error::JavaException) => unreachable!("Result always returns OK if the error was Exception."),
        Err(err) => panic!("{err}"),
    };

    if ex.is_null() {
        None
    } else {
        Some(ex)
    }
}

/// Checks if the **Object** is of *Class* `java.lang.Error`,
/// which should cause a `panic!`.
pub(crate) fn is_error<'local>(object: &JThrowable<'_>, env: &mut JNIEnv<'local>) -> bool {
    /// To make the check faster, **cache** the Class object in memory.
    static ERROR_CLASS: OnceLock<GlobalRef> = OnceLock::new();

    let error_class = ERROR_CLASS.get_or_init(|| {
        let class = env.find_class("java/lang/Error")
            .catch(env)
            .unwrap_jni();
        env.new_global_ref(class)
            .catch(env)
            .unwrap_jni()
    });

    env.is_instance_of(object, error_class)
        .catch(env)
        .unwrap_jni()
}
