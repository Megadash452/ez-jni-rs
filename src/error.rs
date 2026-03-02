use std::{borrow::Cow, cmp::Ordering, fmt::{Debug, Display}, panic::Location as StdLocation};
use either::Either;
use itertools::Itertools;
use jni::{JNIEnv, objects::JObject};
use nonempty::NonEmpty;
use thiserror::Error;
use crate::{__throw::{catch_throwable, panic_exception}, Class, FromObject, JValueType, JavaException, utils::{JNI_CALL_GHOST_EXCEPTION, JniResultExt as _, ResultExt as _, check_object_class, get_object_class_name}};

#[derive(Debug, Error)]
#[error("Could not find method {name}({params}) -> {return_ty} in class {target_class}; maybe its private?{caused_by}",
    params = comma_separated(params),
    caused_by = exception_cause(error.as_ref()),
)]
pub struct MethodNotFoundError {
    pub target_class: String,
    pub name: String,
    pub params: Box<[String]>,
    pub return_ty: String,
    #[source]
    pub(crate) error: Option<JavaException>,
}
impl MethodNotFoundError {
    pub const ERROR_CLASS: &str = "java/lang/NoSuchMethodError";
    pub const EXCEPTION_CLASS: &str = "java/lang/NoSuchMethodException";

    /// Create an instance of this type by reading from an [`Exception`][JavaException].
    /// Use this over [FromObject::from_object_env()] to avoid cloning the [`Exception`][JavaException].
    /// 
    /// As these types of Exceptions are expected to have a certain message format,
    /// this function will `panic!` if it fails to parse the Exception's message.
    pub(super) fn from_exception(exception: JavaException) -> Self {
        let (path, sig) = exception.message() // This Error class always has a message
            .split_once('(')
            .expect("NoSuchMethodError's message was malformed: did not contain opening parenthesis ('(') for parameters.");

        let mut components = path.split(|c| c == '.' || c == '/');
        let mut target_class = String::new();
        let mut name = String::new();

        while let Some(component) = components.next() {
            // Last component is the function name.
            if components.clone().next().is_none() {
                name = component.to_string();
                break;
            }
            // Don't prepend dot separator if this is the first component.
            if !target_class.is_empty() {
                target_class.push('.');
            }
            target_class.push_str(component);
        }

        if target_class.is_empty() || name.is_empty() {
            panic!("NoSuchMethod's message did not contain the Method path")
        }

        let (params, return_ty) = crate::hints::Type::parse_method_sig(sig)
            .expect("NoSuchMethodError's method signature is malformed");

        Self {
            target_class,
            name,
            params: params.into_iter()
                .map(|t| t.to_string())
                .collect(),
            return_ty: return_ty.to_string(),
            error: Some(exception),
        }
    }
    /// Create an instance of [`MethodNotFoundError`] given the data from a [jni::errors::Error::MethodNotFound] and a **target_class**.
    /// 
    /// **sig** is a JNI style *method* signature.
    /// 
    /// This function will `panic!` if the **sig** is malformed.
    pub(super) fn from_jni(target_class: String, name: String, sig: &str) -> Self {
        let (params, return_ty) = crate::hints::Type::parse_method_sig(sig)
            .expect("NoSuchMethodError contained malformed Java method signature");

        MethodNotFoundError {
            target_class,
            name,
            params: params.into_iter()
                .map(|t| t.to_string())
                .collect(),
            return_ty: return_ty.to_string(),
            error: None,
        }
    }
}
impl FromObject<'_> for MethodNotFoundError {
    fn from_object_env(object: &JObject<'_>, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        if object.is_null() {
            return Err(FromObjectError::Null);
        }
        if !env.is_instance_of(object, Self::ERROR_CLASS).catch(env)?
        && !env.is_instance_of(object, Self::EXCEPTION_CLASS).catch(env)? {
            return Err(FromObjectError::ClassMismatch {
                obj_class: get_object_class_name(object, env),
                target_classes: NonEmpty::from_vec([
                    Self::ERROR_CLASS.to_string(),
                    Self::EXCEPTION_CLASS.to_string()
                ].into()).unwrap(),
            });
        }

        let exception = JavaException::from_throwable(object.into(), env);
        Ok(Self::from_exception(exception))
    }
}

#[derive(Debug, Error)]
#[error("Could not find field \"{name}\" of type {ty} in class {target_class}; maybe its private?{}", exception_cause(error.as_ref()))]
pub struct FieldNotFoundError {
    pub target_class: String,
    pub name: String,
    pub ty: String,
    #[source]
    pub(crate) error: Option<JavaException>,
}
impl FieldNotFoundError {
    pub const ERROR_CLASS: &str = "java/lang/NoSuchFieldError";
    pub const EXCEPTION_CLASS: &str = "java/lang/NoSuchFieldException";
    
    /// Create an instance of this type by reading from an [`Exception`][JavaException].
    /// Use this over [FromObject::from_object_env()] to avoid cloning the [`Exception`][JavaException].
    /// 
    /// As these types of Exceptions are expected to have a certain message format,
    /// this function will `panic!` if it fails to parse the Exception's message.
    pub(super) fn from_exception(exception: JavaException) -> Self {
        todo!()
    }
    /// Create an instance of [`FieldNotFoundError`] given the data from a [jni::errors::Error::FieldNotFound] and a **target_class**.
    /// 
    /// **sig** is a JNI style *field* signature.
    /// 
    /// This function will `panic!` if the **sig** is malformed.
    pub(super) fn from_jni(target_class: String, name: String, sig: &str) -> Self {
        FieldNotFoundError {
            target_class,
            name,
            ty: crate::hints::Type::from_sig_type(&sig).to_string(),
            error: None,
        }
    }
}
impl FromObject<'_> for FieldNotFoundError {
    fn from_object_env(object: &JObject<'_>, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        if object.is_null() {
            return Err(FromObjectError::Null);
        }
        if !env.is_instance_of(object, Self::ERROR_CLASS).catch(env)?
        && !env.is_instance_of(object, Self::EXCEPTION_CLASS).catch(env)? {
            return Err(FromObjectError::ClassMismatch {
                obj_class: get_object_class_name(object, env),
                target_classes: NonEmpty::from_vec([
                    Self::ERROR_CLASS.to_string(),
                    Self::EXCEPTION_CLASS.to_string(),
                ].into()).unwrap(),
            });
        }

        let exception = JavaException::from_throwable(object.into(), env);
        Ok(Self::from_exception(exception))
    }
}

#[derive(Debug, Error)]
#[error("Could not find class \"{target_class}\"\n  From Exception:{exception}")]
pub struct ClassNotFoundError {
    pub target_class: String,
    #[source]
    pub(crate) exception: JavaException,
}
impl ClassNotFoundError {
    /// Create an instance of this type by reading from an [`Exception`][JavaException].
    /// Use this over [FromObject::from_object_env()] to avoid cloning the [`Exception`][JavaException].
    /// 
    /// As these types of Exceptions are expected to have a certain message format,
    /// this function will `panic!` if it fails to parse the Exception's message.
    pub(super) fn from_exception(exception: JavaException) -> Self {
        Self {
            target_class: exception.message().to_string(),
            exception,
        }
    }
}
impl Class for ClassNotFoundError {
    #[inline(always)]
    fn class() -> Cow<'static, str> {
        Cow::Borrowed("java/lang/ClassNotFoundException")
    }
}
impl FromObject<'_> for ClassNotFoundError {
    fn from_object_env(object: &JObject<'_>, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        check_object_class(object, <Self as Class>::class().as_ref(), env)?;
        let exception = JavaException::from_throwable(object.into(), env);
        Ok(Self::from_exception(exception))
    }
}

/// Encapsulates a [`JNI Error`][jni::errors::Error] in a similar type that stores the `Exception` variant with the [`Exception Object`][crate::JavaException].
pub enum JniError {
    Exception(JavaException),
    Jni(jni::errors::Error),
}
impl JniError {
    pub(crate) fn from_jni(error: jni::errors::Error, env: &mut JNIEnv<'_>) -> Self {
        match error {
            jni::errors::Error::JavaException => Self::Exception({
                let object = catch_throwable(env)
                    .expect(JNI_CALL_GHOST_EXCEPTION);
                JavaException::from_throwable(&object, env)
            }),
            error => Self::Jni(error)
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
impl __PanicErrorImpl for JniError {
    fn into_payload(self, _: &'static StdLocation<'static>, _: &mut JNIEnv<'_>) -> Either<String, JavaException> {
        match self {
            // Don't create a RustPanic instance here, use this Object directly.
            Self::Exception(ex) => Either::Right(ex),
            Self::Jni(err) => Either::Left(err.to_string())
        }
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

/// An error caused when attempting to instantiate a *Rust* type from a *Java Object* with [`FromObject::from_object()`].
/// 
/// Some errors can be caused by a *Java method* throwing an [`Exception`][JavaException].
/// If that is the case, the variant will contain an **exception** or **error** field.
#[derive(Debug, Error)]
pub enum FromObjectError {
    #[error("Attempted to instantiate Rust Type from a NULL Java Object")]
    Null,
    #[error("{}", match target_classes.len() {
        0 => unreachable!("Always match against at least 1 class"),
        1 => format!("Expected the Object to be the Class or a descendant of the Class \"{}\", but the actual Class of the Object is \"{obj_class}\"", &target_classes.head),
        2.. => format!("Object of Classs \"{obj_class}\" is not an instance of any of these classes: {target_classes:?}")
    })]
    ClassMismatch {
        obj_class: String,
        target_classes: NonEmpty<String>,
    },
    #[error("Method call or Field access returned incorrect Java value: Expected java type '{expected}', found java type '{actual}'")]
    IncorrectType {
        actual: JValueType,
        expected: JValueType,
    },
    #[error("{0}")]
    FieldNotFound(#[from] FieldNotFoundError),
    #[error("{0}")]
    MethodNotFound(#[from] MethodNotFoundError),
    #[error("{0}")]
    ClassNotFound(#[from] ClassNotFoundError),
    #[error("Could not instantiate Rust Type from element in Java Object Array at index {index}:\n    {error}")]
    ArrayElement {
        index: usize,
        #[source]
        error: Box<Self>
    },
    #[error("Couldn not convert Java Array to a Rust fixed-size Array: {}", match actual_len.cmp(expected_len) {
        Ordering::Equal => unreachable!(),
        Ordering::Less => format!("too long! Java Array contains {actual_len} elements, but the Rust Array can only hold {expected_len} elements"),
        Ordering::Greater => format!("too short! Java Array contains {actual_len} elements, but the Rust Array expects {expected_len} elements"),
    })]
    ArraySizeMismatch { expected_len: usize, actual_len: usize },
    #[error("{prefix}: {error}")]
    Other {
        prefix: String,
        #[source]
        error: Box<Self>,
    },
    #[error("{error}")]
    Unknown {
        #[source]
        error: JniError,
    }
}
impl FromObjectError {
    /// Convert from a [`JniError`] to a [`FromObjectError`] with a **prefix message**.
    pub(crate) fn from_jni_with_msg(msg: &'static str, error: JniError) -> Self {
        Self::Other {
            prefix: msg.to_string(),
            error: Box::new(Self::Unknown { error }),
        }
    }
}
impl __PanicErrorImpl for FromObjectError {
    fn into_payload(self, location: &'static StdLocation<'static>, env: &mut JNIEnv<'_>) -> Either<String, JavaException> {
        fn into_payload(error: FromObjectError, full_message: String, location: &'static StdLocation<'static>, env: &mut JNIEnv<'_>) -> Either<String, JavaException> {
            match error {
                FromObjectError::ArrayElement { .. }
                | FromObjectError::Other { .. } => unreachable!("These variants are only handled in the loop"),
                FromObjectError::Null
                | FromObjectError::ClassMismatch { .. }
                | FromObjectError::IncorrectType { .. }
                | FromObjectError::ArraySizeMismatch { .. } => Either::Left(full_message),
                FromObjectError::FieldNotFound(error) => Either::Right(JavaException::new_rust_panic(location, full_message, error.error, env)),
                FromObjectError::MethodNotFound(error) => Either::Right(JavaException::new_rust_panic(location, full_message, error.error, env)),
                FromObjectError::ClassNotFound(error) => Either::Right(JavaException::new_rust_panic(location, full_message, Some(error.exception), env)),
                FromObjectError::Unknown { error } => match error {
                    JniError::Exception(ex) => Either::Right(ex),
                    JniError::Jni(_) => Either::Left(full_message),
                },
            }
        }
        
        let full_message = self.to_string();
        // Iterative method ;)
        let mut current = self;
        loop {
            match current {
                FromObjectError::ArrayElement { error, .. }
                | FromObjectError::Other { error, .. } => current = *error,
                error => break into_payload(error, full_message, location, env),
            }
        }
    }
}
impl From<jni::errors::Error> for FromObjectError {
    fn from(error: jni::errors::Error) -> Self {
        let env = crate::utils::get_env();
        env.with_local_frame(0, |env| {
            Ok(Self::from(JniError::from_jni(error, env)))
        }).catch(env).unwrap_jni()
    }
}
impl From<JniError> for FromObjectError {
    fn from(error: JniError) -> Self {
        use jni::errors::Error as JNIError;
        match error {
            JniError::Exception(ex) => Self::from(ex),
            JniError::Jni(error) => match error {
                JNIError::JavaException => unreachable!("Exception was caught"),
                JNIError::WrongJValueType(expected, actual) => Self::IncorrectType {
                    actual: crate::hints::Type::from_sig_type(actual).into(),
                    expected: crate::hints::Type::from_sig_type(expected).into(),
                },
                JNIError::MethodNotFound { name, sig } => Self::MethodNotFound(
                    MethodNotFoundError::from_jni("<unknown>".to_string(), name, &sig)
                ),
                JNIError::FieldNotFound { name, sig } => Self::FieldNotFound(
                    FieldNotFoundError::from_jni("<unknown>".to_string(), name, &sig)
                ),
                JNIError::NullPtr(msg)
                | JNIError::NullDeref(msg) => Self::Other {
                    prefix: msg.to_string(),
                    error: Box::new(Self::Null),
                },
                _ => Self::Unknown { error: JniError::Jni(error) },
            },
        }
    }
}
impl From<JavaException> for FromObjectError {
    fn from(exception: JavaException) -> Self {
        if exception.is_instance_of(MethodNotFoundError::ERROR_CLASS)
        || exception.is_instance_of(MethodNotFoundError::EXCEPTION_CLASS) {
            Self::MethodNotFound(MethodNotFoundError::from_exception(exception))
        } else if exception.is_instance_of(FieldNotFoundError::ERROR_CLASS)
        || exception.is_instance_of(FieldNotFoundError::EXCEPTION_CLASS) {
            Self::FieldNotFound(FieldNotFoundError::from_exception(exception))
        } else if exception.is_instance_of(ClassNotFoundError::class()) {
            Self::ClassNotFound(ClassNotFoundError::from_exception(exception))
        } else {
            Self::Unknown { error: JniError::Exception(exception) }
        }
    }
}
impl From<MethodCallError> for FromObjectError {
    fn from(error: MethodCallError) -> Self {
        match error {
            MethodCallError::Null => Self::Null,
            MethodCallError::MethodNotFound(error) => Self::MethodNotFound(error),
            MethodCallError::ClassNotFound(error) => Self::ClassNotFound(error),
            MethodCallError::Unknown { error } => Self::Unknown { error },
        }
    }
}
impl From<FieldError> for FromObjectError {
    fn from(error: FieldError) -> Self {
        match error {
            FieldError::Null => Self::Null,
            FieldError::FieldNotFound(error) => Self::FieldNotFound(error),
            error @ FieldError::MethodNotFound { .. } => Self::Other {
                prefix: error.to_string(),
                error: Box::new(match error {
                    FieldError::MethodNotFound { error, .. } => Self::MethodNotFound(error),
                    _ => unreachable!(),
                })
            },
            FieldError::ClassNotFound(error) => Self::ClassNotFound(error),
            FieldError::Unknown { error } => Self::Unknown { error },
        }
    }
}
impl From<FromJValueError> for FromObjectError {
    fn from(err: FromJValueError) -> Self {
        match err {
            FromJValueError::IncorrectType { actual, expected } => Self::IncorrectType { actual, expected },
            FromJValueError::Object(error) => error,
        }
    }
}

// TODO: Implement ToObjectError

/// Error returned by [`FromJValue::from_jvalue()`].
#[derive(Debug, Error)]
pub enum FromJValueError {
    #[error("Expected java type '{expected}', found java type '{actual}'")]
    IncorrectType {
        actual: JValueType,
        expected: JValueType,
    },
    /// This variant occurs only when the [`JValue`][jni::objects::JValue] is [`Object`][jni::objects::JObject] and the [`FromObject`][crate::FromObject] call returned an error.
    #[error("{0}")]
    Object(#[from] FromObjectError)
}
impl __PanicErrorImpl for FromJValueError {
    fn into_payload(self, location: &'static StdLocation<'static>, env: &mut JNIEnv<'_>) -> Either<String, JavaException> {
        match self {
            error @ Self::IncorrectType { .. } => Either::Left(error.to_string()),
            Self::Object(error) => error.into_payload(location, env),
        }
    }
}
impl From<jni::errors::Error> for FromJValueError {
    fn from(error: jni::errors::Error) -> Self {
        let env = crate::utils::get_env();
        env.with_local_frame(0, |env| {
            Ok(Self::from(JniError::from_jni(error, env)))
        }).catch(env).unwrap_jni()
    }
}
impl From<JniError> for FromJValueError {
    fn from(error: JniError) -> Self {
        use jni::errors::Error as JNIError;
        match error {
            JniError::Exception(ex) => Self::Object(FromObjectError::from(ex)),
            JniError::Jni(error) => match error {
                JNIError::JavaException => unreachable!("Exception was caught"),
                JNIError::WrongJValueType(expected, actual) => Self::IncorrectType {
                    actual: crate::hints::Type::from_sig_type(actual).into(),
                    expected: crate::hints::Type::from_sig_type(expected).into(),
                },
                error => Self::Object(FromObjectError::Unknown { error: JniError::Jni(error) }),
            },
        }
    }
}

#[derive(Debug, Error)]
pub enum MethodCallError {
    #[error("Cannot call a method of a NULL Object")]
    Null,
    #[error("{0}")]
    MethodNotFound(#[from] MethodNotFoundError),
    #[error("{0}")]
    ClassNotFound(#[from] ClassNotFoundError),
    #[error("{error}")]
    Unknown { #[source] error: JniError }
}
impl __PanicErrorImpl for MethodCallError {
    fn into_payload(self, location: &'static StdLocation<'static>, env: &mut JNIEnv<'_>) -> Either<String, JavaException> {
        match self {
            error @ Self::Null => Either::Left(error.to_string()),
            Self::MethodNotFound(error) => Either::Right(JavaException::new_rust_panic(location, error.to_string(), error.error, env)),
            Self::ClassNotFound(error) => Either::Right(JavaException::new_rust_panic(location, error.to_string(), Some(error.exception), env)),
            Self::Unknown { error } => error.into_payload(location, env),
        }
    }
}
impl From<jni::errors::Error> for MethodCallError {
    fn from(error: jni::errors::Error) -> Self {
        let env = crate::utils::get_env();
        env.with_local_frame(0, |env| {
            Ok(Self::from(JniError::from_jni(error, env)))
        }).catch(env).unwrap_jni()
    }
}
impl From<JniError> for MethodCallError {
    fn from(error: JniError) -> Self {
        use jni::errors::Error as JNIError;
        match error {
            JniError::Exception(ex) => Self::from(ex),
            JniError::Jni(error) => match error {
                JNIError::JavaException => unreachable!("Exception was caught"),
                JNIError::MethodNotFound { name, sig } => Self::MethodNotFound(
                    MethodNotFoundError::from_jni("<unknown>".to_string(), name, &sig)
                ),
                error => Self::Unknown { error: JniError::Jni(error) },
            },
        }
    }
}
impl From<JavaException> for MethodCallError {
    fn from(exception: JavaException) -> Self {
        if exception.is_instance_of(MethodNotFoundError::ERROR_CLASS)
        || exception.is_instance_of(MethodNotFoundError::EXCEPTION_CLASS) {
            Self::MethodNotFound(MethodNotFoundError::from_exception(exception))
        } else if exception.is_instance_of(ClassNotFoundError::class()) {
            Self::ClassNotFound(ClassNotFoundError::from_exception(exception))
        } else {
            Self::Unknown { error: JniError::Exception(exception) }
        }
    }
}

#[derive(Debug, Error)]
pub enum FieldError {
    #[error("Cannot access a Field of a NULL Object")]
    Null,
    #[error("{0}")]
    FieldNotFound(#[from] FieldNotFoundError),
    #[error("{}", match cause {
        Some(field_error) => format!("{field_error}, but also failed calling getter/setter: {error}"),
        None => error.to_string(),
    })]
    MethodNotFound {
        cause: Option<FieldNotFoundError>,
        #[source]
        error: MethodNotFoundError,
    },
    #[error("{0}")]
    ClassNotFound(#[from] ClassNotFoundError),
    #[error("{error}")]
    Unknown { #[source] error: JniError }
}
impl __PanicErrorImpl for FieldError {
    fn into_payload(self, location: &'static StdLocation<'static>, env: &mut JNIEnv<'_>) -> Either<String, JavaException> {
        match self {
            error @ Self::Null => Either::Left(error.to_string()),
            Self::FieldNotFound(error) => Either::Right(JavaException::new_rust_panic(location, error.to_string(), error.error, env)),
            err @ Self::MethodNotFound { .. } => {
                let message = err.to_string();
                let (cause, method_error) = match err {
                    Self::MethodNotFound { cause, error } => (cause, error),
                    _ => unreachable!(),
                };
                let field_exception = cause
                    .map(|field_error| JavaException::new_rust_panic(location, field_error.to_string(), field_error.error, env));
                let method_exception = JavaException::new_rust_panic(location, method_error.to_string(), field_exception, env);

                Either::Right(JavaException::new_rust_panic(location, message, Some(method_exception), env))
            },
            Self::ClassNotFound(error) => Either::Right(JavaException::new_rust_panic(location, error.to_string(), Some(error.exception), env)),
            Self::Unknown { error } => error.into_payload(location, env),
        }
    }
}
impl From<jni::errors::Error> for FieldError {
    fn from(error: jni::errors::Error) -> Self {
        let env = crate::utils::get_env();
        env.with_local_frame(0, |env| {
            Ok(Self::from(JniError::from_jni(error, env)))
        }).catch(env).unwrap_jni()
    }
}
impl From<JniError> for FieldError {
    fn from(error: JniError) -> Self {
        use jni::errors::Error as JNIError;
        match error {
            JniError::Exception(ex) => Self::from(ex),
            JniError::Jni(error) => match error {
                JNIError::JavaException => unreachable!("Exception was caught"),
                JNIError::MethodNotFound { name, sig } => Self::MethodNotFound {
                    cause: None,
                    error: MethodNotFoundError::from_jni("<unknown>".to_string(), name, &sig)
                },
                error => Self::Unknown { error: JniError::Jni(error) },
            },
        }
    }
}
impl From<JavaException> for FieldError {
    fn from(exception: JavaException) -> Self {
        if exception.is_instance_of(MethodNotFoundError::ERROR_CLASS)
        || exception.is_instance_of(MethodNotFoundError::EXCEPTION_CLASS) {
            Self::MethodNotFound {
                cause: None,
                error: MethodNotFoundError::from_exception(exception),
            }
        } else if exception.is_instance_of(FieldNotFoundError::ERROR_CLASS)
        || exception.is_instance_of(FieldNotFoundError::EXCEPTION_CLASS) {
            Self::FieldNotFound(FieldNotFoundError::from_exception(exception))
        } else if exception.is_instance_of(ClassNotFoundError::class()) {
            Self::ClassNotFound(ClassNotFoundError::from_exception(exception))
        } else {
            Self::Unknown { error: JniError::Exception(exception) }
        }
    }
}

/// Provides the [`panic`][PanicError::panic()] method for **Error** types in [`ez_jni`],
/// allowing the panic *payload* to be either a [`String`] or [`JavaException`],
/// depending on what data the error type contains.
//
// NOTE: do not implement thsi trait directly, implement __PanicErrorImpl instead.
pub trait PanicError: __PanicErrorImpl {
    /// Trigger a `panic!` from the *location* this function was called.
    /// The panic *payload* may be a [`String`] message, or a [`JavaException`] if the error contains one.
    /// 
    /// The panic payload can be read with [`PanicType`][crate::__throw::PanicType] to handle whether it is a [`String`] or [`JavaException`].
    /// If the error turns out to be a [`JavaException`], the Exception Object will be re-thrown to *Java* upon unwinding to the JVM.
    /// Otherwise this will panic with the [inner JNI Error][jni::errors::Error]'s message.
    /// 
    /// > Note: The implementation could `panic!` with an unexpected message/object if it failed to create the actual intended payload.
    #[track_caller]
    #[inline(always)]
    fn panic(self) -> ! {
        __panic_impl(|location, env| self.into_payload(location, env))
    }
}
impl<T> PanicError for T
where T: __PanicErrorImpl { }
/// Implementation of the [`PanicError::panic()`] caller method.
/// This sets up the arguments for the call of __into_payload()
#[doc(hidden)]
#[track_caller]
fn __panic_impl(get_payload: impl FnOnce(&'static StdLocation<'static>, &mut JNIEnv<'_>) -> Either<String, JavaException>) -> ! {
    let payload = {
        let env = crate::utils::get_env();
        let location = StdLocation::caller();

        // Here we push a new local frame to the JVM.
        // This is usually not done (especially when multiple Object references are returned),
        // but since we are panicking here, we are not returning anything.
        // Pushing the local frame helps with freeing all the Pbject references that were created within it.
        env.with_local_frame(0, |env| Ok({
            get_payload(location, env)
        })).catch(env).unwrap()
    };

    match payload {
        Either::Left(msg) => std::panic::panic_any(msg),
        Either::Right(ex) => panic_exception(ex),
    }
}

mod private {
    use super::{Either, JavaException, JNIEnv, StdLocation};

    /// This is the actual traits that implementors of [`PanicError`] must implement.
    pub trait __PanicErrorImpl: std::error::Error + Sized {
        /// Implementors of this method should not `panic!`
        // NOTE: does not need to track caller
        fn into_payload(self, location: &'static StdLocation<'static>, env: &mut JNIEnv<'_>) -> Either<String, JavaException>;
    }
}
use private::*;

/// Print a **list of items** to a string, where each item is separated by a *comma*.
#[allow(unstable_name_collisions)]
fn comma_separated<I: Display>(collection: impl IntoIterator<Item = I>) -> String {
    collection.into_iter()
        .map(|i| i.to_string())
        .intersperse(", ".to_string())
        .collect()
}

fn exception_cause(exception: Option<&JavaException>) -> String {
    match exception {
        Some(ex) => format!("\n  From Exception: {ex}"),
        None => String::new(),
    }
}

// TODO: Test that MethodNotFound and FieldNotFound can actually parse their respective exceptions
#[cfg(test)]
mod tests {
    #[test]
    fn method_not_found() {
        todo!()
    }
}