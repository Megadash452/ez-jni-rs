use std::{borrow::Cow, cmp::Ordering, fmt::Display};
use itertools::Itertools;
use jni::{JNIEnv, objects::JObject};
use thiserror::Error;
use crate::{Class, FromObject, JValueType, JavaException, utils::{JniError, check_object_class}};

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
    /// Create an instance of this type by reading from an [`Exception`][JavaException].
    /// Use this over [FromObject::from_object_env()] to avoid cloning the [`Exception`][JavaException]
    /// 
    /// The caller must check that the [`Exception`][JavaException] is an instance of this type's [Class].
    /// Any failures will cause a `panic!`.
    /// 
    /// As these types of Exceptions are expected to have a certain message format,
    /// this function will `panic!` if it fails to parse the Exception's message.
    pub(super) fn from_exception(exception: JavaException) -> Self {
        todo!()
    }
    /// Create an instance of [`MethodNotFoundError`] given the data from a [jni::errors::Error::MethodNotFound] and a **target_class**.
    /// 
    /// **sig** is a JNI style *method* signature.
    /// 
    /// This function will `panic!` if the **sig** is malformed.
    pub(super) fn from_jni(target_class: String, name: String, sig: String) -> Self {
        let (params, return_ty) = crate::hints::Type::parse_method_sig(&sig)
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
impl Class for MethodNotFoundError {
    #[inline(always)]
    fn class() -> Cow<'static, str> {
        Cow::Borrowed("java/lang/NoSuchMethodError")
    }
}
impl FromObject<'_> for MethodNotFoundError {
    fn from_object_env(object: &JObject<'_>, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        check_object_class(object, <Self as Class>::class().as_ref(), env)?;
        Ok(Self::from_exception(JavaException::from_throwable(object.into(), env)))
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
    /// Create an instance of this type by reading from an [`Exception`][JavaException].
    /// Use this over [FromObject::from_object_env()] to avoid cloning the [`Exception`][JavaException]
    /// 
    /// The caller must check that the [`Exception`][JavaException] is an instance of this type's [Class].
    /// Any failures will cause a `panic!`.
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
    pub(super) fn from_jni(target_class: String, name: String, sig: String) -> Self {
        FieldNotFoundError {
            target_class,
            name,
            ty: crate::hints::Type::from_sig_type(&sig).to_string(),
            error: None,
        }
    }
}
impl Class for FieldNotFoundError {
    #[inline(always)]
    fn class() -> Cow<'static, str> {
        Cow::Borrowed("java/lang/NoSuchFieldError")
    }
}
impl FromObject<'_> for FieldNotFoundError {
    fn from_object_env(object: &JObject<'_>, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        check_object_class(object, <Self as Class>::class().as_ref(), env)?;
        Ok(Self::from_exception(JavaException::from_throwable(object.into(), env)))
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
    /// Use this over [FromObject::from_object_env()] to avoid cloning the [`Exception`][JavaException]
    /// 
    /// The caller must check that the [`Exception`][JavaException] is an instance of this type's [Class].
    /// Any failures will cause a `panic!`.
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
        Ok(Self::from_exception(JavaException::from_throwable(object.into(), env)))
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
    #[error("{}", match target_class {
        Some(target_class) => format!("Expected the Object to be the Class or a descendant of the Class \"{target_class}\", but the actual Class of the Object is \"{obj_class}\""),
        None => format!("The Class Object did not match any of the classes of the variants; Found Class \"{obj_class}\"")
    })]
    ClassMismatch { obj_class: String, target_class: Option<String> },
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
        prefix: &'static str,
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
            prefix: msg,
            error: Box::new(Self::Unknown { error }),
        }
    }
}
impl From<JniError> for FromObjectError {
    fn from(error: JniError) -> Self {
        use jni::errors::Error as JNIError;
        match error {
            JniError::Exception(ex) => Self::from(ex),
            JniError::Jni(error) => match error {
                JNIError::JavaException => unreachable!("Exception was caught"),
                JNIError::WrongJValueType(_, _) => unreachable!("Not using JValue in FromObject methods"),
                JNIError::MethodNotFound { name, sig } => {
                    let (params, return_ty) = crate::hints::Type::parse_method_sig(&sig)
                        .expect("JNI provided a malformed method signature");
                    Self::from(MethodNotFoundError {
                        target_class: "<unknown>".to_string(),
                        name,
                        params: params.into_iter()
                            .map(|ty| ty.to_string())
                            .collect(),
                        return_ty: return_ty.to_string(),
                        error: None,
                    })
                },
                JNIError::FieldNotFound { name, sig } => Self::from(FieldNotFoundError {
                    target_class: "<unkown>".to_string(),
                    name,
                    ty: crate::hints::Type::from_sig_type(&sig).to_string(),
                    error: None,
                }),
                JNIError::NullPtr(msg)
                | JNIError::NullDeref(msg) => Self::Other {
                    prefix: msg,
                    error: Box::new(Self::Null),
                },
                _ => Self::Unknown { error: JniError::Jni(error) },
            },
        }
    }
}
impl From<JavaException> for FromObjectError {
    fn from(exception: JavaException) -> Self {
        if exception.is_instance_of::<MethodNotFoundError>() {
            Self::MethodNotFound(MethodNotFoundError::from_exception(exception))
        } else if exception.is_instance_of::<FieldNotFoundError>() {
            Self::FieldNotFound(FieldNotFoundError::from_exception(exception))
        } else if exception.is_instance_of::<ClassNotFoundError>() {
            Self::ClassNotFound(ClassNotFoundError::from_exception(exception))
        } else {
            Self::Unknown { error: JniError::Exception(exception) }
        }
    }
}
impl From<FieldError> for FromObjectError {
    fn from(error: FieldError) -> Self {
        match error {
            FieldError::Null => Self::Null,
            FieldError::FieldNotFound(error) => Self::FieldNotFound(error),
            FieldError::MethodNotFound { error, .. } => Self::MethodNotFound(error),
            FieldError::ClassNotFound(error) => Self::ClassNotFound(error),
            FieldError::Unknown { error } => Self::Unknown { error },
        }
    }
}
// impl From<FromJValueError> for FromObjectError {
//     fn from(err: FromJValueError) -> Self {
//         Self::Other(err.to_string())
//     }
// }

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

#[derive(Debug, Error)]
pub enum FieldError {
    #[error("Cannot access a Field of a NULL Object")]
    Null,
    #[error("{0}")]
    FieldNotFound(#[from] FieldNotFoundError),
    #[error("{field_error}, but also failed calling getter/setter: {error}")]
    MethodNotFound {
        field_error: FieldNotFoundError,
        #[source]
        error: MethodNotFoundError,
    },
    #[error("{0}")]
    ClassNotFound(#[from] ClassNotFoundError),
    #[error("{error}")]
    Unknown { #[source] error: JniError }
}

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
