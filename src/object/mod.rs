mod r#impl;
mod impl_array;

use either::Either;
use jni::{JNIEnv, objects::{JObject, JThrowable, JValue}};
use thiserror::Error;
use std::io;
use ez_jni_macros::call;
use crate::__throw::panic_uncaught_exception;

#[derive(Debug, Error)]
pub enum FromObjectError {
    #[error("Object can't be NULL")]
    Null,
    #[error("{}", match target_class {
        Some(target_class) => format!("Expected the Object to be the Class or a descendant of the Class \"{target_class}\", but the actual Class of the Object is \"{obj_class}\""),
        None => format!("The Class Object did not match any of the classes of the variants; Found Class \"{obj_class}\"")
    })]
    ClassMismatch { obj_class: String, target_class: Option<String> },
    #[error("Could not find field {name:?} of type {ty} in class {target_class}; maybe its private?")]
    FieldNotFound { name: String, ty: String, target_class: String },
    // #[error("{0}")]
    // Other(String)
}

/// Allows converting a *Java Object* to a Rust type by reading the Object's data.
/// 
/// ### Derive
/// This trait has a **derive macro** available from [`ez_jni_macros`].
/// Use it on *structs* to indicate that only 1 specific Class is expected.
/// Use it on *enums* to expect different Classes (one for each variant).
/// 
/// **Attributes**:
/// - **`class`**: Specifies the **Class Path** that the *struct or enum variant* expects (*optional* for the enum item).
/// - **`field`**: Specify certain properties to constrol how a *struct's field* is assigned by accessing a *Object's member* (field or getter).
///   By default, the struct's field name and type is used to produce the JNI call, but this can be changed.
///   Note: **name** and **call** are mutually exclusive, and either one MUST be used if the field belongs to a *Tuple struct*.
///   - **`name`**: Use a different name for the Object's field lookup instead of the field's name.
///     Mutually exclusive with `call`.
///   - **`call`**: Instead of accessing a field, Call a *getter method* with this name.
///   - **`class`**: If the struct field's type is [`JObject`][jni::objects::JObject] require that it be of this class.
///     This property is required for *non-primitives*.
/// 
/// ```
/// # use ez_jni::FromObject;
/// 
/// #[derive(FromObject)]
/// #[class(me.author.MyClass)]
/// struct MyClass {
///     // Implicitly calls gets field or calls getMessage()
///     message: String
/// }
/// 
/// #[derive(FromObject)]
/// #[class(me.author.MyClass)] // Optional
/// enum MyClasses {
///     #[class(me.author.MyClassDescendant)]
///     Descendant { message: String },
///     #[class(me.author.MyOtherClass)]
///     Other(#[field(name = message)] String),
///     #[class(me.author.MyFinalClass)]
///     Final(#[field(call = getMessage)] String),
/// }
/// ```
pub trait FromObject<'local>
where Self: Sized {
    /// Construct a [`Self`] by reading data from a *Java Object*.
    /// Will [`panic!`] if any of the underlying JNI calls fail.
    fn from_object(object: &JObject, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError>;
}

pub trait ToObject<'local> {
    /// Create an instance of a Class by constructing an object from data in a *Rust struct*.
    /// Will [`panic!`] if any of the underlying JNI calls fail.
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local>;
}



/// Allows converting *Java Exceptions* to Rust types that allow for better error handling.
/// 
/// # Derive
/// Uses the same derive syntax as [`FromObject`].
/// 
/// ```
/// # use ez_jni::FromException;
/// 
/// #[derive(FromException)]
/// #[class(java.lang.Exception)]
/// struct MyStError {
///     // Implicitly calls gets field or calls getMessage()
///     message: String
/// }
/// 
/// #[derive(FromException)]
/// #[class(java.lang.Exception)] // Optional
/// enum MyEnmError {
///     #[class(java.lang.NullPointerException)]
///     Null,
///     #[class(me.author.ElementExists)]
///     AlreadyExists(#[field(name = message)] String),
///     #[class(java.lang.Exception)]
///     Other(#[field(call = getMessage)] String),
/// }
/// ```
pub trait FromException<'local>
where Self: Sized {
    fn from_exception(exception: &JThrowable, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError>;
}

/// Does the required checks to ensure that a Java Object is valid.
fn object_check_boilerplate(object: &JObject, path: &'static str, env: &mut JNIEnv) -> Result<(), FromObjectError> {
    if object.is_null() {
        return Err(FromObjectError::Null)
    }
    
    let obj_class = env.get_object_class(object)
        .unwrap_or_else(|err| panic!("Failed to get Object's class: {err}"));
    
    if !env.is_instance_of(object, path).unwrap() {
        return Err(FromObjectError::ClassMismatch {
            obj_class: call!(obj_class.getName() -> String),
            target_class: Some(path.to_string())
        })
    }

    Ok(())
}

impl FromException<'_> for String {
    fn from_exception(exception: &JThrowable, env: &mut jni::JNIEnv) -> Result<Self, FromObjectError> {
        let class = env.get_object_class(&exception)
            .unwrap_or_else(|err| panic!("Failed to get exception's class: {err}"));
        let class = call!(class.getName() -> String);

        let msg = call!(exception.getMessage() -> Option<String>).unwrap_or_default();

        Ok(format!("{class}: {msg}"))
    }
}

/// Class that all Objects that can be converted to [`std::io::Error`] must be a descendant of.
static IO_ERROR_BASE_PATH: &str = "java/io/IOException";

impl FromObject<'_> for std::io::Error {
    fn from_object(object: &JObject, env: &mut JNIEnv) -> Result<Self, FromObjectError> {
        static MAP: &[(&str, io::ErrorKind)] = &[
            ("java/io/FileNotFoundException", io::ErrorKind::NotFound),
            ("java/nio/file/NoSuchFileException", io::ErrorKind::NotFound),
            ("java/nio/file/AccessDeniedException", io::ErrorKind::PermissionDenied),
            // (, io::ErrorKind::ConnectionRefused),
            // (, io::ErrorKind::ConnectionReset),
            // (, io::ErrorKind::ConnectionAborted),
            ("java/net/ConnectException", io::ErrorKind::NotConnected),
            ("java/net/BindException", io::ErrorKind::AddrInUse),
            ("java/net/NoRouteToHostException", io::ErrorKind::AddrNotAvailable),
            ("java/net/SocketException", io::ErrorKind::BrokenPipe),
            ("java/nio/file/FileAlreadyExistsException", io::ErrorKind::AlreadyExists),
            // (, io::ErrorKind::WouldBlock),
            ("java/lang/IllegalArgumentException", io::ErrorKind::InvalidInput),
            ("java/lang/IllegalFormatException", io::ErrorKind::InvalidData),
            ("java/lang/UnsupportedEncodingException", io::ErrorKind::InvalidData),
            ("java/lang/UTFDataFormatException", io::ErrorKind::InvalidData),
            ("java/nio/charset/CharacterCodingException", io::ErrorKind::InvalidData),
            ("java/nio/charset/MalformedInputException", io::ErrorKind::InvalidData),
            ("java/nio/charset/UnmappableCharacterException", io::ErrorKind::InvalidData),
            ("java/net/SocketTimeoutException", io::ErrorKind::TimedOut),
            ("org/apache/http/conn/ConnectTimeoutException", io::ErrorKind::TimedOut),
            ("java/io/WriteAbortedException", io::ErrorKind::WriteZero),
            ("java/io/InterruptedIOException", io::ErrorKind::Interrupted),
            // (, io::ErrorKind::Unsupported),
            ("java/io/EOFException", io::ErrorKind::UnexpectedEof),
            // (, io::ErrorKind::OutOfMemory),
        ];

        if object.is_null() {
            return Err(FromObjectError::Null);
        }
        
        let class = env.get_object_class(&object)
            .expect("Failed to get Object's class");
        let class_str = call!(class.getName() -> String);

        let msg = call!(object.getMessage() -> Option<String>).unwrap_or_default();

        // All classes in map extend java.io.IOException.
        // Check this before the classes in map to avoid a bunch of pointless JNI calls
        if !env.is_instance_of(object, IO_ERROR_BASE_PATH).unwrap() {
            return Err(FromObjectError::ClassMismatch { obj_class: class_str, target_class: Some(IO_ERROR_BASE_PATH.to_string()) });
        }
        
        for &(class, error_kind) in MAP {
            if env.is_instance_of(object, class).unwrap() {
                return Ok(Self::new(error_kind, msg));
            }
        }

        // Already checked that is java.io.IOException
        Ok(Self::other(format!("{class_str}: {msg}")))
    }
}
impl<'local> ToObject<'local> for std::io::Error {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        static MAP: &[(io::ErrorKind, &str)] = &[
            (io::ErrorKind::NotFound, "java/io/FileNotFoundException"),
            (io::ErrorKind::NotFound, "java/nio/file/NoSuchFileException"),
            (io::ErrorKind::PermissionDenied, "java/nio/file/AccessDeniedException"),
            (io::ErrorKind::ConnectionRefused, "java/net/ConnectException"),
            (io::ErrorKind::ConnectionReset, "java/net/ConnectException"),
            (io::ErrorKind::ConnectionAborted, "java/net/ConnectException"),
            (io::ErrorKind::NotConnected, "java/net/ConnectException"),
            (io::ErrorKind::AddrInUse, "java/net/BindException"),
            (io::ErrorKind::AddrNotAvailable, "java/net/NoRouteToHostException"),
            (io::ErrorKind::BrokenPipe, "java/net/SocketException"),
            (io::ErrorKind::AlreadyExists, "java/nio/file/FileAlreadyExistsException"),
            // (io::ErrorKind::WouldBlock),
            (io::ErrorKind::InvalidInput, "java/lang/IllegalArgumentException"),
            (io::ErrorKind::InvalidData, "java/lang/IllegalFormatException"),
            (io::ErrorKind::TimedOut, "java/net/SocketTimeoutException"),
            (io::ErrorKind::TimedOut, "org/apache/http/conn/ConnectTimeoutException"),
            (io::ErrorKind::WriteZero, "java/io/WriteAbortedException"),
            (io::ErrorKind::Interrupted, "java/io/InterruptedIOException"),
            // (io::ErrorKind::Unsupported),
            (io::ErrorKind::UnexpectedEof, "java/io/EOFException"),
            // (io::ErrorKind::OutOfMemory),
        ];

        let class = MAP.iter()
            .find(|(err, _)| self.kind() == *err)
            .map(|(_, class)| *class)
            .unwrap_or(IO_ERROR_BASE_PATH);

        let msg = self.to_string().to_object(env);

        env.new_object(class, "(Ljava.lang.String;)V", &[
            JValue::Object(&msg)
        ])
            .unwrap_or_else(|err| {
                panic_uncaught_exception(env, Either::Left(class), "ctor"); // Does nothing if there is no exception
                panic!("Error constructing {} object: {err}", IO_ERROR_BASE_PATH)
            })
    }
}

impl FromException<'_> for std::io::Error {
    fn from_exception(exception: &JThrowable, env: &mut JNIEnv) -> Result<Self, FromObjectError> {
        <Self as FromObject>::from_object(exception, env)
    }
}
