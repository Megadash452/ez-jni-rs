use either::Either;
use jni::{objects::{JObject, JThrowable, JValue}, JNIEnv};
use jni_macros::{call, new};
use thiserror::Error;
use std::io;
use crate::{__throw::panic_uncaught_exception, utils::get_string};

#[derive(Debug, Error)]
pub enum FromObjectError {
    #[error("Object can't be NULL")]
    Null,
    #[error("The Class of the Object is not the target Class, or a descendant of the target Class {target_class}")]
    ClassMismatch { obj_class: String, target_class: &'static str },
    #[error("Could not find field {name:?} of type {ty} in class {target_class}; maybe its private?")]
    FieldNotFound { name: String, ty: String, target_class: String },
    // #[error("{0}")]
    // Other(String)
}

/// Allows converting a *Java Object* to a Rust type by reading the Object's data.
/// 
/// ### Derive
/// This trait has a **derive macro** available from [`jni_macros`].
/// Use it on *structs* to indicate that only 1 specific Class is expected.
/// Use it on *enums* to expect different Classes (one for each variant).
/// 
/// **Attributes**:
/// - **`class`**: Specifies the *Class Path* that the *struct or enum variant* expects.
/// - **`field`**: Use it on the struct's fields to assign their values by accessing an *Object's members*.
///   By default, the struct's field name and type is used to produce the JNI call, but this can be changed.
///   Note: **name** and **call** mutually exclusive, and either one MUST be used if the field belongs to a *Tuple struct*.
///   - **`name`**: Use a different name for the Object's field lookup instead of the field's name.
///     Mutually exclusive with `call`.
///   - **`call`**: Instead of accessing a field, Call a *getter method* with this name.
///   - **`class`**: If the struct field's type is [`JObject`][jni::objects::JObject] require that it be of this class.
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
/// #[class(me.author.MyClass)]
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
    const PATH: &'static str;
    /// Construct a [`Self`] by reading data from a *Java Object*.
    /// Will [`panic!`] if any of the underlying JNI calls fail.
    fn from_object(object: &JObject, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError>;
}

pub trait ToObject<'local> {
    /// Create an instance of a Class by constructing an object from data in a *Rust struct*.
    /// Will [`panic!`] if any of the underlying JNI calls fail.
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local>;
}

impl<'local, T> FromObject<'local> for Option<T>
where T: FromObject<'local> {
    const PATH: &'static str = T::PATH;

    fn from_object(object: &JObject, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        if object.is_null() {
            Ok(None)
        } else {
            T::from_object(object, env)
                .map(|t| Some(t))
        }
    }
}
impl<'local, T> ToObject<'local> for Option<T>
where T: ToObject<'local> {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        match self {
            Some(t) => t.to_object(env),
            None => JObject::null()
        }
    }
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
            obj_class: get_string(call!(obj_class.getName() -> java.lang.String), env),
            target_class: path
        })
    }

    Ok(())
}

impl FromObject<'_> for String {
    const PATH: &'static str = "java/lang/String";

    /// Get a [`String`] from some random Object.
    /// 
    /// Don't use this function, it only exist for compatibility.
    /// Use [`get_string()`] instead because you will mostly be using it with [`JString`][jni::objects::JString].
    fn from_object(object: &JObject, env: &mut JNIEnv) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, Self::PATH, env)?;
        // Already checked that it is java.lang.String and is not NULL
        Ok(unsafe {
            env.get_string_unchecked(object.into())
                .unwrap_or_else(|err| panic!("ENV error while getting String: {err}"))
                .into()
        })
    }
}
impl<'local> ToObject<'local> for String {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        self.as_str().to_object(env)
    }
}
impl<'local> ToObject<'local> for str {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        env.new_string(self)
            .unwrap_or_else(|err| panic!("Error converting Rust string to Java String: {err}"))
            .into()
    }
}
impl<'local> ToObject<'local> for &str {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        (**self).to_object(env)
    }
}

impl FromException<'_> for String {
    fn from_exception(exception: &JThrowable, env: &mut jni::JNIEnv) -> Result<Self, FromObjectError> {
        let class = env.get_object_class(&exception)
            .unwrap_or_else(|err| panic!("Failed to get exception's class: {err}"));
        let class = get_string(call!(class.getName() -> java.lang.String), env);

        let msg = get_string(call!(exception.getMessage() -> java.lang.String), env);

        Ok(format!("{class}: {msg}"))
    }
}

impl FromObject<'_> for std::io::Error {
    const PATH: &'static str = "java/io/IOException";

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
        let class_str = get_string(call!(class.getName() -> java.lang.String), env);

        let msg = get_string(call!(object.getMessage() -> java.lang.String), env);

        // All classes in map extend java.io.IOException.
        // Check this before the classes in map to avoid a bunch of pointless JNI calls
        if !env.is_instance_of(object, Self::PATH).unwrap() {
            return Err(FromObjectError::ClassMismatch { obj_class: class_str, target_class: Self::PATH });
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
            .unwrap_or(Self::PATH);

        let msg = self.to_string().to_object(env);

        env.new_object(class, "(Ljava.lang.String;)V", &[
            JValue::Object(&msg)
        ])
            .unwrap_or_else(|err| {
                panic_uncaught_exception(env, Either::Left(class), "ctor"); // Does nothing if there is no exception
                panic!("Error constructing {} object: {err}", Self::PATH)
            })
    }
}

impl FromException<'_> for std::io::Error {
    fn from_exception(exception: &JThrowable, env: &mut JNIEnv) -> Result<Self, FromObjectError> {
        <Self as FromObject>::from_object(exception, env)
    }
}

// Implementation for number types

impl FromObject<'_> for i8 {
    const PATH: &'static str = "java/lang/Byte";

    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, Self::PATH, env)?;
        Ok(call!(object.byteValue() -> byte))
    }
}
impl<'local> ToObject<'local> for i8 {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(java.lang.Byte(byte(*self)))
    }
}
impl FromObject<'_> for i16 {
    const PATH: &'static str = "java/lang/Short";

    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, Self::PATH, env)?;
        Ok(call!(object.shortValue() -> short))
    }
}
impl<'local> ToObject<'local> for i16 {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(java.lang.Short(short(*self)))
    }
}
impl FromObject<'_> for i32 {
    const PATH: &'static str = "java/lang/Integer";

    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, Self::PATH, env)?;
        Ok(call!(object.intValue() -> int))
    }
}
impl<'local> ToObject<'local> for i32 {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(java.lang.Integer(int(*self)))
    }
}
impl FromObject<'_> for i64 {
    const PATH: &'static str = "java/lang/Long";

    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, Self::PATH, env)?;
        Ok(call!(object.longValue() -> long))
    }
}
impl<'local> ToObject<'local> for i64 {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(java.lang.Long(long(*self)))
    }
}
impl FromObject<'_> for f32 {
    const PATH: &'static str = "java/lang/Float";

    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, Self::PATH, env)?;
        Ok(call!(object.floatValue() -> float))
    }
}
impl<'local> ToObject<'local> for f32 {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(java.lang.Float(float(*self)))
    }
}
impl FromObject<'_> for f64 {
    const PATH: &'static str = "java/lang/Double";

    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, Self::PATH, env)?;
        Ok(call!(object.doubleValue() -> double))
    }
}
impl<'local> ToObject<'local> for f64 {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(java.lang.Double(double(*self)))
    }
}

// TODO: missing unsigned integer types

// TODO: missing array types