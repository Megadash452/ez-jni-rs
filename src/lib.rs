#[doc(hidden)]
/// Used only by [`jni_macros`]
pub mod __throw;
#[macro_use]
pub mod utils;
pub mod object;
extern crate self as ez_jni;

use jni::objects::JThrowable;
use jni::JNIEnv;
pub use jni_macros::*;
use object::FromObjectError;
use utils::get_string;

/// Allows converting *Java Exceptions* to Rust types that allow for better error handling.
/// 
/// ### Derive
/// This trait has a **derive macro** available from [`jni_macros`].
/// Use it on *structs* to indicate that only 1 specific Exception class is expected.
/// Use it on *enums* to expect different Exception classes (one for each variant).
/// 
/// **Attributes**:
/// - **`class`**: Specifies the *Class Path* of the Exception that the *struct or enum variant* expects.
/// - **`field`**: Use it on the struct's fields to assign their values by accessing an *Object's members*.
///   By default, the struct's field name and type is used to produce the JNI call.
///   Note: **name** and **call** mutually exclusive, and either one MUST be used if the field belongs to a *Tuple struct*.
///   - **`name`**: Use a different name for the Object's field lookup instead of the field's name.
///     Mutually exclusive with `call`.
///   - **`call`**: Instead of accessing a field, Call a *getter method* with this name.
///   - **`class`**: If the struct field's type is [`JObject`][jni::objects::JObject] require that it be of this class.
/// 
/// ```
/// # use ez_jni::FromException;
/// 
/// #[derive(FromException)]
/// #[class(java.lang.Exception)]
/// struct MyStError {
///     message: String
/// }
/// 
/// #[derive(FromException)]
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
    fn from_exception(env: &mut JNIEnv<'local>, exception: &JThrowable) -> Result<Self, FromObjectError>;
}

impl FromException<'_> for String {
    fn from_exception(env: &mut jni::JNIEnv, exception: &jni::objects::JThrowable) -> Result<Self, FromObjectError> {
        Ok(get_string(call!(exception.getMessage() -> java.lang.String), env))
    }
}

impl FromException<'_> for std::io::Error {
    fn from_exception(env: &mut JNIEnv, exception: &JThrowable) -> Result<Self, FromObjectError> {
        use std::io;
        let msg = get_string(call!(exception.getMessage() -> java.lang.String), env);
        
        let map = [
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
        
        let exception_class = env.get_object_class(exception)
            .expect("Failed to get Exception's class");
        let exception_class_str = get_string(call!(exception_class.getName() -> java.lang.String), env);
        
        for (class, error_kind) in map {
            if env.is_assignable_from(class, &exception_class).unwrap() {
                return Ok(Self::new(error_kind, msg))
            }
        }
        
        if env.is_assignable_from("java/io/IOException", &exception_class).unwrap() {
            return Ok(Self::other(format!("{exception_class_str}: {msg}")));
        }
        
        Err(FromObjectError::ClassMismatch { obj_class: exception_class_str, target_class: "java/io/IOException" })
    }
}
