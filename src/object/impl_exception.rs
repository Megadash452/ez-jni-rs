use crate::{__throw::get_jni_error_msg, utils::get_object_class_name};
use ez_jni_macros::new;
use jni::objects::GlobalRef;
use std::{fmt::{Debug, Display}, io, ops::Deref};

use super::*;

/// A wrapper around a [`GlobalRef`] of a [`JThrowable`] that implements the useful error traits.
///
/// Contains the **Class path** and the exception **message**.
pub struct JavaException {
    exception: GlobalRef,
    class: String,
    /// Some (very rare!) exception classes leave the **message** as `null`.
    /// In the rare case this happens, the **message** must be wrapped with [`Option`].
    /// 
    /// Use [`Self::message()`] to get a default string.
    message: Option<String>,
}
impl JavaException {
    /// The Exception object.
    pub fn object(&self) -> &GlobalRef { &self.exception }
    /// The **Class Path** of the Exception Object.
    pub fn class(&self) -> &str { &self.class }
    /// The **message** obtained from the Exception.
    /// 
    /// If the **message** is null, returns the **class name**.
    pub fn message(&self) -> &str {
        match &self.message {
            Some(msg) => msg,
            None => &self.class,
        }
    }

    /// Like [`Self::from_object()`], but allows any [] objects.
    /// 
    /// This function does *not* do any checks on its own,
    /// and will `panic!` if something is wrong.
    /// 
    /// Not for **`public`** use, as it can cause confusion.
    pub(crate) fn from_throwable(object: &JThrowable<'_>, env: &mut JNIEnv<'_>) -> Self {
        Self {
            class: get_object_class_name(object, env),
            message: call!(env=> object.getMessage() -> Option<String>),
            exception: env.new_global_ref(&object)
                .unwrap_or_else(|err| crate::__throw::handle_jni_call_error(err, env)),
        }
    }
}
impl AsRef<JObject<'static>> for JavaException {
    fn as_ref(&self) -> &JObject<'static> {
        self.exception.as_ref()
    }
}
impl AsRef<JThrowable<'static>> for JavaException {
    fn as_ref(&self) -> &JThrowable<'static> {
        <&JThrowable>::from(self.exception.as_obj())
    }
}
impl Deref for JavaException {
    type Target = JThrowable<'static>;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}
impl Into<GlobalRef> for JavaException {
    fn into(self) -> GlobalRef {
        self.exception
    }
}
impl std::error::Error for JavaException {}
impl Display for JavaException {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.message {
            Some(msg) => write!(f, "{}: {msg}", self.class),
            None => write!(f, "{}", self.class)
        }
    }
}
impl Debug for JavaException {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("JavaException")
            .field("exception", &self.exception.as_raw())
            .field("class", &self.class)
            .field("message", &self.message)
            .finish()
    }
}
impl<'local> FromObject<'_, '_, '_> for JavaException {
    fn from_object_env(object: &'_ JObject<'_>, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        if object.is_null() {
            return Err(FromObjectError::Null);
        }

        let class = get_object_class_name(object, env);

        // Check that Object is an Exception
        if !env.is_instance_of(object, <Self as Class>::class())
            .map_err(|err| FromObjectError::Other(format!("Error calling 'instanceof': {}", get_jni_error_msg(err, env))))?
        {
            return Err(FromObjectError::ClassMismatch {
                obj_class: class,
                target_class: Some(<Self as Class>::class().to_string()),
            });
        }

        Ok(Self {
            class,
            message: call!(env=> object.getMessage() -> Option<String>),
            exception: env.new_global_ref(&object)
                .map_err(|err| FromObjectError::Other(get_jni_error_msg(err, env)))?,
        })
    }
}
impl ToObject for JavaException {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        env.new_local_ref(&self.exception)
            .unwrap_or_else(|err| crate::__throw::handle_jni_call_error(err, env))
    }
}

/// Class that all Objects that can be converted to [`std::io::Error`] must be a descendant of.
static IO_ERROR_BASE_PATH: &str = "java/io/IOException";

impl FromObject<'_, '_, '_> for std::io::Error {
    fn from_object_env(object: &JObject, env: &mut JNIEnv) -> Result<Self, FromObjectError> {
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
        
        let class = get_object_class_name(object, env);
        let msg = call!(env=> object.getMessage() -> String);

        // All classes in map extend java.io.IOException.
        // Check this before the classes in map to avoid a bunch of pointless JNI calls
        if !env.is_instance_of(object, IO_ERROR_BASE_PATH).unwrap() {
            return Err(FromObjectError::ClassMismatch {
                obj_class: class,
                target_class: Some(IO_ERROR_BASE_PATH.to_string()),
            });
        }

        for &(class, error_kind) in MAP {
            if env.is_instance_of(object, class).unwrap() {
                return Ok(Self::new(error_kind, msg));
            }
        }

        // Already checked that is java.io.IOException
        Ok(Self::other(format!("{class}: {msg}")))
    }
}
impl ToObject for std::io::Error {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
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
        let class = env.find_class(class)
            .expect(&format!("Error getting class \"{class}\""));

        new!(env=> class(String(self.to_string())))
    }
}

impl FromObject<'_, '_, '_> for Box<dyn std::error::Error> {
    fn from_object_env(object: &JObject, env: &mut JNIEnv) -> Result<Self, FromObjectError> {
        JavaException::from_object_env(object, env).map(|ex| ex.into())
    }
}
impl ToObject for dyn std::error::Error {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(env=> java.lang.Exception(String(self.to_string())))
    }
}
