use ez_jni_macros::new;
use std::io;
use super::*;

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

impl FromException<'_> for std::io::Error {
    fn from_exception(exception: &JThrowable, env: &mut JNIEnv) -> Result<Self, FromObjectError> {
        <Self as FromObject>::from_object(exception, env)
    }
}
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
        let class = env.find_class(class)
            .expect(&format!("Error getting class \"{class}\""));

        new!(class(String(self.to_string())))
    }
}

impl FromException<'_> for Box<dyn std::error::Error> {
    fn from_exception(exception: &JThrowable, env: &mut JNIEnv) -> Result<Self, FromObjectError> {
        Ok(<String as FromException>::from_exception(exception, env)?.into())
    }
}
impl FromObject<'_> for Box<dyn std::error::Error> {
    fn from_object(object: &JObject, env: &mut JNIEnv) -> Result<Self, FromObjectError> {
        <Self as FromException>::from_exception(<&JThrowable>::from(object), env)
    }
}
impl<'local> ToObject<'local> for dyn std::error::Error {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(java.lang.Exception(String(self.to_string())))
    }
}
