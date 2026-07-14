use jni::objects::{GlobalRef, JClass};
use nonempty::nonempty;
use std::{borrow::Cow, cell::OnceCell, fmt::{Debug, Display}, io, ops::Deref, str::FromStr, sync::OnceLock};
use ez_jni_macros::new;
use crate::{__throw::{Location, backtrace::{Backtrace, inject_backtrace}}, error::{MethodCallError, PanicError}, utils::{JniResultExt as _, ResultExt as _, get_class, get_object_class_name}};
use super::*;

/// A wrapper around a [`GlobalRef`] of a [`JThrowable`] that implements the useful error traits.
///
/// Contains the **Class path** and the exception **message**.
pub struct JavaException {
    object: GlobalRef,
    class: OnceCell<Cow<'static, str>>,
    /// Some (very rare!) exception classes leave the **message** as `null`.
    /// In the rare case this happens, the **message** must be wrapped with [`Option`].
    /// 
    /// Use [`Self::message()`] to get a default string.
    message: OnceCell<Option<String>>,
}
impl JavaException {
    pub const RUST_PANIC_CLASS: &str = "me/ezjni/RustPanic";

    /// The Exception object.
    pub fn object(&self) -> &JThrowable<'static> { <&JThrowable>::from(self.object.as_obj()) }
    /// The **Class Path** of the Exception Object.
    pub fn class(&self) -> &str {
        self.class.get_or_init(|| {
            let env = crate::utils::get_env();
            Cow::Owned(get_object_class_name(self.object.as_obj(), env))
        })
    }
    /// The **message** obtained from the Exception.
    /// 
    /// If the **message** is `null`, returns the **class name**.
    pub fn message(&self) -> &str {
        match self._message() {
            Some(msg) => msg,
            None => self.class(),
        }
    }
    /// Returns the message as an [`Option`], rather than always returning a value, like in the public version of this fn.
    fn _message(&self) -> Option<&str> {
        self.message.get_or_init(|| {
            let env = crate::utils::get_env();
            call!(env=> { self.object.as_obj() }.getMessage() -> Option<String>)
        })
        .as_ref()
        .map(String::as_str)
    }

    /// Like [`Self::from_object()`], but allows any [`Throwable`][JThrowable] objects.
    /// 
    /// This function does *not* do any checks on its own,
    /// and will `panic!` if something is wrong.
    /// 
    /// Not for **`public`** use, as it can cause confusion.
    #[doc(hidden)]
    pub(crate) fn from_throwable(object: &JThrowable<'_>, env: &mut JNIEnv<'_>) -> Self {
        Self {
            // Leave class and message uninitialized to save unnecessary work,
            // because the class and/or message data won't necessarily be accessed.
            class: OnceCell::new(),
            message: OnceCell::new(),
            object: env.new_global_ref(&object)
                .catch(env)
                .unwrap_jni(),
        }
    }

    /// Creates an instance of a [`JavaException`], where the class is `me.ezjni.RustPanic`.
    /// This exception class indicates that a *natve call* did not return successfully (`panic!ked`),
    /// and contains all the information of the panic: **message**, **location**, and **backtrace**.
    /// 
    /// Optionally, the caller can pass a **cause** Exception to the `Throwable` to allow chaining Exceptions.
    /// 
    /// > NOTE: The **location** that is passed in here should be obtained with [`std::panic::Location::caller()`],
    /// > and that should be called from a function with the `#[track_caller]` attribute.
    /// > This is to accurately capture the location where the `panic!` was triggered.
    /// 
    /// `panic!s` if a JNI error occurs (e.g. could not find RustPanic class).
    pub(crate) fn new_rust_panic(location: impl Into<Location>, message: String, cause: Option<Self>, env: &mut JNIEnv<'_>) -> Self {
        let location = location.into();
        let (file, line, col) = (location.file, location.line, location.col);

        /// In-memory cache of custom `Exception` Class Object `me.ezjni.RustPanic`
        static PANIC_CLASS: OnceLock<GlobalRef> = OnceLock::new();
        // Do not try to build Java Class when building documentation for Docs.rs because it does not allow macros to write to the filesystem.
        cfg_if::cfg_if! {
            if #[cfg(not(docsrs))] {
                let class_binary = crate::compile_java_class!("./src/", "me/ezjni/RustPanic");
            } else {
                let class_binary = &[0u8];
            }
        }
        let panic_class = PANIC_CLASS.get_or_init(|| {
            let class = get_class(Self::RUST_PANIC_CLASS, env)
                .or_else(|_| env.define_class(Self::RUST_PANIC_CLASS, &JObject::null(),  class_binary).catch(env))
                .unwrap_or_else(|err| panic!("Failed loading/finding RustPanic class: {err}"));
            env.new_global_ref(class)
                .expect("Failed to make JClass into GlobalRef")
        });
        let panic_class = <&JClass>::from(panic_class.as_obj());

        let exception = JThrowable::from(match cause {
            Some(cause) => new!(env=> panic_class(String(file), u32(line), u32(col), String(message), java.lang.Throwable(cause))),
            None => new!(env=> panic_class(String(file), u32(line), u32(col), String(message))),
        });
        // Inject Backtrace to Exception
        inject_backtrace(&exception, Backtrace::force_capture().as_ref(), env);

        Self {
            class: OnceCell::from(Cow::Borrowed(Self::RUST_PANIC_CLASS)),
            message: OnceCell::from(Some(message)),
            object: env.new_global_ref(&exception)
                .catch(env)
                .unwrap_jni(),
        }
    }

    /// Tells whether the [`Exception`][JavaException] Object's class extends the passed in **class**.
    /// 
    /// `panic!s` if **class** is not loaded in the JVM.
    pub fn is_instance_of(&self, class: impl AsRef<str>) -> bool {
        let env = crate::utils::get_env();
        
        let class = get_class(class.as_ref(), env).unwrap_jni();
        env.is_instance_of(self, &class)
            .catch(env)
            .unwrap_jni()
    }
}
impl FromObject<'_> for JavaException {
    fn from_object_env(object: &'_ JObject<'_>, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        if object.is_null() {
            return Err(FromObjectError::Null);
        }

        // Check that Object is an Exception
        if !env.is_instance_of(object, <Self as Class>::class()).catch(env)? {
            return Err(FromObjectError::ClassMismatch {
                obj_class: get_object_class_name(object, env),
                target_classes: nonempty![<Self as Class>::class().to_string()],
            });
        }

        Ok(Self {
            class: OnceCell::new(),
            message: OnceCell::new(),
            object: env.new_global_ref(&object)
                .catch(env)?,
        })
    }
}
impl ToObject for JavaException {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        env.new_local_ref(self.object())
            .catch(env)
            .unwrap_jni()
    }
}
impl AsRef<JObject<'static>> for JavaException {
    #[inline(always)]
    fn as_ref(&self) -> &JObject<'static> {
        self.object.as_obj()
    }
}
impl AsRef<JThrowable<'static>> for JavaException {
    #[inline(always)]
    fn as_ref(&self) -> &JThrowable<'static> {
        self.object()
    }
}
impl Deref for JavaException {
    type Target = JThrowable<'static>;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}
impl Into<GlobalRef> for JavaException {
    #[inline(always)]
    fn into(self) -> GlobalRef {
        self.object
    }
}
impl PanicError for JavaException {
    #[inline(always)]
    fn panic(self) -> ! {
        crate::__throw::panic_exception(self)
    }
}
impl std::error::Error for JavaException {}
impl Display for JavaException {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self._message() {
            Some(msg) => write!(f, "{}: {msg}", self.class()),
            None => write!(f, "{}", self.class())
        }
    }
}
impl Debug for JavaException {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("JavaException")
            .field("exception", &self.object().as_raw())
            .field("class", &self.class())
            .field("message", &self._message())
            .finish()
    }
}
impl FromStr for JavaException {
    // TODO: use ToObjectError when that is implemented.
    type Err = MethodCallError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let env = crate::utils::get_env();
        let exception_class = <JavaException as Class>::class();

        Ok(Self {
            object: {
                let class = get_class(&exception_class, env)
                    .map_err(MethodCallError::from)?;
                let obj = new!(?, env => class(String(s)))?;
                env.new_global_ref(obj)
                    .catch(env)
                    .map_err(MethodCallError::from)?
            },
            class: OnceCell::from(exception_class),
            message: OnceCell::from(Some(s.to_string())),
        })
    }
}
impl From<&str> for JavaException {
    fn from(value: &str) -> Self {
        <Self as FromStr>::from_str(value).unwrap_jni()
    }
}
impl From<&String> for JavaException {
    #[inline(always)]
    fn from(value: &String) -> Self {
        <Self as From<&str>>::from(value)
    }
}
impl From<String> for JavaException {
    #[inline(always)]
    fn from(value: String) -> Self {
        <Self as From<&str>>::from(&value)
    }
}
impl From<&dyn std::error::Error> for JavaException {
    #[inline(always)]
    fn from(value: &dyn std::error::Error) -> Self {
        <Self as From<String>>::from(value.to_string())
    }
}
impl From<Box<dyn std::error::Error>> for JavaException {
    #[inline(always)]
    fn from(value: Box<dyn std::error::Error>) -> Self {
        <Self as From<&dyn std::error::Error>>::from(value.as_ref())
    }
}
impl From<JObject<'_>> for JavaException {
    #[inline(always)]
    fn from(value: JObject<'_>) -> Self {
        <Self as FromObject<'_>>::from_object(&value).unwrap_jni()
    }
}
impl From<&JObject<'_>> for JavaException {
    #[inline(always)]
    fn from(value: &JObject<'_>) -> Self {
        <Self as FromObject<'_>>::from_object(value).unwrap_jni()
    }
}
impl From<JThrowable<'_>> for JavaException {
    #[inline(always)]
    fn from(value: JThrowable<'_>) -> Self {
        <Self as FromObject<'_>>::from_object(&value).unwrap_jni()
    }
}
impl From<&JThrowable<'_>> for JavaException {
    #[inline(always)]
    fn from(value: &JThrowable<'_>) -> Self {
        <Self as FromObject<'_>>::from_object(value).unwrap_jni()
    }
}

/// Class that all Objects that can be converted to [`std::io::Error`] must be a descendant of.
static IO_ERROR_BASE_PATH: &str = "java/io/IOException";

impl FromObject<'_> for std::io::Error {
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

        // All classes in map extend java.io.IOException.
        // Check this before the classes in map to avoid a bunch of pointless JNI calls
        if !env.is_instance_of(object, IO_ERROR_BASE_PATH).catch(env)? {
            return Err(FromObjectError::ClassMismatch {
                obj_class: class,
                target_classes: nonempty![IO_ERROR_BASE_PATH.to_string()],
            });
        }

        // Get message after checking class
        let msg = call!(env=> object.getMessage() -> String);

        for &(class, error_kind) in MAP {
            if env.is_instance_of(object, class).catch(env)? {
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
        let class = get_class(class, env).unwrap_jni();
        // TODO: return ToObjectError

        new!(env=> class(String(self.to_string())))
    }
}

impl FromObject<'_> for Box<dyn std::error::Error> {
    #[inline(always)]
    fn from_object_env(object: &JObject, env: &mut JNIEnv) -> Result<Self, FromObjectError> {
        JavaException::from_object_env(object, env)
            .map(JavaException::into)
    }
}
impl ToObject for dyn std::error::Error {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(env=> java.lang.Exception(String(self.to_string())))
    }
}
