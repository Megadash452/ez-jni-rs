use crate::utils::get_string;
use either::Either;
use jni::{
    JNIEnv,
    objects::{JObject, JString, JThrowable},
};
use jni_macros::call;
use std::{any::Any, sync::RwLock};

/// A lot like [std::panic::Location], but uses a [String] instead of `&str`.
struct PanicLocation {
    file: String,
    line: u32,
    col: u32,
}
impl<'a> From<&std::panic::Location<'a>> for PanicLocation {
    fn from(value: &std::panic::Location<'a>) -> Self {
        Self {
            file: value.file().to_string(),
            line: value.line(),
            col: value.column(),
        }
    }
}
impl std::fmt::Display for PanicLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line, self.col)
    }
}
static PANIC_LOCATION: RwLock<Option<PanicLocation>> = RwLock::new(None);

/// Runs a Rust function and returns its value, catching any `panics!` and throwing them as Java Exceptions.
///
/// Returns the [`Zeroed`][std::mem::zeroed()] representation of the return type.
/// This means that this function should only return directly to Java,
/// or `R` should only be a type like a *pointer* or an *integer*.
#[doc(hidden)]
pub fn __catch_throw<'local, R>(
    env: &mut JNIEnv<'local>,
    f: impl FnOnce(&mut JNIEnv<'local>) -> R,
) -> R {
    std::panic::set_hook(Box::new(|info| {
        if let Ok(mut panic_location) = PANIC_LOCATION.write() {
            if let Some(location) = info.location() {
                *panic_location = Some(PanicLocation::from(location))
            }
        }
    }));

    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| f(env))) {
        Ok(r) => r,
        Err(payload) => {
            throw_panic(env, payload);
            unsafe { std::mem::zeroed() }
        }
    }
}
fn throw_panic(env: &mut JNIEnv, payload: Box<dyn Any + Send>) {
    let panic_msg = match payload.downcast::<&'static str>() {
        Ok(msg) => Some(msg.as_ref().to_string()),
        Err(payload) => match payload.downcast::<String>() {
            Ok(msg) => Some(*msg),
            Err(payload) => match payload.downcast::<()>() {
                // Did not panic with a message, but with an "uncaught" exception that has already been rethrown
                Ok(_) => return,
                // Unexpected panic type
                Err(_) => None,
            },
        },
    };
    let msg = match (panic_msg, &*PANIC_LOCATION.read().unwrap()) {
        (Some(msg), Some(info)) => format!("panicked at {info}: {msg}"),
        (Some(msg), None) => format!("panicked at unknown location: {msg}"),
        (None, Some(info)) => format!("Rust panicked at {info}, but could not obtain message"),
        (None, None) => "Rust had a panic! but could not obtain any panic data".to_string(),
    };
    let _ = env.throw_new("java/lang/Exception", msg);
}

/// A trait used to construct **Error Enums** from an `Exception` thrown from Java.
/// The conversion from `Exception` to `Self` will succe
pub trait FromException
where Self: Sized {
    fn from_exception(env: &mut JNIEnv, exception: &JThrowable) -> Option<Self>;
}

impl FromException for String {
    fn from_exception(env: &mut jni::JNIEnv, exception: &jni::objects::JThrowable) -> Option<Self> {
        let msg = JString::from(call!(exception.getMessage() -> java.lang.String));
        Some(get_string(env, JString::from(msg)))
    }
}

impl FromException for std::io::Error {
    fn from_exception(env: &mut JNIEnv, exception: &JThrowable) -> Option<Self> {
        use std::io;
        let msg = JString::from(call!(exception.getMessage() -> java.lang.String));
        let msg = get_string(env, JString::from(msg));
        
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
        let exception_class = JString::from(call!(exception_class.getName() -> java.lang.String));
        let exception_class = get_string(env, exception_class);
        
        for (class, error_kind) in map {
            if class == exception_class {
                return Some(Self::new(error_kind, msg))
            }
        }
        
        if object_is_descendant_of(env, exception, "java/io/IOException") {
            return Some(Self::other(format!("{exception_class}: {msg}")));
        }
        
        None
    }
}

/// Check if an Object's class is **class**, or if it **extends** (is descendant of) **class**.
/// 
/// Use this to convert an Object into a rust type, such as in [`FromException`].
pub fn object_is_descendant_of(env: &mut JNIEnv, obj: &JObject, class: &str) -> bool {
    // First check if the top class of obj is `class`.
    let obj_class = env.get_object_class(obj)
        .expect("Failed to get Object's class");
    let obj_class_name = JString::from(call!(obj_class.getName() -> java.lang.String));
    let obj_class_name = get_string(env, obj_class_name);
    if obj_class_name == class {
        return true
    }
    
    let mut current = obj_class;
    while let Some(super_class) = env.get_superclass(&current)
        .expect("Failed to get class' super class")
    {
        let class_name = JString::from(call!(super_class.getName() -> java.lang.String));
        let class_name = get_string(env, class_name);
        if class_name == class {
            return true
        }
        current = super_class
    }
    
    false
}

/// Chekcs if an exception has been thrown from a previous JNI function call,
/// and tries to convert it to an [`E`] so that it can be [`mapped`][Result::map] to get a `Result<T, E>`.
/// 
/// If the `Exception` can't be converted to an [`E`],
/// the program will `panic!` and the `Exception` will be rethrown,
/// in a similar way to how [`__panic_uncaught_exception()`] does it.
///
/// This function is used by [jni_macros::call!].
#[doc(hidden)]
pub fn __catch_exception<E: FromException>(env: &mut JNIEnv) -> Result<(), E> {
    match catch_exception(env) {
        Some(ex) => match E::from_exception(env, &ex) {
            Some(e) => Err(e),
            None => {
                crate::utils::__eprintln(env, format!("Attempted to catch an exception, but failed to convert it to a concrete type."));
                env.throw(ex).unwrap();
                ::std::panic::panic_any(());
            }
        }
        None => Ok(()),
    }
}
/// Checks if there is an **exception** that was thrown by a Java method called from Rust,
/// and that was not caught by the user (did not use [`Result`] for the return type),
/// and **panics** with that exception.
///
/// **target** is a *class* or an *object* (whose class will be determined).
/// **method_name** is the name of the Java Method that threw the *exception*
///
/// This function is used by [jni_macros::call!].
#[doc(hidden)]
pub fn __panic_uncaught_exception(
    env: &mut JNIEnv,
    target: Either<&'static str, &JObject>,
    method_name: &'static str,
) {
    let class = match target {
        Either::Left(s) => s.to_string(),
        Either::Right(obj) => env
            .get_object_class(obj)
            .and_then(|class| env.call_method(class, "getName", "()Ljava/lang/String;", &[]))
            .and_then(|class| class.l())
            .and_then(|class| {
                unsafe { env.get_string_unchecked(&JString::from(class)) }.map(String::from)
            })
            .unwrap_or_else(|_| "<Object>".to_string()),
    };
    if let Some(ex) = catch_exception(env) {
        crate::utils::__eprintln(env, format!("Rust panic: Encountered an uncaught Java Exception after calling {class}.{method_name}():"));
        env.throw(ex).unwrap();
        ::std::panic::panic_any(());
    }
}
/// Checks if an `Exception` was thrown after calling a Java Method, and returns said `Exception`.
/// The `Exception` will be cleared so that it will be possible to do other jni_calls to handle the exception (e.g. rethrowing it).
///
/// The returned Object will NEVER be NULL.
///
/// NOTICE: Can't call any function (including print) between the time when the exception is thrown and when `JNIEnv::exception_clear()` is called.
/// This means that if a method call could throw, the checks (call, type, and null) should be done AFTER the exception is caught.
fn catch_exception<'local>(env: &mut JNIEnv<'local>) -> Option<JThrowable<'local>> {
    env.exception_occurred().ok().and_then(|ex| {
        if !ex.is_null() {
            env.exception_clear().unwrap();
            Some(ex)
        } else {
            None
        }
    })
}
