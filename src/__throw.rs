pub use either::Either;
use jni::{
    objects::{GlobalRef, JObject, JString, JThrowable, JValue}, JNIEnv
};
use thiserror::Error;
use std::{any::Any, backtrace::{Backtrace, BacktraceStatus}, cell::{Cell, RefCell}, error::Error};
use crate::FromException;
use crate::{compile_java_class, eprintln};

/// A lot like [std::panic::Location], but uses a [String] instead of `&str`.
#[derive(Debug, Clone)]
struct Location {
    file: String,
    line: u32,
    col: u32,
}
impl<'a> From<&std::panic::Location<'a>> for Location {
    fn from(value: &std::panic::Location<'a>) -> Self {
        Self {
            file: value.file().to_string(),
            line: value.line(),
            col: value.column(),
        }
    }
}
impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line, self.col)
    }
}

thread_local! {
    static PANIC_LOCATION: RefCell<Option<Location>> = const { RefCell::new(None) };
    static PANIC_BACKTRACE: RefCell<Option<Backtrace>> = const { RefCell::new(None) };
    /// Tells whether [`set_panic_hook()`] was called and the *hook* was set.
    /// 
    /// This will only be `true` if [`catch_throw()`] was called (i.e. Rust was enetered from Java via a [`jni_fn`][ez_jni::jni_fn!]).
    static PANIC_HOOK_SET: Cell<bool> = const { Cell::new(false) };
}

#[derive(Debug)]
struct BacktraceElement {
    symbol: String,
    location: Location,
}
#[derive(Debug, Error)]
enum ParseBacktraceError {
    #[error("Failed to get backtrace: {0:?}")]
    Unavailable(BacktraceStatus),
    #[error("Received malformed data for a Backtrace frame: \n{data}{}", match error {
        Some(error) => format!("\nCaused by: {error}"),
        None => "".to_string(),
    })]
    BadFrameData {
        data: String,
        error: Option<Box<dyn Error>>
    },
    #[error("{0}")]
    Other(Box<dyn Error>)
}
/// Gets full [`Backtrace`] as a `String` and parses each line as a [`BacktraceElement`].
/// 
/// This will also strip the Bactrace of verbose/unecessary Stack frames.`
fn prepare_backtrace() -> Result<Box<[BacktraceElement]>, ParseBacktraceError> {
    // Currenlty we are only able to get the full backtrace as a String.
    // Until [this Issue](https://github.com/rust-lang/rust/issues/79676) is resolved.
    let backtrace_str = PANIC_BACKTRACE.try_with(|backtrace| {
        let borrow = backtrace.try_borrow()
            .map_err(|error| ParseBacktraceError::Other(Box::new(error)))?;
        let backtrace = borrow.as_ref()
            .ok_or_else(|| ParseBacktraceError::Other("PANIC_BACKTRACE was not set by panic hook".to_string().into()))?;

        if backtrace.status() != BacktraceStatus::Captured {
            return Err(ParseBacktraceError::Unavailable(backtrace.status()));
        }

        Ok(backtrace.to_string())
    })
        .map_err(|error| ParseBacktraceError::Other(Box::new(error)))??; // Yes, that's a double try branch

    let backtrace = Result::<Box<[_]>, _>::from_iter(IntoIterator::into_iter(parse_backtrace_frames(&backtrace_str)?)
        // Frames without file data should be ommited
        .filter_map(|(symbol, file_data)| Some((symbol, file_data?)))
        // Each frame has a symbol (module + function name) and file data (file path + line + column).
        .map(|(symbol, file_data)| -> Result<BacktraceElement, ParseBacktraceError> { Ok(BacktraceElement {
            // Trim indentation and skip frame number
            symbol: symbol.trim_start().to_string(),
            location: {
                let frame = format!("{symbol}{file_data}");
                let (rest, file, line, col);

                (rest, col) = file_data
                    .rsplit_once(':')
                    .ok_or_else(|| ParseBacktraceError::BadFrameData { data: frame.clone(), error: None })?;
                (file, line) = rest
                    .rsplit_once(':')
                    .ok_or_else(|| ParseBacktraceError::BadFrameData { data: frame.clone(), error: None })?;

                Location {
                    file: file.to_string(),
                    line: line.parse::<u32>()
                        .map_err(|error| ParseBacktraceError::BadFrameData {
                            data: frame.clone(),
                            error: Some(Box::new(error))
                        })?,
                    col: col.parse::<u32>()
                    .map_err(|error| ParseBacktraceError::BadFrameData {
                        data: frame.clone(),
                        error: Some(Box::new(error))
                    })?,
                }
            }
        })})
    )?;

    let filtered = IntoIterator::into_iter(backtrace)
        .filter(|frame| !{
            frame.symbol.starts_with("ez_jni::__throw::")
            || frame.symbol.contains("std::panicking::")
            || frame.symbol.contains("std::panic::")
            || frame.symbol.contains("core::panicking::")
            || frame.symbol.contains("core::panic::")
            || frame.symbol.starts_with("std::sys::backtrace::")
            // For tests
            || frame.symbol.starts_with("test::run_")
            || frame.symbol.starts_with("std::thread::Builder::spawn_unchecked_")
            || frame.symbol.starts_with("core::ops::function::FnOnce::call_once")
        })
        .filter(|frame| ![
            "<alloc::boxed::Box<F,A> as core::ops::function::Fn<Args>>::call",
            "__rust_try",
            "rust_begin_unwind",
            // Encountered during test
            "test::__rust_begin_short_backtrace",
            "<alloc::boxed::Box<F,A> as core::ops::function::FnOnce<Args>>::call_once",
            "std::sys::pal::unix::thread::Thread::new::thread_start",
        ].contains(&frame.symbol.as_str()))
        .collect::<Box<[_]>>();

    Ok(filtered)
}
/// This is separated for testing purposes
fn parse_backtrace_frames<'a>(backtrace: &'a str) -> Result<Box<[(&'a str, Option<&'a str>)]>, ParseBacktraceError> {
    let mut lines = backtrace.lines()
        .collect::<Vec<_>>()
        .into_iter()
        .peekable();
    let mut frames = Vec::with_capacity(lines.len() / 2);
    let mut current_symbol: Option<&str> = None;

    // Collect Backtrace frames from lines, where frames most likely (but not always!) have a second `file data` line.
    loop {
        // Backtrace frames do NOT start with "at"
        if let Some(file_data) = lines.peek().and_then(|line| line.trim_start().strip_prefix("at ")) {
            // This is the second (and last) line of the frame
            let file_data = file_data.trim_start();
            let symbol = current_symbol
                .take()
                .ok_or_else(|| ParseBacktraceError::BadFrameData {
                    data: file_data.to_string(),
                    error: Some("Frame should not start with file data".into()),
                })?;
            
            frames.push((symbol, Some(file_data)));
        } else if let Some(symbol) = lines.peek() {
            // Line starts with "  {num}: "
            let symbol = symbol.trim_start()
                .split_once(": ")
                .map(|(_, symbol)| symbol.trim_start())
                .ok_or_else(|| ParseBacktraceError::BadFrameData {
                    data: symbol.to_string(),
                    error: Some("Line should start with a number followed by a colon (:)".into()),
                })?;
            // Push the current frame if it was not previously (did not have a second line).
            if let Some(current_symbol) = current_symbol {
                frames.push((current_symbol, None))
            }
            current_symbol = Some(symbol);
        } else { // Reached end
            // // Push the current frame if it was not previously (did not have a second line).
            if let Some(current_symbol) = current_symbol {
                frames.push((current_symbol, None))
            }
            break;
        }

        // Advance the iterator
        lines.next();
    }

    Ok(frames.into_boxed_slice())
}

fn inject_backtrace(exception: &JThrowable, backtrace: &[BacktraceElement], env: &mut JNIEnv) {
    std::process::abort();
    todo!("inject_bactrace")
}

/// Runs a Rust function and returns its value, catching any `panics!` and throwing them as Java Exceptions.
/// Specifically, this will throw a `me.marti.ezjni.RustPanic` exception.
///
/// If **f** `panics!`, this will return the [`Zeroed`][std::mem::zeroed()] representation of the return type.
/// This means that this function should only return directly to Java.
/// 
/// DO NOT try to operate on the return value because you will have no indication wether **f** panicked and returned *zeroed*.
/// 
/// This function is used by [ez_jni_macros::jni_fn].
pub fn catch_throw<'local, R>(
    env: &mut JNIEnv<'local>,
    f: impl FnOnce(&mut JNIEnv<'local>) -> R,
) -> R {
    catch_throw_main(env, |env|
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| f(env)))
    )
}
/// Same as [`catch_throw()`], but maps the returned `R` to a Java value `J`.
pub fn catch_throw_map<'local, R, J>(
    env: &mut JNIEnv<'local>,
    f: impl FnOnce(&mut JNIEnv<'local>) -> R,
    // map function should not capture variables
    map: fn(R, &mut JNIEnv<'local>) -> J,
) -> J {
    catch_throw_main(env, |env|
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| f(env)))
            .and_then(|r| std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| map(r, env))))
    )
}
/// Handles the setting up the *panic hook* and throwing the *panic payload*.
/// 
/// This function exists to avoid repeating code in [`catch_throw()`] and [`catch_throw_map()`].
#[allow(unused_must_use)]
fn catch_throw_main<'local, R>(env: &mut JNIEnv<'local>, catch: impl FnOnce(&mut JNIEnv<'local>) -> std::thread::Result<R>) -> R {
    // Set panic hook to grab [`PANIC_LOCATION`] data.
    std::panic::set_hook(Box::new(|info| {
        PANIC_LOCATION.set(Some(info.location().unwrap().into()));
        // Get full Backtrace`] as a String and store it to process it in `throw_panic()`.
        PANIC_BACKTRACE.set(Some(Backtrace::force_capture()));
    }));
    PANIC_HOOK_SET.set(true);

    let result = match catch(env) {
        Ok(r) => r,
        Err(payload) => {
            throw_panic(env, payload)
                .unwrap_or_else(|err| {
                    std::panic::take_hook();
                    eprintln!("Aborting: {err}");
                    std::process::abort();
                });
            unsafe { std::mem::zeroed() }
        }
    };

    // Reset panic hook so that rust behaves normally after this,
    // though this is only considered for tests.
    std::panic::take_hook();
    PANIC_HOOK_SET.set(false);

    result
}

/// This function MUST NEVER `panic!`.
fn throw_panic(env: &mut JNIEnv, payload: Box<dyn Any + Send>) -> Result<(), Box<dyn std::error::Error>> {
    let panic_msg = match payload.downcast::<&'static str>() {
        Ok(msg) => msg.as_ref().to_string(),
        Err(payload) => match payload.downcast::<String>() {
            Ok(msg) => *msg,
            Err(payload) => match payload.downcast::<GlobalRef>() {
                // Panicked with an Exception (should be rethrown)
                Ok(exception) => {
                    let exception = <&JThrowable>::from(exception.as_obj());
                    // Inject Backtrace to Exception
                    prepare_backtrace().map(|backtrace| {
                        inject_backtrace(exception, &backtrace, env);
                    })?;
                    // Finally, rethrow the new Exception
                    env.throw(exception)
                        .map_err(|err| format!("Failed to rethrow exception: {err}"))?;
                    return Ok(());
                },
                // Unexpected panic type
                Err(_) => "Rust panicked!; Unable to obtain panic message".to_string(),
            },
        },
    };
    let location = PANIC_LOCATION.try_with(|loc| {
        loc.try_borrow()
            .ok()
            .as_deref()
            .map(|loc| loc.as_ref().map(|loc| loc.clone()))
    }).ok()
        .flatten()
        .flatten()
        .ok_or_else(|| format!("Failed to get panic location"))?;
    // Get backtrace

    // Create the RustPanic object to throw
    env.exception_clear()?;

    let panic_class = env.define_class("me/marti/ezjni/RustPanic", &JObject::null(), compile_java_class!("./src/", "me/marti/ezjni/RustPanic"))?;

    let file = env.new_string(location.file)?;
    let msg = env.new_string(panic_msg)?;
    // Call constructor RustPanic(java.lang.String, int, int, java.lang.String)
    let exception = env.new_object(panic_class, "(Ljava/lang/String;IILjava/lang/String;)V", &[
        JValue::Object(&file),
        JValue::Int(unsafe { std::mem::transmute(location.line) }),
        JValue::Int(unsafe { std::mem::transmute(location.line) }),
        JValue::Object(&msg)
    ])?;
    // Inject Backtrace to Exception
    prepare_backtrace().map(|backtrace| {
        inject_backtrace(<&JThrowable>::from(&exception), &backtrace, env);
    })?;
    // Finally, throw the new Exception
    env.throw(JThrowable::from(exception))?;
    
    Ok(())
}

/// Checks if an exception has been thrown from a previous JNI function call,
/// and tries to convert it to an `E` so that it can be [`mapped`][Result::map] to get a `Result<T, E>`.
/// 
/// If the `Exception` can't be converted to an `E`,
/// the program will `panic!` and the `Exception` will be rethrown,
/// in a similar way to how [`panic_uncaught_exception()`] does it.
/// 
/// See [`try_catch`] if panicking is not intended.
///
/// This function is used by [ez_jni_macros::call!].
pub fn catch<'local, E: FromException<'local>>(env: &mut JNIEnv<'local>) -> Result<(), E> {
    match catch_exception(env) {
        Some(ex) => match E::from_exception(&ex, env) {
            Ok(e) => Err(e),
            Err(err) => {
                eprintln!("Attempted to catch an exception, but failed to convert it to a concrete type:\n{err}");
                env.throw(ex).unwrap();
                ::std::panic::panic_any(());
            }
        }
        None => Ok(()),
    }
}
/// Similar to [`catch()`], but will not panic if the exception's type did not match the desired type.
/// Instead it will continue to be thrown until another [`try_catch`] is called with the correct type.
/// 
/// Returns the Exception if it was caught.
pub fn try_catch<'local, E: FromException<'local>>(env: &mut JNIEnv<'local>) -> Option<E> {
    catch_exception(env)
        .and_then(|ex|
            E::from_exception(&ex, env)
                .inspect_err(|_| env.throw(ex).unwrap())
                .ok()
        )
}

/// Panics with a *Java Exception* instead of  *Rust String message*.
pub fn panic_exception(ex: JThrowable, env: &mut JNIEnv) -> ! {
    let ex = env.new_global_ref(ex)
        .unwrap_or_else(|err| panic!("Unable to create Global Reference for Exception Object: {err}"));
    ::std::panic::panic_any(ex)
}

/// Checks if there is an **exception** that was thrown by a Java method called from Rust,
/// and that was not caught by the user (did not use [`Result`] for the return type),
/// and **panics** with that exception.
///
/// **target** is a *class* or an *object* (whose class will be determined).
/// **method_name** is the name of the Java Method that threw the *exception*
///
/// This function is used by [ez_jni_macros::call!].
pub fn panic_uncaught_exception(
    env: &mut JNIEnv,
    target: Either<&str, &JObject>,
    method_name: impl AsRef<str>,
) {
    if let Some(ex) = catch_exception(env) {
        // If the panic hook was set, this means that the panic payload will be thrown to Java, so the payload should be the exception itself.
        // DO NOT inject bactrace here. That is done in throw_panic().
        if PANIC_HOOK_SET.get() {
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
            eprintln!("Rust panic: Encountered an uncaught Java Exception after calling {class}.{}():", method_name.as_ref());
            panic_exception(ex, env)
        } else {
            // Otherwise, just panic with the exception message and exit execution.
            let msg = <String as FromException>::from_exception(&ex, env).unwrap(); // Always returns Ok or panics
            // There is no need to inject backtrace if panics are not being caught.
            panic!("{msg}");
        }
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

#[test]
fn parse_backtrace_test() {
    static SAMPLE: &str = r##"0: ez_jni::__throw::catch_throw_main::{{closure}}
at /home/marti/source/ez-jni-rs/src/__throw.rs:228:34
1: <alloc::boxed::Box<F,A> as core::ops::function::Fn<Args>>::call
at /rustc/eeb90cda1969383f56a2637cbd3037bdf598841c/library/alloc/src/boxed.rs:2084:9
2: std::panicking::rust_panic_with_hook
at /rustc/eeb90cda1969383f56a2637cbd3037bdf598841c/library/std/src/panicking.rs:808:13
3: std::panicking::begin_panic_handler::{{closure}}
at /rustc/eeb90cda1969383f56a2637cbd3037bdf598841c/library/std/src/panicking.rs:667:13
4: std::sys::backtrace::__rust_end_short_backtrace
at /rustc/eeb90cda1969383f56a2637cbd3037bdf598841c/library/std/src/sys/backtrace.rs:168:18
5: rust_begin_unwind
at /rustc/eeb90cda1969383f56a2637cbd3037bdf598841c/library/std/src/panicking.rs:665:5
6: core::panicking::panic_fmt
at /rustc/eeb90cda1969383f56a2637cbd3037bdf598841c/library/core/src/panicking.rs:74:14
7: native_test::my_other_fn
at /home/marti/source/ez-jni-rs/tests/native_test/src/lib.rs:5:5
8: native_test::Java_me_test_Native_native_1test_1bool::{{closure}}
at /home/marti/source/ez-jni-rs/tests/native_test/src/lib.rs:10:9
9: ez_jni::__throw::catch_throw_map::{{closure}}::{{closure}}
at /home/marti/source/ez-jni-rs/src/__throw.rs:215:66
10: <core::panic::unwind_safe::AssertUnwindSafe<F> as core::ops::function::FnOnce<()>>::call_once
at /rustc/eeb90cda1969383f56a2637cbd3037bdf598841c/library/core/src/panic/unwind_safe.rs:272:9
11: std::panicking::try::do_call
at /rustc/eeb90cda1969383f56a2637cbd3037bdf598841c/library/std/src/panicking.rs:557:40
12: __rust_try
13: std::panicking::try
at /rustc/eeb90cda1969383f56a2637cbd3037bdf598841c/library/std/src/panicking.rs:521:19
14: std::panic::catch_unwind
at /rustc/eeb90cda1969383f56a2637cbd3037bdf598841c/library/std/src/panic.rs:350:14
15: ez_jni::__throw::catch_throw_map::{{closure}}
at /home/marti/source/ez-jni-rs/src/__throw.rs:215:9
16: ez_jni::__throw::catch_throw_main
at /home/marti/source/ez-jni-rs/src/__throw.rs:232:24
17: ez_jni::__throw::catch_throw_map
at /home/marti/source/ez-jni-rs/src/__throw.rs:214:5
18: Java_me_test_Native_native_1test_1bool
at /home/marti/source/ez-jni-rs/tests/native_test/src/lib.rs:9:56
19: <unknown>
"##;

    let backtrace = parse_backtrace_frames(SAMPLE)
        .unwrap()
        .iter()
        .enumerate()
        .map(|(i, (symbol, file_data))| format!(
            "{i}: {symbol}{}\n", match file_data {
                Some(file_data) => format!("\nat {file_data}"),
                None => "".to_string(),
            }
        ))
        .collect::<String>();

    assert_eq!(backtrace, SAMPLE);
}