mod jni_fn;
mod call;

pub use jni_fn::*;
pub use call::*;

use thiserror::Error;
use jni::{objects::{GlobalRef, JThrowable}, JNIEnv};
use ez_jni_macros::{call, new};
use std::{any::Any, backtrace::{Backtrace, BacktraceStatus}, borrow::Cow, cell::{Cell, RefCell}, error::Error};

thread_local! {
    static PANIC_LOCATION: RefCell<Option<Location>> = const { RefCell::new(None) };
    static PANIC_BACKTRACE: RefCell<Option<Backtrace>> = const { RefCell::new(None) };
    /// Tells whether [`set_panic_hook()`] was called and the *hook* was set.
    /// 
    /// This will only be `true` if [`catch_throw()`] was called (i.e. Rust was enetered from Java via a [`jni_fn`][ez_jni::jni_fn!]).
    static PANIC_HOOK_SET: Cell<bool> = const { Cell::new(false) };
}

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

/// Data about a **Backtrace Frame**.
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
    #[error("The backtrace is empty")]
    Empty,
    #[error("{0}")]
    Other(Box<dyn Error>)
}

pub type PanicPayload = Box<dyn Any + Send + 'static>;
/// Representations of possible **types** that can be used as the **payload** of a `panic!`.
pub enum PanicPayloadRepr {
    Message(Cow<'static, str>),
    Object(GlobalRef),
    Unknown
}
impl PanicPayloadRepr {
    pub const UNKNOWN_PAYLOAD_TYPE_MSG: &str = "Rust panicked! with an unknown type";

    #[inline(always)]
    pub fn new(payload: PanicPayload) -> Self { Self::from(payload) }
}
impl From<PanicPayload> for PanicPayloadRepr {
    /// Tries to guess the *concrete type* of a panic payload and casts it to that type.
    fn from(payload: PanicPayload) -> Self {
        match payload.downcast::<&'static str>() {
            Ok(msg) => Self::Message(Cow::Borrowed(&msg)),
            Err(payload) => match payload.downcast::<String>() {
                Ok(msg) => Self::Message(Cow::Owned(*msg)),
                Err(payload) => match payload.downcast::<GlobalRef>() {
                    Ok(exception) => Self::Object(*exception),
                    // Unexpected panic type
                    Err(_) => Self::Unknown,
                },
            },
        }
    }
}

/// Gets full [`Backtrace`] as a `String` and parses each line as a [`BacktraceElement`].
/// The returned array will never be empty.
/// 
/// This will also strip the Bakctrace of verbose/unecessary Stack frames.`
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



    // Filter backtrace frames because some are verbose
    let filtered = parse_backtrace_frames(&backtrace_str)?
        .into_iter()
        .filter(|(symbol, _)| !{
            symbol.starts_with("ez_jni::__throw::")
            || symbol.contains("std::panicking::")
            || symbol.contains("std::panic::")
            || symbol.contains("core::panicking::")
            || symbol.contains("core::panic::")
            || symbol.starts_with("std::sys::backtrace::")
            // For tests
            || symbol.starts_with("test::run_")
            || symbol.starts_with("std::thread::Builder::spawn_unchecked_")
            || symbol.starts_with("core::ops::function::FnOnce::call_once")
        })
        .filter(|(symbol, _)| ![ // Check exact equality
            "<alloc::boxed::Box<F,A> as core::ops::function::Fn<Args>>::call",
            "__rust_try",
            "rust_begin_unwind",
            // Encountered during test
            "test::__rust_begin_short_backtrace",
            "<alloc::boxed::Box<F,A> as core::ops::function::FnOnce<Args>>::call_once",
            "std::sys::pal::unix::thread::Thread::new::thread_start",
        ].contains(symbol));

    let backtrace = filtered
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
        .collect::<Result<Box<[_]>, _>>()?;

    if backtrace.is_empty() {
        return Err(ParseBacktraceError::Empty);
    }

    Ok(backtrace)
}
/// This is separated for testing purposes
fn parse_backtrace_frames<'a>(backtrace: &'a str) -> Result<Box<[(&'a str, Option<&'a str>)]>, ParseBacktraceError> {
    let mut lines = backtrace.lines()
        .collect::<Box<[_]>>()
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

/// Insert the Rust [`Backtrace`] into the exception's `StackTrace`,
/// replacing the *Native Method* Java frame with said backtrace.
/// 
/// Does nothing if cannot match the *bottom backtrace frame* in Rust with the *top-most Native Method frame* in Java.
/// Or if this thread's Rust execution was not entered from Java (i.e. the Java StackTrace is empty).
/// 
/// This is purely for reporting errors to an user and can be slow.
/// Some verbose Rust stack frames are also ommited to make it simpler for an user to interpret the data.
fn inject_backtrace(exception: &JThrowable, backtrace: &[BacktraceElement], env: &mut JNIEnv) {
    // Find the Native Method StackTraceElement 
    let mut java_trace = call!(env=> exception.getStackTrace() -> [java.lang.StackTraceElement]).into_vec();
    let index = match java_trace.iter()
        .enumerate()
        .find(|(_, frame)| call!(env=> frame.isNativeMethod() -> bool))
    {
        Some((i, _)) => i,
        None => return,
    };
    let entry_frame = java_trace.remove(index);

    // Assert that the top Native Method frame (not necessarily the top frame) in Java is the same as the bottom frame in Rust
    {
        let bottom = backtrace.last().unwrap();
        let bottom_name = bottom.symbol
            .rsplit_once("::")
            .map(|(_, name)| name)
            .unwrap_or(bottom.symbol.as_str());

        let entry_name = ::utils::java_method_to_symbol(
            &call!(env=> entry_frame.getClassName() -> String),
            &call!(env=> entry_frame.getMethodName() -> String),
        );

        if entry_name != bottom_name {
            return;
        }
    }

    // Convert BacktraceElement to StackTraceElement
    let rust_trace = backtrace.iter()
        .map(|frame| {
            let (class, method) = frame.symbol.rsplit_once("::")
                .unwrap_or(("Rust", &frame.symbol));

            new!(env=> java.lang.StackTraceElement(
                String(class),
                String(method),
                String(frame.location.file),
                u32(frame.location.line)
            ))
        })
        .collect::<Box<[_]>>();

    // Insert the backtrace in place of the Native Method frame
    java_trace.splice(index..index, rust_trace);

    // Set Exception's StackTrace
    call!(env=> exception.setStackTrace([java.lang.StackTraceElement](java_trace)) -> void);
}

#[cfg(test)]
mod tests {
    use super::*;

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

    /// Test that the backtrace can be parsed, and erturned to the original from the parsed data.
    #[test]
    fn parse_backtrace_test() {
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
}