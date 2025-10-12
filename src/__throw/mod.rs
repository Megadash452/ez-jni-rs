mod jni_fn;
mod call;
mod backtrace;

pub use jni_fn::*;
pub use call::*;
use backtrace::*;

use jni::{objects::{GlobalRef, JThrowable}, JNIEnv};
use ez_jni_macros::new;
use std::{any::Any, borrow::Cow, cell::{Cell, RefCell}};

thread_local! {
    /// Tells whether [`set_panic_hook()`] was called and the *hook* was set.
    /// 
    /// This will only be `true` if [`catch_throw()`] was called (i.e. Rust was enetered from Java via a [`jni_fn`][ez_jni::jni_fn!]).
    static PANIC_HOOK_SET: Cell<bool> = const { Cell::new(false) };
}

/// A lot like [std::panic::Location], but uses a [String] instead of `&str`.
#[derive(Debug, Clone)]
pub struct Location {
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

pub type PanicPayload = Box<dyn Any + Send + 'static>;
/// Representations of possible **types** that can be used as the **payload** of a `panic!`.
#[derive(Debug)]
pub enum PanicType {
    Message(Cow<'static, str>),
    Object(GlobalRef),
    Unknown
}
impl PanicType {
    pub const UNKNOWN_PAYLOAD_TYPE_MSG: &str = "Rust panicked! with an unknown type";

    #[inline(always)]
    pub fn new(payload: PanicPayload) -> Self { Self::from(payload) }
}
impl From<PanicPayload> for PanicType {
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
