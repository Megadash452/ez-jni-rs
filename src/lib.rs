#[doc(hidden)]
/// Used only by [`jni_macros`]
pub mod __throw;
#[macro_use]
pub mod utils;
mod object;
extern crate self as ez_jni;

pub use jni_macros::*;
pub use object::*;
