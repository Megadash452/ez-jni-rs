#![cfg(target_os = "android")]

use std::{ffi::{CStr, c_char, c_int}, io, sync::atomic::{AtomicBool, Ordering}};

#[link(name = "ezjnic")]
unsafe extern "C" {
    fn start_native_logger(app_name: *const c_char) -> c_int;
}

/// Creates a thread that reads **stdout** and **stderr** outputs and sends them to be logged by calling NDK's `__android_log_write()`.
/// 
/// This is necessary because in Android, **stdout** and **stderr** are redirected to `/dev/null`.
/// The only way to see those outputs is to track them and write them in Android's internal log system.
/// 
/// This function is *idempotent*: Only has an effect when called the first time.
pub(crate) fn start_logger() -> io::Result<()> {
    static APP_NAME: &CStr = c"Budgiet Rust";
    static STARTED: AtomicBool = AtomicBool::new(false);

    if !STARTED.load(Ordering::Acquire) {
        if unsafe { start_native_logger(APP_NAME.as_ptr()) } == -1 {
            return Err(io::Error::last_os_error());
        }
        STARTED.store(true, Ordering::Release);
    }

    Ok(())
}