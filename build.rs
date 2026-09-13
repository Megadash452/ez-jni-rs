use std::{env::VarError::NotPresent, io};

fn main() {
    #[cfg(target_os = "android")]
    cc::Build::new()
        .include(get_ndk_c_include_dir().unwrap_or_else(|err| panic!("Error getting NDK include dir: {err}")))
        .file("src/utils/log.c")
        .compile("ezjnic");
}

fn get_ndk_c_include_dir() -> io::Result<PathBuf> {
    let ndk_path = match std::env::var("ANDROID_NDK_HOME") {
        // NDK HOME was provided directly, so use that directly.
        Ok(path) => path,
        // Android HOME was provided, must find NDK within it.
        Err(NotPresent) => {
            let android_home = match std::env::var("ANDROID_HOME") {
                Ok(path) => path,
                Err(NotPresent) => {
                    eprintln!("Neither ANDROID_NDK_HOME or ANDROID_HOME environment variables were set; using hardcoded path \"$HOME/Android\"");
                    let home = std::env::home_dir()
                        .ok_or(io::Error::other("HOME directory is not set"))?
                        .display();
                    format!("{home}/Android")
                },
                Err(err) => return Err(io::Error::other(format!("Error reading \"ANDROID_HOME\" environment variable: {err}")))
            };
            
            // Find the NDK directory (and version) from the Android HOME.
            todo!()
        },
        Err(err) => return Err(io::Error::other(format!("Error reading \"ANDROID_NDK_HOME\" environment variable: {err}"))),
    };


}