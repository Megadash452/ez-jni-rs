use std::io;
use proc_macro2::Span;
use syn::LitByteStr;

/// Called by [`crate::compile_java_class`].
pub fn compile_java_class(java_root: impl AsRef<std::path::Path>, class_path: &str) -> syn::Result<LitByteStr> {
    compile_java_class_impl(java_root, class_path)
        .map(|binary| LitByteStr::new(&binary, Span::call_site()))
        .map_err(|err| syn::Error::new(Span::call_site(), err.to_string()))
    
}
fn compile_java_class_impl(java_root: impl AsRef<std::path::Path>, class_path: &str) -> io::Result<Box<[u8]>> {
    use std::{process::Command, path::PathBuf};
    use ::utils::{CLASS_DIR, run, absolute_path};

    let java_root = java_root.as_ref();
    let class_dir = &PathBuf::from(CLASS_DIR);
        /*  FIXME: If the compiled files of each macro invocation are not separated,
            the macro's output will also have the output of ALL other macro invocations combined.

            This is not a problem for now since `ez_jni::__throw::catch_throw()` only compiles one Java file,
            but ideally the macro should only output the files that it's `javac`` command produced.
            
            On the other hand, if the files produced by each invocation are separated,
            Java files won't be able to reference each other.

            The solution is to output all Classes to the same directory (as is done now),
            but also somehow get `javac` to tell you what files it produced.
        */
        // .join(path.to_str().unwrap().replace('/', "$")); // Isolate classes for each macro invocation?
    let class_file = &class_dir.join(class_path).with_extension("class");
    
    // Check if the Class (only the one named the same as the file) exists and abort if it does.
    if std::fs::exists(class_file)? {
        return std::fs::read(class_file)
            .map(|bin| bin.into_boxed_slice())
    }

    // Create directory where Class binaries are stored (if it's not created already)
    std::fs::create_dir_all(&class_dir)?;
    // Compile Java code
    run(Command::new("javac")
        .arg(java_root.join(class_path).with_extension("java"))
        .arg("--class-path").arg(absolute_path(class_dir))
        .arg("-d").arg(absolute_path(class_dir))
    )?;

    Ok(std::fs::read(class_file)?.into_boxed_slice())
}
