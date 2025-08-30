#![cfg(debug_assertions)]

use jni::{
    errors::Result as JNIResult,
    objects::{JObject, JClass},
    JNIEnv
};
use std::fmt::{Display, Write};
use itertools::Itertools as _;
use crate::call;
use crate::eprintln as println;

/// Check why method could not be called.
/// 
/// This function makes *A LOT* Java calls, so it can be quite slow,
/// so it only exists in DEBUG builds.
#[cfg(debug_assertions)]
pub(crate) fn check_method_existence(class: JClass<'_>, method_name: &'static str, method_sig: &str, env: &mut JNIEnv<'_>) {
    // Find all the classes that this Class is a descendant of (including itself)
    let mut classes = vec![class];
    while let Some(class) = env.get_superclass(classes.last().unwrap())
        .unwrap_or_else(|err| crate::__throw::handle_jni_call_error(err, env))
    {
        classes.push(class);
    }

    fn get_sig(method: &JObject<'_>, env: &mut JNIEnv<'_>) -> String {
        let params = call!(env=> method.getParameterTypes() -> [Class])
            .iter()
            .map(|ty| get_jni_type(&call!(env=> ty.getTypeName() -> String)))
            .collect::<String>();
        let rtrn = get_jni_type(&call!(env=> call!(env=>
            method.getReturnType() -> Class)
                .getTypeName() -> String)
        );

        format!("({params}){rtrn}")
    }

    let mut same_name_sig_methods = Vec::new();
    let mut same_name_methods = Vec::new();
    let mut same_sig_methods = Vec::new();

    for class in classes {
        for method in call!(env=> class.getDeclaredMethods() -> [java.lang.reflect.Method]) {
            let name = call!(env=> method.getName() -> String);
            // Build the method's signature as a JNI type signature
            let sig = get_sig(&method, env);

            if name == method_name && sig == method_sig {
                same_name_sig_methods.push(method);
            } else if name == method_name {
                same_name_methods.push(method);
            } else if sig == method_sig {
                same_sig_methods.push(method);
            }
        }
    }

    let mut buf = String::new();
    let method_str = display_method(method_name, method_sig);

    if !same_name_sig_methods.is_empty() {
        let method = &same_name_sig_methods[0];
        let class = call!(env=> call!(env=> method.getDeclaringClass() -> Class).getTypeName() -> String);
        let mods = access_modifiers(call!(env=> method.getModifiers() -> int), env);
        write!(buf, "Found method {method_str} in {class}").unwrap();
        if mods.len() > 0 {
            write!(buf, ": The Method is {}", display_natural_list(&mods)).unwrap();
        }
        write!(buf, ".").unwrap();
        if same_name_sig_methods.len() > 1 {
            writeln!(buf, "\nThe are more methods that match this one:").unwrap();
            write!(buf, "{}", display_methods(&same_name_sig_methods[1..], env)).unwrap();
        }
    } else if !same_name_methods.is_empty() {
        let method = &same_name_methods[0];
        let other_sig = display_sig(&get_sig(method, env));
        let mods = access_modifiers(call!(env=> method.getModifiers() -> int), env);
        write!(buf, "Incorrect signature for method \"{method_str}\": the correct signature is \"{other_sig}\"").unwrap();
        if mods.len() > 0 {
            write!(buf, " and is {}", display_natural_list(&mods)).unwrap();
        }
        write!(buf, ".").unwrap();
        if same_name_methods.len() > 1 {
            writeln!(buf, "\nThere are more methods with this name:").unwrap();
            write!(buf, "{}", display_methods(&same_name_methods[1..], env)).unwrap();
        }
    } else if !same_sig_methods.is_empty() {
        writeln!(buf, "Did not find method with name \"{method_name}\". These methods have the same signature:").unwrap();
        write!(buf, "{}", display_methods(&same_name_methods, env)).unwrap();
    } else {
        write!(buf, "Did not find any Methods that match \"{method_str}\"").unwrap();
    }

    println!("{buf}");
}

/// Check why getting a Field or getter/setter Method failed.
/// 
/// The implementation is messy because this is only meant to provide a hint to the user in a print statement.
/// This means that this will suggest any Field/Method that has any similarity to the intended field.
/// 
/// This function makes *A LOT* Java calls, so it can be quite slow,
/// so it only exists in DEBUG builds.
pub(crate) fn check_field_existence(class: &JClass<'_>, field_name: &'static str, field_ty: &str, env: &mut JNIEnv<'_>) -> JNIResult<()> {
    // Find all the classes that this Class is a descendant of (including itself)
    let mut classes = vec![JClass::from(
            env.new_local_ref(class) // new_local_ref() is ok here because it is only used once with no recursion or anything else fancy
                .unwrap_or_else(|err| crate::__throw::handle_jni_call_error(err, env))
    )];
    while let Some(class) = env.get_superclass(classes.last().unwrap())? {
        classes.push(class);
    }

    let mut same_name_type_fields = Vec::new();
    let mut same_name_fields = Vec::new();
    let mut same_type_fields = Vec::new();
    let mut similar_name_type_methods = Vec::new();
    let mut similar_name_methods = Vec::new();
    let mut same_type_methods = Vec::new();

    // Get all the Fields and Methods of the Class (and its ancestors) and separate them into categories.
    for class in classes {
        let fields = call!(env=> class.getDeclaredFields() -> [java.lang.reflect.Field]);
        let methods = call!(env=> class.getDeclaredMethods() -> [java.lang.reflect.Method]);

        for field in fields {
            let name = call!(env=> field.getName() -> String);
            let ty = get_jni_type(&call!(env=> call!(env=> field.getType() -> Class).getTypeName() -> String));

            if name == field_name && ty == field_ty {
                same_name_type_fields.push(field);
            } else if name == field_name {
                same_name_fields.push(field);
            } else if ty == field_ty {
                same_type_fields.push(field);
            }
        }

        for method in methods {
            let name = call!(env=> method.getName() -> String);
            // Checks similarities in name by making it all lowercase and removing possible underscores (_).
            let similar_name = name
                .to_lowercase()
                .replace('_', "")
                .contains(&field_name.to_lowercase().replace('_', ""));
            // The method is considered to have the "same type" as the field if it has only one argument with the same type OR the return type is the same type.
            let same_type = {
                let params = call!(env=> method.getParameterTypes() -> [Class]);
                let return_type = get_jni_type(&call!(env=> call!(env=> method.getReturnType() -> Class).getTypeName() -> String));

                (params.len() == 1 && return_type == "V" && get_jni_type(&call!(env=> params[0].getTypeName() -> String)) == field_ty)
                || (params.len() == 0 && return_type == field_ty)
            };// one argumetn void return, or no arguments one return

            if similar_name && same_type {
                similar_name_type_methods.push(method);
            } else if similar_name {
                similar_name_methods.push(method);
            } else if same_type {
                same_type_methods.push(method);
            }
        }
    }

    let mut buf = String::new();

    // Messy ahh print statements
    if !same_name_type_fields.is_empty() {
        let field = &same_name_type_fields[0]; 
        let class = call!(env=> call!(env=> field.getDeclaringClass() -> Class).getTypeName() -> String);
        let mods = access_modifiers(call!(env=> field.getModifiers() -> int), env);
        write!(buf, "Found field \"{field_name}\" with type {field_ty} in {class}").unwrap();
        if mods.len() > 0 {
            write!(buf, ": The Field is {}", display_natural_list(&mods)).unwrap();
        }
        write!(buf, ".").unwrap();
        if same_name_type_fields.len() > 1 {
            writeln!(buf, "\nThe are more fields that match this one:").unwrap();
            write!(buf, "{}", display_fields(&same_name_type_fields[1..], env)).unwrap();
        }
    } else if !same_name_fields.is_empty() {
        let ty = get_jni_type(&call!(env=> call!(env=> same_name_fields[0].getType() -> Class).getTypeName() -> String));
        let mods = access_modifiers(call!(env=> same_name_fields[0].getModifiers() -> int), env);
        write!(buf, "Field \"{field_name}\" does not have type {field_ty}: the Field has type {ty}").unwrap();
        if mods.len() > 0 {
            write!(buf, " and is {}", display_natural_list(&mods)).unwrap();
        }
        write!(buf, ".").unwrap();
        if same_name_fields.len() > 1 {
            writeln!(buf, "\nThere are more fields with this name and different types:").unwrap();
            write!(buf, "{}", display_fields(&same_name_fields[1..], env)).unwrap();
        }
    } else if !similar_name_type_methods.is_empty() {
        writeln!(buf, "Field not found, but found methods that might provide access to the field:").unwrap();
        write!(buf, "{}", display_methods(&similar_name_type_methods, env)).unwrap();
    } else if !similar_name_methods.is_empty() {
        writeln!(buf, "Found methods with similar name, but don't have the same type:").unwrap();
        write!(buf, "{}", display_methods(&similar_name_methods, env)).unwrap();
    } else if !same_type_fields.is_empty() || !same_type_methods.is_empty() {
        writeln!(buf, "Did not find any fields or methods with this name. These fields and methods have the same type:").unwrap();
        write!(buf, "{}", display_fields(&same_type_fields, env)).unwrap();
        write!(buf, "{}", display_methods(&same_type_methods, env)).unwrap();
    } else {
        write!(buf, "Did not find any other Fields or Methods that are similar to \"{field_name}: {field_ty}\"").unwrap();
    }

    println!("{buf}");
    Ok(())
}

/// Converts a regular *Java Type name* to a type that would be used by *JNI*.
/// No Generic types included.
/// E.g. `int` -> `I`, `java.lang.String[]` -> `[Ljava.lang.String;`
fn get_jni_type(mut type_name: &str) -> String {
    // Check if is Array
    let mut array_dimensions = 0;
    while let Some(ty) = type_name.strip_suffix("[]") {
        array_dimensions += 1;
        type_name = ty;
    }

    let ty = match type_name {
        "byte"    => "B".to_string(),
        "boolean" => "Z".to_string(),
        "char"    => "C".to_string(),
        "short"   => "S".to_string(),
        "int"     => "I".to_string(),
        "long"    => "J".to_string(),
        "float"   => "F".to_string(),
        "double"  => "D".to_string(),
        _ => format!("L{};", type_name.replace('.', "/")),
    };

    format!("{brackets}{ty}", brackets = "[".repeat(array_dimensions))
}

/// Converts a JNI *type signature* to a regular *Java Type name*.
/// E.g. `I` -> `int`, `[Ljava.lang.String;` -> `java.lang.String[]`
/// 
/// The type does not need to encompass the whole string.
/// The inputted string will be moved to to the point after the parsed section.
/// 
/// `panic!s` if the input was malformed.
fn display_jni_type(jni_type: &mut &str) -> String {
    // Check if is Array
    let mut array_dimensions = 0;
    while let Some(ty) = jni_type.strip_prefix("[") {
        array_dimensions += 1;
        *jni_type = ty;
    }

    let (ty, next) = match jni_type.as_bytes()[0] {
        b'L' => {
            jni_type[1..].split_once(';')
                .map(|(ty, next)| (ty.replace('/', "."), next))
                .expect("Object types must be terminated with a semicolon (;)")
        },
        c => {
            let next = &jni_type[1..];
            let ty = match c {
                b'V' => "void",
                b'B' => "byte",
                b'Z' => "boolean",
                b'C' => "char",
                b'S' => "short",
                b'I' => "int",
                b'J' => "long",
                b'F' => "float",
                b'D' => "double",
                _ if c.is_ascii_lowercase() => panic!("JNI type signatures must have types in UPPERCASE, encountered '{c}'"),
                _ => panic!("Unexpected character in JNI type signature: '{c}'")
            };
            (ty.to_string(), next)
        },
    };
    *jni_type = next;
    
    format!("{ty}{brackets}", brackets = "[]".repeat(array_dimensions))
}

// Display an array of items in natural language.
fn display_natural_list<T: Display>(slice: &[T]) -> String {
    if slice.len() == 0 {
        String::new()
    } else if slice.len() == 1 {
        slice[0].to_string()
    } else if slice.len() == 2 {
        format!("{} and {}", slice[0], slice[1])
    } else {
        // Put comma after every item, except last
        let mut buf = slice[..slice.len() - 2].iter()
            .map(|t| format!("{t}, "))
            .collect::<String>();
        buf.push_str(&format!("and {}", slice.last().unwrap()));
        buf
    }
}

fn access_modifiers(modifier: i32, env: &mut JNIEnv<'_>) -> Box<[&'static str]> {
    let mut mods = Vec::new();
    
    if call!(env=> static java.lang.reflect.Modifier.isPrivate(int(modifier)) -> bool) {
        mods.push("private")
    } else if call!(env=> static java.lang.reflect.Modifier.isProtected(int(modifier)) -> bool) {
        mods.push("protected")
    }
    if call!(env=> static java.lang.reflect.Modifier.isStatic(int(modifier)) -> bool) {
        mods.push("static")
    } else {
        mods.push("non-static")
    }

    mods.into_boxed_slice()
}

fn display_fields(fields: &[JObject<'_>], env: &mut JNIEnv<'_>) -> String {
    let mut buf = String::new();

    for field in fields {
        let class = call!(env=> call!(env=> field.getDeclaringClass() -> Class).getTypeName() -> String);
        let name = call!(env=> field.getName() -> String);
        let ty = call!(env=> call!(env=> field.getType() -> Class).getTypeName() -> String);
        let mods = access_modifiers(call!(env=> field.getModifiers() -> int), env)
            .iter()
            .map(|m| format!("{m} "))
            .collect::<String>();

        write!(buf, "\t{mods}{ty} {class}.{name}").unwrap();
    }

    buf
}
fn display_methods(methods: &[JObject<'_>], env: &mut JNIEnv<'_>) -> String {
    #![allow(unstable_name_collisions)]
    let mut buf = String::new();

    for method in methods {
        let class = call!(env=> call!(env=> method.getDeclaringClass() -> Class).getTypeName() -> String);
        let name = call!(env=> method.getName() -> String);
        let params = call!(env=> method.getParameterTypes() -> [Class])
            .iter()
            .map(|ty| call!(env=> ty.getTypeName() -> String))
            .intersperse(", ".to_string())
            .collect::<String>();
        let return_ty = call!(env=> call!(env=> method.getReturnType() -> Class).getTypeName() -> String);
        let mods = access_modifiers(call!(env=> method.getModifiers() -> int), env)
            .iter()
            .map(|m| format!("{m} "))
            .collect::<String>();

        writeln!(buf, "\t{mods}{return_ty} {class}.{name}({params})").unwrap();
    }

    buf
}

/// Returns a method's signature as it would look like written in the method's declaration.
/// The input signature must have the format of a JNI [*method type signature*](https://docs.oracle.com/javase/8/docs/technotes/guides/jni/spec/types.html#type_signatures).
/// E.g. `(II)V` -> `void (int, int)`.
/// 
/// `panic!s` if the input sig was malformed.
#[allow(unstable_name_collisions)]
fn display_sig(sig: &str) -> String {
    let (mut params, mut return_ty) = sig.strip_prefix('(')
        .unwrap()
        .split_once(')')
        .unwrap();

    let mut params_buf = Vec::new();
    while !params.is_empty() {
        params_buf.push(display_jni_type(&mut params));
    }
    let params = params_buf.into_iter()
        .intersperse(", ".to_string())
        .collect::<String>();
    let return_ty = display_jni_type(&mut return_ty);

    format!("{return_ty} ({params})")
}
/// The same as [`display_sig()`] but includes **method name**.
fn display_method(method_name: &str, sig: &str) -> String {
    let sig = display_sig(sig);
    let (return_ty, params) = sig.split_once(' ').unwrap();
    format!("{return_ty} {method_name}{params}")
}

#[cfg(test)]
mod tests {
    use std::sync::LazyLock;
    use jni::JavaVM;
    use crate::println;
    use super::*;

    pub static JVM: LazyLock<JavaVM> = LazyLock::new(|| {
        JavaVM::new(jni::InitArgsBuilder::new()
            .build()
            .unwrap()
        )
            .unwrap_or_else(|err| panic!("Error starting JavaVM: {err}"))
    });

    #[macro_export]
    macro_rules! setup_env {
        ($var:ident) => {
            let mut $var = JVM.attach_current_thread_permanently()
                .unwrap_or_else(|err| panic!("Error attaching current thread to JavaVM: {err}"));
        };
    }

    #[test]
    fn test_check_field_existence() {
        setup_env!(env);

        println!("--- Same Name and same Type Fields");
        check_field_existence(&env.find_class("java/lang/Integer").unwrap(), "SIZE", "I", &mut env).unwrap();
        println!("--- Same Name but different Type Fields");
        check_field_existence(&env.find_class("java/lang/Integer").unwrap(), "SIZE", "J", &mut env).unwrap();
        println!("--- Similar Name and same Type Methods");
        check_field_existence(&env.find_class("java/lang/Integer").unwrap(), "intValue", "I", &mut env).unwrap();
        check_field_existence(&env.find_class("java/lang/Integer").unwrap(), "string", "Ljava/lang/String;", &mut env).unwrap();
        println!("--- Similar Name but different Type Methods");
        check_field_existence(&env.find_class("java/lang/Integer").unwrap(), "intValue", "J", &mut env).unwrap();
        println!("--- Fields and Methods with the same Type");
        check_field_existence(&env.find_class("java/lang/Integer").unwrap(), "myOwnField", "I", &mut env).unwrap();
        println!("--- No matches");
        check_field_existence(&env.find_class("java/lang/Integer").unwrap(), "myOwnField", "Lme.my.Class;", &mut env).unwrap();
    }

    #[test]
    fn test_check_method_existence() {
        setup_env!(env);

        println!("--- Same Name and same Sig Methods");
        check_method_existence(env.find_class("java/lang/Integer").unwrap(), "hashCode", "()I", &mut env);
        println!("--- Same Name but different Sig Methods");
        check_method_existence(env.find_class("java/lang/Integer").unwrap(), "hashCode", "()V", &mut env);
        println!("--- Different Name but same Sig Methods");
        check_method_existence(env.find_class("java/lang/Integer").unwrap(), "myOwnMethod", "(II)I", &mut env);
        println!("--- No matches");
        check_method_existence(env.find_class("java/lang/Integer").unwrap(), "myOwnMethod", "(Lme.my.Class;)V", &mut env);
    }

    #[test]
    fn test_display_jni_type() {
        let mut ty = "BZCSIJFDLjava/lang/String;V";
        let mut arrs = "[I[[Ljava/lang/String;";

        assert_eq!(display_jni_type(&mut ty), "byte"); 
        assert_eq!(display_jni_type(&mut ty), "boolean"); 
        assert_eq!(display_jni_type(&mut ty), "char"); 
        assert_eq!(display_jni_type(&mut ty), "short"); 
        assert_eq!(display_jni_type(&mut ty), "int"); 
        assert_eq!(display_jni_type(&mut ty), "long"); 
        assert_eq!(display_jni_type(&mut ty), "float"); 
        assert_eq!(display_jni_type(&mut ty), "double"); 
        assert_eq!(display_jni_type(&mut ty), "java.lang.String"); 
        assert_eq!(display_jni_type(&mut ty), "void"); 
        assert_eq!(display_jni_type(&mut arrs), "int[]"); 
        assert_eq!(display_jni_type(&mut arrs), "java.lang.String[][]"); 
    }
}