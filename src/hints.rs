use jni::{
    errors::Result as JNIResult,
    objects::{JObject, JClass},
    JNIEnv
};
use std::fmt::Display;
use itertools::Itertools as _;
use crate::call;
#[allow(unused)]
use crate::{println, eprintln};

// TODO: implement hint for method calls

/// Check why getting a Field or getter/setter Method failed.
/// 
/// The implementation is messy because this is only meant to provide a hint to the user in a print statement.
/// This means that this will suggest any Field/Method that has any similarity to the intended field.
/// 
/// This function makes *A LOT* Java calls, so it can be quite slow,
/// so it only exists in DEBUG builds.
#[cfg(debug_assertions)]
pub fn check_field_existence(class: JClass<'_>, field_name: &'static str, field_ty: &'static str, env: &mut JNIEnv<'_>) -> JNIResult<()> {
    // Find all the classes that this Class is a descendant of (including itself)
    let mut classes = vec![class];
    while let Some(class) = env.get_superclass(classes.last().unwrap())? {
        classes.push(class);
    }

    /// Converts a regular *Java Type name* to a type that would be used by *JNI*.
    /// No Generic types included.
    /// E.g. `int` -> `I`, `java.lang.String[]` -> `[Ljava.lang.String;`
    fn get_jni_type(mut type_name: &str) -> String {
        fn component_ty(type_name: &str) -> String {
            match type_name {
                "byte"    => "B".to_string(),
                "boolean" => "Z".to_string(),
                "char"    => "C".to_string(),
                "short"   => "S".to_string(),
                "int"     => "I".to_string(),
                "long"    => "J".to_string(),
                "float"   => "F".to_string(),
                "double"  => "D".to_string(),
                _ => format!("L{};", type_name.replace('.', "/")),
            }
        }

        // Check if is Array
        let mut array_dimensions = 0;
        while let Some(ty) = type_name.strip_suffix("[]") {
            array_dimensions += 1;
            type_name = ty;
        }
        if array_dimensions > 0 {
            return format!("{brackets}{ty}", ty = component_ty(type_name), brackets = "[".repeat(array_dimensions));
        }

        component_ty(type_name)
    }

    let mut same_name_type_fields = Vec::new();
    let mut same_name_fields = Vec::new();
    let mut same_type_fields = Vec::new();
    let mut similar_name_type_methods = Vec::new();
    let mut similar_name_methods = Vec::new();
    let mut same_type_methods = Vec::new();

    // Get all the Fields and Methods of the Class (and its ancestors) and separate them into categories.
    for class in classes {
        let fields = call!(class.getDeclaredFields() -> [java.lang.reflect.Field]);
        let methods = call!(class.getDeclaredMethods() -> [java.lang.reflect.Method]);

        for field in fields {
            let name = call!(field.getName() -> String);
            let ty = get_jni_type(&call!(call!(field.getType() -> java.lang.Class).getTypeName() -> String));

            if name == field_name && ty == field_ty {
                same_name_type_fields.push(field);
            } else if name == field_name {
                same_name_fields.push(field);
            } else if ty == field_ty {
                same_type_fields.push(field);
            }
        }

        for method in methods {
            let name = call!(method.getName() -> String);
            // TODO: use all lowercase
            let similar_name = name.ends_with(&utils::first_char_uppercase(field_name)) || name.ends_with(field_name);
            // The method is considered to have the "same type" as the field if it has only one argument with the same type OR the return type is the same type.
            let same_type = {
                let params = call!(method.getParameterTypes() -> [java.lang.Class]);
                let return_type = get_jni_type(&call!(call!(method.getReturnType() -> java.lang.Class).getTypeName() -> String));

                (params.len() == 1 && return_type == "V" && get_jni_type(&call!(params[0].getTypeName() -> String)) == field_ty)
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

    // Display an array of items in natural language.
    fn natural_display<T: Display>(slice: &[T]) -> String {
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
        
        if call!(static java.lang.reflect.Modifier.isPrivate(int(modifier)) -> bool) {
            mods.push("private")
        } else if call!(static java.lang.reflect.Modifier.isProtected(int(modifier)) -> bool) {
            mods.push("protected")
        }
        if call!(static java.lang.reflect.Modifier.isStatic(int(modifier)) -> bool) {
            mods.push("static")
        } else {
            mods.push("non-static")
        }

        mods.into_boxed_slice()
    }

    fn print_fields(fields: &[JObject<'_>], env: &mut JNIEnv<'_>) {
        for field in fields {
            let class = call!(call!(field.getDeclaringClass() -> java.lang.Class).getTypeName() -> String);
            let name = call!(field.getName() -> String);
            let ty = call!(call!(field.getType() -> java.lang.Class).getTypeName() -> String);
            let mods = access_modifiers(call!(field.getModifiers() -> int), env)
                .iter()
                .map(|m| format!("{m} "))
                .collect::<String>();
            println!("\t{mods}{class}.{name}: {ty}");
        }
    }
    fn print_methods(methods: &[JObject<'_>], env: &mut JNIEnv<'_>) {
        #![allow(unstable_name_collisions)]
        for method in methods {
            let class = call!(call!(method.getDeclaringClass() -> java.lang.Class).getTypeName() -> String);
            let name = call!(method.getName() -> String);
            let params = call!(method.getParameterTypes() -> [java.lang.Class])
                .iter()
                .map(|ty| call!(ty.getTypeName() -> String))
                .intersperse(", ".to_string())
                .collect::<String>();
            let return_ty = call!(call!(method.getReturnType() -> java.lang.Class).getTypeName() -> String);
            let mods = access_modifiers(call!(method.getModifiers() -> int), env)
                .iter()
                .map(|m| format!("{m} "))
                .collect::<String>();
            println!("\t{mods}{class}.{name}({params}): {return_ty}");
        }
    }

    // Messy ahh print statements
    Ok(if !same_name_type_fields.is_empty() {
        let mods = access_modifiers(call!(same_name_type_fields[0].getModifiers() -> int), env);
        print!("Found field \"{field_name}\" with type {field_ty}");
        if mods.len() > 0 {
            print!(": The Field is {}", natural_display(&mods));
        }
        println!(".");
        if same_name_type_fields.len() > 1 {
            println!("The are more fields that match this one:");
            print_fields(&same_name_type_fields[1..], env);
        }
    } else if !same_name_fields.is_empty() {
        let ty = get_jni_type(&call!(call!(same_name_fields[0].getType() -> java.lang.Class).getTypeName() -> String));
        let mods = access_modifiers(call!(same_name_fields[0].getModifiers() -> int), env);
        print!("Field \"{field_name}\" does not have type {field_ty}: the Field has type {ty}");
        if mods.len() > 0 {
            print!(" and is {}", natural_display(&mods));
        }
        println!(".");
        if same_name_fields.len() > 1 {
            println!("There are more fields with this name and different types:");
            print_fields(&same_name_fields[1..], env);
        }
    } else if !similar_name_type_methods.is_empty() {
        println!("Field not found, but found methods that might provide access to the field:");
        print_methods(&similar_name_type_methods, env);
    } else if !similar_name_methods.is_empty() {
        println!("Found methods with similar name, but don't have the same type:");
        print_methods(&similar_name_methods, env);
    } else if !same_type_fields.is_empty() || !same_type_methods.is_empty() {
        println!("Did not find any fields or methods with this name. These fields and methods have the same type:");
        print_fields(&same_type_fields, env);
        print_methods(&same_type_methods, env);
    } else {
        println!("Did not find any other Fields or Methods that are similar to \"{field_name}: {field_ty}\"");
    })
}

#[cfg(test)]
mod tests {
    use jni::JavaVM;
    use crate::println;
    use super::*;

    #[test]
    fn test_check_field_existence() {
        let jvm = JavaVM::new(jni::InitArgsBuilder::new()
            .build()
            .unwrap()
        )
            .unwrap_or_else(|err| panic!("Error starting JavaVM: {err}"));
        let mut env = jvm.attach_current_thread_permanently()
            .unwrap_or_else(|err| panic!("Error attaching current thread to JavaVM: {err}"));

        println!("--- Same Name and same Type Fields");
        check_field_existence(env.find_class("java/lang/Integer").unwrap(), "SIZE", "I", &mut env).unwrap();
        println!("--- Same Name but different Type Fields");
        check_field_existence(env.find_class("java/lang/Integer").unwrap(), "SIZE", "J", &mut env).unwrap();
        println!("--- Similar Name and same Type Methods");
        check_field_existence(env.find_class("java/lang/Integer").unwrap(), "intValue", "I", &mut env).unwrap();
        check_field_existence(env.find_class("java/lang/Integer").unwrap(), "string", "Ljava/lang/String;", &mut env).unwrap();
        println!("--- Similar Name but different Type Methods");
        check_field_existence(env.find_class("java/lang/Integer").unwrap(), "intValue", "J", &mut env).unwrap();
        println!("--- Fields and Methods with the same Type");
        check_field_existence(env.find_class("java/lang/Integer").unwrap(), "myOwnField", "I", &mut env).unwrap();
        println!("--- No matches");
        check_field_existence(env.find_class("java/lang/Integer").unwrap(), "myOwnField", "Lme.my.Class;", &mut env).unwrap();
        panic!("---");
    }
}