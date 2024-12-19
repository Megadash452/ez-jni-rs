mod r#impl;
mod impl_array;
mod impl_exception;

use jni::{JNIEnv, objects::{JObject, JThrowable}};
use thiserror::Error;
use ez_jni_macros::call;


#[derive(Debug, Error)]
pub enum FromObjectError {
    #[error("Object can't be NULL")]
    Null,
    #[error("{}", match target_class {
        Some(target_class) => format!("Expected the Object to be the Class or a descendant of the Class \"{target_class}\", but the actual Class of the Object is \"{obj_class}\""),
        None => format!("The Class Object did not match any of the classes of the variants; Found Class \"{obj_class}\"")
    })]
    ClassMismatch { obj_class: String, target_class: Option<String> },
    #[error("Could not find field {name:?} of type {ty} in class {target_class}; maybe its private?")]
    FieldNotFound { name: String, ty: String, target_class: String },
    // #[error("{0}")]
    // Other(String)
}

/// Allows converting a *Java Object* to a Rust type by reading the Object's data.
/// 
/// ### Derive
/// This trait has a **derive macro** available from [`ez_jni_macros`].
/// Use it on *structs* to indicate that only 1 specific Class is expected.
/// Use it on *enums* to expect different Classes (one for each variant).
/// 
/// **Attributes**:
/// - **`class`**: Specifies the **Class Path** that the *struct or enum variant* expects (*optional* for the enum item).
/// - **`field`**: Specify certain properties to constrol how a *struct's field* is assigned by accessing a *Object's member* (field or getter).
///   By default, the struct's field name and type is used to produce the JNI call, but this can be changed.
///   Note: **name** and **call** are mutually exclusive, and either one MUST be used if the field belongs to a *Tuple struct*.
///   - **`name`**: Use a different name for the Object's field lookup instead of the field's name.
///     Mutually exclusive with `call`.
///   - **`call`**: Instead of accessing a field, Call a *getter method* with this name.
///   - **`class`**: If the struct field's type is [`JObject`] require that it be of this class.
///     This property is required for *non-primitives*.
/// 
/// ```
/// # use ez_jni::FromObject;
/// 
/// #[derive(FromObject)]
/// #[class(me.author.MyClass)]
/// struct MyClass {
///     // Implicitly calls gets field or calls getMessage()
///     message: String
/// }
/// 
/// #[derive(FromObject)]
/// #[class(me.author.MyClass)] // Optional
/// enum MyClasses {
///     #[class(me.author.MyClassDescendant)]
///     Descendant { message: String },
///     #[class(me.author.MyOtherClass)]
///     Other(#[field(name = message)] String),
///     #[class(me.author.MyFinalClass)]
///     Final(#[field(call = getMessage)] String),
/// }
/// ```
pub trait FromObject<'local>
where Self: Sized {
    /// Construct a [`Self`] by reading data from a *Java Object*.
    /// Will [`panic!`] if any of the underlying JNI calls fail.
    fn from_object(object: &JObject, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError>;
}

pub trait ToObject<'local> {
    /// Create an instance of a Class by constructing an object from data in a *Rust struct*.
    /// Will [`panic!`] if any of the underlying JNI calls fail.
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local>;
}



/// Allows converting *Java Exceptions* to Rust types that allow for better error handling.
/// 
/// # Derive
/// Uses the same derive syntax as [`FromObject`].
/// 
/// ```
/// # use ez_jni::FromException;
/// 
/// #[derive(FromException)]
/// #[class(java.lang.Exception)]
/// struct MyStError {
///     // Implicitly calls gets field or calls getMessage()
///     message: String
/// }
/// 
/// #[derive(FromException)]
/// #[class(java.lang.Exception)] // Optional
/// enum MyEnmError {
///     #[class(java.lang.NullPointerException)]
///     Null,
///     #[class(me.author.ElementExists)]
///     AlreadyExists(#[field(name = message)] String),
///     #[class(java.lang.Exception)]
///     Other(#[field(call = getMessage)] String),
/// }
/// ```
pub trait FromException<'local>
where Self: Sized {
    fn from_exception(exception: &JThrowable, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError>;
}

/// Does the required checks to ensure that a Java Object is valid.
fn object_check_boilerplate(object: &JObject, path: &'static str, env: &mut JNIEnv) -> Result<(), FromObjectError> {
    if object.is_null() {
        return Err(FromObjectError::Null)
    }
    
    let obj_class = env.get_object_class(object)
        .unwrap_or_else(|err| panic!("Failed to get Object's class: {err}"));
    
    if !env.is_instance_of(object, path).unwrap() {
        return Err(FromObjectError::ClassMismatch {
            obj_class: call!(obj_class.getName() -> String),
            target_class: Some(path.to_string())
        })
    }

    Ok(())
}
