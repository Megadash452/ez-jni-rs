#![cfg(debug_assertions)]

use jni::{JNIEnv, objects::{JObject, JClass}};
use std::fmt::Display;
use itertools::Itertools;
use crate::{call, utils::{check_object_class, ResultExt as _, JniResultExt as _}, Class, FromObject};

pub fn print_method_existence_report(class: &JClass<'_>, method_name: &'static str, method_sig: &str, is_static: bool, env: &mut JNIEnv<'_>) {
    println!("{}", MethodHintReport::check_method_existence(class, method_name, method_sig, is_static, env))
}
pub fn print_field_existence_report(class: &JClass<'_>, field_name: &'static str, field_ty: &str, is_static: bool, env: &mut JNIEnv<'_>) {
    println!("{}", FieldHintReport::check_field_existence(class, field_name, field_ty, is_static, env))
}

#[derive(Debug)]
struct Method {
    mods: Mods,
    class: String,
    name: String,
    params: Box<[Type]>,
    return_ty: Type
}
impl Method {
    /// Get a [`Method`] struct from arbitrary values.
    /// 
    /// `panic!`s if the **signature** has incorrect formatting.
    pub fn new(class: &JClass, name: &str, sig: &str, is_static: bool, env: &mut JNIEnv<'_>) -> Self {
        let (params, return_ty) = Type::parse_method_sig(sig).unwrap_display();
        Self {
            mods: Mods::new_static(is_static),
            class: call!(env=> class.getName() -> String),
            name: name.to_string(),
            params,
            return_ty,
        }
    }

    pub fn jni_signature(&self) -> String {
        let params = self.params.iter()
            .map(|ty| ty.sig_type())
            .collect::<String>();
        let return_ty = &self.return_ty.sig_type();

        format!("({params}){return_ty}")
    }

    /// A method is considered a **getter** if it contains `0` *parameters* AND *returns* a value.
    /// 
    /// This method returns the *return* [`Type`] of the **getter**.
    pub fn is_getter(&self) -> Option<&Type> {
        if self.params.len() == 0
        && self.return_ty != "void" {
            Some(&self.return_ty)
        } else {
            None
        }
    }
    /// A method is considered a **setter** if it contains exactly `1` *parameter* AND *returns* void.
    /// 
    /// This method returns the *parameter* [`Type`] of the **setter**.
    pub fn is_setter(&self) -> Option<&Type> {
        if self.params.len() == 1
        && self.return_ty == "void" {
            Some(&self.params[0])
        } else {
            None
        }
    }
}
impl FromObject<'_, '_, '_> for Method {
    fn from_object_env(obj: &JObject<'_>, env: &mut JNIEnv<'_>) -> Result<Self, crate::FromObjectError> {
        check_object_class(obj, &Self::class(), env)?;
        Ok(Self {
            mods: Mods(call!(env=> obj.getModifiers() -> int)),
            class: call!(env=> call!(env=> obj.getDeclaringClass() -> Class).getTypeName() -> String),
            name: call!(env=> obj.getName() -> String),
            params: call!(env=> obj.getParameterTypes() -> [Class])
                .iter()
                .map(|ty| Type(call!(env=> ty.getTypeName() -> String)))
                .collect(),
            return_ty: Type(call!(env=> call!(env=> obj.getReturnType() -> Class).getTypeName() -> String)),
        })
    }
}
impl Class for Method {
    fn class() -> std::borrow::Cow<'static, str> {
        std::borrow::Cow::Borrowed("java/lang/reflect/Method")
    }
}
impl Display for Method {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        #![allow(unstable_name_collisions)]

        let mods = self.mods.item_display();
        let class = &self.class;
        let name = &self.name;
        let params = self.params.iter()
            .map(|ty| ty.0.as_str())
            .intersperse(", ")
            .collect::<String>();
        let return_ty = &self.return_ty;
        write!(f, "{mods} {return_ty} {class}.{name}({params})")
    }
}

#[derive(Debug)]
struct Field {
    mods: Mods,
    class: String,
    name: String,
    ty: Type
}
impl Field {
    /// Get a [`Field`] struct from arbitrary values.
    /// 
    /// `panic!`s if the **signature type** has incorrect formatting.
    pub fn new(class: &JClass, name: &str, sig_type: &str, is_static: bool, env: &mut JNIEnv<'_>) -> Self {
        Self {
            mods: Mods::new_static(is_static),
            class: call!(env=> class.getName() -> String),
            name: name.to_string(),
            ty: Type::from_sig_type(sig_type),
        }
    }
}
impl FromObject<'_, '_, '_> for Field {
    fn from_object_env(obj: &JObject<'_>, env: &mut JNIEnv<'_>) -> Result<Self, crate::FromObjectError> {
        Ok(Self {
            class: call!(env=> call!(env=> obj.getDeclaringClass() -> Class).getTypeName() -> String),
            name: call!(env=> obj.getName() -> String),
            ty: Type(call!(env=> call!(env=> obj.getType() -> Class).getTypeName() -> String)),
            mods: Mods(call!(env=> obj.getModifiers() -> int)),
        })
    }
}
impl Class for Field {
    fn class() -> std::borrow::Cow<'static, str> {
        std::borrow::Cow::Borrowed("java/lang/reflect/Field")
    }
}
impl Display for Field {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        #![allow(unstable_name_collisions)]

        let mods = self.mods.item_display();
        let class = &self.class;
        let name = &self.name;
        let ty = &self.ty;

        write!(f, "{mods} {ty} {class}.{name}")
    }
}

/// Mods (e.g. private) are bitflags.
#[derive(Debug, Copy, Clone)]
struct Mods(i32);
impl Mods {
    // Java uses these mask values to determine the visibility of a method. See https://docs.oracle.com/javase/8/docs/api/constant-values.html#java.lang.reflect.Modifier.ABSTRACT
    const PUBLIC_MASK: i32 = 0b0001;
    const PRIVATE_MASK: i32 = 0b0100;
    const PROTECTED_MASK: i32 = 0b0010;
    const STATIC_MASK: i32 = 0b1000;

    /// Create a new [`Modifiers`][Mods] struct with only the `static` flag set.
    pub fn new_static(is_static: bool) -> Self {
        if is_static { Self(Self::PUBLIC_MASK | Self::STATIC_MASK) } else { Self(0) }
    }

    pub fn visibility(self) -> &'static str {
        static PRIVATE_STR: &str = "private";

        if {
            // Check if multiple visibility flags are set
            let n = self.0 & (Self::PRIVATE_MASK | Self::PROTECTED_MASK | Self::PUBLIC_MASK);
            n > 0 && (n & (n - 1)) != 0
        } {
            panic!("more than 1 visibility flag is set for Mods({})", self.0)
        // Assume 
        } if self.apply(Self::PRIVATE_MASK) {
            PRIVATE_STR
        } else if self.apply(Self::PROTECTED_MASK) {
            "protected"
        } else if self.apply(Self::PUBLIC_MASK) {
            "public"
        } else {
            // Assume private if no visibility bits are set.
            // Although it's not stated in https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-4.html#jvms-4.6-200-A.1
            // but that is the behavior i'm experiencing
            PRIVATE_STR
        }
    }
    pub fn staticness(self) -> &'static str {
        if self.apply(Self::STATIC_MASK) {
            "static"
        } else {
            "non-static"
        }
    }
    #[doc(hidden)]
    #[inline(always)]
    fn apply(self, mask: i32) -> bool {
        self.0 & mask != 0
    }

    /// Get the *difference* between 2 [`Modifiers`][Mods].
    /// Essentially, this outputs the **names** of the bit flags that are on in `self` but not in `other`.
    /// 
    /// Will output a **message** if `self == other`.
    fn contrast(self, other: Self) -> String {
        let mut buf = Vec::new();

        if self.0 == other.0 {
            return "... hmmm, that's weird. This method matches exactly with the one that was called... Something is wrong".to_string();
        }

        if !std::ptr::eq(self.visibility(), other.visibility()) {
            buf.push(self.visibility());
        }
        if self.apply(Self::STATIC_MASK) != other.apply(Self::STATIC_MASK) {
            buf.push(self.staticness());
        }

        if buf.is_empty() {
            panic!("Self({}) is NOT equal to Other({}), but still no difference was found.", self.0, other.0);
        }

        display_natural_list(&buf)

    }

    /// Returns a string with the name of the active modifiers as it would be written in Java source code.
    pub fn item_display(self) -> String {
        if self.apply(Self::STATIC_MASK) {
            format!("{} static", self.visibility())
        } else {
            self.visibility().to_string()
        }
    }
}
impl Display for Mods {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&display_natural_list(&[self.visibility(), self.staticness()]))
    }
}

/// A **Type** as it would be written in *Java source code*.
#[derive(Debug, Eq, PartialEq)]
struct Type(String);
impl Type {
    /// Parse a Java **method's signature** by splitting the *parameter types* by the character.
    /// 
    /// The input signature must have the format of a JNI [*method type signature*](https://docs.oracle.com/javase/8/docs/technotes/guides/jni/spec/types.html#type_signatures).
    /// 
    /// Returns a tuple with the the **list of parameters** and the **return type**.
    fn parse_method_sig(sig: &str) -> Result<(Box<[Self]>, Self), &'static str> {
        // The params variable is a cursor of which character the param parser is looking at.
        let (mut params, return_ty) = sig.strip_prefix('(')
            .ok_or("Expected method signature to start with a perenthesis (")?
            .split_once(')')
            .ok_or("Did not find closing perenthesis ) for the method signature")?;

        let mut params_buf = Vec::new();
        while !params.is_empty() {
            params_buf.push(Self::from_sig_helper(&mut params));
        }

        Ok((params_buf.into_boxed_slice(), Self::from_sig_type(return_ty)))
    }

    /// Converts a JNI *type signature* to a regular *Java Type name*.
    /// E.g. `I` -> `int`, `[Ljava.lang.String;` -> `java.lang.String[]`
    /// 
    /// Parses according to [*JNI type signature*](https://docs.oracle.com/javase/8/docs/technotes/guides/jni/spec/types.html#type_signatures).
    /// 
    /// `panic!s` if the input was malformed.
    pub fn from_sig_type(sig_type: &str) -> Self {
        let mut cursor = sig_type;
        Self::from_sig_helper(&mut cursor)
    }

    /// Parses **part** of a string for a JNI *type signature*.
    /// The passed-in string is advanced to the next JNI *type* (if any).
    #[doc(hidden)]
    fn from_sig_helper(cursor: &mut &str) -> Self {
        // Check if is Array
        let mut array_dimensions = 0;
        while let Some(ty) = cursor.strip_prefix("[") {
            array_dimensions += 1;
            *cursor = ty;
        }

        let (ty, next) = match cursor.chars().next().expect("cursor can't be empty") {
            'L' => cursor[1..].split_once(';')
                .map(|(ty, next)| (ty.replace('/', "."), next))
                .expect("Object types must be terminated with a semicolon (;)"),
            c => {
                let next = &cursor[1..];
                let ty = match c {
                    'V' => "void",
                    'B' => "byte",
                    'Z' => "boolean",
                    'C' => "char",
                    'S' => "short",
                    'I' => "int",
                    'J' => "long",
                    'F' => "float",
                    'D' => "double",
                    _ if c.is_ascii_lowercase() => panic!("JNI type signatures must have types in UPPERCASE, encountered '{c}'"),
                    _ => panic!("Unexpected character in JNI type signature: '{c}'")
                };
                (ty.to_string(), next)
            },
        };
        *cursor = next;
        
        Self(format!("{ty}{brackets}", brackets = "[]".repeat(array_dimensions)))
    }

    /// Converts a regular *Java Type name* to a type that would be used by *JNI*.
    /// No Generic types included.
    /// E.g. `int` -> `I`, `java.lang.String[]` -> `[Ljava.lang.String;`
    pub fn sig_type(&self) -> String {
        let mut type_name = self.0.as_str();

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
}
impl PartialEq<&Type> for Type {
    fn eq(&self, other: &&Type) -> bool {
        self.0 == other.0
    }
}
impl PartialEq<&str> for Type {
    fn eq(&self, other: &&str) -> bool {
        self.0 == *other
    }
}
impl PartialEq<str> for Type {
    fn eq(&self, other: &str) -> bool {
        self.0 == other.as_ref()
    }
}
impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

/// Find all the **classes** that this [JClass] is a descendant of (including itself).
/// 
/// `panic!`s if the JNI function returns an error.
fn get_superclasses<'local>(class: &JClass<'_>, env: &mut JNIEnv<'local>) -> Box<[JClass<'local>]> {
    // new_local_ref() is ok here because it is only used once with no recursion or anything else fancy
    let first = env.new_local_ref(class).unwrap_jni(env);

    let mut classes = vec![JClass::from(first)];
    while let Some(class) = env.get_superclass(classes.last().unwrap()).unwrap_jni(env) {
        classes.push(class);
    }

    classes.into_boxed_slice()
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

/// Contains information on the reason why **JNI** could not find a **method** for a class.
/// 
/// The report can be printed out in natureal language.
pub struct MethodHintReport {
    /// The **method** that the caller tried to use/find.
    user_method: Method,
    hint: MethodHint
}
// Enum variants are instatntiated by priority in the order that they are declared
#[derive(Debug)]
enum MethodHint {
    /// Found a **method** where the only difference from `user_method` is its [`Modifiers`][Mods].
    MatchNameAndSig {
        method: Method,
        /// Other **methods** that also match *name* and *signature*, but are from **super classes**.
        others: Box<[Method]>,
    },
    /// Found a **method** with the same *name* as `user_method`, but has a different *signature*.
    MatchName {
        method: Method,
        /// Other **methods** that also match *name*, but are from **super classes**.
        others: Box<[Method]>,
    },
    /// Found at least one **method** with the same *signature* (but different *names*) as `user_method`.
    MatchSig {
        methods: Box<[Method]>,
    },
    NoMatch
}
impl MethodHintReport {
    /// Check why method could not be called.
    /// 
    /// This function makes *A LOT* Java calls, so it can be quite slow,
    /// so it only exists in DEBUG builds.
    pub fn check_method_existence(class: &JClass<'_>, method_name: &'static str, method_sig: &str, is_static: bool, env: &mut JNIEnv<'_>) -> Self {
        let classes = get_superclasses(class, env);
        let user_method = Method::new(class, method_name, method_sig, is_static, env);
        let mut same_name_sig_methods = Vec::new();
        let mut same_name_methods = Vec::new();
        let mut same_sig_methods = Vec::new();

        for class in classes {
            for obj in call!(env=> class.getDeclaredMethods() -> [java.lang.reflect.Method]) {
                let method = Method::from_object_env(&obj, env).unwrap_display();

                if method.name == method_name
                && method.jni_signature() == user_method.jni_signature() {
                    same_name_sig_methods.push(method);
                } else if method.name == method_name {
                    same_name_methods.push(method);
                } else if method.jni_signature() == user_method.jni_signature() {
                    same_sig_methods.push(method);
                }
            }
        }

        Self {
            user_method,
            hint: if !same_name_sig_methods.is_empty() {
                MethodHint::MatchNameAndSig {
                    method: same_name_sig_methods.remove(0),
                    others: same_name_sig_methods.into_boxed_slice(),
                }
            } else if !same_name_methods.is_empty() {
                MethodHint::MatchName {
                    method: same_name_methods.remove(0),
                    others: same_name_methods.into_boxed_slice(),
                }
            } else if !same_sig_methods.is_empty() {
                MethodHint::MatchSig {
                    methods: same_sig_methods.into_boxed_slice(),
                }
            } else {
                MethodHint::NoMatch
            }
        }
    }
}
impl Display for MethodHintReport {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.hint {
            MethodHint::MatchNameAndSig { method, others } => {
                writeln!(f, "Found method \"{method}\", but the method is {}.", method.mods.contrast(self.user_method.mods))?;
                if others.len() > 1 {
                    writeln!(f, "The are more methods that match this one:")?;
                    for method in others {
                        writeln!(f, "\t{method}")?;
                    }
                }
            },
            MethodHint::MatchName { method, others } => {
                writeln!(f, "Incorrect signature for method \"{}\":\n      The correct signature is \"{method}\".", self.user_method)?;
                if others.len() > 1 {
                    writeln!(f, "There are more methods with this name:")?;
                    for method in others {
                        writeln!(f, "\t{method}")?;
                    }
                }
            },
            MethodHint::MatchSig { methods } => {
                writeln!(f, "Did not find method with name \"{}\". These methods have the same signature:", self.user_method.name)?;
                for method in methods {
                    writeln!(f, "\t{method}")?;
                }
            },
            MethodHint::NoMatch => write!(f, "Did not find any Methods similar to \"{}\"", self.user_method)?,
        };
        Ok(())
    }
}

/// Contains information on the reason why **JNI** could not find a **field** for a class.
/// 
/// The report can be printed out in natureal language.
pub struct FieldHintReport {
    /// The **field** that the caller tried to use/find.
    user_field: Field,
    hint: FieldHint
}
// Enum variants are instatntiated by priority in the order that they are declared
#[derive(Debug)]
enum FieldHint {
    /// Found a **field** where the only difference from `user_field` is its [`Modifiers`][Mods].
    MatchFieldNameAndType {
        field: Field,
        /// Other **fields** that also match *name* and *type*, but are from **super classes**.
        others: Box<[Field]>,
    },
    /// Found a **field** with the same *name* as `user_field`, but has a different *type*.
    MatchFieldName {
        field: Field,
        /// Other **fields** that also match *name*, but are from **super classes**.
        others: Box<[Field]>,
    },
    /// Found **getter/setter methods** that could possibly be used for `user_field`.
    /// 
    /// A method is a match if:
    ///  * **Name**: The method *name* contains the `user_field`'s name.
    ///  * **Type**: The method has no *parameters* and the same *return type* as the `user_field`'s type.
    ///    * OR The method's *return type* is `void` and has exactly 1 *parameter* of the same type as the `user_field`'s type.
    MatchMethodNameAndType {
        methods: Box<[Method]>,
    },
    /// Found **getter/setter methods** whose *name* contains the name of `user_field`, but don't have the same *type*.
    MatchMethodName {
        methods: Box<[Method]>,
    },
    /// Found **fields** and/or **methods** that match the `user_field`'s *type*, but with a different name.
    MatchType {
        fields: Box<[Field]>,
        methods: Box<[Method]>,
    },
    NoMatch
}
impl FieldHintReport {
    /// Check why getting a Field or getter/setter Method failed.
    /// 
    /// The implementation is messy because this is only meant to provide a hint to the user in a print statement.
    /// This means that this will suggest any Field/Method that has any similarity to the intended field.
    /// 
    /// This function makes *A LOT* Java calls, so it can be quite slow,
    /// so it only exists in DEBUG builds.
    pub fn check_field_existence(class: &JClass<'_>, field_name: &'static str, field_ty: &str, is_static: bool, env: &mut JNIEnv<'_>) -> Self {
        let classes = get_superclasses(class, env);
        let user_field = Field::new(class, field_name, field_ty, is_static, env);
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

            for obj in fields {
                let field = Field::from_object_env(&obj, env).unwrap_display();

                if field.name == user_field.name
                && field.ty == user_field.ty {
                    same_name_type_fields.push(field);
                } else if field.name == field_name {
                    same_name_fields.push(field);
                } else if field.ty == user_field.ty {
                    same_type_fields.push(field);
                }
            }

            for obj in methods {
                let method = Method::from_object_env(&obj, env).unwrap_display();
                // Checks similarities in name by making it all lowercase and removing possible underscores (_).
                let similar_name = method.name
                    .to_lowercase()
                    .replace('_', "")
                    .contains(&user_field.name.to_lowercase().replace('_', ""));
                // The method is considered to have the "same type" as user_field if it is a setter or getter with the same type as the user_field
                let same_type = method.is_getter()
                    .or(method.is_setter())
                    .is_some_and(|ty| user_field.ty == ty);

                if similar_name && same_type {
                    similar_name_type_methods.push(method);
                } else if similar_name {
                    similar_name_methods.push(method);
                } else if same_type {
                    same_type_methods.push(method);
                }
            }
        }

        Self {
            user_field,
            hint: if !same_name_type_fields.is_empty() {
                FieldHint::MatchFieldNameAndType {
                    field: same_name_type_fields.remove(0),
                    others: same_name_type_fields.into_boxed_slice(),
                }
            } else if !same_name_fields.is_empty() {
                FieldHint::MatchFieldName {
                    field: same_name_fields.remove(0),
                    others: same_name_fields.into_boxed_slice(),
                }
            } else if !similar_name_type_methods.is_empty() {
                FieldHint::MatchMethodNameAndType {
                    methods: similar_name_type_methods.into_boxed_slice()
                }
            } else if !similar_name_methods.is_empty() {
                FieldHint::MatchMethodName {
                    methods: similar_name_methods.into_boxed_slice()
                }
            } else if !same_type_fields.is_empty() || !same_type_methods.is_empty() {
                FieldHint::MatchType {
                    fields: same_type_fields.into_boxed_slice(),
                    methods: same_type_methods.into_boxed_slice()
                }
            } else {
                FieldHint::NoMatch
            }
        }
    }
}
impl Display for FieldHintReport {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.hint {
            FieldHint::MatchFieldNameAndType { field, others } => {
                // The first field in this list is for the same class that was passed in, so it should be highlighted.
                writeln!(f, "Found field \"{field}\", but the field is {}.", field.mods.contrast(self.user_field.mods))?;
                // Other fields in the list are from superclasses, which should still be outputted.
                if others.len() > 1 {
                    writeln!(f, "The are more fields that match this one:")?;
                    for field in others {
                        writeln!(f, "\t{field}")?
                    }
                }
            },
            FieldHint::MatchFieldName { field, others } => {
                writeln!(f, "Incorrect type for field \"{field}\":\n\tThe correct type is {} and is {}", field.ty, field.mods)?;
                if others.len() > 1 {
                    writeln!(f, "\nThere are more fields with this name and different types:")?;
                    for field in others {
                        writeln!(f, "\t{field}")?
                    }
                }
            },
            FieldHint::MatchMethodNameAndType { methods } => {
                writeln!(f, "Field not found, but found getter methods that might provide access to the field:")?;
                for method in methods {
                    writeln!(f, "\t{method}")?
                }
            },
            FieldHint::MatchMethodName { methods } => {
                writeln!(f, "Found getter methods with similar name, but don't have the same type:")?;
                for method in methods {
                    writeln!(f, "\t{method}")?
                }
            },
            FieldHint::MatchType { fields, methods } => {
                writeln!(f, "Did not find any fields or methods with this name. These fields and methods have the same type:")?;
                for field in fields {
                    writeln!(f, "\t{field}")?
                }
                for method in methods {
                    writeln!(f, "\t{method}")?
                }
            },
            FieldHint::NoMatch => write!(f, "Did not find any other Fields or Methods that are similar to \"{}\"", self.user_field)?,
        };

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::sync::LazyLock;
    use jni::JavaVM;
    use super::*;

    pub static JVM: LazyLock<JavaVM> = LazyLock::new(|| {
        JavaVM::new(jni::InitArgsBuilder::new()
            .build()
            .unwrap()
        )
            .unwrap_or_else(|err| panic!("Error starting JavaVM: {err}"))
    });

    macro_rules! setup_env {
        ($var:ident) => {
            let mut $var = JVM.attach_current_thread_permanently()
                .unwrap_or_else(|err| panic!("Error attaching current thread to JavaVM: {err}"));
        };
    }

    macro_rules! check_report_hint {
        ($var:expr => $enm_ident:ident::$variant_ident:ident $pattern:tt => $block:block) => {
            match $var {
                $enm_ident::$variant_ident $pattern => $block,
                v => panic!("expected {enm}::{variant}, but got {enm}::{v:?}", enm = stringify!($enm_ident), variant = stringify!($variant_ident))
            }
        };
    }

    #[test]
    fn test_check_method_existence() {
        setup_env!(env);
        let mut report;

        // Same Name and same Sig Methods
        report = MethodHintReport::check_method_existence(&env.find_class("java/lang/Integer").unwrap(), "hashCode", "()I", true, &mut env);
        check_report_hint!(report.hint => MethodHint::MatchNameAndSig { method: Method { mods, name, params, return_ty, .. }, .. } => {
            assert_eq!(mods.staticness(), "non-static");
            assert_eq!(name, "hashCode");
            assert!(params.is_empty());
            assert_eq!(return_ty, "int");
        });
        // Same Name but different Sig Methods
        report = MethodHintReport::check_method_existence(&env.find_class("java/lang/Integer").unwrap(), "hashCode", "()V", false, &mut env);
        check_report_hint!(report.hint => MethodHint::MatchName { method: Method { name, params, return_ty, .. }, .. } => {
            assert_eq!(name, "hashCode");
            assert!(params.is_empty());
            assert_eq!(return_ty, "int");
        });
        // Different Name but same Sig Methods
        report = MethodHintReport::check_method_existence(&env.find_class("java/lang/Integer").unwrap(), "myOwnMethod", "(II)I", false, &mut env);
        check_report_hint!(report.hint => MethodHint::MatchSig { methods, .. } => {
            let method = methods.iter().find(|method| method.name == "compare").unwrap();
            assert!(method.params[0] == "int");
            assert!(method.params[1] == "int");
            assert_eq!(method.return_ty, "int");
        });
        // No matches
        report = MethodHintReport::check_method_existence(&env.find_class("java/lang/Integer").unwrap(), "myOwnMethod", "(Lme.my.Class;)V", false, &mut env);
        assert!(matches!(report.hint, MethodHint::NoMatch))
    }

    #[test]
    fn test_check_field_existence() {
        setup_env!(env);
        let mut report;

        // Same Name and same Type Fields
        report = FieldHintReport::check_field_existence(&env.find_class("java/lang/Integer").unwrap(), "SIZE", "I", false, &mut env);
        check_report_hint!(report.hint => FieldHint::MatchFieldNameAndType { field: Field { mods, name, ty, .. }, .. } => {
            assert_eq!(mods.staticness(), "static");
            assert_eq!(name, "SIZE");
            assert_eq!(ty, "int");
        });
        // Same Name but different Type Fields
        report = FieldHintReport::check_field_existence(&env.find_class("java/lang/Integer").unwrap(), "SIZE", "J", true, &mut env);
        check_report_hint!(report.hint => FieldHint::MatchFieldName { field: Field { name, ty, .. }, .. } => {
            assert_eq!(name, "SIZE");
            assert_eq!(ty, "int");
        });
        // Similar Name and same Type Methods
        report = FieldHintReport::check_field_existence(&env.find_class("java/lang/Integer").unwrap(), "intValue", "I", false, &mut env);
        check_report_hint!(report.hint => FieldHint::MatchMethodNameAndType { methods, .. } => {
            assert_eq!(methods[0].name, "intValue");
            assert_eq!(methods[0].return_ty, "int");
        });
        report = FieldHintReport::check_field_existence(&env.find_class("java/lang/Integer").unwrap(), "string", "Ljava/lang/String;", false, &mut env);
        check_report_hint!(report.hint => FieldHint::MatchMethodNameAndType { methods, .. } => {
            assert_eq!(methods[0].name, "toString");
            assert_eq!(methods[0].return_ty, "java.lang.String");
        });
        // Similar Name but different Type Methods
        report = FieldHintReport::check_field_existence(&env.find_class("java/lang/Integer").unwrap(), "intValue", "J", false, &mut env);
        check_report_hint!(report.hint => FieldHint::MatchMethodName { methods, .. } => {
            assert_eq!(methods[0].name, "intValue");
            assert_eq!(methods[0].return_ty, "int");
        });
        // Fields and Methods with the same Type
        report = FieldHintReport::check_field_existence(&env.find_class("java/lang/Integer").unwrap(), "myOwnField", "I", false, &mut env);
        check_report_hint!(report.hint => FieldHint::MatchType { fields, methods, .. } => {
            let field = fields.iter().find(|field| field.name == "SIZE").unwrap();
            assert_eq!(field.ty, "int");
            let method = methods.iter().find(|method| method.name == "intValue").unwrap();
            assert!(method.params.is_empty());
            assert_eq!(method.return_ty, "int");
        });
        // No matches
        report = FieldHintReport::check_field_existence(&env.find_class("java/lang/Integer").unwrap(), "myOwnField", "Lme.my.Class;", false, &mut env);
        assert!(matches!(report.hint, FieldHint::NoMatch))
    }

    #[test]
    #[should_panic(expected = "more than 1 visibility flag is set for Mods(5)")]
    fn test_invalid_modifiers() {
        Mods(Mods::PRIVATE_MASK | Mods::PUBLIC_MASK).visibility();
    }
    /// Test to_string() methods
    #[test]
    fn method_modifiers_string() {
        let mut mods = Mods(Mods::PUBLIC_MASK | Mods::STATIC_MASK);
        assert_eq!(mods.visibility(), "public");
        assert_eq!(mods.staticness(), "static");
        mods = Mods(Mods::PRIVATE_MASK);
        assert_eq!(mods.visibility(), "private");
        assert_eq!(mods.staticness(), "non-static");
    }
    /// Test difference message
    #[test]
    fn method_modifiers_difference() {
        let mut mods = Mods(Mods::PROTECTED_MASK | Mods::STATIC_MASK);
        let mut other = Mods(Mods::PUBLIC_MASK);
        assert_eq!(mods.contrast(other), "protected and static");
        mods = Mods(Mods::PUBLIC_MASK);
        other = Mods(Mods::PUBLIC_MASK | Mods::STATIC_MASK);
        assert_eq!(mods.contrast(other), "non-static");
    }

    #[test]
    fn method_from_object() {
        // Test Java compatibility
        use ez_jni_macros::class;
        use std::collections::HashMap;

        setup_env!(env);
        let env = &mut env;
        let methods = call!(env=> class!(env=> java.lang.Integer).getDeclaredMethods() -> [java.lang.reflect.Method])
            .into_iter()
            .map(|obj| Method::from_object_env(&obj, env).unwrap_display())
            .filter(|method| method.name == "compare" || method.name == "compareTo")
            .map(|method| (method.name.clone(), method))
            .collect::<HashMap<String, Method>>();
        assert_eq!(methods.len(), 2);

        assert_eq!(methods["compare"].to_string(), "public static int java.lang.Integer.compare(int, int)");
        assert_eq!(methods["compareTo"].to_string(), "public int java.lang.Integer.compareTo(java.lang.Object)");
    }

    #[test]
    fn test_display_jni_type() {
        let basic = Type::parse_method_sig("(BZCSIJFDLjava/lang/String;V)V")
            .unwrap().0
            .into_iter()
            .map(|ty| ty.to_string())
            .collect::<Box<[_]>>();
        let arrays = Type::parse_method_sig("([I[[Ljava/lang/String;)V")
            .unwrap().0
            .into_iter()
            .map(|ty| ty.to_string())
            .collect::<Box<[_]>>();

        assert_eq!(basic[0], "byte"); 
        assert_eq!(basic[1], "boolean"); 
        assert_eq!(basic[2], "char"); 
        assert_eq!(basic[3], "short"); 
        assert_eq!(basic[4], "int"); 
        assert_eq!(basic[5], "long"); 
        assert_eq!(basic[6], "float"); 
        assert_eq!(basic[7], "double"); 
        assert_eq!(basic[8], "java.lang.String"); 
        assert_eq!(basic[9], "void"); 
        assert_eq!(arrays[0], "int[]"); 
        assert_eq!(arrays[1], "java.lang.String[][]"); 
    }
}