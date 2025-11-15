use std::borrow::Cow;
use jni::{
    JNIEnv,
    errors::Result as JniResult,
    objects::{JClass, JPrimitiveArray, JString},
    sys::{jboolean, jbyte, jchar, jdouble, jfloat, jint, jlong, jshort, jsize},
};

/// A trait that allows a *base* **Java Class** to be assigned to a *Rust Type*.
/// 
/// The class must be in *Class Path* format (e.g. `"java/lang/String"`).
/// 
/// When converting [from an object][crate::FromObject],
/// the *Rust Type* will expect the *Java Object* to be an instance of the **Class**.
pub trait Class {
    /// Returns the **Class** assigned to this *Type*.
    fn class() -> Cow<'static, str>;
}

// TODO: Add separate trait to get Class from &self, and rename Class to something like TypeClass

pub(crate) trait Primitive: Class + Copy + 'static {
    /// The **name** of a *Java Primitive*.
    const JNAME: &'static str;
    /// The **signature** character (Uppercase) that corresponds to a *Java Primitive*.
    const JSIG: char;
    /// The *Rust type* counterpart that `JNI` uses for this *primitive*.
    type JNIType: jni::objects::TypeArray + 'static;

    fn array_alloc<'local>(len: jsize, env: &JNIEnv<'local>) -> JniResult<JPrimitiveArray<'local, Self::JNIType>>;
    fn array_filler(array: &JPrimitiveArray<'_, Self::JNIType>, buf: &[Self::JNIType], env: &JNIEnv<'_>) -> JniResult<()>;
    fn slice_filler(array: &JPrimitiveArray<'_, Self::JNIType>, buf: &mut [Self::JNIType], env: &JNIEnv<'_>) -> JniResult<()>;

    const CONVERT_JAVA_TO_RUST: Option<fn(Self::JNIType) -> Self> = None;
    const CONVERT_RUST_TO_JAVA: Option<fn(Self) -> Self::JNIType> = None;
}

macro_rules! impl_class {
    // Assign an UNIQUE Class
    (for $ty:ty => $class:literal) => {
        impl Class for $ty {
            fn class() -> Cow<'static, str> {
                Cow::Borrowed($class)
            }
        }
    };
    // Derive Class from another type
    (for $ty:ty => $other:ty) => {
        impl Class for $ty {
            #[inline(always)]
            fn class() -> Cow<'static, str> {
                <$other as Class>::class()
            }
        }
    };
    // Same as Derive but with a generic
    (<T> for $ty:ty => $other:ty) => {
        impl<T> Class for $ty
        where T: Class {
            #[inline(always)]
            fn class() -> Cow<'static, str> {
                <$other as Class>::class()
            }
        }
    };
}

impl_class!(for String => "java/lang/String");
impl_class!(for str => String);
impl_class!(for &str => String);
impl_class!(for JString<'_> => String);
impl_class!(for JClass<'_> => "java/lang/Class");
/* NOTE: JObject and JThrowable don't implement Class because these types shouldn't implicitly expect an Object to be an instance of a certain class.
         Instead, an user should be required to specify the class that they expect the Object to be an instance of. */
// impl_class!(for JObject<'_> => "java/lang/Object");
// impl_class!(for JThrowable<'_> => crate::JavaException);
impl_class!(for crate::JavaException => "java/lang/Exception");
impl_class!(for std::io::Error => crate::JavaException);
impl_class!(for dyn std::error::Error => crate::JavaException);
impl_class!(for Box<dyn std::error::Error> => crate::JavaException);
impl_class!(<T> for &T => T);
impl_class!(<T> for Option<T> => T);

impl<T> Class for [T]
where T: Class {
    fn class() -> Cow<'static, str> {
        let elem_class = T::class();
        // If T is also an array, simply add a dimension.
        Cow::Owned(if elem_class.contains('[') {
            format!("[{elem_class}")
        } else {
            // FIXME: Primitives (e.g. Z) don't use L{}; .... May need different implementation for primitives
            format!("[L{elem_class};")
        })
    }
}
impl<const N: usize, T> Class for [T; N]
where T: Class {
    #[inline(always)]
    fn class() -> Cow<'static, str> {
        <[T] as Class>::class()
    }
}
impl_class!(<T> for &[T] => [T]);
impl_class!(<T> for Box<[T]> => [T]);
impl_class!(<T> for Vec<T> => [T]);

// TODO: when generic_const_items https://github.com/rust-lang/rust/issues/113521 is stablized, delete DynamicClass trait and use `const_format::formatcp!("[L{};", T::CLASS_PATH)`
// impl<T> Class for [T]
// where T: Class {
//     const CLASS_PATH: &'static str = formatcp!("[L{};", T::CLASS_PATH);
// }
// impl<T> Class for Box<[T]>
// where T: Class {
//     const CLASS_PATH: &'static str = [T]::CLASS_PATH;
// }
// impl<const N: usize, T> Class for [T; N]
// where T: Class {
//     const CLASS_PATH: &'static str = [T]::CLASS_PATH;
// }

impl_class!(for bool => "java/lang/Boolean");
impl_class!(for char => "java/lang/Character");

impl_class!(for i8 => "java/lang/Byte");
impl_class!(for i16 => "java/lang/Short");
impl_class!(for i32 => "java/lang/Integer");
impl_class!(for i64 => "java/lang/Long");
impl_class!(for f32 => "java/lang/Float");
impl_class!(for f64 => "java/lang/Double");
impl_class!(for u8 => i8);
impl_class!(for u16 => i16);
impl_class!(for u32 => i32);
impl_class!(for u64 => i64);

impl Primitive for bool {
    const JNAME: &'static str = "boolean";
    const JSIG: char = 'Z';
    type JNIType = jboolean;

    #[inline(always)]
    fn array_alloc<'local>(len: jsize, env: &JNIEnv<'local>) -> JniResult<JPrimitiveArray<'local, Self::JNIType>> {
        JNIEnv::new_boolean_array(env, len)
    }
    #[inline(always)]
    fn array_filler(array: &JPrimitiveArray<'_, Self::JNIType>, buf: &[Self::JNIType], env: &JNIEnv<'_>) -> JniResult<()> {
        JNIEnv::set_boolean_array_region(env, array, 0, buf)
    }
    #[inline(always)]
    fn slice_filler(array: &JPrimitiveArray<'_, Self::JNIType>, buf: &mut [Self::JNIType], env: &JNIEnv<'_>) -> JniResult<()> {
        JNIEnv::get_boolean_array_region(env, array, 0, buf)
    }

    const CONVERT_JAVA_TO_RUST: Option<fn(Self::JNIType) -> Self> = Some(crate::utils::jboolean_to_bool);
    const CONVERT_RUST_TO_JAVA: Option<fn(Self) -> Self::JNIType> = Some(crate::utils::bool_to_jboolean);
}
impl Primitive for char {
    const JNAME: &'static str = "char";
    const JSIG: char = 'C';
    type JNIType = jchar;

    #[inline(always)]
    fn array_alloc<'local>(len: jsize, env: &JNIEnv<'local>) -> JniResult<JPrimitiveArray<'local, Self::JNIType>> {
        JNIEnv::new_char_array(env, len)
    }
    #[inline(always)]
    fn array_filler(array: &JPrimitiveArray<'_, Self::JNIType>, buf: &[Self::JNIType], env: &JNIEnv<'_>) -> JniResult<()> {
        JNIEnv::set_char_array_region(env, array, 0, buf)
    }
    #[inline(always)]
    fn slice_filler(array: &JPrimitiveArray<'_, Self::JNIType>, buf: &mut [Self::JNIType], env: &JNIEnv<'_>) -> JniResult<()> {
        JNIEnv::get_char_array_region(env, array, 0, buf)
    }

    const CONVERT_JAVA_TO_RUST: Option<fn(Self::JNIType) -> Self> = Some(crate::utils::jchar_to_char);
    const CONVERT_RUST_TO_JAVA: Option<fn(Self) -> Self::JNIType> = Some(crate::utils::char_to_jchar);
}
impl Primitive for i8 {
    const JNAME: &'static str = "byte";
    const JSIG: char = 'B';
    type JNIType = jbyte;

    #[inline(always)]
    fn array_alloc<'local>(len: jsize, env: &JNIEnv<'local>) -> JniResult<JPrimitiveArray<'local, Self::JNIType>> {
        JNIEnv::new_byte_array(env, len)
    }
    #[inline(always)]
    fn array_filler(array: &JPrimitiveArray<'_, Self::JNIType>, buf: &[Self::JNIType], env: &JNIEnv<'_>) -> JniResult<()> {
        JNIEnv::set_byte_array_region(env, array, 0, buf)
    }
    #[inline(always)]
    fn slice_filler(array: &JPrimitiveArray<'_, Self::JNIType>, buf: &mut [Self::JNIType], env: &JNIEnv<'_>) -> JniResult<()> {
        JNIEnv::get_byte_array_region(env, array, 0, buf)
    }
}
impl Primitive for i16 {
    const JNAME: &'static str = "short";
    const JSIG: char = 'S';
    type JNIType = jshort;

    #[inline(always)]
    fn array_alloc<'local>(len: jsize, env: &JNIEnv<'local>) -> JniResult<JPrimitiveArray<'local, Self::JNIType>> {
        JNIEnv::new_short_array(env, len)
    }
    #[inline(always)]
    fn array_filler(array: &JPrimitiveArray<'_, Self::JNIType>, buf: &[Self::JNIType], env: &JNIEnv<'_>) -> JniResult<()> {
        JNIEnv::set_short_array_region(env, array, 0, buf)
    }
    #[inline(always)]
    fn slice_filler(array: &JPrimitiveArray<'_, Self::JNIType>, buf: &mut [Self::JNIType], env: &JNIEnv<'_>) -> JniResult<()> {
        JNIEnv::get_short_array_region(env, array, 0, buf)
    }
}
impl Primitive for i32 {
    const JNAME: &'static str = "int";
    const JSIG: char = 'I';
    type JNIType = jint;

    #[inline(always)]
    fn array_alloc<'local>(len: jsize, env: &JNIEnv<'local>) -> JniResult<JPrimitiveArray<'local, Self::JNIType>> {
        JNIEnv::new_int_array(env, len)
    }
    #[inline(always)]
    fn array_filler(array: &JPrimitiveArray<'_, Self::JNIType>, buf: &[Self::JNIType], env: &JNIEnv<'_>) -> JniResult<()> {
        JNIEnv::set_int_array_region(env, array, 0, buf)
    }
    #[inline(always)]
    fn slice_filler(array: &JPrimitiveArray<'_, Self::JNIType>, buf: &mut [Self::JNIType], env: &JNIEnv<'_>) -> JniResult<()> {
        JNIEnv::get_int_array_region(env, array, 0, buf)
    }
}
impl Primitive for i64 {
    const JNAME: &'static str = "long";
    const JSIG: char = 'J';
    type JNIType = jlong;

    #[inline(always)]
    fn array_alloc<'local>(len: jsize, env: &JNIEnv<'local>) -> JniResult<JPrimitiveArray<'local, Self::JNIType>> {
        JNIEnv::new_long_array(env, len)
    }
    #[inline(always)]
    fn array_filler(array: &JPrimitiveArray<'_, Self::JNIType>, buf: &[Self::JNIType], env: &JNIEnv<'_>) -> JniResult<()> {
        JNIEnv::set_long_array_region(env, array, 0, buf)
    }
    #[inline(always)]
    fn slice_filler(array: &JPrimitiveArray<'_, Self::JNIType>, buf: &mut [Self::JNIType], env: &JNIEnv<'_>) -> JniResult<()> {
        JNIEnv::get_long_array_region(env, array, 0, buf)
    }
}
// TODO: implement Primitive for isize. but should it be implemented??
impl Primitive for f32 {
    const JNAME: &'static str = "float";
    const JSIG: char = 'F';
    type JNIType = jfloat;

    #[inline(always)]
    fn array_alloc<'local>(len: jsize, env: &JNIEnv<'local>) -> JniResult<JPrimitiveArray<'local, Self::JNIType>> {
        JNIEnv::new_float_array(env, len)
    }
    #[inline(always)]
    fn array_filler(array: &JPrimitiveArray<'_, Self::JNIType>, buf: &[Self::JNIType], env: &JNIEnv<'_>) -> JniResult<()> {
        JNIEnv::set_float_array_region(env, array, 0, buf)
    }
    #[inline(always)]
    fn slice_filler(array: &JPrimitiveArray<'_, Self::JNIType>, buf: &mut [Self::JNIType], env: &JNIEnv<'_>) -> JniResult<()> {
        JNIEnv::get_float_array_region(env, array, 0, buf)
    }
}
impl Primitive for f64 {
    const JNAME: &'static str = "double";
    const JSIG: char = 'D';
    type JNIType = jdouble;

    #[inline(always)]
    fn array_alloc<'local>(len: jsize, env: &JNIEnv<'local>) -> JniResult<JPrimitiveArray<'local, Self::JNIType>> {
        JNIEnv::new_double_array(env, len)
    }
    #[inline(always)]
    fn array_filler(array: &JPrimitiveArray<'_, Self::JNIType>, buf: &[Self::JNIType], env: &JNIEnv<'_>) -> JniResult<()> {
        JNIEnv::set_double_array_region(env, array, 0, buf)
    }
    #[inline(always)]
    fn slice_filler(array: &JPrimitiveArray<'_, Self::JNIType>, buf: &mut [Self::JNIType], env: &JNIEnv<'_>) -> JniResult<()> {
        JNIEnv::get_double_array_region(env, array, 0, buf)
    }
}
// Unsigned Primitives
impl Primitive for u8 {
    const JNAME: &'static str = i8::JNAME;
    const JSIG: char = i8::JSIG;
    type JNIType = <i8 as Primitive>::JNIType;

    #[inline(always)]
    fn array_alloc<'local>(len: jsize, env: &JNIEnv<'local>) -> JniResult<JPrimitiveArray<'local, Self::JNIType>> {
        <i8 as Primitive>::array_alloc(len, env)
    }
    #[inline(always)]
    fn array_filler(array: &JPrimitiveArray<'_, Self::JNIType>, buf: &[Self::JNIType], env: &JNIEnv<'_>) -> JniResult<()> {
        <i8 as Primitive>::array_filler(array, buf, env)
    }
    #[inline(always)]
    fn slice_filler(array: &JPrimitiveArray<'_, Self::JNIType>, buf: &mut [Self::JNIType], env: &JNIEnv<'_>) -> JniResult<()> {
        <i8 as Primitive>::slice_filler(array, buf, env)
    }
}
impl Primitive for u16 {
    const JNAME: &'static str = i16::JNAME;
    const JSIG: char = i16::JSIG;
    type JNIType = <i16 as Primitive>::JNIType;

    #[inline(always)]
    fn array_alloc<'local>(len: jsize, env: &JNIEnv<'local>) -> JniResult<JPrimitiveArray<'local, Self::JNIType>> {
        <i16 as Primitive>::array_alloc(len, env)
    }
    #[inline(always)]
    fn array_filler(array: &JPrimitiveArray<'_, Self::JNIType>, buf: &[Self::JNIType], env: &JNIEnv<'_>) -> JniResult<()> {
        <i16 as Primitive>::array_filler(array, buf, env)
    }
    #[inline(always)]
    fn slice_filler(array: &JPrimitiveArray<'_, Self::JNIType>, buf: &mut [Self::JNIType], env: &JNIEnv<'_>) -> JniResult<()> {
        <i16 as Primitive>::slice_filler(array, buf, env)
    }
}
impl Primitive for u32 {
    const JNAME: &'static str = i32::JNAME;
    const JSIG: char = i32::JSIG;
    type JNIType = <i32 as Primitive>::JNIType;

    #[inline(always)]
    fn array_alloc<'local>(len: jsize, env: &JNIEnv<'local>) -> JniResult<JPrimitiveArray<'local, Self::JNIType>> {
        <i32 as Primitive>::array_alloc(len, env)
    }
    #[inline(always)]
    fn array_filler(array: &JPrimitiveArray<'_, Self::JNIType>, buf: &[Self::JNIType], env: &JNIEnv<'_>) -> JniResult<()> {
        <i32 as Primitive>::array_filler(array, buf, env)
    }
    #[inline(always)]
    fn slice_filler(array: &JPrimitiveArray<'_, Self::JNIType>, buf: &mut [Self::JNIType], env: &JNIEnv<'_>) -> JniResult<()> {
        <i32 as Primitive>::slice_filler(array, buf, env)
    }
}
impl Primitive for u64 {
    const JNAME: &'static str = i64::JNAME;
    const JSIG: char = i64::JSIG;
    type JNIType = <i64 as Primitive>::JNIType;

    #[inline(always)]
    fn array_alloc<'local>(len: jsize, env: &JNIEnv<'local>) -> JniResult<JPrimitiveArray<'local, Self::JNIType>> {
        <i64 as Primitive>::array_alloc(len, env)
    }
    #[inline(always)]
    fn array_filler(array: &JPrimitiveArray<'_, Self::JNIType>, buf: &[Self::JNIType], env: &JNIEnv<'_>) -> JniResult<()> {
        <i64 as Primitive>::array_filler(array, buf, env)
    }
    #[inline(always)]
    fn slice_filler(array: &JPrimitiveArray<'_, Self::JNIType>, buf: &mut [Self::JNIType], env: &JNIEnv<'_>) -> JniResult<()> {
        <i64 as Primitive>::slice_filler(array, buf, env)
    }
}
// TODO: implement Primitive for usize. but should it be implemented??
