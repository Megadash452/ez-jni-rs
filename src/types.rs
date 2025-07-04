use jni::{errors::Result as JniResult, objects::{JClass, JPrimitiveArray, JString, JThrowable}, sys::{jboolean, jbyte, jchar, jdouble, jfloat, jint, jlong, jshort, jsize}, JNIEnv};

pub trait Class {
    const CLASS_PATH: &'static str;
}

pub(crate) trait Primitive: Class + Copy + 'static {
    const JNAME: &'static str;
    type JType: jni::objects::TypeArray + 'static;

    fn array_alloc<'local>(len: jsize, env: &JNIEnv<'local>) -> JniResult<JPrimitiveArray<'local, Self::JType>>;
    fn array_filler(array: &JPrimitiveArray<'_, Self::JType>, buf: &[Self::JType], env: &JNIEnv<'_>) -> JniResult<()>;
    fn slice_filler(array: &JPrimitiveArray<'_, Self::JType>, buf: &mut [Self::JType], env: &JNIEnv<'_>) -> JniResult<()>;

    const CONVERT_JAVA_TO_RUST: Option<fn(Self::JType) -> Self> = None;
    const CONVERT_RUST_TO_JAVA: Option<fn(Self) -> Self::JType> = None;
}

impl Class for String {
    const CLASS_PATH: &'static str = "java/lang/String";
}
impl Class for JString<'_> {
    const CLASS_PATH: &'static str = String::CLASS_PATH;
}
impl Class for JClass<'_> {
    const CLASS_PATH: &'static str = "java/lang/Class";
}
impl Class for JThrowable<'_> {
    const CLASS_PATH: &'static str = "java/lang/Exception";
}
impl<T> Class for Option<T>
where T: Class {
    const CLASS_PATH: &'static str = T::CLASS_PATH;
}

impl Class for bool {
    const CLASS_PATH: &'static str = "java/lang/Boolean";
}
impl Class for char {
    const CLASS_PATH: &'static str = "java/lang/Character";
}
impl Class for i8 {
    const CLASS_PATH: &'static str = "java/lang/Byte";
}
impl Class for i16 {
    const CLASS_PATH: &'static str = "java/lang/Short";
}
impl Class for i32 {
    const CLASS_PATH: &'static str = "java/lang/Integer";
}
impl Class for i64 {
    const CLASS_PATH: &'static str = "java/lang/Long";
}
impl Class for f32 {
    const CLASS_PATH: &'static str = "java/lang/Float";
}
impl Class for f64 {
    const CLASS_PATH: &'static str = "java/lang/Double";
}
impl Class for u8 {
    const CLASS_PATH: &'static str = i8::CLASS_PATH;
}
impl Class for u16 {
    const CLASS_PATH: &'static str = i16::CLASS_PATH;
}
impl Class for u32 {
    const CLASS_PATH: &'static str = i32::CLASS_PATH;
}
impl Class for u64 {
    const CLASS_PATH: &'static str = i64::CLASS_PATH;
}

impl Primitive for bool {
    const JNAME: &'static str = "boolean";
    type JType = jboolean;

    #[inline(always)]
    fn array_alloc<'local>(len: jsize, env: &JNIEnv<'local>) -> JniResult<JPrimitiveArray<'local, Self::JType>> {
        JNIEnv::new_boolean_array(env, len)
    }
    #[inline(always)]
    fn array_filler(array: &JPrimitiveArray<'_, Self::JType>, buf: &[Self::JType], env: &JNIEnv<'_>) -> JniResult<()> {
        JNIEnv::set_boolean_array_region(env, array, 0, buf)
    }
    #[inline(always)]
    fn slice_filler(array: &JPrimitiveArray<'_, Self::JType>, buf: &mut [Self::JType], env: &JNIEnv<'_>) -> JniResult<()> {
        JNIEnv::get_boolean_array_region(env, array, 0, buf)
    }

    const CONVERT_JAVA_TO_RUST: Option<fn(Self::JType) -> Self> = Some(crate::utils::jboolean_to_bool);
    const CONVERT_RUST_TO_JAVA: Option<fn(Self) -> Self::JType> = Some(crate::utils::bool_to_jboolean);
}
impl Primitive for char {
    const JNAME: &'static str = "char";
    type JType = jchar;

    #[inline(always)]
    fn array_alloc<'local>(len: jsize, env: &JNIEnv<'local>) -> JniResult<JPrimitiveArray<'local, Self::JType>> {
        JNIEnv::new_char_array(env, len)
    }
    #[inline(always)]
    fn array_filler(array: &JPrimitiveArray<'_, Self::JType>, buf: &[Self::JType], env: &JNIEnv<'_>) -> JniResult<()> {
        JNIEnv::set_char_array_region(env, array, 0, buf)
    }
    #[inline(always)]
    fn slice_filler(array: &JPrimitiveArray<'_, Self::JType>, buf: &mut [Self::JType], env: &JNIEnv<'_>) -> JniResult<()> {
        JNIEnv::get_char_array_region(env, array, 0, buf)
    }

    const CONVERT_JAVA_TO_RUST: Option<fn(Self::JType) -> Self> = Some(crate::utils::jchar_to_char);
    const CONVERT_RUST_TO_JAVA: Option<fn(Self) -> Self::JType> = Some(crate::utils::char_to_jchar);
}
impl Primitive for i8 {
    const JNAME: &'static str = "byte";
    type JType = jbyte;

    #[inline(always)]
    fn array_alloc<'local>(len: jsize, env: &JNIEnv<'local>) -> JniResult<JPrimitiveArray<'local, Self::JType>> {
        JNIEnv::new_byte_array(env, len)
    }
    #[inline(always)]
    fn array_filler(array: &JPrimitiveArray<'_, Self::JType>, buf: &[Self::JType], env: &JNIEnv<'_>) -> JniResult<()> {
        JNIEnv::set_byte_array_region(env, array, 0, buf)
    }
    #[inline(always)]
    fn slice_filler(array: &JPrimitiveArray<'_, Self::JType>, buf: &mut [Self::JType], env: &JNIEnv<'_>) -> JniResult<()> {
        JNIEnv::get_byte_array_region(env, array, 0, buf)
    }
}
impl Primitive for i16 {
    const JNAME: &'static str = "short";
    type JType = jshort;

    #[inline(always)]
    fn array_alloc<'local>(len: jsize, env: &JNIEnv<'local>) -> JniResult<JPrimitiveArray<'local, Self::JType>> {
        JNIEnv::new_short_array(env, len)
    }
    #[inline(always)]
    fn array_filler(array: &JPrimitiveArray<'_, Self::JType>, buf: &[Self::JType], env: &JNIEnv<'_>) -> JniResult<()> {
        JNIEnv::set_short_array_region(env, array, 0, buf)
    }
    #[inline(always)]
    fn slice_filler(array: &JPrimitiveArray<'_, Self::JType>, buf: &mut [Self::JType], env: &JNIEnv<'_>) -> JniResult<()> {
        JNIEnv::get_short_array_region(env, array, 0, buf)
    }
}
impl Primitive for i32 {
    const JNAME: &'static str = "int";
    type JType = jint;

    #[inline(always)]
    fn array_alloc<'local>(len: jsize, env: &JNIEnv<'local>) -> JniResult<JPrimitiveArray<'local, Self::JType>> {
        JNIEnv::new_int_array(env, len)
    }
    #[inline(always)]
    fn array_filler(array: &JPrimitiveArray<'_, Self::JType>, buf: &[Self::JType], env: &JNIEnv<'_>) -> JniResult<()> {
        JNIEnv::set_int_array_region(env, array, 0, buf)
    }
    #[inline(always)]
    fn slice_filler(array: &JPrimitiveArray<'_, Self::JType>, buf: &mut [Self::JType], env: &JNIEnv<'_>) -> JniResult<()> {
        JNIEnv::get_int_array_region(env, array, 0, buf)
    }
}
impl Primitive for i64 {
    const JNAME: &'static str = "long";
    type JType = jlong;

    #[inline(always)]
    fn array_alloc<'local>(len: jsize, env: &JNIEnv<'local>) -> JniResult<JPrimitiveArray<'local, Self::JType>> {
        JNIEnv::new_long_array(env, len)
    }
    #[inline(always)]
    fn array_filler(array: &JPrimitiveArray<'_, Self::JType>, buf: &[Self::JType], env: &JNIEnv<'_>) -> JniResult<()> {
        JNIEnv::set_long_array_region(env, array, 0, buf)
    }
    #[inline(always)]
    fn slice_filler(array: &JPrimitiveArray<'_, Self::JType>, buf: &mut [Self::JType], env: &JNIEnv<'_>) -> JniResult<()> {
        JNIEnv::get_long_array_region(env, array, 0, buf)
    }
}
impl Primitive for f32 {
    const JNAME: &'static str = "float";
    type JType = jfloat;

    #[inline(always)]
    fn array_alloc<'local>(len: jsize, env: &JNIEnv<'local>) -> JniResult<JPrimitiveArray<'local, Self::JType>> {
        JNIEnv::new_float_array(env, len)
    }
    #[inline(always)]
    fn array_filler(array: &JPrimitiveArray<'_, Self::JType>, buf: &[Self::JType], env: &JNIEnv<'_>) -> JniResult<()> {
        JNIEnv::set_float_array_region(env, array, 0, buf)
    }
    #[inline(always)]
    fn slice_filler(array: &JPrimitiveArray<'_, Self::JType>, buf: &mut [Self::JType], env: &JNIEnv<'_>) -> JniResult<()> {
        JNIEnv::get_float_array_region(env, array, 0, buf)
    }
}
impl Primitive for f64 {
    const JNAME: &'static str = "double";
    type JType = jdouble;

    #[inline(always)]
    fn array_alloc<'local>(len: jsize, env: &JNIEnv<'local>) -> JniResult<JPrimitiveArray<'local, Self::JType>> {
        JNIEnv::new_double_array(env, len)
    }
    #[inline(always)]
    fn array_filler(array: &JPrimitiveArray<'_, Self::JType>, buf: &[Self::JType], env: &JNIEnv<'_>) -> JniResult<()> {
        JNIEnv::set_double_array_region(env, array, 0, buf)
    }
    #[inline(always)]
    fn slice_filler(array: &JPrimitiveArray<'_, Self::JType>, buf: &mut [Self::JType], env: &JNIEnv<'_>) -> JniResult<()> {
        JNIEnv::get_double_array_region(env, array, 0, buf)
    }
}
// Unsigned Primitives
impl Primitive for u8 {
    const JNAME: &'static str = i8::JNAME;
    type JType = <i8 as Primitive>::JType;

    #[inline(always)]
    fn array_alloc<'local>(len: jsize, env: &JNIEnv<'local>) -> JniResult<JPrimitiveArray<'local, Self::JType>> {
        <i8 as Primitive>::array_alloc(len, env)
    }
    #[inline(always)]
    fn array_filler(array: &JPrimitiveArray<'_, Self::JType>, buf: &[Self::JType], env: &JNIEnv<'_>) -> JniResult<()> {
        <i8 as Primitive>::array_filler(array, buf, env)
    }
    #[inline(always)]
    fn slice_filler(array: &JPrimitiveArray<'_, Self::JType>, buf: &mut [Self::JType], env: &JNIEnv<'_>) -> JniResult<()> {
        <i8 as Primitive>::slice_filler(array, buf, env)
    }
}
impl Primitive for u16 {
    const JNAME: &'static str = i16::JNAME;
    type JType = <i16 as Primitive>::JType;

    #[inline(always)]
    fn array_alloc<'local>(len: jsize, env: &JNIEnv<'local>) -> JniResult<JPrimitiveArray<'local, Self::JType>> {
        <i16 as Primitive>::array_alloc(len, env)
    }
    #[inline(always)]
    fn array_filler(array: &JPrimitiveArray<'_, Self::JType>, buf: &[Self::JType], env: &JNIEnv<'_>) -> JniResult<()> {
        <i16 as Primitive>::array_filler(array, buf, env)
    }
    #[inline(always)]
    fn slice_filler(array: &JPrimitiveArray<'_, Self::JType>, buf: &mut [Self::JType], env: &JNIEnv<'_>) -> JniResult<()> {
        <i16 as Primitive>::slice_filler(array, buf, env)
    }
}
impl Primitive for u32 {
    const JNAME: &'static str = i32::JNAME;
    type JType = <i32 as Primitive>::JType;

    #[inline(always)]
    fn array_alloc<'local>(len: jsize, env: &JNIEnv<'local>) -> JniResult<JPrimitiveArray<'local, Self::JType>> {
        <i32 as Primitive>::array_alloc(len, env)
    }
    #[inline(always)]
    fn array_filler(array: &JPrimitiveArray<'_, Self::JType>, buf: &[Self::JType], env: &JNIEnv<'_>) -> JniResult<()> {
        <i32 as Primitive>::array_filler(array, buf, env)
    }
    #[inline(always)]
    fn slice_filler(array: &JPrimitiveArray<'_, Self::JType>, buf: &mut [Self::JType], env: &JNIEnv<'_>) -> JniResult<()> {
        <i32 as Primitive>::slice_filler(array, buf, env)
    }
}
impl Primitive for u64 {
    const JNAME: &'static str = i64::JNAME;
    type JType = <i64 as Primitive>::JType;

    #[inline(always)]
    fn array_alloc<'local>(len: jsize, env: &JNIEnv<'local>) -> JniResult<JPrimitiveArray<'local, Self::JType>> {
        <i64 as Primitive>::array_alloc(len, env)
    }
    #[inline(always)]
    fn array_filler(array: &JPrimitiveArray<'_, Self::JType>, buf: &[Self::JType], env: &JNIEnv<'_>) -> JniResult<()> {
        <i64 as Primitive>::array_filler(array, buf, env)
    }
    #[inline(always)]
    fn slice_filler(array: &JPrimitiveArray<'_, Self::JType>, buf: &mut [Self::JType], env: &JNIEnv<'_>) -> JniResult<()> {
        <i64 as Primitive>::slice_filler(array, buf, env)
    }
}