use jni::objects::JClass;
use crate::{utils::{create_java_prim_array, create_object_array, create_object_array_converted, get_java_prim_array, get_object_array}, Class};
use super::*;

/// TODO: doc
pub trait FromArrayObject<'local>
where Self: Sized + for<'a, 'obj> FromObject<'a, 'obj, 'local> + 'local {
    fn from_object_array_helper(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Box<[Self]>, FromObjectError>;
}
/// TODO: doc
pub trait ToArrayObject
where Self: ToObject + Sized {
    fn to_object_array_helper<'local>(slice: &[Self], env: &mut JNIEnv<'local>) -> JObject<'local>;
}

// -- Blanket Implementations --

impl<'local, T> FromObject<'_, '_, 'local> for Box<[T]>
where T: FromArrayObject<'local> + 'local {
    fn from_object_env(object: &'_ JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        <T as FromArrayObject>::from_object_array_helper(object, env)
    }
}
impl<T> ToObject for [T]
where T: ToArrayObject + ToObject {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        <T as ToArrayObject>::to_object_array_helper(self, env)
    }
}
impl<T> ToObject for &[T]
where T: ToArrayObject + ToObject {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        <[T] as ToObject>::to_object_env(self, env)
    }
}
impl<const N: usize, T> ToObject for [T; N]
where [T]: ToObject {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        <[T] as ToObject>::to_object_env(self, env)
    }
}

impl<'local, T> FromObject<'_, '_, 'local> for Box<[Option<T>]>
where T: FromArrayObject<'local> + 'local {
    fn from_object_env(object: &'_ JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        get_object_array(object, None, env)? // TODO: Using None for now, try to work something out so that the class is not always None
            .into_iter()
            .map(|obj| Option::<T>::from_object_env(&obj, env))
            .collect::<Result<_, _>>()
    }
}
impl<T> ToObject for [Option<T>]
where T: ToObject + Class {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array_converted(
            self,
            T::CLASS_PATH,
            |opt, env| opt.to_object_env(env),
        env)
    }
}
impl<T> ToObject for [&Option<T>]
where T: ToObject + Class {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array_converted(
            self,
            T::CLASS_PATH,
            |opt, env| opt.to_object_env(env),
        env)
    }
}

impl<'local, T> FromObject<'_, '_, 'local> for Vec<T>
where Box<[T]>: for<'a, 'obj> FromObject<'a, 'obj, 'local> {
    fn from_object_env(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        Ok(Box::<[T]>::from_object_env(object, env)?.into_vec())
    }
}

// -- --

// -- Implementations --

macro_rules! impl_prim_array {
    ($prim:ty) => {
        impl FromArrayObject<'_> for $prim {
            #[inline(always)]
            fn from_object_array_helper(object: &JObject<'_>, env: &mut JNIEnv<'_>) -> Result<Box<[Self]>, FromObjectError> {
                get_java_prim_array(object, env)
            }
        }
        impl ToArrayObject for $prim {
            #[inline(always)]
            fn to_object_array_helper<'local>(slice: &[Self], env: &mut JNIEnv<'local>) -> JObject<'local> {
                create_java_prim_array(slice, env)
            }
        }
    };
}
impl_prim_array!(bool);
impl_prim_array!(char);
impl_prim_array!(i8);
impl_prim_array!(i16);
impl_prim_array!(i32);
impl_prim_array!(i64);
impl_prim_array!(f32);
impl_prim_array!(f64);
impl_prim_array!(u8);
impl_prim_array!(u16);
impl_prim_array!(u32);
impl_prim_array!(u64);

impl FromArrayObject<'_> for String {
    #[inline(always)]
    fn from_object_array_helper(object: &JObject<'_>, env: &mut JNIEnv<'_>) -> Result<Box<[Self]>, FromObjectError> {
        get_object_array(object, Some(Self::CLASS_PATH), env)?
            .into_iter()
            .map(|obj| Self::from_object_env(&obj, env))
            .collect::<Result<_, _>>()
    }
}
impl ToArrayObject for String {
    #[inline(always)]
    fn to_object_array_helper<'local>(slice: &[Self], env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array_converted(slice, Self::CLASS_PATH, |t, env| t.to_object_env(env), env)
    }
}
impl ToArrayObject for &String {
    #[inline(always)]
    fn to_object_array_helper<'local>(slice: &[Self], env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array_converted(slice, Self::CLASS_PATH, |t, env| t.to_object_env(env), env)
    }
}
impl ToArrayObject for &str {
    #[inline(always)]
    fn to_object_array_helper<'local>(slice: &[Self], env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array_converted(slice, Self::CLASS_PATH, |t, env| t.to_object_env(env), env)
    }
}
// For recursive blanket implementation
impl<'local, T> FromArrayObject<'local> for Box<[T]>
where T: FromArrayObject<'local> {
    #[inline(always)]
    fn from_object_array_helper(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Box<[Self]>, FromObjectError> {
        get_object_array(object, None, env)? // TODO: Using None for now, try to work something out so that the class is not always None
            .into_iter()
            .map(|obj| Self::from_object_env(&obj, env))
            .collect::<Result<_, _>>()
    }
}
impl<T> ToArrayObject for &[T]
where T: ToArrayObject + Class {
    #[inline(always)]
    fn to_object_array_helper<'local>(slice: &[Self], env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array_converted(
            slice,
            &format!("[L{};", T::CLASS_PATH),
            |t, env| t.to_object_env(env),
        env)
    }
}
impl<const N: usize, T> ToArrayObject for [T; N]
where T: ToArrayObject + Class {
    #[inline(always)]
    fn to_object_array_helper<'local>(slice: &[Self], env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array_converted(
            slice,
            &format!("[L{};", T::CLASS_PATH),
            |t, env| t.to_object_env(env),
        env)
    }
}
impl<const N: usize, T> ToArrayObject for &[T; N]
where T: ToArrayObject + Class {
    #[inline(always)]
    fn to_object_array_helper<'local>(slice: &[Self], env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array_converted(
            slice,
            &format!("[L{};", T::CLASS_PATH),
            |t, env| t.to_object_env(env),
        env)
    }
}
// -- --

// -- Edge cases implementations --

// JClass and JThrowable
macro_rules! impl_owned_object {
    ($($ty:tt)*) => {
        impl<'local> FromObject<'_, '_, 'local> for Box<[$($ty)*<'local>]> {
            fn from_object_env(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
                get_object_array(object, Some(<$($ty)* as crate::Class>::CLASS_PATH), env)?
                    .into_iter()
                    .map(|obj| <$($ty)* as crate::FromObjectOwned>::from_object_owned_env(obj, env))
                    .collect::<Result<_, _>>()
            }
        }
        impl<'local> FromObject<'_, '_, 'local> for Box<[Option<$($ty)*<'local>>]> {
            fn from_object_env(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
                get_object_array(object, Some(<$($ty)* as crate::Class>::CLASS_PATH), env)?
                    .into_iter()
                    .map(|obj| <Option::<$($ty)*> as crate::FromObjectOwned>::from_object_owned_env(obj, env))
                    .collect::<Result<_, _>>()
            }
        }
        impl ToArrayObject for $($ty)*<'_> {
            #[inline(always)]
            fn to_object_array_helper<'local>(slice: &[Self], env: &mut JNIEnv<'local>) -> JObject<'local> {
                create_object_array_converted(slice, Self::CLASS_PATH, |t, env| t.to_object_env(env), env)
            }
        }
        impl ToArrayObject for &$($ty)*<'_> {
            #[inline(always)]
            fn to_object_array_helper<'local>(slice: &[Self], env: &mut JNIEnv<'local>) -> JObject<'local> {
                create_object_array_converted(slice, Self::CLASS_PATH, |t, env| t.to_object_env(env), env)
            }
        }
    };
}
impl_owned_object!(JClass);
impl_owned_object!(JThrowable);


impl<'local> FromObject<'_, '_, 'local> for Box<[JObject<'local>]> {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        get_object_array(object, None, env)?
            .into_iter()
            .map(|obj| JObject::from_object_owned_env(obj, env))
            .collect::<Result<Self, _>>()
    }
}

/// Implementation for a slice of Objects of an ambiguous Class paired with the element Class
macro_rules! impl_paired_obj_slice {
    (ref $($impl_decl:tt)*) => {
        $($impl_decl)* {
            fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
                create_object_array(self.1.as_ref(), self.0, env)
            }
        }
    };
    ($($impl_decl:tt)*) => {
        $($impl_decl)* {
            fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
                create_object_array(self.1.as_ref(), self.0, env)
            }
        }
    };
}
impl_paired_obj_slice!(    impl ToObject for (&str, [JObject<'_>]));
impl_paired_obj_slice!(ref impl ToObject for (&str, [&JObject<'_>]));
impl_paired_obj_slice!(    impl ToObject for (&str, &[JObject<'_>]));
impl_paired_obj_slice!(ref impl ToObject for (&str, &[&JObject<'_>]));
impl_paired_obj_slice!(    impl<const N: usize> ToObject for (&str, [JObject<'_>; N]));
impl_paired_obj_slice!(ref impl<const N: usize> ToObject for (&str, [&JObject<'_>; N]));
impl_paired_obj_slice!(    impl<const N: usize> ToObject for (&str, &[JObject<'_>; N]));
impl_paired_obj_slice!(ref impl<const N: usize> ToObject for (&str, &[&JObject<'_>; N]));

// Implementation for [Option<T>]

impl<'local> FromObject<'_, '_, 'local> for Box<[Option<JObject<'local>>]> {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        get_object_array(object, None, env)?
            .into_iter()
            .map(|obj| Option::from_object_owned_env(obj, env))
            .collect::<Result<Self, _>>()
    }
}
impl ToObject for (&str, &[Option<JObject<'_>>]) {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array_converted(
            &self.1,
            self.0,
            |opt, env| opt.to_object_env(env),
        env)
    }
}
impl ToObject for (&str, &[Option<&JObject<'_>>]) {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array_converted(
            &self.1,
            self.0,
            |opt, env| opt.to_object_env(env),
        env)
    }
}
impl<const N: usize> ToObject for (&str, [Option<JObject<'_>>; N]) {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        <(&str, &[Option<JObject<'_>>]) as ToObject>::to_object_env(&(self.0, self.1.as_slice()), env)
    }
}
impl<const N: usize> ToObject for (&str, [Option<&JObject<'_>>; N]) {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        <(&str, &[Option<&JObject<'_>>]) as ToObject>::to_object_env(&(self.0, self.1.as_slice()), env)
    }
}


// // Multi-dimensional Arrays (up to 10 dimensions)
// // 2 Dimensions
// impl<'local, T> FromObject<'_, '_, 'local> for Box<[Box<[T]>]>
// where Box<[T]>: for<'a, 'obj> FromObject<'a, 'obj, 'local>,
//       T: Class
// {
//     fn from_object_env(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
//         get_object_array(object, Some(&format!("[[L{};", T::CLASS_PATH)), env)?
//             .into_iter()
//             .map(|obj| Box::<[T]>::from_object_env(&obj, env))
//             .collect()
//     }
// }
// // 3 Dimensions
// impl<'local, T> FromObject<'_, '_, 'local> for Box<[Box<[Box<[T]>]>]>
// where Box<[T]>: for<'a, 'obj> FromObject<'a, 'obj, 'local>,
//       T: Class
// {
//     fn from_object_env(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
//         get_object_array(object, Some(&format!("[[[L{};", T::CLASS_PATH)), env)?
//             .into_iter()
//             .map(|obj| Box::<[Box<[T]>]>::from_object_env(&obj, env))
//             .collect()
//     }
// }
// // 4 Dimensions
// impl<'local, T> FromObject<'_, '_, 'local> for Box<[Box<[Box<[Box<[T]>]>]>]>
// where Box<[T]>: for<'a, 'obj> FromObject<'a, 'obj, 'local>,
//       T: Class
// {
//     fn from_object_env(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
//         get_object_array(object, Some(&format!("[[[[L{};", T::CLASS_PATH)), env)?
//             .into_iter()
//             .map(|obj| Box::<[Box<[Box<[T]>]>]>::from_object_env(&obj, env))
//             .collect()
//     }
// }
// // 5 Dimensions
// impl<'local, T> FromObject<'_, '_, 'local> for Box<[Box<[Box<[Box<[Box<[T]>]>]>]>]>
// where Box<[T]>: for<'a, 'obj> FromObject<'a, 'obj, 'local>,
//       T: Class
// {
//     fn from_object_env(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
//         get_object_array(object, Some(&format!("[[[[[L{};", T::CLASS_PATH)), env)?
//             .into_iter()
//             .map(|obj| Box::<[Box<[Box<[Box<[T]>]>]>]>::from_object_env(&obj, env))
//             .collect()
//     }
// }
// // 6 Dimensions
// impl<'local, T> FromObject<'_, '_, 'local> for Box<[Box<[Box<[Box<[Box<[Box<[T]>]>]>]>]>]>
// where Box<[T]>: for<'a, 'obj> FromObject<'a, 'obj, 'local>,
//       T: Class
// {
//     fn from_object_env(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
//         get_object_array(object, Some(&format!("[[[[[[L{};", T::CLASS_PATH)), env)?
//             .into_iter()
//             .map(|obj| Box::<[Box<[Box<[Box<[Box<[T]>]>]>]>]>::from_object_env(&obj, env))
//             .collect()
//     }
// }
// // 7 Dimensions
// impl<'local, T> FromObject<'_, '_, 'local> for Box<[Box<[Box<[Box<[Box<[Box<[Box<[T]>]>]>]>]>]>]>
// where Box<[T]>: for<'a, 'obj> FromObject<'a, 'obj, 'local>,
//       T: Class
// {
//     fn from_object_env(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
//         get_object_array(object, Some(&format!("[[[[[[[L{};", T::CLASS_PATH)), env)?
//             .into_iter()
//             .map(|obj| Box::<[Box<[Box<[Box<[Box<[Box<[T]>]>]>]>]>]>::from_object_env(&obj, env))
//             .collect()
//     }
// }
// // 8 Dimensions
// impl<'local, T> FromObject<'_, '_, 'local> for Box<[Box<[Box<[Box<[Box<[Box<[Box<[Box<[T]>]>]>]>]>]>]>]>
// where Box<[T]>: for<'a, 'obj> FromObject<'a, 'obj, 'local>,
//       T: Class
// {
//     fn from_object_env(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
//         get_object_array(object, Some(&format!("[[[[[[[[L{};", T::CLASS_PATH)), env)?
//             .into_iter()
//             .map(|obj| Box::<[Box<[Box<[Box<[Box<[Box<[Box<[T]>]>]>]>]>]>]>::from_object_env(&obj, env))
//             .collect()
//     }
// }
// // 9 Dimensions
// impl<'local, T> FromObject<'_, '_, 'local> for Box<[Box<[Box<[Box<[Box<[Box<[Box<[Box<[Box<[T]>]>]>]>]>]>]>]>]>
// where Box<[T]>: for<'a, 'obj> FromObject<'a, 'obj, 'local>,
//       T: Class
// {
//     fn from_object_env(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
//         get_object_array(object, Some(&format!("[[[[[[[[[L{};", T::CLASS_PATH)), env)?
//             .into_iter()
//             .map(|obj| Box::<[Box<[Box<[Box<[Box<[Box<[Box<[Box<[T]>]>]>]>]>]>]>]>::from_object_env(&obj, env))
//             .collect()
//     }
// }
// // 10 Dimensions
// impl<'local, T> FromObject<'_, '_, 'local> for Box<[Box<[Box<[Box<[Box<[Box<[Box<[Box<[Box<[Box<[T]>]>]>]>]>]>]>]>]>]>
// where Box<[T]>: for<'a, 'obj> FromObject<'a, 'obj, 'local>,
//       T: Class
// {
//     fn from_object_env(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
//         get_object_array(object, Some(&format!("[[[[[[[[[[L{};", T::CLASS_PATH)), env)?
//             .into_iter()
//             .map(|obj| Box::<[Box<[Box<[Box<[Box<[Box<[Box<[Box<[Box<[T]>]>]>]>]>]>]>]>]>::from_object_env(&obj, env))
//             .collect()
//     }
// }

// // 2 Dimensions
// impl<T> ToObject for [&[T]]
// where [T]: ToObject,
//       T: Class
// {
//     fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
//         todo!();
//     }
// }
// // 3 Dimensions
// impl<T> ToObject for [&[&[T]]]
// where [T]: ToObject,
//       T: Class
// {
//     fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
//         todo!();
//     }
// }
// // 4 Dimensions
// impl<T> ToObject for [&[&[&[T]]]]
// where [T]: ToObject,
//       T: Class
// {
//     fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
//         todo!();
//     }
// }
// // 5 Dimensions
// impl<T> ToObject for [&[&[&[&[T]]]]]
// where [T]: ToObject,
//       T: Class
// {
//     fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
//         todo!();
//     }
// }
// // 6 Dimensions
// impl<T> ToObject for [&[&[&[&[&[T]]]]]]
// where [T]: ToObject,
//       T: Class
// {
//     fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
//         todo!();
//     }
// }
// // 7 Dimensions
// impl<T> ToObject for [&[&[&[&[&[&[T]]]]]]]
// where [T]: ToObject,
//       T: Class
// {
//     fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
//         todo!();
//     }
// }
// // 8 Dimensions
// impl<T> ToObject for [&[&[&[&[&[&[&[T]]]]]]]]
// where [T]: ToObject,
//       T: Class
// {
//     fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
//         todo!();
//     }
// }
// // 9 Dimensions
// impl<T> ToObject for [&[&[&[&[&[&[&[&[T]]]]]]]]]
// where [T]: ToObject,
//       T: Class
// {
//     fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
//         todo!();
//     }
// }
// // 10 Dimensions
// impl<T> ToObject for [&[&[&[&[&[&[&[&[&[T]]]]]]]]]]
// where [T]: ToObject,
//       T: Class
// {
//     fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
//         todo!();
//     }
// }