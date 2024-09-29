use either::Either;
use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{ItemEnum, ItemStruct};
use crate::utils::merge_errors;
use super::*;

pub fn from_exception_struct(st: ItemStruct) -> syn::Result<TokenStream> {
    let class = get_class_attribute_required(&st.attrs, st.ident.span())?
        .to_jni_class_path();
    
    let mut st_generic_params = st.generics.params.clone();
    let env_lt = get_local_lifetime(Either::Left(&st), &mut st_generic_params);
    let st_ident = st.ident;
    let st_generics = &st.generics;
    let st_ctor = struct_constructor(&st.fields, &class)?;
    
    Ok(quote! {
        impl <#st_generic_params> ::ez_jni::FromException<#env_lt> for #st_ident #st_generics {
            fn from_exception(object: &::jni::objects::JThrowable, env: &mut ::jni::JNIEnv<#env_lt>) -> Result<Self, ::ez_jni::FromObjectError> {
                // object is guaranteed to not be null by the catch function

                let __class = env.get_object_class(object)
                    .unwrap_or_else(|err| panic!("Failed to get Object's class: {err}"));
                
                if !env.is_instance_of(object, #class).unwrap() {
                    return Err(::ez_jni::FromObjectError::ClassMismatch {
                        obj_class: ::ez_jni::utils::get_string(::ez_jni::call!(__class.getName() -> java.lang.String), env),
                        target_class: #class
                    })
                }

                Ok(Self #st_ctor)
            }
        }
    })
}

pub fn from_exception_enum(enm: ItemEnum) -> syn::Result<TokenStream> {
    let mut errors = Vec::new();

    let base_class = get_class_attribute(&enm.attrs)
        .map_err(|err| errors.push(err))
        .ok()
        .and_then(|o| o)
        .map(|class| class.to_jni_class_path());
    let base_class_check = base_class.map(|base_class| quote_spanned! {enm.ident.span()=>
        if !env.is_instance_of(object, #base_class).unwrap() {
            return Err(::ez_jni::FromObjectError::ClassMismatch {
                obj_class: ::ez_jni::utils::get_string(::ez_jni::call!(__class.getName() -> java.lang.String), env),
                target_class: #base_class
            })
        }
    });
    
    if enm.variants.is_empty() {
        errors.push(syn::Error::new(Span::call_site(), "Enum must have at least 1 variant"));
    }

    let class_checks = construct_variants(enm.variants.iter())
        .filter_map(|res| res.map_err(|err| errors.push(err)).ok())
        .collect::<Box<[_]>>();

    merge_errors(errors)?;

    let mut enm_generic_params = enm.generics.params.clone();
    let env_lt = get_local_lifetime(Either::Right(&enm), &mut enm_generic_params);
    let enm_ident = enm.ident;
    let enm_generics = &enm.generics;
    Ok(quote! {
        impl <#enm_generic_params> ez_jni::FromException<#env_lt> for #enm_ident #enm_generics {
            fn from_exception(object: &::jni::objects::JThrowable, env: &mut ::jni::JNIEnv<#env_lt>) -> Result<Self, ::ez_jni::FromObjectError> {
                // object is guaranteed to not be null by the catch function

                let __class = env.get_object_class(object)
                    .unwrap_or_else(|err| panic!("Failed to get Object's class: {err}"));
                
                #base_class_check
                #(#class_checks)* else {
                    Err(::ez_jni::FromObjectError::ClassMismatch {
                        obj_class: ::ez_jni::utils::get_string(::ez_jni::call!(__class.getName() -> java.lang.String), env),
                        target_class: ""
                    })
                }
            }
        }
    })
}