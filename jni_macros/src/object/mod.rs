mod class;
mod exception;

pub use class::*;
pub use exception::*;

use either::Either;
use convert_case::{Case, Casing};
use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned, ToTokens, TokenStreamExt};
use syn::{parse::Parse, punctuated::Punctuated, spanned::Spanned, AngleBracketedGenericArguments, Field, Fields, GenericArgument, GenericParam, Generics, Ident, ItemEnum, ItemStruct, Lifetime, LitStr, Token, Type, TypePath, Variant};
use std::str::FromStr;
use crate::{
    types::{ClassPath, JavaPrimitive, RustPrimitive, SigType, SpecialCaseConversion}, utils::{get_class_attribute_required, merge_errors, take_class_attribute}
};

/// Find the JObject field with a lifetime, and use that lifetime's name for the JNIEnv lifetime,
/// Defaults to `'local` if such object could not be found, and **appends** the lifetime to the *generics*.
///
/// `P` is just the punctuation type; don't worry about it.
fn get_local_lifetime<P: Default>(item: Either<&ItemStruct, &ItemEnum>, generics: &mut Punctuated<GenericParam, P>) -> Lifetime {
    static DEFAULT: &str = "local";
    fn find_local(generics: &Generics) -> Option<Lifetime> {
        generics.lifetimes()
            .map(|lt| &lt.lifetime)
            .find(|lt| lt.ident.to_string() == DEFAULT)
            .map(Clone::clone)
    }
    fn find_in_fields(fields: &Fields) -> Option<Lifetime> {
        fields.iter()
            .find_map(|field| match &field.ty {
                Type::Path(TypePath { path, .. } ) if path.segments.last()
                    .is_some_and(|seg| seg.ident.to_string() == "JObject")
                => match path.segments.last().map(|seg| &seg.arguments) {
                    Some(syn::PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }))
                    => match args.first() {
                        Some(GenericArgument::Lifetime(lt)) => Some(lt.clone()),
                        _ => None
                    },
                    _ => None
                },
                _ => None
            })
    }

    /// Generate the default Lifetime: `'local`.
    /// Appends the lifetime to the generic params.
    /// The failure of [`find_local()`] indicates that there is no `'local` lifetime and it needs to be added.
    fn default<P: Default>(span: Span, generics: &mut Punctuated<GenericParam, P>) -> Lifetime {
        let default = Lifetime::new(&format!("'{DEFAULT}"), span);

        generics.push(syn::GenericParam::Lifetime(syn::LifetimeParam::new(default.clone())));

        default
    }
    
    match item {
        Either::Left(st) =>
            find_local(&st.generics)
                .or_else(|| find_in_fields(&st.fields))
                .unwrap_or_else(|| default(st.ident.span(), generics)),
        Either::Right(enm) =>
            find_local(&enm.generics)
                .or_else(||
                    enm.variants.iter()
                        .find_map(|variant| find_in_fields(&variant.fields))
                )
                .unwrap_or_else(|| default(enm.ident.span(), generics)),
    }
}

/// Content of the `field` attribute: *key-value pairs* inside a parenthesis.
/// * **name** - Use this name for the Object's field lookup instead of the field's name.
///                     Mutually exclusive with `call`.
/// * **call** - Instead of a looking up a *member*, call one of the Object's *method*s.
///                     Mutually exclusive with `name`.
/// * **class** - The Class of the object if the field is a `JObject`.
/// 
/// Either *name* or *call* MUST be used if the field belongs to a Tuple struct.
/// 
/// Example: `#[field(call = getInt, class = java.lang.Integer)] int: i32`
struct FieldAttr {
    name: Option<Ident>,
    call: Option<Ident>,
    class: Option<ClassPath>,
}
impl FieldAttr {
    /// Find the `field` attribute in a struct's field and parse its content.
    pub fn get_from_attrs(field: &Field) -> syn::Result<Self> {
        field.attrs
            .iter()
            .find(|&attr|
                attr.path()
                    .get_ident()
                    .is_some_and(|ident| ident.to_string() == "field")
            )
            .map_or_else(
                || Ok(Self { name: None, call: None, class: None }),
                |attr| Self::parse_attr_meta(&attr.meta)
            )
    }
    
    /// Parses the tokens in the attribute as `comma-separated` `key-value pairs`.
    fn parse_attr_meta(meta: &syn::Meta) -> syn::Result<Self> {
        let input = match meta {
            syn::Meta::List(meta) => meta.tokens.clone(),
            _ => return Err(syn::Error::new(meta.span(), "Attribute \"field\" must have content in parenthesis"))
        };
        let mut unparsed_kvs = Vec::<TokenStream>::new();
        
        // Separate the tokens between commas (,)
        let mut current = TokenStream::new();
        for token in input {
            match token {
                // At every comma (,) push the current tokenstream, and start with a new tokenstream
                proc_macro2::TokenTree::Punct(p) if p.as_char() == ',' => {
                    unparsed_kvs.push(current);
                    current = TokenStream::new();
                },
                _ => current.append(token),
            }
        }
        // Push whatever is left
        if !current.is_empty() {
            unparsed_kvs.push(current);
        }
        
        // Then parse the key-value pairs
        struct KVPair { key: Ident, val: TokenStream }
        impl Parse for KVPair {
            fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
                Ok(Self {
                    key: input.parse::<Ident>()?,
                    val: {
                        input.parse::<Token![=]>()?;
                        input.parse()?
                    }
                })
            }
        }
        fn parse_val<T: Parse>(key: Ident, val: TokenStream, store: &mut Option<T>, ) -> syn::Result<()> {
            if store.is_none() {
                *store = Some(syn::parse2(val)?)
            } else {
                return Err(syn::Error::new(key.span(), "Already used \"{key}\""))
            }
            
            Ok(())
        }
        let mut name = None;
        let mut call = None;
        let mut class = None;
        
        for tokens in unparsed_kvs {
            let KVPair { key, val } = syn::parse2::<KVPair>(tokens)?;
            
            match key.to_string().as_str() {
                "name" => parse_val(key, val, &mut name)?,
                "call" => parse_val(key, val, &mut call)?,
                "class" => parse_val(key, val, &mut class)?,
                _ => return Err(syn::Error::new(key.span(), "Unknown key; Expected one of the following keys: \"name\", \"call\", \"class\""))
            }
        }
        
        Ok(Self { name, call, class })
    }
}

/// The type that will be used in the JNI call to *get field* or *call gettter method*.
enum FieldType {
    Prim(Ident, RustPrimitive),
    Object(ClassPath),
    // The Java Class is obtained from the struct/enum's `Class` implementation.
    Implicit(syn::Type)
}
impl FieldType {
    pub fn from_field(field_ty: &Type, class_attr: Option<ClassPath>) -> syn::Result<Self> {
        let ty_str = field_ty.to_token_stream().to_string();
        // Check if the Field's type is a primitive
        match RustPrimitive::from_str(&ty_str) {
            Ok(prim) => match class_attr {
                Some(class) => Err(syn::Error::new(class.span(), "Can't use \"class\" when the field is prmitive")),
                None => Ok(Self::Prim(Ident::new(&ty_str, field_ty.span()), prim))
            },
            Err(_) => match class_attr {
                // If not a primitive, then use the provided Class
                Some(class) => match &field_ty {
                    // There is no way to know if the Object is the same `::jni::objects::JObject`,
                    // just that it could be if the type is a Type Path.
                    Type::Path(_) => Ok(Self::Object(class)),
                    _ => Err(syn::Error::new(class.span(), "\"class\" is only allowed for JObject"))
                },
                // If none of the previous, then the class will be obtained from the `Class` impl
                None => Ok(Self::Implicit(field_ty.clone()))
            }
        }
    }

    /// See [`crate::utils::SigType::sig_char()`].
    pub fn sig_char(&self) -> Ident {
        match self {
            Self::Prim(ident, prim) => {
                let mut i = JavaPrimitive::from(*prim).sig_char();
                i.set_span(ident.span());
                i
            },
            Self::Object(class) => class.sig_char(),
            Self::Implicit(ty) => Ident::new("l", ty.span()),
        }
    }
    /// The signature of a Java field using this type.
    /// Is an expression of a *format string* if is [`FieldType::Implicit`].
    pub fn field_sig_type(&self) -> TokenStream {
        match self {
            Self::Prim(ident, prim) => {
                let mut sig_ty: LitStr = JavaPrimitive::from(*prim).sig_type();
                sig_ty.set_span(ident.span());
                sig_ty.to_token_stream()
            },
            Self::Object(class) => class.sig_type().to_token_stream(),
            Self::Implicit(ty) => quote_spanned! {ty.span()=> &format!("L{};", <#ty as ::ez_jni::FromObject>::PATH) }
        }
    }
    /// The same [`Self::field_sig_type()`], but for use in the getter method signature.
    pub fn method_sig_type(&self) -> TokenStream {
        match self {
            Self::Prim(ident, prim) => {
                let sig_ty = JavaPrimitive::from(*prim).sig_type().value();
                LitStr::new(&format!("(){sig_ty}"), ident.span()).to_token_stream()
            },
            Self::Object(class) => {
                let sig_ty = class.sig_type().value();
                LitStr::new(&format!("(){sig_ty}"), class.span()).to_token_stream()
            },
            Self::Implicit(ty) => quote_spanned! {ty.span()=> format!("()L{};", <#ty as ::ez_jni::FromObject>::PATH) },
        }
    }

    pub fn converted(&self, value: TokenStream) -> TokenStream {
        match self {
            FieldType::Prim(_, prim) => prim.convert_rust_to_java(&value)
                .unwrap_or(value),
            FieldType::Implicit(ty) => quote_spanned! {ty.span()=> <#ty as ::ez_jni::FromObject>::from_object(&#value, env)? },
            FieldType::Object(_) => quote_spanned! {value.span()=> #value.into() }
        }
    }
}

/// Builds a struct *constructor literal*, where all its fields are initialized with JNI calls to *read fields* or *call getter methods*.
/// Can handle *Unit, Tuple, and Named structs*.
/// Can also be used for enum variants.
/// 
/// The *type* and *name* of the field will be used to create a JNI call to the Object which will be converted from.
/// By default, the **name** will be used to get the value from a *member of the Object*,
/// and the **type** will be converted from a Java Object if it implements `ez_jni::Class`.
/// 
/// **class** is the Java Class (in *slash-separated* form) of the struct/enum.
/// 
/// See [`FieldAttr`] for syntax.
fn struct_constructor(fields: &Fields, class: &str) -> syn::Result<TokenStream> {    
    let mut errors = Vec::new();
    
    // Produce the value that will be assigned for each field
    let values = fields.iter()
        .map(|field| {
            let attr = FieldAttr::get_from_attrs(field)?;
            // Determine the type of the Java member
            let ty = FieldType::from_field(&field.ty, attr.class)?;

            let get_field = |name: String, getter_fallback: bool| {
                let sig_char = ty.sig_char();
                let sig_ty = ty.field_sig_type();
                
                ty.converted(quote_spanned! {field.span()=>
                    ::ez_jni::utils::get_field(&object, #name, #sig_ty, #getter_fallback, env)?
                        .#sig_char().unwrap_or_else(|err| panic!("The method call did not return the expected type: {err}"))
                })
            };
            
            Ok(if let Some(name) = attr.name {
                // Use the "name" of the field attribute
                get_field(name.to_string(), false)
            } else if let Some(call) = attr.call {
                // Call the Java method
                let call = LitStr::new(&call.to_string(), call.span());
                let sig_char = ty.sig_char();
                let sig_ty = ty.method_sig_type();
                
                ty.converted(quote_spanned! {call.span()=>
                    env.call_method(&object, #call, #sig_ty, &[])
                        .map_err(|err| if let ::jni::errors::Error::MethodNotFound { name, sig } = err {
                            ::ez_jni::FromObjectError::FieldNotFound { name, ty: sig, target_class: #class.to_string() }
                        } else {
                            panic!("Error occurred while calling getter method: {err}")
                        })?
                        .#sig_char()
                        .unwrap_or_else(|err| panic!("The method call did not return the expected type: {err}"))
                })
            } else if let Some(name) = &field.ident {
                // Use the name of the field, and also call "get{Name}" if field not found
                get_field(name.to_string().to_case(Case::Camel), true)
            } else {
                return Err(syn::Error::new(field.span(), "Field must have \"name\" or \"call\" if it is unnamed. See the 'field' attribute."))
            })
        })
        .filter_map(|res| res.map_err(|err| errors.push(err)).ok())
        .collect::<Box<[_]>>();
    
    merge_errors(errors)?;
    
    let span = fields.span();
    Ok(match fields {
        Fields::Unit => fields.to_token_stream(), // Unit structs have no fields to construct
        Fields::Unnamed(_) => quote_spanned! {span=> ( #(#values),* ) },
        Fields::Named(syn::FieldsNamed{ named: fields, .. }) => {
            let names = fields.iter()
                .map(|field| field.ident.as_ref().unwrap());
            quote_spanned! {span=> {
                #( #names: #values ),*
            } }
        }
    })
}

/// Creates constructor literals for each of the *enum's variants*,
/// checking that the object is the right class.
/// 
/// The returned results can be [`filter_maped`][`Iterator::filter_map()`],
/// stripping out the error by pushing them to an *error [`Vec`]*.
/// ```ignore
/// .filter_map(|res| res.map_err(|err| errors.push(err)).ok())
/// ```
/// 
/// The constructor literal is built by [`struct_constructor()`].
fn construct_variants<'a>(variants: impl Iterator<Item = &'a mut Variant> + 'a) -> impl Iterator<Item = syn::Result<TokenStream>> + 'a {
    variants.enumerate()
        .map(|(i, variant)| {
            // Get class name for this variant
            let class = get_class_attribute_required(&mut variant.attrs, variant.ident.span())?
                .to_jni_class_path();

            let ident = &variant.ident;
            // Get a constructor for this variant
            let ctor = struct_constructor(&variant.fields, &class)?;
            let _if = if i == 0 { quote!(if) } else { quote!(else if) };
            // Check if Exception is the class that this Variant uses, and construct the variant
            Ok(quote_spanned! {variant.span()=>
                #_if env.is_instance_of(object, #class).unwrap() {
                    Ok(Self::#ident #ctor)
                }
            })
        })
}
