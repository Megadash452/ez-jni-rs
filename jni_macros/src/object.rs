use either::Either;
use convert_case::{Case, Casing};
use proc_macro2::{Span, TokenStream, TokenTree};
use quote::{quote, quote_spanned, ToTokens, TokenStreamExt};
use syn::{AngleBracketedGenericArguments, Field, Fields, GenericArgument, Generics, Ident, ItemEnum, ItemStruct, Lifetime, LitStr, Token, TypePath, Variant, bracketed, parse::{Parse, ParseStream, Parser}, token::Bracket};
use itertools::Itertools as _;
use std::{cell::RefCell, collections::{HashMap, HashSet}};
use crate::{
    call::Env,
    types::{Class, SigType},
    utils::{merge_errors, ops_prelude, take_class_attribute, take_class_attribute_required, Spanned}
};

/// Outputs the trait Implementation of [`FromObject`][https://docs.rs/ez_jni/0.5.2/ez_jni/trait.FromObject.html] for **structs**.
pub fn derive_struct(mut st: ItemStruct) -> syn::Result<TokenStream> {
    let class = take_class_attribute_required(&mut st.attrs, st.ident.span())?
        .to_jni_class_path();
    let ops_prelude = ops_prelude(Env::Argument);
    let env_lt = get_local_lifetime(Either::Left(&st));
    let st_ident = st.ident;
    let st_generics = st.generics;
    let st_ctor = struct_constructor(&st.fields)?;

    Ok(quote! {
        impl #st_generics ::ez_jni::FromJValue<#env_lt> for #st_ident #st_generics {
            ::ez_jni::impl_from_jvalue_env!(<#env_lt>);
        }
        impl #st_generics ::ez_jni::FromObject<#env_lt> for #st_ident #st_generics {
            fn from_object_env(object: &::jni::objects::JObject<'_>, env: &mut ::jni::JNIEnv<#env_lt>) -> ::std::result::Result<Self, ::ez_jni::FromObjectError> {
                #ops_prelude
                ::ez_jni::utils::check_object_class(object, &<Self as ::ez_jni::Class>::class(), env)?;
                Ok(Self #st_ctor)
            }
        }
        impl #st_generics ::ez_jni::Class for #st_ident #st_generics {
            fn class() -> ::std::borrow::Cow<'static, str> {
                ::std::borrow::Cow::Borrowed(#class)
            }
        }
    })
}

/// Outputs the trait Implementation of [`FromObject`][https://docs.rs/ez_jni/0.5.2/ez_jni/trait.FromObject.html] for **enums**.
pub fn derive_enum(mut enm: ItemEnum) -> syn::Result<TokenStream> {
    let mut errors = Vec::new();

    if enm.variants.is_empty() {
        errors.push(syn::Error::new(Span::call_site(), "Enum must have at least 1 variant"));
    }

    let base_class = take_class_attribute(&mut enm.attrs)
        .map_err(|err| errors.push(err))
        .ok()
        .and_then(|o| o)
        .map(|class| class.to_jni_class_path());

    let base_class_check = base_class.as_ref()
        .map(|_| quote_spanned! {enm.ident.span()=> {
            let class = <Self as ::ez_jni::Class>::class();
            if !env.is_instance_of(object, &class).unwrap_jni(env) {
                return Err(::ez_jni::FromObjectError::ClassMismatch {
                    obj_class: __class,
                    target_class: Some(::std::borrow::Cow::into_owned(class))
                })
            }
        } })
        .unwrap_or(TokenStream::new());

    let class_checks = construct_variants(enm.variants.iter_mut())
        .filter_map(|res| res.map_err(|err| errors.push(err)).ok())
        .collect::<Box<[_]>>();

    merge_errors(errors)?;

    let ops_prelude = ops_prelude(Env::Argument);
    let env_lt = get_local_lifetime(Either::Right(&enm));
    let enm_ident = &enm.ident;
    let enm_generics = &enm.generics;

    // Types that implement FromObject are also given a Class implementation.
    // However, for enums this is optional because only the variants need to specify a class.
    let base_class_impl = base_class.as_ref()
        .map(|class| quote! {
            impl #enm_generics ::ez_jni::Class for #enm_ident #enm_generics {
                fn class() -> ::std::borrow::Cow<'static, str> {
                    ::std::borrow::Cow::Borrowed(#class)
                }
            }
        })
        .unwrap_or(TokenStream::new());

    Ok(quote! {
        impl #enm_generics ::ez_jni::FromJValue<#env_lt> for #enm_ident #enm_generics {
            ::ez_jni::impl_from_jvalue_env!(<#env_lt>);
        }
        impl #enm_generics ::ez_jni::FromObject<#env_lt> for #enm_ident #enm_generics {
            fn from_object_env(object: &::jni::objects::JObject<'_>, env: &mut ::jni::JNIEnv<#env_lt>) -> ::std::result::Result<Self, ::ez_jni::FromObjectError> {
                #ops_prelude

                if object.is_null() {
                    return Err(::ez_jni::FromObjectError::Null);
                }

                let __class = ::ez_jni::utils::get_object_class_name(object, env);

                #base_class_check

                #(#class_checks)* else {
                    Err(::ez_jni::FromObjectError::ClassMismatch {
                        obj_class: __class,
                        target_class: None
                    })
                }
            }
        }
        #base_class_impl
    })
}

/// Properties parsed from the value of an [`Attribute`][syn::Attribute].
/// 
/// The properties are *Key-Value Pairs*,
/// where the *Key* is an [`Ident`] and the *Value* can be any tokens.
/// The *attribute* must have tokens wrapped in parenthesis, like this: `#[attr(key = val)]`.
/// 
/// Parse the attribute's content to get an instance of [`AttributeProps`],
/// and [`take`][Self::take] properties with a *name*.
/// Then call [`Self::finish`] to make sure all the properties provided by the user are valid for the macro.
struct AttributeProps {
    properties: RefCell<HashMap<String, TokenStream>>,
    /// The keys that were used as the **name** in all calls to [`self.take()`][Self::take].
    probed: RefCell<HashSet<String>>,
}
impl AttributeProps {
    /// Parse the content of an [`Attribute`][syn::Attribute] and
    /// with the parsed *properties* **build** an instance of a Type that stores the properties.
    /// 
    /// ## Example
    /// ```ignore
    /// AttributeProps::parse_attr_with(attr, |props| {
    ///     Ok(Self {
    ///         name: props.take("name")?,
    ///         call: props.take("call")?,
    ///         class: props.take("class")?,
    ///     })
    /// })
    /// ```
    pub fn parse_attr_with<T>(attr: &syn::Attribute, builder: impl FnOnce(&Self) -> syn::Result<T>) -> syn::Result<T> {
        let props = Self::parse_attr(attr)?;
        let rtrn = builder(&props)?;
        props.finish()?;
        Ok(rtrn)
    }
    /// Parse the content of an [`Attribute`][syn::Attribute] to get *properties*.
    pub fn parse_attr(attr: &syn::Attribute) -> syn::Result<Self> {
        let attr_val = match &attr.meta {
            syn::Meta::List(meta) => meta.tokens.clone(),
            _ => return Err(syn::Error::new(attr.meta.span(), "This attribute takes properties as input, so it must have content in parenthesis"))
        };
    
        syn::parse2(attr_val)
    }

    /// Get and *parse* the **value** of a property, marking it as *taken*.
    /// 
    /// `panic!s` if [`take()`][Self::take] was already called with the same **name**.
    /// Return `Ok(None)` if there is no property with this **name**.
    /// Returns [`syn::Error`] if the value could not be parsed with `T`.
    pub fn take<T: Parse>(&self, name: &str) -> syn::Result<Option<T>> {
        if !self.probed.borrow_mut().insert(name.to_string()) {
            panic!("The property with name \"{name}\" was already taken")
        }

        match self.properties.borrow_mut().remove(name) {
            Some(val) => Ok(Some(syn::parse2(val)?)),
            None => Ok(None)
        }
    }

    #[allow(unstable_name_collisions)]
    pub fn finish(self) -> syn::Result<()> {
        let probed = self.probed.borrow();
        let properties = self.properties.borrow();

        if !properties.is_empty() {
            // Collect all the *names* used with [`Self::take()`] to print as the expected keys 
            let expected_props = probed.iter()
                .map(|key| format!("\"{key}\""))
                .intersperse(", ".to_string())
                .collect::<String>();
            // Create errors for the leftover properties
            Err(merge_errors(
                properties.iter()
                    .map(|(key, _)| syn::Error::new(key.span(), format!("Unknown key \"{key}\"; Expected one of the following keys: {expected_props}")))
            ).unwrap_err())
        } else {
            Ok(())
        }
    }
}
impl Parse for AttributeProps {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut unparsed_kvs = Vec::<TokenStream>::new();
        
        // Separate the tokens between commas (,)
        let mut current = TokenStream::new();
        while let Ok(token) = input.parse::<TokenTree>() {
            match token {
                // At every comma (,) push the current tokenstream, and start with a new tokenstream
                TokenTree::Punct(p) if p.as_char() == ',' => {
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

        let mut errors = Vec::new();
        // Make the key Ident so that the span can be used for errors
        let mut properties = HashMap::new();

        // Then parse the key-value pairs
        for tokens in unparsed_kvs {
            match Parser::parse2(|input: ParseStream|
                // Syntax: key = value
                Ok((input.parse::<Ident>()?, {
                    input.parse::<Token![=]>()?;
                    input.parse()?
                })),
                tokens
            ) {
                Ok((key, value)) =>
                    // Error when a property is assigned more than once
                    if properties.contains_key(&key) {
                        errors.push(syn::Error::new(key.span(), "Already used property \"{key}\""))
                    } else {
                        properties.insert(key, value);
                    },
                Err(error) => errors.push(error)
            }
        }

        merge_errors(errors)?;

        Ok(Self {
            probed: RefCell::new(HashSet::new()),
            properties: RefCell::new(properties.into_iter()
                .map(|(key, val)| (key.to_string(), val))
                .collect()
            ),
        })
    }
}

/// Find the *name* of the lifetime that should be used in the [`JNIEnv`][jni::JNIEnv] as the `'local` lifetime.
/// 
/// If the item contains a lifetime named `'local`,
/// or has only 1 lifetime named anything,
/// then that lifetime will be used.
/// 
/// Defaults to the ellided lifetime if none could be found.
fn get_local_lifetime(item: Either<&ItemStruct, &ItemEnum>) -> Lifetime {
    /// Find the lifetime in the struct/enum's generics.
    /// If only 1 lifetime exists, just grabs that one.
    fn find_local(generics: &Generics) -> Option<Lifetime> {
        let mut lifetimes = generics.lifetimes()
            .map(|lt| &lt.lifetime);

        // Check if only 1 lifetime exists.
        let first = lifetimes.next()?;
        let second = lifetimes.next();
        if second == None {
            return Some(first.clone());
        }

        // Otherwise, find 'local
        Some(first).into_iter()
            .chain(second)
            .chain(lifetimes)
            .find(|lt| lt.ident.to_string() == "local")
            .map(Clone::clone)
    }
    /// Find a field that has a lifetime in its generics OR is a JObject or one of its counterparts
    fn find_in_fields(fields: &Fields) -> Option<Lifetime> {
        fields.iter()
            .find_map(|field| match &field.ty {
                syn::Type::Path(TypePath { path, .. } ) if path.segments.last()
                    .is_some_and(|seg| matches!(seg.ident.to_string().as_str(), "JObject" | "JClass" | "JThrowable"))
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

    match item {
        Either::Left(st) =>
            find_local(&st.generics)
                .or_else(|| find_in_fields(&st.fields))
                .unwrap_or_else(|| Lifetime::new("'_", st.ident.span())),
        Either::Right(enm) =>
            find_local(&enm.generics)
                .or_else(||
                    enm.variants.iter()
                        .find_map(|variant| find_in_fields(&variant.fields))
                )
                .unwrap_or_else(|| Lifetime::new("'_", enm.ident.span())),
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
    class: Option<ClassAttr>,
}
impl FieldAttr {
    /// Find the `field` attribute in a struct's field and parse its content.
    /// 
    /// If there is no `field` attribute, then returns with everything set to [`None`].
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
                |attr| AttributeProps::parse_attr_with(attr, |props| {
                    let field_attr = Self {
                        name: props.take("name")?,
                        call: props.take("call")?,
                        class: props.take("class")?,
                    };
                    if field_attr.name.is_some() && field_attr.call.is_some() {
                        return Err(syn::Error::new(attr.bracket_token.span.span(), "The field attributes 'name' and 'call' are mutually exclusive; only one can be used."))
                    }
                    Ok(field_attr)
                })
            )
    }
}
struct ClassAttr {
    dimensions: u32,
    outer_brackets: Option<Bracket>,
    base_component_class: Class,
}
impl Spanned for ClassAttr {
    fn span(&self) -> Span {
        match self.outer_brackets {
            Some(brackets) => {
                let tokens = &mut TokenStream::new();
                brackets.surround(tokens, |tokens| {
                    tokens.append(Ident::new("a", self.base_component_class.span()));
                });
                tokens.span()
            },
            None => self.base_component_class.span()
        }
    }
}
impl SigType for ClassAttr {
    fn sig_type(&self) -> LitStr {
        LitStr::new(&format!("{}{}", "[".repeat(self.dimensions as usize), self.base_component_class.sig_type().value()), self.span())
    }
}
impl Parse for ClassAttr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if !input.peek(Bracket) {
            return Ok(Self {
                dimensions: 0,
                outer_brackets: None,
                base_component_class: input.parse()?,
            });
        };

        // Parse the brackets (if any) and determine the number of dimensions
        let mut current;
        let outer_brackets = Some(bracketed!(current in input));
        let mut dimensions = 1;
        while current.peek(Bracket) {
            dimensions += 1;
            let next;
            bracketed!(next in current);
            // There should not be any other tokens after the inner array
            if !current.cursor().eof() {
                return Err(current.error("Unexpected tokens"));
            }
            current = next;
        }
        let base_component_class = current.parse()?;

        // There should not be any other tokens after the array
        if !input.cursor().eof() {
            return Err(input.error("Unexpected tokens"));
        }
        
        Ok(Self { dimensions, outer_brackets, base_component_class })
    }
}

/// Converts the value obtained from a *field access* or *getter call* to a *Rust value* using a custom hidden trait of [`ez_jni::FromJValue`]
/// (or [`ez_jni::FromObject`] if **class_attr** is [`Some`]).
/// 
/// Passing a **class** will force the use of `FromObject` instead of `FromJValue`.
/// **class** is *required* if the type is [`JObject`][jni::objects::JObject] or [`JThrowable`][jni::objects::JThrowable].
fn convert_field_value(ty: &syn::Type, class_attr: Option<&ClassAttr>, value: &TokenStream) -> TokenStream {
    match class_attr {
        Some(class) => {
            let class = class.sig_type();
            quote_spanned! {ty.span()=> <#ty as ::ez_jni::utils::FieldFromJValueClass>::field_from_jvalue_with_class(#value, #class, env)? }
        },
        None => quote_spanned! {ty.span()=> <#ty as ::ez_jni::utils::FieldFromJValue>::field_from_jvalue(#value, env)? }
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
fn struct_constructor(fields: &Fields) -> syn::Result<TokenStream> {    
    let mut errors = Vec::new();
    
    // Produce the value that will be assigned for each field
    let values = fields.iter()
        .map(|field| {
            let attr = FieldAttr::get_from_attrs(field)?;
            let field_ty = &field.ty;
            // The type signature of the Java field.
            // Is determined by either the class attribute or the field_ty (in that order).
            let sig_ty = match &attr.class {
                Some(class_attr) => class_attr.sig_type().to_token_stream(),
                // Get the field signature at runtime
                None => quote_spanned! {field_ty.span()=>
                    &<#field_ty as ::ez_jni::utils::FieldFromJValue>::guess_sig(env)
                }
            };

            let get_field = |name: String| {
                convert_field_value(&field_ty, attr.class.as_ref(), &quote_spanned! {field.span()=>
                    ::ez_jni::utils::from_object_get_field(&object, #name, #sig_ty, env)?
                })
            };
            
            Ok(if let Some(name) = attr.name {
                // Use the "name" of the field attribute
                get_field(name.to_string())
            } else if let Some(call) = attr.call {
                // Call the Java gettter method instead of accessing field
                let method = LitStr::new(&call.to_string(), call.span());
                // Transform the obtained field signature to a function signature
                let sig = quote_spanned! {field_ty.span()=> &format!("(){}", #sig_ty) };
                
                convert_field_value(&field_ty, attr.class.as_ref(), &quote_spanned! {method.span()=>
                    ::ez_jni::utils::call_obj_method(&object, #method, #sig, &[], env)
                        .unwrap_or_else(|exception| ::ez_jni::__throw::panic_exception(exception))
                })
            } else if let Some(name) = &field.ident {
                // Get name from Rust field convert it to camelCase for the Java field
                get_field(name.to_string().to_case(Case::Camel))
            } else {
                return Err(syn::Error::new(field.span(), "Field must have \"name\" or \"call\" properties if it is unnamed. See the 'field' attribute."));
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
            let class = take_class_attribute_required(&mut variant.attrs, variant.ident.span())?
                .to_jni_class_path();

            let ident = &variant.ident;
            // Get a constructor for this variant
            let ctor = struct_constructor(&variant.fields)?;
            let _if = if i == 0 { quote!(if) } else { quote!(else if) };
            // Check if Exception is the class that this Variant uses, and construct the variant
            Ok(quote_spanned! {variant.span()=>
                #_if env.is_instance_of(object, #class).unwrap_jni(env) {
                    Ok(Self::#ident #ctor)
                }
            })
        })
}
