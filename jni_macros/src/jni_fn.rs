use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned, ToTokens, TokenStreamExt};
use syn::{ext::IdentExt as _, parenthesized, parse::{Parse, ParseStream}, punctuated::Punctuated, token::Paren, Attribute, Block, Expr, GenericParam, Generics, Ident, LifetimeParam, LitStr, Stmt, Token};
use crate::{
    types::{Class, InnerType, JavaPrimitive, SigType, SpecialCaseConversion as _, Type}, utils::{gen_signature, merge_errors, take_class_attribute_required, Spanned}
};

/// Processes the input for [`crate::jni_fns`].
/// Converts the parsed [`JniFn`] to a regular function used in Rust.
pub fn jni_fn(input: ParseStream) -> syn::Result<Vec<TokenStream>> {
    let mut inputs = Vec::new();

    // Parse multiple jni_fn
    while !input.is_empty() {
        inputs.push(input.parse::<JniFn>()?.to_token_stream());
    }

    // Convert all JniFn to ItemFn 
    Ok(inputs)
}

// TODO: Allow generics in the arguments and return type if they are a Java Class

/// A rust function that uses Java types (or some Rust types) and is called by Java Code.
/// 
/// A [`JniFn`] is one that MUST be exported by the user library.
pub struct JniFn {
    pub attrs: Vec<Attribute>,
    /// The Java Class the function is a method of.
    /// This takes the form of an attribute that is removed after parsing.
    class: Class,
    pub pub_token: Token![pub],
    pub fn_token: Token![fn],
    pub name: Ident,
    pub lt_token: Token![<],
    pub lifetime: LifetimeParam,
    pub gt_token: Token![>],
    pub paren_token: Paren,
    pub inputs: Punctuated<JniFnArg, Token![,]>,
    // pub variadic: Option<Variadic>, Should this be allowed?
    pub output: JniReturn,
    pub body: Block,
}
impl Parse for JniFn {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        static LIFETIME_ERROR: &str = "jni_fn must have one and only one lifetime, named \"local\"";

        // -- Parse input function. Collect multiple errors
        let mut errors = Vec::new();

        let attrs = input.call(Attribute::parse_outer)
            .map_err(|err| errors.push(err))
            .ok();

        // Parse `pub`
        let pub_token = input.parse::<Token![pub]>()
            .map_err(|err| errors.push(
                syn::Error::new(err.span(), format!("{err}; jn_fn must have `pub` because they must be exported by the library"))
            ))
            .ok();

        // Parse `fn`
        let fn_token = input.parse::<Token![fn]>()
            .map_err(|err| errors.push(err))
            .ok();

        // Parse function name
        let name = input.parse::<Ident>()
            .map_err(|err| errors.push(syn::Error::new(err.span(), format!("{err}; Expected function name"))))
            .ok();

        // Parse all generics, but will only accept one lifetime
        let generics = input.parse::<Generics>()
            .map_err(|err| errors.push(err))
            .ok()
            .and_then(|generics| {
                let lt_token = match generics.lt_token {
                    Some(lt_token) => Some(lt_token),
                    None => {
                        errors.push(syn::Error::new(Span::call_site(), format!("Must have a generic lifetime")));
                        None
                    },
                }?;
                Some((lt_token, generics.params, generics.gt_token?))
            });

        // Parse arguments with Java types
        let (paren_token, inputs) = {
            let inner;
            (
                parenthesized!(inner in input),
                Punctuated::<JniFnArg, Token![,]>::parse_terminated(&inner)
                    .map_err(|err| errors.push(err))
                    .ok()
            )
        };

        // Parse return arrow `->` and return type (is void if there is none)
        let output = input.parse::<JniReturn>()
            .map_err(|err| errors.push(err))
            .ok();

        // Parse the content of the function inside the braces `{ ... }`
        let body = input.parse()?;

        merge_errors(errors)?;
        // If there were any errors, the function returned on the line above, so reaching here means all tokens were parsed successfully.
        let mut attrs = attrs.unwrap();
        let pub_token = pub_token.unwrap();
        let fn_token = fn_token.unwrap();
        let name = name.unwrap();
        let (lt_token, generics, gt_token) = generics.unwrap();
        let inputs = inputs.unwrap();
        let output = output.unwrap();

        // -- Perform checks on the successfully parsed function
        errors = Vec::new();

        // jni_fn must have a `class` attribute
        let class = take_class_attribute_required(&mut attrs, name.span())
            .map_err(|err| errors.push(err))
            .ok();

        let mut iter = generics.into_iter();

        // Parsed all generics, but will only accept one lifetime, and no other generic types
        let lifetime = match iter.next() {
            Some(GenericParam::Lifetime(lifetime)) =>
                if lifetime.lifetime.ident.to_string() != "local" {
                    errors.push(syn::Error::new(lifetime.span(), LIFETIME_ERROR));
                    None
                } else {
                    Some(lifetime)
                },
            Some(generic) => {
                errors.push(syn::Error::new(generic.span(), "jni_fn can't have generic constants or types"));
                None
            },
            None => {
                errors.push(syn::Error::new(name.span(), LIFETIME_ERROR));
                None
            },
        };

        // Only allow 1 lifetime
        if let Some(lifetime) = iter.next() {
            errors.push(syn::Error::new(lifetime.span(), LIFETIME_ERROR))
        }

        // Check that the function doesn't have arguments named "env" or "_class" because those are implicitly added
        for arg_name in ["env", "_class"] {
            if let Some(arg) = inputs.iter().find(|arg| arg.name.to_string() == arg_name) {
                errors.push(syn::Error::new(
                    arg.span(),
                    format!("Function can't have an argument named \"{arg_name}\"; this is added implicitly")
                ))
            }
        }

        merge_errors(errors)?;

        let class = class.unwrap();
        let lifetime = lifetime.unwrap();

        Ok(Self { attrs, class, pub_token, fn_token, name, lt_token, lifetime, gt_token, paren_token, inputs, output, body })
    }
}
impl ToTokens for JniFn {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        #[allow(unused_variables)] let a = 0;
        // The real name of the function as it will be used by Java
        let name = {
            let class = self.class.to_string()
                .replace('.', "_");
            let name = self.name.to_string().replace('_', "_1");
    
            Ident::new(&format!("Java_{class}_{name}"), self.name.span())
        };

        // Build a java method signature, something like (Ljava.lang.String;)I
        let method_sig = gen_signature(self.inputs.iter().map(|i| &i.ty), &self.output).value();

        tokens.append_all(&self.attrs);
        tokens.append_all(quote!(#[doc = ""]));
        tokens.append_all(quote!(#[doc = #method_sig]));
        tokens.append_all(quote!(#[no_mangle]));
        self.pub_token.to_tokens(tokens);
        tokens.append_all(quote!(extern "system"));
        self.fn_token.to_tokens(tokens);
        name.to_tokens(tokens);
        self.lt_token.to_tokens(tokens);
        self.lifetime.to_tokens(tokens);
        self.gt_token.to_tokens(tokens);
        self.paren_token.surround(tokens, |tokens| {
            tokens.append_all(quote_spanned!(self.paren_token.span.span()=> mut env: ::jni::JNIEnv<'local>, _class: ::jni::objects::JClass<'local>,));
            self.inputs.to_tokens(tokens);
        });
        self.output.to_tokens(tokens);
        self.body.brace_token.surround(tokens, |tokens| {
            let mut body = TokenStream::new();
            self.body.brace_token.surround(&mut body, |tokens| {
                // Create local variables that hold the converted values of the arguments
                for arg in &self.inputs {
                    tokens.append_all(arg.create_rust_val());
                }
                // Add the satements to the closure's body
                self.body.stmts.iter()
                    .map(|stmt| modified_stmt(stmt.clone(), &self.output))
                    .for_each(|stmt| stmt.to_tokens(tokens));
            });

            tokens.append_all(quote! {
                ::ez_jni::__throw::catch_throw(&mut env, move |#[allow(unused_variables)] env| #body)
            });
        });
    }
}

pub struct JniFnArg {
    pub attrs: Vec<Attribute>,
    pub mutability: Option<Token![mut]>,
    pub name: Ident,
    pub colon_token: Token![:],
    pub ty: Type
}
impl JniFnArg {
    /// Outputs a statement (*local variable definition*) that converts the argument to a Rust value.
    /// 
    /// The argument is shadowed by the local variable with the same name.
    pub fn create_rust_val(&self) -> TokenStream {
        let attrs = &self.attrs;
        let mutability = &self.mutability;
        let name = &self.name;
        // Output nothing if parameter name is ellided
        if name.to_string() == "_" {
            return TokenStream::new()
        }
        self.ty.convert_java_to_rust(&quote!(#name))
            .map(|conversion| quote! { #(#attrs)* let #mutability #name = #conversion; })
            // Output nothing if there is no conversion to be done.
            .unwrap_or_default()
    }
}
impl Parse for JniFnArg {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            attrs: Attribute::parse_outer(input)?,
            name: input.call(Ident::parse_any)?,
            mutability: input.parse()?,
            colon_token: input.parse()?,
            ty: input.parse()?
        })
    }
}
impl ToTokens for JniFnArg {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append_all(&self.attrs);
        self.mutability.to_tokens(tokens);
        self.name.to_tokens(tokens);
        self.colon_token.to_tokens(tokens);
        
        // Convert Primitive Type to Java Primitive and Class to JObject (or JString)
        fn primitive(j_prim: JavaPrimitive, span: Span) -> TokenStream {
            let j_type = Ident::new(&format!("j{j_prim}"), span);
            quote_spanned! {span=> ::jni::sys::#j_type}
        }
        tokens.append_all(match &self.ty {
            Type::Assertive(InnerType::RustPrimitive { ident, ty }) => primitive(JavaPrimitive::from(*ty), ident.span()),
            Type::Assertive(InnerType::JavaPrimitive { ident, ty }) => primitive(*ty, ident.span()),
            Type::Assertive(InnerType::Object(class)) if class.to_string() == "java.lang.String" => quote_spanned! {class.span()=> ::jni::objects::JString<'local>},
            Type::Assertive(InnerType::Object(_))
            | Type::Array(_)
            | Type::Option { .. } => quote_spanned! {self.ty.span()=> ::jni::objects::JObject<'local>},
        })
    }
}

pub enum JniReturn {
    Void,
    Type {
        arrow_token: Token![->],
        ty: Type
    }
}
impl JniReturn {
    pub fn convert_rust_to_java(&self, value: &TokenStream) -> Option<TokenStream> {
        match self {
            Self::Void => None,
            Self::Type { ty, .. } => ty.convert_rust_to_java(value)
                // If Type is a JObject, convert it to *jobject
                .map(|conversion| match ty {
                    Type::Assertive(InnerType::RustPrimitive { .. } | InnerType::JavaPrimitive { .. }) => conversion,
                    Type::Assertive(InnerType::Object(_))
                    | Type::Array(_)
                    | Type::Option { .. } => quote_spanned!(conversion.span()=> #conversion.as_raw())
                }),
        }
    }
}
impl SigType for JniReturn {
    fn sig_char(&self) -> Ident {
        match self {
            Self::Void => Ident::new("v", Span::call_site()),
            Self::Type { ty, .. } => ty.sig_char()
        }
    }
    fn sig_type(&self) -> LitStr {
        match self {
            Self::Void => LitStr::new("V", Span::call_site()),
            Self::Type { ty, .. } => ty.sig_type()
        }
    }
}
impl Parse for JniReturn {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // Parse return arrow `->` and return type (is void if there is none)
        Ok(if let Ok(arrow_token) = input.parse::<Token![->]>() {
            Self::Type { arrow_token, ty: input.parse()? }
        } else {
            Self::Void
        })
    }
}
impl ToTokens for JniReturn {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Void => { }, // Void return has no tokens
            Self::Type { arrow_token, ty: output } => {
                arrow_token.to_tokens(tokens);
                // Convert Primitive Type to Java Primitive and Class to *jobject (or *jstring)
                fn primitive(j_prim: JavaPrimitive, span: Span) -> TokenStream {
                    let j_type = Ident::new(&format!("j{j_prim}"), span);
                    quote_spanned! {span=> ::jni::sys::#j_type}
                }
                tokens.append_all(match output {
                    Type::Assertive(InnerType::RustPrimitive { ident, ty }) => primitive(JavaPrimitive::from(*ty), ident.span()),
                    Type::Assertive(InnerType::JavaPrimitive { ident, ty }) => primitive(*ty, ident.span()),
                    Type::Assertive(InnerType::Object(class)) if class.to_string() == "java.lang.String" => quote_spanned! {class.span()=> ::jni::sys::jstring},
                    Type::Assertive(InnerType::Object(_))
                    | Type::Array(_)
                    | Type::Option { .. } => quote_spanned! {output.span()=> ::jni::sys::jobject},
                });
            },
        }
    }
}

/// Modifies a [`Statement`][Stmt] in a Rust function's body so that if it contains any *return statements*,
/// the value being returned will be converted to a *Java value*.
fn modified_stmt(mut stmt: Stmt, ty: &JniReturn) -> Stmt {
    /// Modify an expression to return a Java value instead of a Rust value.
    /// The conversion is derived form the **type** provided.
    fn convert_expr(expr: &mut Expr, ty: &JniReturn) {
        ty.convert_rust_to_java(&quote_spanned! {expr.span()=> (#expr)})
            .map(|conversion| *expr = Expr::Verbatim(conversion));
    }
    /// Finds a **return statement** in the [`Expr`] and converts it to a *Java value* if necessary.
    /// The conversion is derived form the **type** provided.
    /// 
    /// This functions is recursive!!
    fn find_expr_to_convert(expr: &mut Expr, ty: &JniReturn) {
        fn find_in_block(block: &mut Block, ty: &JniReturn) {
            for stmt  in &mut block.stmts {
                match stmt {
                    Stmt::Local(syn::Local { init: Some(syn::LocalInit { expr, diverge, .. }), .. }) => {
                        find_expr_to_convert(expr.as_mut(), ty);
                        if let Some((_, expr)) = diverge {
                            find_expr_to_convert(expr.as_mut(), ty);
                        }
                    },
                    // An expression that itself is not returned, but may have returns within it.
                    Stmt::Expr(expr, Some(_))
                    | Stmt::Expr(expr, None) => find_expr_to_convert(expr, ty),
                    _ => {},
                }
            }
        }

        match expr {
            // Expressions with a single inner expression
            Expr::Await(syn::ExprAwait { base: expr, .. })
            | Expr::Break(syn::ExprBreak { expr: Some(expr), .. })
            | Expr::Cast(syn::ExprCast { expr, .. })
            | Expr::Field(syn::ExprField { base: expr, .. })
            | Expr::Group(syn::ExprGroup { expr, .. })
            | Expr::Let(syn::ExprLet { expr, .. })
            | Expr::Paren(syn::ExprParen { expr, .. })
            | Expr::Reference(syn::ExprReference { expr, .. })
            | Expr::Try(syn::ExprTry { expr, .. })
            | Expr::Unary(syn::ExprUnary { expr, .. }) => find_expr_to_convert(expr, ty),
            // Expressions with blocks { }
            Expr::Async(syn::ExprAsync { block, .. })
            | Expr::Block(syn::ExprBlock { block, .. })
            | Expr::Loop(syn::ExprLoop { body: block, .. })
            | Expr::TryBlock(syn::ExprTryBlock { block, .. })
            | Expr::Unsafe(syn::ExprUnsafe { block, .. }) => find_in_block(block, ty),
            Expr::ForLoop(syn::ExprForLoop { expr, body: block, .. })
            | Expr::While(syn::ExprWhile { cond: expr, body: block, .. }) => {
                find_expr_to_convert(expr, ty);
                find_in_block(block, ty);
            },
            // Expressions with more than one inner expressions
            Expr::Assign(syn::ExprAssign { left, right, .. })
            | Expr::Binary(syn::ExprBinary { left, right, ..})
            | Expr::Index(syn::ExprIndex { expr: left, index: right, .. })
            | Expr::Repeat(syn::ExprRepeat { expr: left, len: right, .. }) => {
                find_expr_to_convert(left, ty);
                find_expr_to_convert(right, ty);
            },
            Expr::Range(syn::ExprRange { start, end, .. }) => {
                if let Some(expr) = start {
                    find_expr_to_convert(expr, ty);
                }
                if let Some(expr) = end {
                    find_expr_to_convert(expr, ty);
                }
            },
            Expr::If(syn::ExprIf { cond, then_branch, else_branch, .. }) => {
                find_expr_to_convert(cond, ty);
                find_in_block(then_branch, ty);
                if let Some((_, expr)) = else_branch {
                    find_expr_to_convert(expr, ty);
                }
            },
            // Expressions with a list of inner expressions
            Expr::Array(syn::ExprArray { elems, .. })
            | Expr::Tuple(syn::ExprTuple { elems, .. })
                => elems.iter_mut()
                    .for_each(|expr| find_expr_to_convert(expr, ty)),
            Expr::Call(syn::ExprCall { func: expr, args, .. })
            | Expr::MethodCall(syn::ExprMethodCall { receiver: expr, args, .. }) => {
                find_expr_to_convert(expr, ty);
                args.iter_mut()
                    .for_each(|expr| find_expr_to_convert(expr, ty));
            },
            Expr::Match(syn::ExprMatch { expr, arms, .. }) => {
                find_expr_to_convert(expr, ty);
                for arm in arms {
                    if let Some((_, expr)) = &mut arm.guard {
                        find_expr_to_convert(expr, ty);
                    }
                    find_expr_to_convert(&mut arm.body, ty);
                }
            },
            Expr::Struct(syn::ExprStruct { fields, rest, .. }) => {
                for field in fields {
                    find_expr_to_convert(&mut field.expr, ty)
                }
                if let Some(expr) = rest {
                    find_expr_to_convert(expr, ty);
                }
            }
            // Expressions that are ignored
            Expr::Closure(syn::ExprClosure { .. })
            | Expr::Const(syn::ExprConst { .. })
            | Expr::Continue(syn::ExprContinue { .. })
            | Expr::Infer(syn::ExprInfer { .. })
            | Expr::Lit(syn::ExprLit { .. })
            | Expr::Macro(syn::ExprMacro { .. })
            | Expr::Path(syn::ExprPath { .. }) => { },
            // Base cases
            Expr::Return(syn::ExprReturn { expr, ..})
            | Expr::Yield(syn::ExprYield { expr, .. }) =>
                if let Some(expr) = expr {
                    convert_expr(expr, ty)
                },
            Expr::Verbatim(_) => { },
            _ => {}
        }
    }

    match &mut stmt {
        Stmt::Local(syn::Local { init: Some(syn::LocalInit { expr, diverge, .. }), .. }) => {
            find_expr_to_convert(expr, ty);
            if let Some((_, expr)) = diverge {
                find_expr_to_convert(expr, ty);
            }
        },
        // An expression that itself is not returned, but may have returns within it.
        Stmt::Expr(expr, Some(_)) => find_expr_to_convert(expr, ty),
        // An expression at the end of the body, which is returned, but may have other returns within it
        Stmt::Expr(expr, None) => {
            find_expr_to_convert(expr, ty);
            convert_expr(expr, ty);
        },
        _ => {},
    }

    stmt
}
