//! --
//! A simple `Derive` implementation that allows a simple rust enum to be serialized to / deserialized from a text column in a database via [diesel](https://docs.diesel.rs/2.0.x/diesel/index.html).
//!
//! This really is a simple crate, and was mostly written because there is no concept of enums in SQLite, so Diesel does not ship an implementation of `ToSql`/`FromSql` for enums like they do for things like Postgres. All that this derive macro really does is generate `ToSql<diesel::sql_types::Text>` and `ToSql<diesel::sql_types::Text>` implementations. Note that these are only able to be generated for enums with unit-like values. Any other type of enum value is not supported. Also, any value not in the database will fail deserialization.
//!
//! By default all names are converted to snake case to be stored in the database, but some customization is allowed.
//!
//! - `#[diesel_enum(case="XXX")]` can be added at either the enum level, or on a specific enum value. This changes how the value is serialized/deserialized. By default, this is snake case, but any value from [convert_case::Case](https://docs.rs/convert_case/latest/convert_case/enum.Case.html) can be used. The value should be a string with pascal cased variant that is desired.
//! - `#[diesel_enum(name="YYY")]` can be added to individual values to specify the string they should be serialized as. If this is specified, this is used verbatim. No case conversion is done.
//!
//! # Example
//!
//! ```noexec
//! use diesel_enum::DbEnum;
//!
//! /// Serializes as "bar", "baz", "baz_like"
//! #[derive(DbEnum)]
//! enum Foo {
//!     Bar,
//!     Baz,
//!     BazLike,
//! }
//!
//! /// Serializes as "bar", "baz", "bazLike"
//! #[derive(DbEnum)]
//! #[diesel_enum(case="Camel")]
//! enum Foo {
//!     Bar,
//!     Baz,
//!     BazLike,
//! }
//!
//! /// Serializes as "bar_1", "baz_1", "bazLike"
//! #[derive(DbEnum)]
//! enum Foo {
//!     Bar1,
//!     Baz1,
//!     #[diesel_enum(case="Camel")]
//!     BazLike,
//! }
//!
//! /// Serializes as "bar", "baz", "not_aBaz"
//! #[derive(DbEnum)]
//! enum Foo {
//!     Bar,
//!     Baz,
//!     #[diesel_enum(name="not_aBaz")]
//!     BazLike,
//! }
//! ```

use convert_case::Case;
use convert_case::Casing;
use darling::FromDeriveInput;
use darling::FromVariant;
use quote::quote;
use quote::quote_spanned;
use quote::ToTokens;
use syn::spanned::Spanned;
use syn::Data;
use syn::Variant;

#[derive(FromDeriveInput)]
#[darling(attributes(diesel_enum))]
struct DieselEnumMainOpts {
    case: Option<String>,
}

#[derive(FromVariant)]
#[darling(attributes(diesel_enum))]
struct DieselEnumOpts {
    name: Option<String>,
    case: Option<String>,
    unknown: Option<bool>,
}

impl DieselEnumOpts {
    fn is_unknown(&self) -> bool {
        self.unknown.unwrap_or(false)
    }

    fn parse_attrs(&self, ident: &syn::Ident) -> syn::Result<ValueAttrs> {
        let unknown = self.is_unknown();
        match (&self.name, &self.case, unknown) {
            (None, None, false) => Ok(ValueAttrs::None),
            (Some(n), None, false) => Ok(ValueAttrs::Name(n.as_str())),
            (None, Some(c), false) => Ok(ValueAttrs::Case(c.as_str())),
            (None, None, true) => Ok(ValueAttrs::Unknown),
            (_, _, _) => {
                let message = format!(
                    "Value `{}` may only have one of `name`, `case`, or `unknown`set",
                    ident
                );
                Err(syn::Error::new(ident.span(), message))
            }
        }
    }
}

enum ValueAttrs<'a> {
    None,
    Case(&'a str),
    Name(&'a str),
    Unknown,
}

fn case_from_string(s: &str, span: proc_macro2::Span) -> syn::Result<Case> {
    match s {
        "Upper" => Ok(Case::Upper),
        "Lower" => Ok(Case::Lower),
        "Title" => Ok(Case::Title),
        "Toggle" => Ok(Case::Toggle),
        "Camel" => Ok(Case::Camel),
        "Pascal" => Ok(Case::Pascal),
        "UpperCamel" => Ok(Case::UpperCamel),
        "Snake" => Ok(Case::Snake),
        "UpperSnake" => Ok(Case::UpperSnake),
        "ScreamingSnake" => Ok(Case::Constant),
        "Constant" => Ok(Case::Constant),
        "Kebab" => Ok(Case::Kebab),
        "Cobol" => Ok(Case::Cobol),
        "UpperKebab" => Ok(Case::UpperKebab),
        "Train" => Ok(Case::Train),
        "Flat" => Ok(Case::Flat),
        "UpperFlat" => Ok(Case::UpperFlat),
        "Alternating" => Ok(Case::Alternating),
        e => Err(syn::Error::new(
            span,
            format!("Invalid case conversion `{}` requested", e),
        )),
    }
}

fn render_unknown_variant(
    diesel_crate: &syn::Path,
    e: &syn::DataEnum,
) -> syn::Result<Option<(proc_macro2::TokenStream, proc_macro2::TokenStream)>> {
    let mut found: Option<&Variant> = None;
    for variant in &e.variants {
        let opts = DieselEnumOpts::from_variant(variant)?;
        if let Some(true) = opts.unknown {
            found = match found {
                None if variant.fields.len() == 1 => {
                    let field = variant.fields.iter().next().unwrap();
                    match (&field.ident, &field.ty) {
                        (None, syn::Type::Path(p)) if p.path.is_ident("String") => {
                            Ok(Some(variant))
                        }
                        (None, t) => {
                            let message = format!("'unknown' variants must have a exactly one unnamed `String` value. `{}` has one unnamed field of type `{}`", variant.ident, t.to_token_stream());
                            Err(syn::Error::new(variant.span(), message))
                        }
                        (Some(ident), _) => {
                            let message = format!("'unknown' variants must have a exactly one unnamed `String` value. `{}` has one field named `{}`", variant.ident, ident);
                            Err(syn::Error::new(variant.span(), message))
                        }
                    }
                }
                None => {
                    let message = format!("'unknown' variants must have a exactly one unnamed `String` value. `{}` had {} values", variant.ident, variant.fields.len());
                    Err(syn::Error::new(variant.span(), message))
                }
                Some(v) => {
                    let message = format!("'unknown' was specified on both `{}` and `{}`. It may only be present on one variant.", v.ident, variant.ident);
                    Err(syn::Error::new(variant.span(), message))
                }
            }?;
        }
    }
    Ok(found.map(|v| {
        let span = v.span();
        let ident = v.ident.clone();
        let to_sql = quote_spanned! {span=> Self::#ident(v) => <String as #diesel_crate::serialize::ToSql<#diesel_crate::sql_types::Text, DB>>::to_sql(v, out) };
        // "s" is the string from the parent context. Let's not clone an object we already have.
        let from_sql = quote_spanned! {span=> _ => Ok(Self::#ident(s)) };
        (to_sql, from_sql)
    }))
}

fn render_variant(
    default_casing: Case,
    variant: Variant,
) -> syn::Result<Option<(proc_macro2::TokenStream, proc_macro2::TokenStream)>> {
    let opts = DieselEnumOpts::from_variant(&variant)?;
    if !variant.fields.is_empty() && !opts.is_unknown() {
        let message = format!(
            "Found {} fields on `{}`. Variant cannot have any fields",
            variant.fields.len(),
            variant.ident
        );
        return Err(syn::Error::new(variant.span(), message));
    }

    let span = variant.span();

    let name_str = match opts.parse_attrs(&variant.ident)? {
        ValueAttrs::Name(n) => n.to_owned(),
        ValueAttrs::Case(c) => variant
            .ident
            .to_string()
            .to_case(case_from_string(c, span)?),
        _ => variant.ident.to_string().to_case(default_casing),
    };

    if opts.is_unknown() {
        Ok(None)
    } else {
        let ident = variant.ident;
        let to_sql = quote_spanned! {span=> Self::#ident => #name_str.to_sql(out) };
        let from_sql = quote_spanned! {span=> #name_str => Ok(Self::#ident) };
        Ok(Some((to_sql, from_sql)))
    }
}

fn impl_diesel_enum(ast: syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let diesel_crate: syn::Path = syn::parse_str("diesel")?;
    let opts = DieselEnumMainOpts::from_derive_input(&ast)?;
    let name = &ast.ident;
    let default_casing = match opts.case.as_ref() {
        Some(case_str) => case_from_string(case_str, ast.ident.span())?,
        None => Case::Snake,
    };

    let (to_sqls, from_sqls): (Vec<_>, Vec<_>) = match ast.data {
        Data::Enum(e) => {
            let unknown = render_unknown_variant(&diesel_crate, &e)?;
            let mut res = e
                .variants
                .into_iter()
                .filter_map(|variant| match render_variant(default_casing, variant) {
                    Ok(Some(v)) => Some(Ok(v)),
                    Err(e) => Some(Err(e)),
                    Ok(None) => None,
                })
                .collect::<syn::Result<Vec<_>>>()?;
            if let Some(u) = unknown {
                res.push(u);
            }
            res.into_iter().unzip()
        }
        Data::Struct(_) | Data::Union(_) => {
            return Err(syn::Error::new(
                ast.span(),
                "DeriveEnum can only be applied to enums",
            ));
        }
    };

    let to_impl = quote! {
        impl<DB> #diesel_crate::serialize::ToSql<#diesel_crate::sql_types::Text, DB> for #name
            where
                DB: #diesel_crate::backend::Backend,
                str: #diesel_crate::serialize::ToSql<#diesel_crate::sql_types::Text, DB> {
            fn to_sql<'b>(&'b self, out: &mut #diesel_crate::serialize::Output<'b, '_, DB>) -> #diesel_crate::serialize::Result {
                match self {
                    #(#to_sqls),*
                }
            }
        }
    };

    let from_impl = quote! {
        impl<DB> #diesel_crate::deserialize::FromSql<diesel::sql_types::Text, DB> for #name
            where
                DB: #diesel_crate::backend::Backend,
                String: #diesel_crate::deserialize::FromSql<diesel::sql_types::Text, DB> {
        fn from_sql(bytes: #diesel_crate::backend::RawValue<DB>) -> #diesel_crate::deserialize::Result<Self> {
                String::from_sql(bytes).and_then(|s| {
                    match s.as_str() {
                        #(#from_sqls),*,
                       _ => Err(format!("Could not convert {} into an enum", &s).into())
                    }
                })
            }

        }
    };
    let gen = quote! {
        #to_impl
        #from_impl
    };
    Ok(gen)
}

#[proc_macro_derive(DieselEnum, attributes(diesel_enum))]
pub fn db_enum_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = syn::parse(input).unwrap();

    match impl_diesel_enum(ast) {
        Ok(r) => r,
        Err(e) => e.into_compile_error(),
    }
    .into()
}
