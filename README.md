# diesel_enum 

A simple `Derive` implementation that allows a simple rust enum to be serialized to / deserialized from a text column in a database via [diesel](https://docs.diesel.rs/2.2.x/diesel/index.html). 

This really is a simple crate, and was mostly written because there is no concept of enums in SQLite, so Diesel does not ship an implementation of `ToSql`/`FromSql` for enums like they do for things like Postgres. All that this derive macro really does is generate `ToSql<diesel::sql_types::Text>` and `ToSql<diesel::sql_types::Text>` implementations. Note that these are only able to be generated for enums with unit-like values. Any other type of enum value is not supported. Also, any value not in the database will fail deserialization.

By default all names are converted to snake case to be stored in the database, but some customization is allowed.

- `#[diesel_enum(case="XXX")]` can be added at either the enum level, or on a specific enum value. This changes how the value is serialized/deserialized. By default, this is snake case, but any value from [convert_case::Case](https://docs.rs/convert_case/latest/convert_case/enum.Case.html) can be used. The value should be a string with camel cased variant that is desired.
- `#[diesel_enum(name="YYY")]` can be added to individual values to specify the string they should be serialized as. If this is specified, this is used verbatim. No case conversion is done.
- `#[diesel_enum(unknown)]` can be added to at most one enum member to marshall and unmarshall unknown values in the database. This member must have one unnamed [`String`] field. An error will occur if this specified twice in a single enum

# Example

```rust
use diesel_enum::DieselEnum;

/// Serializes as "bar", "baz", "baz_like"
#[derive(DieselEnum)]
enum Foo {
    Bar,
    Baz,
    BazLike,
}

/// Serializes as "bar", "baz", "bazLike"
#[derive(DieselEnum)]
#[diesel_enum(case="Camel")]
enum Foo {
    Bar,
    Baz,
    BazLike,
}

/// Serializes as "bar_1", "baz_1", "bazLike"
#[derive(DieselEnum)]
enum Foo {
    Bar1,
    Baz1,
    #[diesel_enum(case="Camel")]
    BazLike,
}

/// Serializes as "bar", "baz", "not_aBaz"
#[derive(DieselEnum)]
enum Foo {
    Bar,
    Baz,
    #[diesel_enum(name="not_aBaz")]
    BazLike,
}

/// Serializes as "bar", "baz", or any other value if it's placed in `Unknown`
/// Values in the database that are not "bar" or "baz" are deserialized without modification into `Unknown` 
#[derive(DieselEnum)]
enum Foo {
    Bar,
    Baz,
    #[diesel_enum(unknown)]
    Unknown(String),
}
```
