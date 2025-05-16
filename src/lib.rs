#![deny(clippy::all)]

#[cfg(test)]
mod tests {
    use diesel::backend::Backend;
    use diesel::deserialize::FromSql;
    use diesel::deserialize::QueryableByName;
    use diesel::row::NamedRow;
    use diesel::sql_query;
    use diesel::AsExpression;
    use diesel::Connection;
    use diesel::FromSqlRow;
    use diesel::Insertable;
    use diesel::Queryable;
    use diesel::RunQueryDsl;
    use diesel::SqliteConnection;
    use diesel_enum_derive::DieselEnum;
    use schema::test_rows;

    mod schema {
        use diesel::table;

        table! {
            test_rows (id) {
                id -> Integer,
                x -> Text,
                y -> Nullable<Text>,
            }
        }
    }

    #[derive(Debug, AsExpression, Eq, PartialEq, FromSqlRow, DieselEnum)]
    #[diesel(sql_type = diesel::sql_types::Text)]
    #[diesel_enum(case = "UpperSnake")]
    enum X {
        ValueA,
        #[diesel_enum(case = "Snake")]
        ValueB,
        #[diesel_enum(name = "VALUEC")]
        ValueC,
    }

    #[derive(Debug, AsExpression, Eq, PartialEq, FromSqlRow, DieselEnum)]
    #[diesel(sql_type = diesel::sql_types::Text)]
    enum Y {
        ValueA,
        ValueB,
        ValueC,
    }

    #[derive(Insertable, Queryable, Eq, PartialEq, Debug)]
    struct TestRow {
        id: i32,
        x: X,
        y: Option<Y>,
    }

    fn create_table() -> anyhow::Result<SqliteConnection> {
        let mut connection = diesel::sqlite::SqliteConnection::establish(":memory:")?;
        diesel::sql_query(
            "CREATE TABLE test_rows (id PRIMARY KEY NOT NULL, x TEXT NOT NULL, y TEXT NULL)",
        )
        .execute(&mut connection)?;
        Ok(connection)
    }

    #[test]
    fn loads_valid_data() -> anyhow::Result<()> {
        let mut connection = create_table()?;
        sql_query(concat!(
            "INSERT INTO test_rows (id, x, y) VALUES ",
            "(1, \"VALUE_A\", \"value_a\"), ",
            "(2, \"value_b\", \"value_b\"), ",
            "(3, \"VALUEC\", NULL)",
        ))
        .execute(&mut connection)?;

        let expected = vec![
            TestRow {
                id: 1,
                x: X::ValueA,
                y: Some(Y::ValueA),
            },
            TestRow {
                id: 2,
                x: X::ValueB,
                y: Some(Y::ValueB),
            },
            TestRow {
                id: 3,
                x: X::ValueC,
                y: None,
            },
        ];

        use schema::test_rows::dsl::*;

        let rows = test_rows.load::<TestRow>(&mut connection)?;

        assert_eq!(expected, rows);
        Ok(())
    }

    #[test]
    fn fails_on_invalid_values() -> anyhow::Result<()> {
        use schema::test_rows::dsl::*;

        let mut connection = create_table()?;
        diesel::sql_query(
            "INSERT INTO test_rows (id, x, y) VALUES (1, \"INVALID_NOT_NULL\", \"value_b\")",
        )
        .execute(&mut connection)?;

        assert!(test_rows.load::<TestRow>(&mut connection).is_err());

        let mut connection = create_table()?;
        diesel::sql_query(
            "INSERT INTO test_rows (id, x, y) VALUES (1, \"value_a\", \"INVALID_NOT_NULL\")",
        )
        .execute(&mut connection)?;

        assert!(test_rows.load::<TestRow>(&mut connection).is_err());

        Ok(())
    }

    #[test]
    fn inserts() -> anyhow::Result<()> {
        use schema::test_rows::dsl::*;

        let mut connection = create_table()?;
        diesel::insert_into(test_rows)
            .values(vec![
                &TestRow {
                    id: 1,
                    x: X::ValueA,
                    y: Some(Y::ValueA),
                },
                &TestRow {
                    id: 2,
                    x: X::ValueB,
                    y: None,
                },
            ])
            .execute(&mut connection)?;

        #[derive(Clone, Debug, PartialEq, Eq)]
        struct Row {
            id: i32,
            x: String,
            y: Option<String>,
        }

        impl<DB> QueryableByName<DB> for Row
        where
            DB: Backend,
            String: FromSql<diesel::sql_types::Text, DB>,
            Option<String>: FromSql<diesel::sql_types::Nullable<diesel::sql_types::Text>, DB>,
            i32: FromSql<diesel::sql_types::Integer, DB>,
        {
            fn build<'a>(row: &impl NamedRow<'a, DB>) -> diesel::deserialize::Result<Self> {
                let id_: i32 = NamedRow::get(row, "id")?;
                let x_: String = NamedRow::get(row, "x")?;
                let y_: Option<String> = NamedRow::get(row, "y")?;
                Ok(Row {
                    id: id_,
                    x: x_,
                    y: y_,
                })
            }
        }

        let expected = vec![
            Row {
                id: 1,
                x: "VALUE_A".to_owned(),
                y: Some("value_a".to_owned()),
            },
            Row {
                id: 2,
                x: "value_b".to_owned(),
                y: None,
            },
        ];

        let results: Vec<Row> =
            sql_query("SELECT id, x, y FROM test_rows").load(&mut connection)?;

        assert_eq!(expected, results);
        Ok(())
    }

    #[derive(Debug, AsExpression, Eq, PartialEq, FromSqlRow, DieselEnum)]
    #[diesel(sql_type = diesel::sql_types::Text)]
    enum Z {
        ValueA,
        #[diesel_enum(unknown)]
        Unknown(String),
    }

    #[derive(Insertable, Queryable, Eq, PartialEq, Debug)]
    #[diesel(table_name = schema::test_rows)]
    struct TestRowZ {
        id: i32,
        x: Z,
        y: Option<Z>,
    }

    #[test]
    fn handles_unknown_values() -> anyhow::Result<()> {
        let mut connection = create_table()?;
        sql_query(concat!(
            "INSERT INTO test_rows (id, x, y) VALUES ",
            "(1, \"UNKNOWN1\", NULL), ",
            "(2, \"value_a\", \"UNKNOWN2\"); ",
        ))
        .execute(&mut connection)?;

        let expected = vec![
            TestRowZ {
                id: 1,
                x: Z::Unknown("UNKNOWN1".to_owned()),
                y: None,
            },
            TestRowZ {
                id: 2,
                x: Z::ValueA,
                y: Some(Z::Unknown("UNKNOWN2".to_owned())),
            },
            TestRowZ {
                id: 3,
                x: Z::Unknown("UNKNOWN3".to_owned()),
                y: Some(Z::Unknown("UNKNOWN4".to_owned())),
            },
        ];

        use schema::test_rows::dsl::*;

        diesel::insert_into(test_rows)
            .values(vec![&TestRowZ {
                id: 3,
                x: Z::Unknown("UNKNOWN3".to_owned()),
                y: Some(Z::Unknown("UNKNOWN4".to_owned())),
            }])
            .execute(&mut connection)?;

        let rows = test_rows.load::<TestRowZ>(&mut connection)?;

        assert_eq!(expected, rows);
        Ok(())
    }
}
