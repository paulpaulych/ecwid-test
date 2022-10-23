package io.github.paulpaulych.sql

import io.github.paulpaulych.TestUtils.expectFailure
import io.github.paulpaulych.TestUtils.expectSuccess
import io.github.paulpaulych.parser.ErrorItem.ParseError
import io.kotest.core.spec.style.DescribeSpec
import io.kotest.matchers.shouldBe

internal class CommonSqlParsersTest: DescribeSpec({

    it("column parser") {
        expectSuccess(CommonSqlParsers.column,
            "col10A" to Column("col10A", null),
            "source.col" to Column(name = "col", source = SqlId(null, "source")),
            "schema.table.col" to Column(name = "col", source = SqlId("schema", "table")),
            "some_col_name" to Column("some_col_name", null),
            "source_alias.some_col_name" to Column("some_col_name", SqlId(null, "source_alias")),
            "*" to Column("*", null),
            "source.*" to Column(name = "*", source = SqlId(null, "source")),
            "schema.table.*" to Column(name = "*", source = SqlId("schema", "table")),
        )

        expectFailure(CommonSqlParsers.column,
            "" to {
                error shouldBe ParseError("column", "invalid column syntax")
            },
            "123" to {
                error shouldBe ParseError("column", "invalid column syntax")
            },
        )
    }

    it("columns by comma parser") {
        expectSuccess(CommonSqlParsers.columnsByComma,
            "table_a.col_a,schema_b.table_b.col_b,col_c" to listOf(
                Column("col_a", source = SqlId(null, "table_a")),
                Column("col_b", source = SqlId("schema_b", "table_b")),
                Column("col_c", source = null),
            ),
            "table_a.col_a   \n\t,   schema_b.table_b.col_b        ,col_c" to listOf(
                Column("col_a", source = SqlId(null, "table_a")),
                Column("col_b", source = SqlId("schema_b", "table_b")),
                Column("col_c", source = null),
            ),
        )

        val err = ParseError("columns sep by comma", "columns sep by comma expected")
        expectFailure(CommonSqlParsers.columnsByComma,
            "" to { error shouldBe err },
            "," to { error shouldBe err },
            "  " to { error shouldBe err },
            ")" to { error shouldBe err },
        )
    }
})