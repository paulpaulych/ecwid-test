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
            "source.col" to Column(name = "col", source = "source"),
            "schema.table.col" to Column(name = "col", source = "schema.table"),
            "some_col_name" to Column("some_col_name", null),
            "source_alias.some_col_name" to Column("some_col_name", "source_alias"),
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

    it("wildcard parser") {
        expectSuccess(CommonSqlParsers.wildcard,
            "*" to Wildcard(null),
            "source.*" to Wildcard("source"),
            "schema.table.*" to Wildcard("schema.table"),
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
})