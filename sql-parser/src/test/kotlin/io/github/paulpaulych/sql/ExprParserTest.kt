package io.github.paulpaulych.sql

import io.github.paulpaulych.TestUtils.expectFailure
import io.github.paulpaulych.TestUtils.expectSuccess
import io.github.paulpaulych.parser.ErrorItem.ParseError
import io.github.paulpaulych.sql.Expr.LitExpr.*
import io.github.paulpaulych.sql.Expr.SelectableExpr.ColumnExpr
import io.github.paulpaulych.sql.Expr.SelectableExpr.WildcardExpr
import io.github.paulpaulych.sql.SqlScopes.SELECTABLE_EXPR
import io.kotest.core.spec.style.DescribeSpec
import io.kotest.matchers.shouldBe

class ExprParserTest: DescribeSpec({

    /*
        TODO: wildcard allowed flag
         columns: true
         where: false
         having: true
         join: false
     */

    //language=sql
    val sqlId = listOf(
        "some_table_name",
        "soMe_function_name",
        "some_schema.some_table_name",
        "some_schema.some_function_name",
    )

    //language=sql
    val logical = listOf(
        "not(false)",
        "not('1')",

        "false and true",
        "(false or true) or not(false)",
    )

    //language=sql
    val comparison = listOf(
        "a = b",
        "a != b",
        "a > b",
        "a >= b",
        "a < b",
        "a <= b",
    )

    val funExpr = listOf(
        "some_fun(id, table_a.id, false or true, 'some_string', 1,)",
        "some_fun()",
        "some_schema.some_fun(id, id)",
        "some_schema.some_fun()"
    )

    val queryExpr = listOf(
        "(select * from table_a)"
    )

    //language=sql
    val invalid = listOf(
        "",
    )

    it("sql literal parser") {
        val parser = ExprParser(wildcardAllowed = false).litExpr()

        expectSuccess(parser,
            "null" to SqlNullExpr,
            "'abla'" to StrLitExpr("abla"),
            "'abla abla'" to StrLitExpr("abla abla"),
            "-1" to IntLitExpr(-1),
            "12.5" to DoubleLitExpr(12.5),
            "-1.25e2" to DoubleLitExpr(-1.25e2),
            "+1.25E2" to DoubleLitExpr(+1.25E2),
            "false" to BoolLitExpr(false),
            "true" to BoolLitExpr(true)
        )
    }

    it("selectable expr") {
        expectSuccess(ExprParser(wildcardAllowed = false).selectableExpr(),
            "table.col" to ColumnExpr(Column("col", "table")),
            "col" to ColumnExpr(Column("col", null)),
            "schema.table.col" to ColumnExpr(Column("col", "schema.table")),
        )

        expectSuccess(ExprParser(wildcardAllowed = true).selectableExpr(),
            "table.col" to ColumnExpr(Column("col", "table")),
            "table.*" to WildcardExpr(Wildcard("table")),
            "schema.table.*" to WildcardExpr(Wildcard("schema.table")),
        )

        expectFailure(ExprParser(wildcardAllowed = false).selectableExpr(),
            "" to {
                error shouldBe ParseError(SELECTABLE_EXPR.get, "expected column")
            },
        )

        expectFailure(ExprParser(wildcardAllowed = true).selectableExpr(),
            "" to {
                error shouldBe ParseError(SELECTABLE_EXPR.get, "expected column or wildcard")
            },
        )
    }
})