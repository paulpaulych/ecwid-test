package io.github.paulpaulych.sql

import io.github.paulpaulych.TestUtils.expectFailure
import io.github.paulpaulych.TestUtils.expectSuccess
import io.github.paulpaulych.parser.ErrorItem.ParseError
import io.github.paulpaulych.sql.Expr.*
import io.github.paulpaulych.sql.Expr.LitExpr.*
import io.github.paulpaulych.sql.Expr.SelectableExpr.ColumnExpr
import io.github.paulpaulych.sql.Expr.SelectableExpr.WildcardExpr
import io.github.paulpaulych.sql.Op1Type.*
import io.github.paulpaulych.sql.Op2Type.*
import io.kotest.core.spec.style.DescribeSpec
import io.kotest.matchers.shouldBe

class ExprParserTest: DescribeSpec({

    it("sql literal parser") {
        val parser = ExprParser(wildcardAllowed = false).expr()

        expectSuccess(parser,
            "null" to SqlNullExpr,
            "'abla'" to StrExpr("abla"),
            "''" to StrExpr(""),
            "'abla abla'" to StrExpr("abla abla"),
            "1" to IntExpr(1),
            "12.5" to DoubleExpr(12.5),
            "1.25e2" to DoubleExpr(1.25e2),
            "1.25E2" to DoubleExpr(1.25E2),
            "false" to BoolExpr(false),
            "true" to BoolExpr(true)
        )
    }

    it("selectable expr") {
        expectSuccess(ExprParser(wildcardAllowed = false).expr(),
            "table.col" to ColumnExpr(Column("col", SqlId(null, "table"))),
            "col" to ColumnExpr(Column("col", null)),
            "schema.table.col" to ColumnExpr(Column("col", SqlId("schema", "table"))),
        )

        expectSuccess(ExprParser(wildcardAllowed = true).expr(),
            "table.col" to ColumnExpr(Column("col", SqlId(null, "table"))),
            "table.*" to WildcardExpr(Wildcard("table")),
            "schema.table.*" to WildcardExpr(Wildcard("schema.table")),
        )
    }

    it("parentheses expressions") {
        expectSuccess(ExprParser(wildcardAllowed = true).expr(),
            "(true)" to BoolExpr(true),
            "((true))" to BoolExpr(true),
            "(( (true          )))" to BoolExpr(true)
        )
        expectFailure(ExprParser(wildcardAllowed = true).expr(),
            "(" to {
                error shouldBe ParseError("expression", "expression expected")
            },
        )
    }

    it("unary operator expr") {
        expectSuccess(ExprParser(wildcardAllowed = false).expr(),
            "not false" to Op1Expr(NOT, BoolExpr(false)),
            "NOT ( true)" to Op1Expr(NOT, BoolExpr(true)),
            "NOT( true)" to Op1Expr(NOT, BoolExpr(true)),
            "NOT  \t true" to Op1Expr(NOT, BoolExpr(true)),
            "not( \n\t'a'\t )" to Op1Expr(NOT, StrExpr("a")),

            "-  \t 5" to Op1Expr(UN_MINUS, IntExpr(5)),
            "+( \n\t27.0\t )" to Op1Expr(UN_PLUS, DoubleExpr(27.0)),
        )

        expectFailure(ExprParser(wildcardAllowed = false).expr(),
            "NOT(" to {
//                error shouldBe ParseError("expression", "invalid expression syntax")
            },
            "NOT(false" to {
//                error shouldBe ParseError("')'", "expected ')'")
            },
            "-" to {
//                error shouldBe ScopesTried(listOf("'('", "expression"))
            },
            "-(" to {
//                error shouldBe ParseError("expression", "invalid expression syntax")
            },
        )
    }

    it("binary operator expr") {
        expectSuccess(ExprParser(wildcardAllowed = false).expr(),
            "false OR true" to Op2Expr(OR, BoolExpr(false), BoolExpr(true)),
            "false or true" to Op2Expr(OR, BoolExpr(false), BoolExpr(true)),
            "false and true" to Op2Expr(AND, BoolExpr(false), BoolExpr(true)),
            "false <= true" to Op2Expr(LTE, BoolExpr(false), BoolExpr(true)),
            "5 \t<= \t10" to Op2Expr(LTE, IntExpr(5), IntExpr(10)),
        )
    }

    it("function invocation expression") {
        expectSuccess(ExprParser(wildcardAllowed = false).expr(),
            "blabla(false)" to FunExpr(
                function = SqlId(null, "blabla"),
                args = listOf(BoolExpr(false))
            ),
            "some_func()" to FunExpr(
                function = SqlId(null, "some_func"),
                args = listOf()
            ),
            "schema.blabla(\t false  \n,true,'a')" to FunExpr(
                function = SqlId("schema", "blabla"),
                args = listOf(BoolExpr(false), BoolExpr(true), StrExpr("a"))
            ),
        )
    }

    it("operator priority test") {
        expectSuccess(ExprParser(wildcardAllowed = true).expr(),
            "false or true and false" to Op2Expr(
                OR,
                BoolExpr(false),
                Op2Expr(AND, BoolExpr(true), BoolExpr(false))
            ),
            "(false or true) and false" to Op2Expr(
                AND,
                Op2Expr(OR, BoolExpr(false), BoolExpr(true)),
                BoolExpr(false)
            ),
            "false and true or false" to Op2Expr(
                OR,
                Op2Expr(AND, BoolExpr(false), BoolExpr(true)),
                BoolExpr(false)
            ),
            "false and (true or false)" to Op2Expr(
                AND,
                BoolExpr(false),
                Op2Expr(OR, BoolExpr(true), BoolExpr(false))
            ),
            "true or not false and true" to Op2Expr(
                OR,
                BoolExpr(true),
                Op2Expr(
                    AND,
                    Op1Expr(NOT, BoolExpr(false)),
                    BoolExpr(true)
                )
            ),
            "- 1 and 2" to Op2Expr(
                AND,
                Op1Expr(UN_MINUS, IntExpr(1)),
                IntExpr(2)
            ),
            "not 1 and 2" to Op2Expr(
                AND,
                Op1Expr(NOT, IntExpr(1)),
                IntExpr(2)
            ),
            "- 1 + 2" to Op2Expr(
                PLUS,
                Op1Expr(UN_MINUS, IntExpr(1)),
                IntExpr(2)
            ),
            "true and false and true" to Op2Expr(AND,
                BoolExpr(true),
                Op2Expr(AND, BoolExpr(false), BoolExpr(true))
            ),
            "1 - 2 + 3" to Op2Expr(PLUS,
                Op2Expr(MINUS, IntExpr(1), IntExpr(2)),
                IntExpr(3),
            ),
            "1 + 2 - 3" to Op2Expr(PLUS,
                IntExpr(1),
                Op2Expr(MINUS, IntExpr(2), IntExpr(3)),
            ),
            "1 * 2 * 3" to Op2Expr(MULT,
                IntExpr(1),
                Op2Expr(MULT, IntExpr(2), IntExpr(3)),
            ),
            "1 * 2 / 3 * 4" to Op2Expr(MULT,
                IntExpr(1),
                Op2Expr(DIV,
                    IntExpr(2),
                    Op2Expr(MULT, IntExpr(3), IntExpr(4))
                ),
            ),
            "not not 2" to Op1Expr(NOT, Op1Expr(NOT, IntExpr(2))),
            Pair(
                """
                    true and (not false) or not
                    + table1.a + b * (c - d) <= (a + (-table2.b)) % c
                """.trimIndent(),
                Op2Expr(OR,
                    Op2Expr(AND, BoolExpr(true), Op1Expr(NOT, BoolExpr(false))),
                    Op1Expr(NOT,
                        Op2Expr(LTE,
                            Op2Expr(PLUS,
                                Op1Expr(UN_PLUS, ColumnExpr(Column("a", SqlId(null, "table1")))),
                                Op2Expr(MULT,
                                    ColumnExpr(Column("b", null)),
                                    Op2Expr(MINUS,
                                        ColumnExpr(Column("c", null)),
                                        ColumnExpr(Column("d", null))
                                    )
                                )
                            ),
                            Op2Expr(MOD,
                                Op2Expr(PLUS,
                                    ColumnExpr(Column("a", null)),
                                    Op1Expr(UN_MINUS, ColumnExpr(Column("b", SqlId(null, "table2"))))
                                ),
                                ColumnExpr(Column("c", null))
                            ),
                        )
                    )
                )
            ),
            Pair(
                """
                    true and (not false) or not
                    + table1.a + b * (c - d) <= (a + (-table2.b)) % c
                """.trimIndent(),
                Op2Expr(OR,
                    Op2Expr(AND, BoolExpr(true), Op1Expr(NOT, BoolExpr(false))),
                    Op1Expr(NOT,
                        Op2Expr(LTE,
                            Op2Expr(PLUS,
                                Op1Expr(UN_PLUS, ColumnExpr(Column("a", SqlId(null, "table1")))),
                                Op2Expr(MULT,
                                    ColumnExpr(Column("b", null)),
                                    Op2Expr(MINUS,
                                        ColumnExpr(Column("c", null)),
                                        ColumnExpr(Column("d", null))
                                    )
                                )
                            ),
                            Op2Expr(MOD,
                                Op2Expr(PLUS,
                                    ColumnExpr(Column("a", null)),
                                    Op1Expr(UN_MINUS, ColumnExpr(Column("b", SqlId(null, "table2"))))
                                ),
                                ColumnExpr(Column("c", null))
                            ),
                        )
                    )
                )
            )
        )
    }

    it("spaces between expressions") {
        expectSuccess(ExprParser(wildcardAllowed = false).expr(),
            "(true) and (false)" to Op2Expr(AND, BoolExpr(true), BoolExpr(false)),
            "(true)and(false)" to Op2Expr(AND, BoolExpr(true), BoolExpr(false)),
            "(true)and (false)" to Op2Expr(AND, BoolExpr(true), BoolExpr(false)),
            "(true) and (false)" to Op2Expr(AND, BoolExpr(true), BoolExpr(false)),
            "true and false" to Op2Expr(AND, BoolExpr(true), BoolExpr(false)),
            "trueandfalse" to ColumnExpr(Column("trueandfalse", null)),
            "(1) - (2)" to Op2Expr(MINUS, IntExpr(1), IntExpr(2)),
            "(1)-(2)" to Op2Expr(MINUS, IntExpr(1), IntExpr(2)),
            "(1) -(2)" to Op2Expr(MINUS, IntExpr(1), IntExpr(2)),
            "(1)- (2)" to Op2Expr(MINUS, IntExpr(1), IntExpr(2)),
            "1-2" to Op2Expr(MINUS, IntExpr(1), IntExpr(2)),
            "1 - 2" to Op2Expr(MINUS, IntExpr(1), IntExpr(2)),
            "1 -2" to Op2Expr(MINUS, IntExpr(1), IntExpr(2)),
            "1- 2" to Op2Expr(MINUS, IntExpr(1), IntExpr(2)),
            "not1" to ColumnExpr(Column("not1", null)),
            "not 1" to Op1Expr(NOT, IntExpr(1)),
            "not(1)" to Op1Expr(NOT, IntExpr(1)),
            "not (1)" to Op1Expr(NOT, IntExpr(1)),
            "-1" to Op1Expr(UN_MINUS, IntExpr(1)),
            "- 1" to Op1Expr(UN_MINUS, IntExpr(1)),
            "-(1)" to Op1Expr(UN_MINUS, IntExpr(1)),
            "- (1)" to Op1Expr(UN_MINUS, IntExpr(1)),
            "'' and false" to Op2Expr(AND, StrExpr(""), BoolExpr(false)),
            "truecolumn" to ColumnExpr(Column("truecolumn", null)),
            "not_table.true_column" to ColumnExpr(Column("true_column", SqlId(null, "not_table"))),
            "not_table.true_column-5" to Op2Expr(MINUS, ColumnExpr(Column("true_column", SqlId(null, "not_table"))), IntExpr(5)),
        )

        expectFailure(ExprParser(wildcardAllowed = false).expr(),
            "-1and false" to {
                cause?.cause?.error shouldBe ParseError("word separator", "word separator expected")
                cause?.cause?.state?.offset shouldBe 2
            },
            "-1.0and false" to {
                cause?.cause?.error shouldBe ParseError("word separator", "word separator expected")
                cause?.cause?.state?.offset shouldBe 4
            },
            "''and false" to {
                cause?.cause?.error shouldBe ParseError("word separator", "word separator expected")
                cause?.cause?.state?.offset shouldBe 2
            },
        )
    }
})