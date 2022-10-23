package io.github.paulpaulych.parser.sql

import io.github.paulpaulych.TestUtils.expectSuccess
import io.github.paulpaulych.parser.sql.Expr.*
import io.github.paulpaulych.parser.sql.JoinType.*
import io.github.paulpaulych.parser.sql.Op2Type.EQ
import io.github.paulpaulych.parser.sql.Op2Type.LT
import io.github.paulpaulych.parser.sql.Source.*
import io.github.paulpaulych.parser.sql.SourceParser.source
import io.kotest.core.spec.style.DescribeSpec

class SourceParserTest : DescribeSpec({

    it("sqlId source parser") {
        expectSuccess(source,
            "table_a" to SqlIdSource(SqlId(schema = null, name = "table_a"), alias = null),
            "some_SCHEMA.table_a a" to SqlIdSource(SqlId(schema = "some_SCHEMA", name = "table_a"), alias = "a"),
            "some_SCHEMA.table_a as a" to SqlIdSource(SqlId(schema = "some_SCHEMA", name = "table_a"), alias = "a"),
        )
    }

    it("cross join source parser") {
        expectSuccess(source,
            "table_a,table_b,table_c" to JoinSource(
                type = CROSS, condition = null,
                lhs = JoinSource(
                    type = CROSS,
                    condition = null,
                    lhs = SqlIdSource(SqlId(null, "table_a"), null),
                    rhs = SqlIdSource(SqlId(null, "table_b"), null),
                ),
                rhs = SqlIdSource(SqlId(schema = null, name = "table_c"), alias = null)
            ),
            "table_a    full  outer JOIN table_b" to JoinSource(
                type = CROSS,
                condition = null,
                lhs = SqlIdSource(SqlId(null, "table_a"), null),
                rhs = SqlIdSource(SqlId(null, "table_b"), null),
            ),
            """
                table_a a  ,table_b
                    cross join table_c c
                    cross join table_d
            """.trimIndent() to JoinSource(
                type = CROSS, condition = null,
                lhs = JoinSource(
                    type = CROSS,
                    condition = null,
                    lhs = JoinSource(
                        type = CROSS,
                        condition = null,
                        lhs = SqlIdSource(SqlId(null, "table_a"), "a"),
                        rhs = SqlIdSource(SqlId(null, "table_b"), null),
                    ),
                    rhs = SqlIdSource(SqlId(null, "table_c"), "c"),
                ),
                rhs = SqlIdSource(SqlId(null, "table_d"), null)
            ),
            """
                schema_a.table_a a , table_b
                    cross join table_c c, schema_d.table_d
            """.trimIndent() to JoinSource(
                type = CROSS, condition = null,
                lhs = JoinSource(
                    type = CROSS, condition = null,
                    lhs = JoinSource(
                        type = CROSS, condition = null,
                        lhs = SqlIdSource(SqlId("schema_a", "table_a"), "a"),
                        rhs = SqlIdSource(SqlId(null, "table_b"), null),
                    ),
                    rhs = SqlIdSource(SqlId(null, "table_c"), "c"),
                ),
                rhs = SqlIdSource(SqlId("schema_d", "table_d"), null)
            ),
            """
                table_a a , (table_b
                    cross join table_c),table_d
            """.trimIndent() to JoinSource(
                type = CROSS, condition = null,
                lhs = JoinSource(
                    type = CROSS, condition = null,
                    lhs = SqlIdSource(SqlId(null, "table_a"), "a"),
                    rhs = JoinSource(
                        type = CROSS, condition = null,
                        lhs = SqlIdSource(SqlId(null, "table_b"), null),
                        rhs = SqlIdSource(SqlId(null, "table_c"), null),
                    ),
                ),
                rhs = SqlIdSource(SqlId(null, "table_d"), null)
            ),
        )
    }

    it("selective joins parsers") {
        expectSuccess(source,
            "table_a inner join table_b on x < 1" to JoinSource(
                type = INNER,
                condition = Op2Expr(LT, ColumnExpr(Column("x", null)), IntExpr(1)),
                lhs = SqlIdSource(SqlId(null, "table_a"), null),
                rhs = SqlIdSource(SqlId(null, "table_b"), null)
            ),
            "table_a join table_b on x < 1" to JoinSource(
                type = INNER,
                condition = Op2Expr(LT, ColumnExpr(Column("x", null)), IntExpr(1)),
                lhs = SqlIdSource(SqlId(null, "table_a"), null),
                rhs = SqlIdSource(SqlId(null, "table_b"), null)
            ),
            "table_a left join table_b on x < 1" to JoinSource(
                type = LEFT,
                condition = Op2Expr(LT, ColumnExpr(Column("x", null)), IntExpr(1)),
                lhs = SqlIdSource(SqlId(null, "table_a"), null),
                rhs = SqlIdSource(SqlId(null, "table_b"), null)
            ),
            "table_a left OUTER join table_b on x < 1" to JoinSource(
                type = LEFT,
                condition = Op2Expr(LT, ColumnExpr(Column("x", null)), IntExpr(1)),
                lhs = SqlIdSource(SqlId(null, "table_a"), null),
                rhs = SqlIdSource(SqlId(null, "table_b"), null)
            ),
            "table_a RIGHT join table_b b1_2 on x < 1" to JoinSource(
                type = RIGHT,
                condition = Op2Expr(LT, ColumnExpr(Column("x", null)), IntExpr(1)),
                lhs = SqlIdSource(SqlId(null, "table_a"), null),
                rhs = SqlIdSource(SqlId(null, "table_b"), "b1_2")
            ),
            "table_a RIGHT outer join table_b b1_2 on x < 1" to JoinSource(
                type = RIGHT,
                condition = Op2Expr(LT, ColumnExpr(Column("x", null)), IntExpr(1)),
                lhs = SqlIdSource(SqlId(null, "table_a"), null),
                rhs = SqlIdSource(SqlId(null, "table_b"), "b1_2")
            ),
        )
    }

    it("source precedence test") {
        expectSuccess(source,
            """
                a a1, (a a2 cross join a a3)
                    join (a a4 left join a a5 on false right OUTER JOIN a a6 on 'a'=1) on (true),
                    (a a7, a a8
                    ), a a9
                    inner join a a10 on (((false)))
            """.trimIndent() to innerJoin(
                BoolExpr(false),
                crossJoin(
                    crossJoin(
                        innerJoin(
                            BoolExpr(true),
                            crossJoin(
                                tableA("a1"),
                                crossJoin(
                                    tableA("a2"),
                                    tableA("a3"),
                                ),
                            ),
                            rightJoin(
                                Op2Expr(EQ, StrExpr("a"), IntExpr(1)),
                                leftJoin(
                                    BoolExpr(false),
                                    tableA("a4"),
                                    tableA("a5")
                                ),
                                tableA("a6")
                            )
                        ),
                        crossJoin(
                            tableA("a7"),
                            tableA("a8")
                        )
                    ),
                    tableA("a9"),
                ),
                tableA("a10")
            )
        )

    }

    it("subQuery source parser") {
        expectSuccess(source,
            "(select * from a) a1" to SubQuerySource(
                query = Query(
                    columns = listOf(
                        SelectedItem(ColumnExpr(Column("*", null)), alias = null)
                    ),
                    source = SqlIdSource(SqlId(null, "a"), null),
                    where = null,
                    groupBy = listOf(),
                    having = null,
                    sorts = listOf(),
                    limit = null,
                    offset = null
                ),
                alias = "a1"
            ),
            "(select * from a) as a1" to SubQuerySource(
                query = Query(
                    columns = listOf(
                        SelectedItem(ColumnExpr(Column("*", null)), alias = null)
                    ),
                    source = SqlIdSource(SqlId(null, "a"), null),
                    where = null,
                    groupBy = listOf(),
                    having = null,
                    sorts = listOf(),
                    limit = null,
                    offset = null
                ),
                alias = "a1"
            ),
        )
    }
})

private fun crossJoin(l: Source, r: Source) = JoinSource(CROSS, null, l, r)
private fun innerJoin(e: Expr, l: Source, r: Source) = JoinSource(INNER, e, l, r)
private fun leftJoin(cond: Expr, l: Source, r: Source) = JoinSource(LEFT, cond, l, r)
private fun rightJoin(e: Expr, l: Source, r: Source) = JoinSource(RIGHT, e, l, r)
private fun tableA(alias: String) = SqlIdSource(SqlId(schema = null, name = "a"), alias = alias)