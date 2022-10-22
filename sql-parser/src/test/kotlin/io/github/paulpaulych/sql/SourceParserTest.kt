package io.github.paulpaulych.sql

import io.github.paulpaulych.TestUtils.expectSuccess
import io.github.paulpaulych.sql.Expr.LitExpr.IntExpr
import io.github.paulpaulych.sql.Expr.Op2Expr
import io.github.paulpaulych.sql.Expr.SelectableExpr.ColumnExpr
import io.github.paulpaulych.sql.JoinType.CROSS
import io.github.paulpaulych.sql.JoinType.INNER
import io.github.paulpaulych.sql.Op2Type.LT
import io.github.paulpaulych.sql.Source.JoinSource
import io.github.paulpaulych.sql.Source.SqlIdSource
import io.kotest.core.spec.style.DescribeSpec

class SourceParserTest : DescribeSpec({

    it("sqlId source parser") {
        expectSuccess(
            SourceParser.source,
            "table_a" to SqlIdSource(SqlId(schema = null, name = "table_a"), alias = null),
            "some_SCHEMA.table_a a" to SqlIdSource(SqlId(schema = "some_SCHEMA", name = "table_a"), alias = "a"),
        )
    }

    it("cross join source parser") {
        expectSuccess(SourceParser.source,
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
        expectSuccess(SourceParser.source,
            "table_a inner join table_b on x < 1" to JoinSource(
                type = INNER,
                condition = Op2Expr(LT, ColumnExpr(Column("x", null)), IntExpr(1)),
                lhs = SqlIdSource(SqlId(null, "table_a"), null),
                rhs = SqlIdSource(SqlId(null, "table_b"), null)
            )
        )
    }

    //language=sql
    val cases = listOf(
        "select * from table_a",
        "select a1, a2 from table_a",
        "select (a1), (a2) from table_a",

        // implicit cross join
        """table_a, table_b""".trimIndent(),

        // implicit inner join
        """
            table_a a
                join table_b b on a.id = b.id
        """.trimIndent(),

        // left join
        """
            table_a a
                left join table_b b on a.id = b.id
        """.trimIndent(),


        // right join
        """
            table_a a
                right join table_b b on a.id = b.id
        """.trimIndent(),

        // sub-query as source
        """
            (select * from table_a) a
                left join (select * from table_b) b on a.id = b.id
        """.trimIndent(),

        //  sub-query as source in implicit cross join
        """
            (select * from table_a) a, (select * from table_b) b
        """.trimIndent()
    )

    //language=sql
    val invalid = listOf(
        // derived must have alias
        "",
        "(select * from table_a), (select * from table_b)",
        // ON required
        "table_a a join table_b b",
        "table_a a inner table_b b",
        "table_a a right join table_b b",
        "table_a a left join table_b b",
    )
})