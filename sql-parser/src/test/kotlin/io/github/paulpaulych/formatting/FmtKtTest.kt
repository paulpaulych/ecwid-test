package io.github.paulpaulych.formatting

import io.github.paulpaulych.TestUtils.shouldHaveSameLines
import io.github.paulpaulych.parser.sql.Expr.*
import io.github.paulpaulych.parser.sql.Fixtures.col
import io.github.paulpaulych.parser.sql.Fixtures.tableSource
import io.github.paulpaulych.parser.sql.JoinType.CROSS
import io.github.paulpaulych.parser.sql.JoinType.INNER
import io.github.paulpaulych.parser.sql.Op2Type.*
import io.github.paulpaulych.parser.sql.Query
import io.github.paulpaulych.parser.sql.SelectedItem
import io.github.paulpaulych.parser.sql.Sort
import io.github.paulpaulych.parser.sql.SortOrder.ASC
import io.github.paulpaulych.parser.sql.SortOrder.DESC
import io.github.paulpaulych.parser.sql.Source.JoinSource
import io.github.paulpaulych.parser.sql.Source.SubQuerySource
import io.kotest.core.spec.style.DescribeSpec

class FmtKtTest: DescribeSpec({

    it("query fmt") {
        val res = Query(
            columns = listOf(
                SelectedItem(IntExpr(1), "a"),
                SelectedItem(IntExpr(2), "b"),
            ),
            source = JoinSource(CROSS, condition = null,
                lhs = JoinSource(INNER,
                    condition = BoolExpr(false),
                    lhs = tableSource("a"),
                    rhs = tableSource("b")
                ),
                rhs = tableSource("c")
            ),
            where = Op2Expr(AND,
                Op2Expr(LTE, IntExpr(1), IntExpr(2)),
                Op2Expr(GTE, IntExpr(3), IntExpr(4))
            ),
            groupBy = listOf(col("col1"), col("col2")),
            having = Op2Expr(LTE, IntExpr(5), IntExpr(6)),
            sorts = listOf(
                Sort(ColumnExpr(col("col3")), ASC),
                Sort(ColumnExpr(col("col4")), DESC),
            ),
            limit = 12,
            offset = 15
        ).fmt(Indent.empty())

        res shouldHaveSameLines """
            SELECT
                1 as a,
                2 as b
            FROM a
                INNER JOIN b ON false
                CROSS JOIN c
            WHERE 1 <= 2
                    AND 3 >= 4
            GROUP BY col1, col2
            HAVING 5 <= 6
            ORDER BY col3 ASC, col4 DESC
            LIMIT 12
            OFFSET 15
        """.trimIndent()
    }

    it("query fmt with sub query") {
        val subQuery = Query(
            columns = listOf(
                SelectedItem(IntExpr(1), "sub_a"),
                SelectedItem(IntExpr(2), "sub_b"),
            ),
            source = JoinSource(CROSS, condition = null,
                lhs = JoinSource(INNER,
                    condition = BoolExpr(false),
                    lhs = tableSource("sub_a"),
                    rhs = tableSource("sub_b")
                ),
                rhs = tableSource("sub_c")
            ),
            where = Op2Expr(AND,
                Op2Expr(LTE, IntExpr(1), IntExpr(2)),
                Op2Expr(GTE, IntExpr(3), IntExpr(4))
            ),
            groupBy = listOf(col("sub_col1"), col("sub_col2")),
            having = Op2Expr(LTE, IntExpr(5), IntExpr(6)),
            sorts = listOf(
                Sort(ColumnExpr(col("sub_col3")), ASC),
                Sort(ColumnExpr(col("sub_col4")), DESC),
            ),
            limit = 13,
            offset = 16
        )

        val res = Query(
            columns = listOf(
                SelectedItem(IntExpr(1), "a"),
                SelectedItem(
                    Op2Expr(MINUS,
                        Op2Expr(PLUS,
                            IntExpr(1),
                            SubQueryExpr(subQuery)
                        ),
                        IntExpr(7)
                    ),
                    alias ="sub"
                ),
                SelectedItem(IntExpr(2), "b"),
            ),
            source = JoinSource(CROSS, condition = null,
                lhs = JoinSource(INNER,
                    condition = BoolExpr(false),
                    lhs = tableSource("a"),
                    rhs = SubQuerySource(subQuery, "sub")
                ),
                rhs = tableSource("c")
            ),
            where = Op2Expr(AND,
                Op2Expr(LTE, IntExpr(1), IntExpr(2)),
                Op2Expr(GTE, IntExpr(3), IntExpr(4))
            ),
            groupBy = listOf(col("col1"), col("col2")),
            having = Op2Expr(LTE, IntExpr(5), IntExpr(6)),
            sorts = listOf(
                Sort(ColumnExpr(col("col3")), ASC),
                Sort(ColumnExpr(col("col4")), DESC),
            ),
            limit = 12,
            offset = 15
        ).fmt(Indent.empty())

        res shouldHaveSameLines """
            SELECT
                1 as a,
                1 + (
                    SELECT
                        1 as sub_a,
                        2 as sub_b
                    FROM sub_a
                        INNER JOIN sub_b ON false
                        CROSS JOIN sub_c
                    WHERE 1 <= 2
                            AND 3 >= 4
                    GROUP BY sub_col1, sub_col2
                    HAVING 5 <= 6
                    ORDER BY sub_col3 ASC, sub_col4 DESC
                    LIMIT 13
                    OFFSET 16
                ) - 7 as sub,
                2 as b
            FROM a
                INNER JOIN (
                    SELECT
                        1 as sub_a,
                        2 as sub_b
                    FROM sub_a
                        INNER JOIN sub_b ON false
                        CROSS JOIN sub_c
                    WHERE 1 <= 2
                            AND 3 >= 4
                    GROUP BY sub_col1, sub_col2
                    HAVING 5 <= 6
                    ORDER BY sub_col3 ASC, sub_col4 DESC
                    LIMIT 13
                    OFFSET 16
                ) AS sub ON false
                CROSS JOIN c
            WHERE 1 <= 2
                    AND 3 >= 4
            GROUP BY col1, col2
            HAVING 5 <= 6
            ORDER BY col3 ASC, col4 DESC
            LIMIT 12
            OFFSET 15
        """.trimIndent()
    }

})
