package io.github.paulpaulych.formatting

import io.github.paulpaulych.TestUtils.expectFmtFailure
import io.github.paulpaulych.TestUtils.expectFmtSuccess
import io.kotest.core.spec.style.DescribeSpec

class QueryFormatterTest: DescribeSpec({

    it("valid queries") {
        //language=SQL
        expectFmtSuccess(
            """
                SELECT author.name, count(book.id), sum(book.cost) 
                FROM author 
                LEFT JOIN book ON (author.id = book.author_id) 
                GROUP BY author.name 
                HAVING COUNT(*) > 1 AND SUM(book.cost) > 500
                LIMIT 10
                OFFSET 172
            """.trimIndent() to """
                SELECT
                    author.name,
                    count(book.id),
                    sum(book.cost)
                FROM author
                    LEFT JOIN book ON author.id = book.author_id
                GROUP BY author.name
                HAVING COUNT(*) > 1
                        AND SUM(book.cost) > 500
                LIMIT 10
                OFFSET 172
            """.trimIndent(),

            "select * from table_a" to """
                SELECT
                    *
                FROM table_a
            """.trimIndent(),

            "select a1, a2 from table_a" to """
                SELECT
                    a1,
                    a2
                FROM table_a
            """.trimIndent(),

            "select (a1) as a1, (a2) as a from table_a" to """
                SELECT
                    a1 as a1,
                    a2 as a
                FROM table_a
            """.trimIndent(),

            """
                select
                count(*)
                 as c
                from
                    (select
                        * from table_a) a
                    join (select * from table_b) b on a.id = b.id
            """.trimIndent() to """
                SELECT
                    count(*) as c
                FROM (
                         SELECT
                             *
                         FROM table_a
                     ) AS a
                    INNER JOIN (
                                   SELECT
                                       *
                                   FROM table_b
                               ) AS b ON a.id = b.id
            """.trimIndent()
            )
    }

    it("invalid cases") {
        expectFmtFailure(
            //language=SQL
            "(select * from table_a), (select * from table_b)" to """
                stacktrace:
                [1:1] keyword SELECT expected: expected one of ['select', 'SELECT']
                
                [1:1] (select * from table_a), (select * from table_b)
                here--^
                error: expected one of ['select', 'SELECT']
            """.trimIndent(),

            "" to """
                stacktrace:
                [1:1] keyword SELECT expected: expected one of ['select', 'SELECT']
                
                [1:1] 
                here--^
                error: expected one of ['select', 'SELECT']
            """.trimIndent(),

            "select * table_a a left join table_b b" to """
                stacktrace:
                [1:10] keyword FROM expected: expected one of ['from', 'FROM']

                [1:10] select * table_a a left join table_b b
                         here--^
                error: expected one of ['from', 'FROM']
            """.trimIndent(),

            "select * from table_a a left join table_b b limit 10" to """
                stacktrace:
                [1:15] source expected
                [1:45] invalid join condition syntax: keyword ON expected: expected one of ['on', 'ON']
                
                [1:45] select * from table_a a left join table_b b limit 10
                                                            here--^
                error: expected one of ['on', 'ON']
            """.trimIndent(),
        )
    }

    //language=sql
    val invalid = listOf(
        // derived must have alias
        "",
        "(select * from table_a), (select * from table_b)",
        // ON required
        "select * table_a a join table_b b",
        "table_a a inner table_b b",
        "table_a a right join table_b b",
        "table_a a left join table_b b",
    )

})