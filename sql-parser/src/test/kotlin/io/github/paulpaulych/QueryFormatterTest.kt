package io.github.paulpaulych

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
                OFFSET 172;
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
                OFFSET 172;
            """.trimIndent(),

            "select * from table_a;" to """
                SELECT
                    *
                FROM table_a;
            """.trimIndent(),
            """
                select * from table_a
                
                    ;
            """.trimIndent() to """
                SELECT
                    *
                FROM table_a;
            """.trimIndent(),

            "select a1, a2 from table_a;" to """
                SELECT
                    a1,
                    a2
                FROM table_a;
            """.trimIndent(),

            "select (a1) as a1, (a2) as a from table_a;" to """
                SELECT
                    a1 AS a1,
                    a2 AS a
                FROM table_a;
            """.trimIndent(),

            """
                select
                count(*)
                 AS c
                from
                    (select
                        * from table_a) a
                    join (select * from table_b) b on a.id = b.id;
            """.trimIndent() to """
                SELECT
                    count(*) AS c
                FROM (
                         SELECT
                             *
                         FROM table_a
                     ) AS a
                    INNER JOIN (
                                   SELECT
                                       *
                                   FROM table_b
                               ) AS b ON a.id = b.id;
            """.trimIndent()
            )
    }

    it("invalid cases") {
        expectFmtFailure(
            //language=SQL
            "(select * from table_a), (select * from table_b);" to """
                stacktrace:
                [1:1] keyword SELECT expected: expected one of ['select', 'SELECT']

                [1:1] (select * from table_a), (select * from table_b);
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

            "select * from a a a    ;" to """
                stacktrace:
                [1:19] expected ';'

                [1:19] select * from a a a    ;
                                   here--^
                error: expected ';'
            """.trimIndent(),

            "select * from a a where    ;" to """
                stacktrace:
                [1:28] expression expected: no expression found
                
                [1:28] select * from a a where    ;
                                            here--^
                error: no expression found
            """.trimIndent(),

            "select * table_a a left join table_b b;" to """
                stacktrace:
                [1:10] keyword FROM expected: expected one of ['from', 'FROM']

                [1:10] select * table_a a left join table_b b;
                          here--^
                error: expected one of ['from', 'FROM']
            """.trimIndent(),

            "select * from table_a a left join table_b b limit 10;" to """
                stacktrace:
                [1:15] source expected
                [1:45] invalid join condition syntax: keyword ON expected: expected one of ['on', 'ON']

                [1:45] select * from table_a a left join table_b b limit 10;
                                                             here--^
                error: expected one of ['on', 'ON']
            """.trimIndent(),

            "select about as 12;" to """
                stacktrace:
                [1:17] expected expression matching regex '[a-zA-Z_][a-zA-Z0-9_]*'
                
                [1:17] select about as 12;
                                 here--^
                error: expected expression matching regex '[a-zA-Z_][a-zA-Z0-9_]*'
            """.trimIndent(),

            "select * from a group by;" to """
                stacktrace:
                [1:25] columns sep by comma expected: invalid column syntax: expected one of [expression matching regex '[a-zA-Z_][a-zA-Z0-9_]*', '*']

                [1:25] select * from a group by;
                                         here--^
                error: expected one of [expression matching regex '[a-zA-Z_][a-zA-Z0-9_]*', '*']
            """.trimIndent(),

            "select * from a having    ;" to """
                stacktrace:
                [1:27] expression expected: no expression found

                [1:27] select * from a having    ;
                                           here--^
                error: no expression found
            """.trimIndent(),

            "select a from b limit ;" to """
                stacktrace:
                [1:23] invalid int syntax: expected expression matching regex '\d+'

                [1:23] select a from b limit ;
                                       here--^
                error: expected expression matching regex '\d+'
            """.trimIndent(),

            "select a from (select * from b) ;" to """
                stacktrace:
                [1:15] source expected: source expected: derived query expected
                [1:33] invalid alias syntax: expected expression matching regex '[a-zA-Z_][a-zA-Z0-9_]*'
                
                [1:33] select a from (select * from b) ;
                                                 here--^
                error: expected expression matching regex '[a-zA-Z_][a-zA-Z0-9_]*'
            """.trimIndent()
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