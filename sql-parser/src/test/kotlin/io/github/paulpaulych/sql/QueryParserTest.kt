package io.github.paulpaulych.sql

import io.github.paulpaulych.TestUtils.expectFmtSuccess
import io.kotest.core.spec.style.DescribeSpec

class QueryParserTest: DescribeSpec({

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
                SELECT author.name, count(book.id), sum(book.cost)
                FROM (author LEFT JOIN book ON (author.id = book.author_id))
                GROUP BY author.name
                HAVING ((COUNT(*) > 1) AND (SUM(book.cost) > 500))
                LIMIT 10
                OFFSET 172
            """.trimIndent(),

            "select * from table_a" to """
                SELECT *
                FROM table_a
            """.trimIndent(),

            "select a1, a2 from table_a" to """
                SELECT a1, a2
                FROM table_a
            """.trimIndent(),

            "select (a1) as a1, (a2) as a from table_a" to """
                SELECT a1 as a1, a2 as a
                FROM table_a
            """.trimIndent(),

//            """
//                select
//                count(*)
//                 as c
//                from
//                    (select
//                        * from table_a) a
//                    join (select * from table_b) b on a.id = b.id
//            """.trimIndent() to """
//                SELECT count(*) as c
//                FROM ((SELECT * FROM table_a) a INNER JOIN (SELECT * FROM table_b) b ON a.id = b.id)
//            """.trimIndent()
            )
    }

    //TODO: move to QueryParserTest
    //language=sql
    val cases = listOf(
        "select (a1), (a2) from table_a",


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