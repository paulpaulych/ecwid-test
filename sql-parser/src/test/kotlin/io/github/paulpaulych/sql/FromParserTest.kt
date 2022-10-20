package io.github.paulpaulych.sql

import io.kotest.core.spec.style.DescribeSpec

class FromParserTest: DescribeSpec({

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