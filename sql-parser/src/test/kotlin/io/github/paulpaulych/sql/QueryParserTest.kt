package io.github.paulpaulych.sql

import io.kotest.core.spec.style.DescribeSpec

class QueryParserTest: DescribeSpec({

    //language=sql
    val cases = listOf(
        "select * from table_a",
        "select a1, a2 from table_a",
        "select (a1), (a2) from table_a",

        // implicit cross join
        """
            select 
                *
            from table_a, table_b
        """.trimIndent(),

        // implicit inner join
        """
            select
                a.*,
                b.value as b_value
            from table_a a
                join table_b b on a.id = b.id
        """.trimIndent(),

        // left join
        """
            select 
                a.value,
                b.value
            from table_a a
                left join table_b b on a.id = b.id
        """.trimIndent(),


        // right join
        """
            select 
                a.value,
                b.value
            from table_a a
                right join table_b b on a.id = b.id
        """.trimIndent(),

        // sub-query as source
        """
            select 
                a.value,
                b.value
            from (select * from table_a) a
                left join (select * from table_b) b on a.id = b.id
        """.trimIndent(),

        //  sub-query as source in implicit cross join
        """
            select 
              a.value,
              b.value
            from (select * from table_a) a, (select * from table_b) b;
        """.trimIndent()
    )

    //language=sql
    val invalid = listOf(
        "select () from table_a",
        "select (a1, a2) from table_a",

        // derived must have alias
        """
            select 
              a.value,
              b.value
            from (select * from table_a), (select * from table_b);
        """.trimIndent()
    )

})