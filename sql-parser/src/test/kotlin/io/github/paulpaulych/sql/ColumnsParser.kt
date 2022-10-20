package io.github.paulpaulych.sql

import io.kotest.core.spec.style.DescribeSpec

class ColumnsParserTest: DescribeSpec({

    val columnsList = listOf(
        "id",
        "table_a.id",
        "*",
        "count(*)",
        "table_a.*",
        "(a1), (a2)",
        "(a1), (*)",
        "table.a1, *",

        "id as some_alias",

        // with expressions
        "false as some_alias",
        "(select id from table_b) as a",
    )


    //language=sql
    val invalid = listOf(
        "()",
        "(a1, a2)",
    )
})