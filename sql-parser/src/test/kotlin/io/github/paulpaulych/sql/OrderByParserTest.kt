package io.github.paulpaulych.sql

import io.kotest.core.spec.style.DescribeSpec

class OrderByParserTest: DescribeSpec({

    //language=sql
    val cases = listOf(
        "source_a.source_a asc, id desc, b, a asc",
        "a",
        "table.a desc"
    )

    //language=sql
    val invalid = listOf(
        "",
        "id asc desc, id2",
        "asc, desc"
    )
})