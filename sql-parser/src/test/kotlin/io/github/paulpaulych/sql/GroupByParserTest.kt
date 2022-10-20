package io.github.paulpaulych.sql

import io.kotest.core.spec.style.DescribeSpec

class GroupByParserTest: DescribeSpec({

    //language=sql
    val cases = listOf(
        "source_a.source_a, id, b",
    )

    //language=sql
    val invalid = listOf(
        "",
        "a b",
        "a A"
    )
})