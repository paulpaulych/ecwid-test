package io.github.paulpaulych.sql

import io.kotest.core.spec.style.DescribeSpec

class ExprParserTest: DescribeSpec({

    //language=sql
    val sqlId = listOf(
        "some_table_name",
        "soMe_function_name",
        "some_schema.some_table_name",
        "some_schema.some_function_name",
    )

    //language=sql
    val logical = listOf(
        "not(false)",
        "not('1')",

        "false and true",
        "(false or true) or not(false)",
    )

    //language=sql
    val comparison = listOf(
        "a = b",
        "a != b",
        "a > b",
        "a >= b",
        "a < b",
        "a <= b",
    )

    //language=sql
    val column = listOf(
        "id",
        "table_a.id"
    )

    val funExpr = listOf(
        "some_fun(id, table_a.id, false or true, 'some_string', 1,)",
        "some_fun()",
        "some_schema.some_fun(id, id)",
        "some_schema.some_fun()"
    )

    //language=sql
    val literals = listOf(
        "true",
        "false",

        "123",
        "-175",

        "45.56",
        "-1.4553e3",


        "'able ble'",
        "''",

        "null",
    )


    //language=sql
    val invalid = listOf(
        ""
    )

})