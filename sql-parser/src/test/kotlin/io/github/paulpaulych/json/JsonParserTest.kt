package io.github.paulpaulych.json

import io.github.paulpaulych.TestUtils.err
import io.github.paulpaulych.TestUtils.ok
import io.github.paulpaulych.TestUtils.runParserTest
import io.github.paulpaulych.json.JSON.*
import io.github.paulpaulych.parser.State
import io.github.paulpaulych.parser.TextParsers
import io.kotest.core.spec.style.DescribeSpec
import io.kotest.data.forAll
import io.kotest.data.headers
import io.kotest.data.row
import io.kotest.data.table
import io.kotest.matchers.shouldBe

class JsonParserTest: DescribeSpec({

    it("json literal parser") {
        val parser = JsonParser.literal
        runParserTest(
            row(parser, "null", ok(JNull, 4)),

            row(parser, """"abla"""", ok(JString("abla"), 6)),
            row(parser, """"abla abla"""", ok(JString("abla abla"), 11)),

            row(parser, """12.5""", ok(JNumber(12.5), 4)),
            row(parser, """-1""", ok(JNumber(-1.0), 2)),
            row(parser, """-1.25e2""", ok(JNumber(-1.25e2), 7)),
            row(parser, """+1.25E2""", ok(JNumber(1.25e2), 7)),

            row(parser, """true""", ok(JBoolean(true), 4)),
            row(parser, """false""", ok(JBoolean(false), 5)),
        )
    }

    it("unexpected json literal") {
        val parser = JsonParser.literal
        forAll(
            table(headers("input"),
                row(" "),
                row(""),
                row("[]"),
                row("{}"),
                row("{}"),
                row("abla"),
                row("\t"),
            )
        ) { input ->
            TextParsers.run(parser, input) shouldBe err(
                stack = listOf(
                    State(input, 0) to "expected",
                    State(input, 0) to "literal",
                    State(input, 0) to "expected",
                    State(input, 0) to "'\"'",
                )
            )
        }
    }

    it("json object entry parser") {
        val parser = JsonParser.objEntry()
        runParserTest(
            row(parser, "\"a\":null", ok(Pair("a", JNull), 8)),
            row(parser, "\"k\":\"v\"", ok(Pair("k", JString("v")), 7)),
            row(parser, "\"k\":true", ok(Pair("k", JBoolean(true)), 8)),
            row(parser, "\"k\": true", ok(Pair("k", JBoolean(true)), 9)),
            row(parser, "\"k\" \t\n\r\n\t  : \ttrue", ok(Pair("k", JBoolean(true)), 18)),
        )
    }

    it("json array parser") {
        val parser = JsonParser.arr()
        runParserTest(
            row(parser, "[]", ok(JArray(listOf()), 2)),
            row(parser,
                "[ \"a\"\t\t,123.0, true,\n\n null]",
                ok(JArray(listOf(JString("a"), JNumber(123.0), JBoolean(true), JNull)),28)
            ),
        )
    }

    it("json object parser") {
        val parser = JsonParser.obj()
        runParserTest(
            row(parser, "{}", ok(JObject(mapOf()), 2)),
            row(parser,
                //language=json
                """{
                    "a":true,   "b":{"k":"v"}, "c" : 17.3,
                    "d"  :[], "e": [1, 2], "f": null 
                }""".trimIndent(),
                ok(JObject(mapOf(
                    "a" to JBoolean(true),
                    "b" to JObject(mapOf("k" to JString("v"))),
                    "c" to JNumber(17.3),
                    "d" to JArray(listOf()),
                    "e" to JArray(listOf(JNumber(1.0), JNumber(2.0))),
                    "f" to JNull
                )), consumed = 132)
            ),
        )
    }
})