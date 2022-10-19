package io.github.paulpaulych.json

import io.github.paulpaulych.TestUtils.ok
import io.github.paulpaulych.TestUtils.runParserTest
import io.github.paulpaulych.common.Either.Left
import io.github.paulpaulych.common.Either.Right
import io.github.paulpaulych.json.JSON.*
import io.github.paulpaulych.parser.ErrorItem.ParseError
import io.github.paulpaulych.parser.ErrorItem.ScopesTried
import io.github.paulpaulych.parser.StackTrace
import io.github.paulpaulych.parser.State
import io.github.paulpaulych.parser.TextParsers
import io.github.paulpaulych.parser.fmt.fmt
import io.kotest.core.spec.style.DescribeSpec
import io.kotest.data.forAll
import io.kotest.data.headers
import io.kotest.data.row
import io.kotest.data.table
import io.kotest.matchers.collections.shouldContainExactly
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
            val res = TextParsers.run(parser, input)
            with((res as Left<StackTrace>).value) {
                state shouldBe State(input, 0)
                error shouldBe ParseError("literal", "invalid literal syntax")
                with(checkNotNull(cause)) {
                    state shouldBe State(input, 0)
                    (error as ScopesTried).scopes shouldContainExactly listOf(
                        "null",
                        "number",
                        "boolean",
                        "string"
                    )
                }
            }
        }
    }

    it("json field parser") {
        val parser = JsonParser.objEntry()
        runParserTest(
            row(parser, "\"a\":null", ok(Pair("a", JNull), 8)),
            row(parser, "\"k\":\"v\"", ok(Pair("k", JString("v")), 7)),
            row(parser, "\"k\":true", ok(Pair("k", JBoolean(true)), 8)),
            row(parser, "\"k\": true", ok(Pair("k", JBoolean(true)), 9)),
            row(parser, "\"k\" \t\n\r\n\t  : \ttrue", ok(Pair("k", JBoolean(true)), 18)),
        )

        forAll(
            table(
                headers("input", "error"),
                row("bla") { e: StackTrace ->
                    e.error shouldBe ParseError("field", "invalid field syntax")
                },
                row("123") { e: StackTrace ->
                    e.error shouldBe ParseError("field", "invalid field syntax")
                    e.cause?.error shouldBe ParseError("string", "expected quoted string")
                    e.cause?.cause?.error shouldBe ParseError("'\"'", "expected '\"'")
                },
                row("\"key\":abne") { e: StackTrace ->
                    e.error shouldBe ParseError("field", "invalid field syntax")
                    e.cause?.error shouldBe ScopesTried(listOf("object", "array", "literal"))
                },
            )
        ) { input, matcher ->
            val res = TextParsers.run(parser, input)
            matcher((res as Left).value)
        }
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
        val parser = JsonParser.json()
        //language=json
        val json = """{
            "a":true,   "b":
            {"k":[
45.5, {"innerKey": false}]}, "c" : 17.3,
            "d"  :[], "e": [1,
            
            2], "f": null 
        }"""

        val res = TextParsers.run(parser, json)
        (res as Right).value.get shouldBe JObject(mapOf(
            "a" to JBoolean(true),
            "b" to JObject(mapOf("k" to JArray(listOf(JNumber(45.5), JObject(mapOf("innerKey" to JBoolean(false))))))),
            "c" to JNumber(17.3),
            "d" to JArray(listOf()),
            "e" to JArray(listOf(JNumber(1.0), JNumber(2.0))),
            "f" to JNull
        ))
    }

    it("deep error trace") {
        val json = """{
            "a":true,   "b":
            {"k":[
45.5, {"innerKey": }]}, "c" : 17.3,
            "d"  :[], "e": [1,
            
            2], "f": null 
        }"""

        val res = TextParsers.run(JsonParser.rootJson(), json) as Left
        with(res.value) {
            state shouldBe State(json, 0)
            error shouldBe ParseError("JSON", "invalid JSON syntax")
            with(checkNotNull(cause)) {
                state shouldBe State(json, 0)
                error shouldBe ParseError("object", "invalid object syntax")
                with(checkNotNull(cause)) {
                    state shouldBe State(json, 26)
                    error shouldBe ParseError("field", "invalid field syntax")
                    with(checkNotNull(cause)) {
                        state shouldBe State(json, 43)
                        error shouldBe ParseError("object", "invalid object syntax")
                        with(checkNotNull(cause)) {
                            state shouldBe State(json, 44)
                            error shouldBe ParseError("field", "invalid field syntax")
                            with(checkNotNull(cause)) {
                                state shouldBe State(json, 48)
                                error shouldBe ParseError("array", "invalid array syntax")
                                with(checkNotNull(cause)) {
                                    state shouldBe State(json, 56)
                                    error shouldBe ParseError("object", "invalid object syntax")
                                    with(checkNotNull(cause)) {
                                        state shouldBe State(json, 57)
                                        error shouldBe ParseError("field", "invalid field syntax")
                                        with(checkNotNull(cause)) {
                                            state shouldBe State(json, 69)
                                            error shouldBe ScopesTried(listOf("object", "array", "literal"))
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    it("deep stacktrace fmt test") {
        val json = """{
            "a":true,   "b":
            {"k":[
45.5, {"innerKey": }]}, "c" : 17.3,
            "d"  :[], "e": [1,
            
            2], "f": null 
        }"""

        val res = TextParsers.run(JsonParser.rootJson(), json) as Left

        fmt(res.value).lines() shouldContainExactly """
            stacktrace:
            [1:1] invalid JSON syntax: invalid object syntax
            [2:25] invalid field syntax
            [3:13] invalid object syntax
            [3:14] invalid field syntax
            [3:18] invalid array syntax
            [4:7] invalid object syntax
            [4:8] invalid field syntax
            [4:20] expected one of [object, array, literal]

            [4:20] 45.5, {"innerKey": }]}, "c" : 17.3,
                               here --^
            error: expected one of [object, array, literal]
        """.trimIndent().lines()
    }
})