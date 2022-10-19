package io.github.paulpaulych.json

import io.github.paulpaulych.json.JSON.*
import io.github.paulpaulych.parser.Parser
import io.github.paulpaulych.parser.TextParsers.or
import io.github.paulpaulych.parser.TextParsers.regex
import io.github.paulpaulych.parser.TextParsers.scoped
import io.github.paulpaulych.parser.TextParsersDsl.map
import io.github.paulpaulych.parser.TextParsersDsl.or
import io.github.paulpaulych.parser.TextParsersDsl.parser
import io.github.paulpaulych.parser.TextParsersDsl.and
import io.github.paulpaulych.parser.TextParsersDsl.sepBy
import io.github.paulpaulych.parser.TextParsersDsl.skipL
import io.github.paulpaulych.parser.TextParsersDsl.skipR
import io.github.paulpaulych.parser.TextParsersDsl.surround
import io.github.paulpaulych.parser.TextParsersDsl.thru
import io.github.paulpaulych.parser.TextParsers.string
import io.github.paulpaulych.parser.TextParsersDsl.defer
import io.github.paulpaulych.parser.TextParsersDsl.scope

object JsonParser {

    val double: Parser<Double> = scoped(
        scope = "number",
        msg = "invalid number format",
        parser = Regex("[-+]?(\\d*\\.)?\\d+([eE][-+]?\\d+)?").parser
            .map { it.toDouble() }
    )

    val boolean: Parser<Boolean> = scoped(
        scope = "boolean",
        parser = or(
            string("true").map { true },
            string("false").map { false }.defer()
        )
    )

    val quoted: Parser<String> = scoped(
        scope = "string",
        msg = "expected quoted string",
        parser = "\"".parser skipL thru("\"").map { it.dropLast(1) }
    )

    val NULL: Parser<JSON> = scoped(
        scope = "null",
        parser = string("null").map { JNull }
    )

    val literal: Parser<JSON> =
        NULL or
            double.map(::JNumber).defer() or
            boolean.map(::JBoolean).defer() or
            quoted.map(::JString).defer() scope "literal"

    fun json() = literal or ::obj or ::arr

    fun objEntry(): Parser<Pair<String, JSON>> = scoped(
        scope = "object entry",
        msg = "invalid object entry",
        parser = quoted skipR (ws and ":".parser.defer() and ws.defer()) and json().defer()
    )

    fun obj(): Parser<JSON> = scoped(
        scope = "object",
        msg = "invalid object syntax",
        parser = surround(
            start = "{".parser and ws,
            stop = ws and "}".parser,
            parser = (objEntry() sepBy (ws skipL ",".parser skipR ws)).map { kvs -> JObject(kvs.toMap()) }
        )
    )

    fun arr(): Parser<JSON> = scoped(
        scope = "array",
        parser = surround(
            start = s("[") and ws,
            stop = ws and s("]"),
            parser = (json() sepBy (ws skipL s(",") skipR ws)).map { vs -> JArray(vs) }
        )
    )

    private val ws: Parser<String> = Regex("[\u0020\u0009\u000A\u000D]*").parser
    fun s(s: String) = string(s)

    fun r(regex: Regex) = regex(regex)
}