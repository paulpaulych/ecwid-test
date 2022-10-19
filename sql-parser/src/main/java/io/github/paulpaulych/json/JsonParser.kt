package io.github.paulpaulych.json

import io.github.paulpaulych.json.JSON.*
import io.github.paulpaulych.parser.Parser
import io.github.paulpaulych.parser.TextParsers.or
import io.github.paulpaulych.parser.TextParsers.regex
import io.github.paulpaulych.parser.TextParsers.scoped
import io.github.paulpaulych.parser.TextParsersDsl.map
import io.github.paulpaulych.parser.TextParsersDsl.or
import io.github.paulpaulych.parser.TextParsersDsl.and
import io.github.paulpaulych.parser.TextParsersDsl.sepBy
import io.github.paulpaulych.parser.TextParsersDsl.skipL
import io.github.paulpaulych.parser.TextParsersDsl.skipR
import io.github.paulpaulych.parser.TextParsersDsl.surround
import io.github.paulpaulych.parser.TextParsersDsl.thru
import io.github.paulpaulych.parser.TextParsers.string
import io.github.paulpaulych.parser.TextParsersDsl.defer

object JsonParser {

    private val double: Parser<Double> = scoped(
        scope = "number",
        msg = "invalid number format",
        parser = r(Regex("[-+]?(\\d*\\.)?\\d+([eE][-+]?\\d+)?"))
            .map { it.toDouble() }
    )

    private val boolean: Parser<Boolean> = scoped(
        scope = "boolean",
        parser = or(
            string("true").map { true },
            string("false").map { false }.defer()
        )
    )

    private val quoted: Parser<String> = scoped(
        scope = "string",
        msg = "expected quoted string",
        parser = s("\"") skipL thru("\"").map { it.dropLast(1) }
    )

    private val NULL: Parser<JSON> = scoped(
        scope = "null",
        parser = string("null").map { JNull }
    )

    val literal: Parser<JSON> = scoped(
        scope = "literal",
        parser = NULL or
            double.map(::JNumber).defer() or
            boolean.map(::JBoolean).defer() or
            quoted.map(::JString).defer()
    )

    fun rootJson(): Parser<JSON> = scoped(
        scope = "JSON",
        parser = ws skipL obj() or arr().defer() skipR ws
    )

    fun json(): Parser<JSON> = obj() or ::arr or literal.defer()

    fun objEntry(): Parser<Pair<String, JSON>> = scoped(
        scope = "field",
        parser = quoted skipR (ws and s(":") and ws) and ::json
    )

    fun obj(): Parser<JSON> = scoped(
        scope = "object",
        parser = surround(
            start = s("{") and ws,
            stop = ws and s("}"),
            parser = ((ws skipL objEntry() skipR ws) sepBy s(",")).map { kvs -> JObject(kvs.toMap()) }
        )
    )

    fun arr(): Parser<JSON> = scoped(
        scope = "array",
        parser = surround(
            start = s("[") and ws,
            stop = ws and s("]"),
            parser = ((ws skipL json() skipR ws) sepBy s(",")).map { vs -> JArray(vs) }
        )
    )

    private val ws: Parser<String> = r(Regex("[\u0020\u0009\u000A\u000D]*"))
    private fun s(s: String) = string(s)
    private fun r(regex: Regex) = regex(regex)
}