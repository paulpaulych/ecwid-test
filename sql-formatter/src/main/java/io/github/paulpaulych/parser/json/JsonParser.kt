package io.github.paulpaulych.parser.json

import io.github.paulpaulych.parser.json.JSON.*
import io.github.paulpaulych.parser.lib.Parser
import io.github.paulpaulych.parser.lib.TextParsers.regex
import io.github.paulpaulych.parser.lib.TextParsers.scoped
import io.github.paulpaulych.parser.lib.TextParsers.string
import io.github.paulpaulych.parser.lib.TextParsersDsl.and
import io.github.paulpaulych.parser.lib.TextParsersDsl.defer
import io.github.paulpaulych.parser.lib.TextParsersDsl.map
import io.github.paulpaulych.parser.lib.TextParsersDsl.or
import io.github.paulpaulych.parser.lib.TextParsersDsl.sepBy
import io.github.paulpaulych.parser.lib.TextParsersDsl.skipL
import io.github.paulpaulych.parser.lib.TextParsersDsl.skipR
import io.github.paulpaulych.parser.lib.TextParsersDsl.surround

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
        parser = surround(s("\""), s("\""), r(Regex("[^\"]*")))
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

    fun json(): Parser<JSON> = obj() or JsonParser::arr or literal.defer()

    fun objEntry(): Parser<Pair<String, JSON>> = scoped(
        scope = "field",
        parser = quoted skipR (ws and s(":") and ws) and JsonParser::json
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