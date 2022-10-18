package io.github.paulpaulych.json

import io.github.paulpaulych.json.JSON.*
import io.github.paulpaulych.parser.Parser
import io.github.paulpaulych.parser.TextParsers.or
import io.github.paulpaulych.parser.TextParsers.regex
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
import io.github.paulpaulych.parser.TextParsersDsl.tag

object JsonParser {

    val double: Parser<Double> =
        Regex("[-+]?(\\d*\\.)?\\d+([eE][-+]?\\d+)?").parser
            .map { it.toDouble() }

    val boolean: Parser<Boolean> =
        or(
            string("true").map { true },
            string("false").map { false }.defer()
        )

    val quoted: Parser<String> =
        "\"".parser skipL thru("\"").map { it.dropLast(1) }

    val NULL: Parser<JSON> = string("null").map { JNull }

    val literal: Parser<JSON> =
        NULL or
            double.map(::JNumber).defer() or
            boolean.map(::JBoolean).defer() or
            quoted.map(::JString).defer() scope "literal" tag "expected"

    fun value() = literal or ::obj or ::arr

    fun objEntry(): Parser<Pair<String, JSON>> =
        quoted skipR (ws and ":".parser.defer() and ws.defer()) and value().defer()

    fun obj(): Parser<JSON> = surround(
        start = "{".parser and ws,
        stop = ws and "}".parser,
        parser = (objEntry() sepBy (ws skipL ",".parser skipR ws)).map { kvs -> JObject(kvs.toMap()) }
    )

    val ws: Parser<String> = Regex("[\u0020\u0009\u000A\u000D]*").parser

    fun <A> Parser<A>.withWs() = this skipR ws

    fun s(s: String) = string(s)
    fun r(regex: Regex) = regex(regex)

    fun arr(): Parser<JSON> = surround(
        start = s("[") and ws,
        stop = ws and s("]"),
        parser = (value() sepBy (ws skipL s(",") skipR ws)).map { vs -> JArray(vs) }
    )

    val eof: Parser<String> = r(Regex("\\z*"))

    val jsonParser = ws skipL value() skipR eof
}