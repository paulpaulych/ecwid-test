package io.github.paulpaulych.json

import io.github.paulpaulych.json.JSON.*
import io.github.paulpaulych.parser.Parser
import io.github.paulpaulych.parser.TextParsersDsl.map
import io.github.paulpaulych.parser.TextParsersDsl.or
import io.github.paulpaulych.parser.TextParsersDsl.parser
import io.github.paulpaulych.parser.TextParsersDsl.then
import io.github.paulpaulych.parser.TextParsersDsl.sep
import io.github.paulpaulych.parser.TextParsersDsl.skipL
import io.github.paulpaulych.parser.TextParsersDsl.skipR
import io.github.paulpaulych.parser.TextParsersDsl.surround
import io.github.paulpaulych.parser.TextParsersDsl.thru
import io.github.paulpaulych.parser.TextParsers.string

object JsonParser {

    private val double: Parser<Double> =
        Regex("[-+]?(\\d*\\.)?\\d+([eE][-+]?\\d+)?").parser
            .map { it.toDouble() }

    private val boolean: Parser<Boolean> =
        string("true").map { true }
            .or(string("false").map { false })

    private val quoted: Parser<String> =
        "\"".parser skipL thru("\"").map { it.dropLast(1) }

    private val jNull: Parser<JSON> = string("null").map { JNull }

    private fun json(): Parser<JSON> =
        jNull or
            double.map(::JNumber) or
            boolean.map(::JBoolean) or
            quoted.map(::JString) or
            jObj() or
            jArr()

    private val keyval: Parser<Pair<String, JSON>> =
        quoted then (":".parser skipL json())

    private fun jObj(): Parser<JSON> =
        surround("{".parser, "}".parser,
            (keyval sep ",".parser).map { kvs -> JObject(kvs.toMap()) })

    private fun jArr(): Parser<JSON> =
        surround("[".parser, "]".parser,
            (json() sep ",".parser).map { vs -> JArray(vs) })

    private val whitespace: Parser<String> = Regex("\\s*").parser

    private val eof: Parser<String> = Regex("\\z*").parser

    val jsonParser = whitespace skipL json() skipR eof
}