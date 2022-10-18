package io.github.paulpaulych.json

import io.github.paulpaulych.json.JSON.*
import io.github.paulpaulych.parser.Parser
import io.github.paulpaulych.parser.ParsersDsl.map
import io.github.paulpaulych.parser.ParsersDsl.or
import io.github.paulpaulych.parser.ParsersDsl.parser
import io.github.paulpaulych.parser.ParsersDsl.product
import io.github.paulpaulych.parser.ParsersDsl.sep
import io.github.paulpaulych.parser.ParsersDsl.skipL
import io.github.paulpaulych.parser.ParsersDsl.skipR
import io.github.paulpaulych.parser.ParsersDsl.surround
import io.github.paulpaulych.parser.ParsersDsl.thru
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
        quoted product (":".parser skipL json())

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