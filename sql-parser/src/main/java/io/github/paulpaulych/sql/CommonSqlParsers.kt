package io.github.paulpaulych.sql

import io.github.paulpaulych.parser.Parser
import io.github.paulpaulych.parser.TextParsers.opt
import io.github.paulpaulych.parser.TextParsers.or
import io.github.paulpaulych.parser.TextParsers.regex
import io.github.paulpaulych.parser.TextParsers.scoped
import io.github.paulpaulych.parser.TextParsers.string
import io.github.paulpaulych.parser.TextParsersDsl
import io.github.paulpaulych.parser.TextParsersDsl.defer
import io.github.paulpaulych.parser.TextParsersDsl.map
import io.github.paulpaulych.parser.TextParsersDsl.plus
import io.github.paulpaulych.parser.TextParsersDsl.skipL

object CommonSqlParsers {

    val ws: Parser<String> = r(Regex("[\u0020\u0009\u000A\u000D]*"))

    fun s(s: String) = string(s)

    fun r(regex: Regex) = regex(regex)

    val latinWord: Parser<String> = r(Regex("[a-zA-Z_]\\w*"))

    val column: Parser<Column> = scoped(
        scope = "column",
        parser = (latinWord + (s(".") skipL latinWord).opt())
            .map { (l, r) -> r?.let { Column(r, l) } ?: Column(l, null) }
    )

    val double: Parser<Double> = scoped(
        scope = "double",
        parser = r(Regex("[-+]?\\d+\\.\\d+([eE][-+]?\\d+)?"))
            .map { it.toDouble() }
    )

    val int: Parser<Int> = scoped(
        scope = "int",
        parser = r(Regex("[-+]?\\d+")).map { it.toInt() }
    )

    val boolean: Parser<Boolean> = scoped(
        scope = "boolean",
        parser = or(
            string("true").map { true },
            string("false").map { false }.defer()
        )
    )

    val quoted: Parser<String> = scoped(
        scope = "string literal",
        msg = "expected quoted string",
        parser = s("\'") skipL TextParsersDsl.thru("\'").map { it.dropLast(1) }
    )
}