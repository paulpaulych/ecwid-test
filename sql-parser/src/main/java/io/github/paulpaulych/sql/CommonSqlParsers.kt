package io.github.paulpaulych.sql

import io.github.paulpaulych.parser.Parser
import io.github.paulpaulych.parser.TextParsers.regex
import io.github.paulpaulych.parser.TextParsers.scoped
import io.github.paulpaulych.parser.TextParsers.string
import io.github.paulpaulych.parser.TextParsersDsl
import io.github.paulpaulych.parser.TextParsersDsl.defer
import io.github.paulpaulych.parser.TextParsersDsl.many
import io.github.paulpaulych.parser.TextParsersDsl.map
import io.github.paulpaulych.parser.TextParsersDsl.or
import io.github.paulpaulych.parser.TextParsersDsl.sepBy1
import io.github.paulpaulych.parser.TextParsersDsl.skipL
import io.github.paulpaulych.parser.TextParsersDsl.skipR
import io.github.paulpaulych.parser.TextParsersDsl.surround

object CommonSqlParsers {

    val ws: Parser<String> = r(Regex("[\u0020\u0009\u000A\u000D]*"))

    fun s(s: String) = string(s)

    fun sOrS(text: String) = s(text.lowercase()) or s(text.uppercase()).defer()

    fun r(regex: Regex) = regex(regex)

    val latinWord: Parser<String> = r(Regex("[a-zA-Z_]\\w*"))

    val column: Parser<Column> = scoped(
        scope = "column",
        parser = (latinWord sepBy1 s("."))
            .map { segments ->
                Column(
                    name = segments.last(),
                    source = segments.dropLast(1).joinToString(".").takeIf { it.isNotEmpty() }
                )
            }
    )

    val wildcard: Parser<Wildcard> = scoped(
        scope = "wildcard",
        parser = (latinWord skipR s(".")).many().skipR(s("*"))
            .map { sourceSegments ->
                Wildcard(
                    source = sourceSegments.joinToString(".").takeIf { it.isNotEmpty() }
                )
            }
    )

    val double: Parser<Double> = scoped(
        scope = "double",
        parser = r(Regex("\\d+\\.\\d+([eE][-+]?\\d+)?"))
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

    fun <A> Parser<A>.inParentheses(): Parser<A> =
        { this }.inParentheses()

    fun <A> (() -> Parser<A>).inParentheses(): Parser<A> =
        surround(s("("), s(")"), ws skipL this skipR ws)
}