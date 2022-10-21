package io.github.paulpaulych.sql

import io.github.paulpaulych.parser.Parser
import io.github.paulpaulych.parser.TextParsers.peekOnly
import io.github.paulpaulych.parser.TextParsers.regex
import io.github.paulpaulych.parser.TextParsers.scoped
import io.github.paulpaulych.parser.TextParsers.string
import io.github.paulpaulych.parser.TextParsersDsl.defer
import io.github.paulpaulych.parser.TextParsersDsl.many
import io.github.paulpaulych.parser.TextParsersDsl.map
import io.github.paulpaulych.parser.TextParsersDsl.or
import io.github.paulpaulych.parser.TextParsersDsl.sepBy1
import io.github.paulpaulych.parser.TextParsersDsl.skipL
import io.github.paulpaulych.parser.TextParsersDsl.skipR
import io.github.paulpaulych.parser.TextParsersDsl.surround
import io.github.paulpaulych.sql.Expr.LitExpr.*

@Suppress("RegExpSimplifiable")
object CommonSqlParsers {

    val ws: Parser<String> = r(Regex("[\u0020\u0009\u000A\u000D]*"))
    val latinWord: Parser<String> = r(Regex("[a-zA-Z_][a-zA-Z0-9_]*"))

    private val floatParser: Parser<String> = r(Regex("\\d+\\.\\d+([eE][-+]?\\d+)?"))
    private val stringContent: Parser<String> = r(Regex("[^']*"))
    private val endOfWord: Parser<String> = r(Regex("([^a-zA-Z0-9_]|\$)"))

    private val wordSep: Parser<String> = scoped("word separator", "word separator expected", endOfWord.peekOnly())

    fun s(s: String): Parser<String> = string(s)

    private fun w(s: String): Parser<String> = s(s) skipR wordSep
    fun wOrW(s: String): Parser<String> = (s(s.lowercase()) or s(s.uppercase()).defer()) skipR wordSep

    private fun r(regex: Regex): Parser<String> = regex(regex)

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

    val sqlNull: Parser<Expr> = w("null").map { SqlNullExpr }

    val double: Parser<Expr> = scoped(
        scope = "double",
        parser = floatParser.map { DoubleExpr(it.toDouble()) as Expr } skipR wordSep
    )

    val int: Parser<Expr> = scoped(
        scope = "int",
        parser = r(Regex("\\d+")).map { IntExpr(it.toInt()) as Expr } skipR wordSep
    )

    val boolean: Parser<Expr> = scoped(
        scope = "boolean",
        parser = or(
            w("true").map { BoolExpr(true) },
            w("false").map { BoolExpr(false) }.defer()
        )
    )

    val quoted: Parser<Expr> = scoped(
        scope = "string literal",
        msg = "expected quoted string",
        parser = surround(s("'"), s("'"), stringContent).map { StrExpr(it) as Expr } skipR wordSep
    )

    fun <A> Parser<A>.inParentheses(): Parser<A> =
        { this }.inParentheses()

    fun <A> (() -> Parser<A>).inParentheses(): Parser<A> =
        surround(s("("), s(")"), ws skipL this skipR ws)
}