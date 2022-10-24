package io.github.paulpaulych.parser.sql

import io.github.paulpaulych.parser.lib.Parser
import io.github.paulpaulych.parser.lib.TextParsers.optional
import io.github.paulpaulych.parser.lib.TextParsers.peekOnly
import io.github.paulpaulych.parser.lib.TextParsers.regex
import io.github.paulpaulych.parser.lib.TextParsers.scoped
import io.github.paulpaulych.parser.lib.TextParsers.string
import io.github.paulpaulych.parser.lib.TextParsers.succeed
import io.github.paulpaulych.parser.lib.TextParsersDsl.and
import io.github.paulpaulych.parser.lib.TextParsersDsl.defer
import io.github.paulpaulych.parser.lib.TextParsersDsl.failed
import io.github.paulpaulych.parser.lib.TextParsersDsl.flatMap
import io.github.paulpaulych.parser.lib.TextParsersDsl.map
import io.github.paulpaulych.parser.lib.TextParsersDsl.or
import io.github.paulpaulych.parser.lib.TextParsersDsl.plus
import io.github.paulpaulych.parser.lib.TextParsersDsl.sepBy1
import io.github.paulpaulych.parser.lib.TextParsersDsl.skipL
import io.github.paulpaulych.parser.lib.TextParsersDsl.skipR
import io.github.paulpaulych.parser.lib.TextParsersDsl.surround
import io.github.paulpaulych.parser.sql.Expr.*
import io.github.paulpaulych.parser.sql.SortOrder.ASC
import io.github.paulpaulych.parser.sql.SortOrder.DESC

@Suppress("RegExpSimplifiable")
object CommonSqlParsers {

    val ws: Parser<String> = r(Regex("[\u0020\u0009\u000A\u000D]*"))
    val ws1: Parser<String> = scoped("space", "space expected", r(Regex("[\u0020\u0009\u000A\u000D]+")))

    private val floatParser: Parser<String> = r(Regex("\\d+\\.\\d+([eE][-+]?\\d+)?"))
    private val stringContent: Parser<String> = r(Regex("[^']*"))

    private val wordSep: Parser<String> = scoped(
        scope = "word separator",
        msg = "word separator expected",
        parser = r(Regex("([^a-zA-Z0-9_]|\$)")).peekOnly()
    )

    fun s(s: String): Parser<String> = string(s)

    private fun wOrW(s: String): Parser<String> = (s(s.lowercase()) or s(s.uppercase()).defer()) skipR wordSep

    private val star: Parser<String> = s("*") skipR wordSep

    private val wordWithoutSep: Parser<String> = r(Regex("[a-zA-Z_][a-zA-Z0-9_]*")).excludingKeywords()
    val anyWord: Parser<String> = wordWithoutSep skipR wordSep

    private fun r(regex: Regex): Parser<String> = regex(regex)

    val column: Parser<Column> = scoped(
        scope = "column",
        parser =
            (wordWithoutSep skipR s(".")).optional()
                .and((wordWithoutSep skipR s(".")).optional())
                .and((anyWord or { star }))
                .map { (sqlId, name) ->
                    Column(
                        name = name,
                        source = sqlId.second
                            ?.let { SqlId(sqlId.first, it) }
                            ?: sqlId.first?.let { SqlId(null, it) },
                    )
                }
    )

    val sqlNull: Parser<Expr> = Keyword.NULL.parser().map { SqlNullExpr }

    val double: Parser<Expr> = scoped(
        scope = "double",
        parser = (floatParser skipR wordSep).map { DoubleExpr(it.toDouble()) }
    )

    val comma: Parser<String> = s(",")

    val intValue = scoped(
        scope = "int",
        parser = r(Regex("\\d+")).map { it.toInt() } skipR wordSep
    )

    val int: Parser<Expr> = intValue.map { IntExpr(it) }

    val boolean: Parser<Expr> = scoped(
        scope = "boolean",
        parser = or(
            Keyword.TRUE.parser().map { BoolExpr(true) },
            Keyword.FALSE.parser().map { BoolExpr(false) }.defer()
        )
    )

    val sqlId: Parser<SqlId> = scoped(
        scope = "table or function",
        msg = "table or function expected",
        parser = ((wordWithoutSep skipR s(".")).optional() + wordWithoutSep)
            .map { (schema, name) -> SqlId(schema, name) }
    )

    val quoted: Parser<Expr> = scoped(
        scope = "string literal",
        msg = "expected quoted string",
        parser = surround(s("'"), s("'"), stringContent).skipR(wordSep).map (::StrExpr)
    )

    val sortOrder: Parser<SortOrder> = scoped(
        scope = "sort order",
        parser = Keyword.ASC.parser().map { ASC }
            .or { Keyword.DESC.parser().map { DESC } }
            .or { succeed(ASC) }
    )

    val columnsByComma: Parser<List<Column>> = scoped(
        scope = "columns sep by comma",
        msg = "columns sep by comma expected",
        parser = (ws skipL column skipR ws).sepBy1(comma)
    )

    fun Keyword.parser(): Parser<String> {
        val scope = "keyword ${this.value.uppercase()}"
        return scoped(
            scope = scope,
            msg = "$scope expected",
            parser = wOrW(this.value)
        )
    }

    fun <A> Parser<A>.inParentheses(): Parser<A> =
        { this }.inParentheses()

    fun <A> (() -> Parser<A>).inParentheses(): Parser<A> =
        surround(s("("), s(")"), ws skipL this skipR ws)

    private fun Parser<String>.excludingKeywords(): Parser<String> =
        this.flatMap { value ->
            if (value in Keyword.ALL) {
                failed("not keyword", "$value not allowed here")
            } else {
                succeed(value)
            }
        }
}