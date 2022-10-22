package io.github.paulpaulych.sql

import io.github.paulpaulych.common.Either.Left
import io.github.paulpaulych.parser.ErrorItem.ParseError
import io.github.paulpaulych.parser.Parser
import io.github.paulpaulych.parser.StackTrace
import io.github.paulpaulych.parser.TextParsers.optional
import io.github.paulpaulych.parser.TextParsers.peekOnly
import io.github.paulpaulych.parser.TextParsers.regex
import io.github.paulpaulych.parser.TextParsers.scoped
import io.github.paulpaulych.parser.TextParsers.string
import io.github.paulpaulych.parser.TextParsers.succeed
import io.github.paulpaulych.parser.TextParsersDsl.and
import io.github.paulpaulych.parser.TextParsersDsl.defer
import io.github.paulpaulych.parser.TextParsersDsl.flatMap
import io.github.paulpaulych.parser.TextParsersDsl.many
import io.github.paulpaulych.parser.TextParsersDsl.map
import io.github.paulpaulych.parser.TextParsersDsl.or
import io.github.paulpaulych.parser.TextParsersDsl.plus
import io.github.paulpaulych.parser.TextParsersDsl.sepBy1
import io.github.paulpaulych.parser.TextParsersDsl.skipL
import io.github.paulpaulych.parser.TextParsersDsl.skipR
import io.github.paulpaulych.parser.TextParsersDsl.surround
import io.github.paulpaulych.sql.Expr.LitExpr.*
import io.github.paulpaulych.sql.SortOrder.ASC
import io.github.paulpaulych.sql.SortOrder.DESC

@Suppress("RegExpSimplifiable")
object CommonSqlParsers {

    val ws: Parser<String> = r(Regex("[\u0020\u0009\u000A\u000D]*"))
    val ws1: Parser<String> = scoped("space", "space expected", r(Regex("[\u0020\u0009\u000A\u000D]+")))

    private val latinWord: Parser<String> = r(Regex("[a-zA-Z_][a-zA-Z0-9_]*"))
    private val floatParser: Parser<String> = r(Regex("\\d+\\.\\d+([eE][-+]?\\d+)?"))
    private val stringContent: Parser<String> = r(Regex("[^']*"))
    private val wordSep: Parser<String> = scoped(
        scope = "word separator",
        msg = "word separator expected",
        parser = r(Regex("([^a-zA-Z0-9_]|\$)")).peekOnly()
    )

    fun s(s: String): Parser<String> = string(s)

    fun wOrW(s: String): Parser<String> = (s(s.lowercase()) or s(s.uppercase()).defer()) skipR wordSep
    private fun w(s: String): Parser<String> = s(s) skipR wordSep

    val anyWord: Parser<String> = latinWord skipR wordSep

    private fun r(regex: Regex): Parser<String> = regex(regex)

    val column: Parser<Column> = scoped(
        scope = "column",
        parser =
            (latinWord skipR s(".")).optional()
                .and((latinWord skipR s(".")).optional())
                .and(latinWord)
                .map { (sqlId, name) ->
                    Column(
                        name = name,
                        source = sqlId.second
                            ?.let { SqlId(sqlId.first, it) }
                            ?: sqlId.first?.let { SqlId(null, it) },
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

    val comma: Parser<String> = s(",")

    val intValue = scoped(
        scope = "int",
        parser = r(Regex("\\d+")).map { it.toInt() } skipR wordSep
    )

    val int: Parser<Expr> = intValue.map { IntExpr(it) }

    val boolean: Parser<Expr> = scoped(
        scope = "boolean",
        parser = or(
            w("true").map { BoolExpr(true) },
            w("false").map { BoolExpr(false) }.defer()
        )
    )

    val sqlId: Parser<SqlId> = scoped(
        scope = "table or function",
        msg = "table or function expected",
        parser = ((latinWord skipR s(".")).optional() + latinWord)
            .map { (schema, name) -> SqlId(schema, name) }
    )

    val quoted: Parser<Expr> = scoped(
        scope = "string literal",
        msg = "expected quoted string",
        parser = surround(s("'"), s("'"), stringContent).map { StrExpr(it) as Expr } skipR wordSep
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

    private fun <A> failed(scope: String, msg: String) = Parser<A> { state ->
        Left(StackTrace(state, ParseError(scope, msg)))
    }

    fun Keyword.parser(): Parser<String> = wOrW(this.value)

    fun Parser<String>.excludingKeywords(): Parser<String> =
        this.flatMap { value ->
            if (value in Keyword.ALL) {
                failed("not keyword", "$value not allowed here")
            } else {
                succeed(value)
            }
        }

    fun <A> Parser<A>.inParentheses(): Parser<A> =
        { this }.inParentheses()

    fun <A> (() -> Parser<A>).inParentheses(): Parser<A> =
        surround(s("("), s(")"), ws skipL this skipR ws)
}