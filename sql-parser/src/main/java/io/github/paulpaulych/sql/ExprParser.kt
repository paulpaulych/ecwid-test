package io.github.paulpaulych.sql

import io.github.paulpaulych.parser.Parser
import io.github.paulpaulych.parser.TextParsers.attempt
import io.github.paulpaulych.parser.TextParsers.notEof
import io.github.paulpaulych.parser.TextParsers.optional
import io.github.paulpaulych.parser.TextParsers.oneOf
import io.github.paulpaulych.parser.TextParsers.scoped
import io.github.paulpaulych.parser.TextParsers.succeed
import io.github.paulpaulych.parser.TextParsersDsl.and
import io.github.paulpaulych.parser.TextParsersDsl.defer
import io.github.paulpaulych.parser.TextParsersDsl.flatMap
import io.github.paulpaulych.parser.TextParsersDsl.map
import io.github.paulpaulych.parser.TextParsersDsl.or
import io.github.paulpaulych.parser.TextParsersDsl.plus
import io.github.paulpaulych.parser.TextParsersDsl.sepBy
import io.github.paulpaulych.parser.TextParsersDsl.skipL
import io.github.paulpaulych.parser.TextParsersDsl.skipR
import io.github.paulpaulych.sql.CommonSqlParsers.boolean
import io.github.paulpaulych.sql.CommonSqlParsers.column
import io.github.paulpaulych.sql.CommonSqlParsers.double
import io.github.paulpaulych.sql.CommonSqlParsers.inParentheses
import io.github.paulpaulych.sql.CommonSqlParsers.int
import io.github.paulpaulych.sql.CommonSqlParsers.latinWord
import io.github.paulpaulych.sql.CommonSqlParsers.quoted
import io.github.paulpaulych.sql.CommonSqlParsers.s
import io.github.paulpaulych.sql.CommonSqlParsers.sOrS
import io.github.paulpaulych.sql.CommonSqlParsers.wildcard
import io.github.paulpaulych.sql.CommonSqlParsers.ws
import io.github.paulpaulych.sql.Expr.*
import io.github.paulpaulych.sql.Expr.LitExpr.*
import io.github.paulpaulych.sql.Expr.SelectableExpr.ColumnExpr
import io.github.paulpaulych.sql.Expr.SelectableExpr.WildcardExpr
import io.github.paulpaulych.sql.Op1Type.*
import io.github.paulpaulych.sql.Op2Type.*
import io.github.paulpaulych.sql.SqlScopes.*

class ExprParser(
    val wildcardAllowed: Boolean
) {

    private val exprParsers: List<Parser<Expr>> = listOf(
        binOperator(OR, sOrS("or"), arg = { expr(skipParsers = 1)}).attempt(),
        binOperator(AND, sOrS("and"), arg = { expr(skipParsers = 2)}).attempt(),
        unaryOperator(NOT, sOrS("not"), arg = { expr(skipParsers = 2)}),
        binOperator(EQ, s("="), arg = { expr(skipParsers = 4)}).attempt(),
        binOperator(NEQ, s("!="), arg = { expr(skipParsers = 5)}).attempt(),
        binOperator(LTE, s("<="), arg = { expr(skipParsers = 6)}).attempt(),
        binOperator(LT, s("<"), arg = { expr(skipParsers = 7)}).attempt(),
        binOperator(GTE, s(">="), arg = { expr(skipParsers = 8)}).attempt(),
        binOperator(GT, s(">"), arg = { expr(skipParsers = 9)}).attempt(),
        binOperator(PLUS, s("+"), arg = { expr(skipParsers = 10)}).attempt(),
        binOperator(MINUS, s("-"), arg = { expr(skipParsers = 11)}).attempt(),
        binOperator(MULT, s("*"), arg = { expr(skipParsers = 12)}).attempt(),
        binOperator(DIV, s("/"), arg = { expr(skipParsers = 13)}).attempt(),
        binOperator(MOD, s("%"), arg = { expr(skipParsers = 14)}).attempt(),
        unaryOperator(UN_MINUS, s("-"), arg = { expr(skipParsers = 16)}),
        unaryOperator(UN_PLUS, s("+"), arg = { expr(skipParsers = 16)}),
        { expr(skipParsers = 0) }.inParentheses(),
        literal().attempt(),
        functionCall(arg = { expr(skipParsers = 0) }).attempt(),
        columnOrWildcard()
    )

    fun expr(): Parser<Expr> =
        scoped("expression", "expression expected", expr(skipParsers = 0))

    private fun expr(skipParsers: Int): Parser<Expr> {
        val parsers = exprParsers.asSequence().drop(skipParsers)
        return notEof() skipL oneOf(parsers)
    }

    private fun binOperator(
        op2Type: Op2Type,
        typeParser: Parser<String>,
        arg: () -> Parser<Expr>
    ): Parser<Expr> {
        return (ws skipL arg skipR ws)
            .flatMap { leftSuccess ->
                typeParser
                    .map { Pair(leftSuccess, op2Type) }
                    .or { succeed(Pair(leftSuccess, null as Op2Type?)) }
            }
            .flatMap { (lhs, op) ->
                when(op) {
                   null -> succeed(lhs)
                   else -> (ws skipL arg)
                       .map { rhs -> Op2Expr(op, lhs, rhs) }
                }
            }
    }

    private fun unaryOperator(
        type: Op1Type,
        typeParser: Parser<String>,
        arg: () -> Parser<Expr>
    ): Parser<Expr> {
        return ((typeParser skipL ws).attempt() skipL arg)
            .map { operand -> Op1Expr(type, operand) }
    }

    private fun literal(): Parser<Expr> =
        s(NULL.get).map { SqlNullExpr } or
                double.map(::DoubleExpr).defer() or
                int.map(::IntExpr).defer() or
                quoted.map(::StrExpr).defer() or
                boolean.map(::BoolExpr).defer()

    private fun columnOrWildcard(): Parser<Expr> = scoped(
        scope = SELECTABLE_EXPR.get,
        msg = "expected ${COLUMN.get}${" or ${WILDCARD.get}".takeIf { wildcardAllowed } ?: ""}",
        parser = when(wildcardAllowed) {
            true -> wildcard.attempt().map(::WildcardExpr) or column.map(::ColumnExpr).defer()
            false -> column.map(::ColumnExpr)
        }
    )

    private fun functionCall(arg: () -> Parser<Expr>): Parser<Expr> {
        val args = (ws skipL arg skipR ws) sepBy s(",")
        return ((latinWord skipR s(".")).optional() + latinWord)
            .and(args.inParentheses())
            .map { (func, args) ->
                val (schema, name) = func
                FunExpr(SqlId(schema, name), args)
            }
    }

    private fun queryExpr(): Parser<QueryExpr> = TODO()
}