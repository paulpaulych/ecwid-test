package io.github.paulpaulych.sql

import io.github.paulpaulych.parser.Parser
import io.github.paulpaulych.parser.TextParsers.attempt
import io.github.paulpaulych.parser.TextParsers.notEof
import io.github.paulpaulych.parser.TextParsers.oneOf
import io.github.paulpaulych.parser.TextParsers.scoped
import io.github.paulpaulych.parser.TextParsers.succeed
import io.github.paulpaulych.parser.TextParsersDsl.and
import io.github.paulpaulych.parser.TextParsersDsl.defer
import io.github.paulpaulych.parser.TextParsersDsl.flatMap
import io.github.paulpaulych.parser.TextParsersDsl.map
import io.github.paulpaulych.parser.TextParsersDsl.or
import io.github.paulpaulych.parser.TextParsersDsl.sepBy
import io.github.paulpaulych.parser.TextParsersDsl.skipL
import io.github.paulpaulych.parser.TextParsersDsl.skipR
import io.github.paulpaulych.sql.CommonSqlParsers.boolean
import io.github.paulpaulych.sql.CommonSqlParsers.column
import io.github.paulpaulych.sql.CommonSqlParsers.double
import io.github.paulpaulych.sql.CommonSqlParsers.inParentheses
import io.github.paulpaulych.sql.CommonSqlParsers.int
import io.github.paulpaulych.sql.CommonSqlParsers.quoted
import io.github.paulpaulych.sql.CommonSqlParsers.s
import io.github.paulpaulych.sql.CommonSqlParsers.sqlId
import io.github.paulpaulych.sql.CommonSqlParsers.sqlNull
import io.github.paulpaulych.sql.CommonSqlParsers.wOrW
import io.github.paulpaulych.sql.CommonSqlParsers.wildcard
import io.github.paulpaulych.sql.CommonSqlParsers.ws
import io.github.paulpaulych.sql.Expr.*
import io.github.paulpaulych.sql.Expr.SelectableExpr.ColumnExpr
import io.github.paulpaulych.sql.Expr.SelectableExpr.WildcardExpr
import io.github.paulpaulych.sql.Op1Type.*
import io.github.paulpaulych.sql.Op2Type.*
import io.github.paulpaulych.sql.SqlScopes.*

class ExprParser(
    val wildcardAllowed: Boolean
) {

    // TODO: parse operators with same precedence as list
    private val exprParsers: List<Parser<Expr>> = listOf(
        binOperator(OR, wOrW("or"), arg1 = { expr(skipParsers = 1) }, arg2 = { expr(skipParsers = 0) }).attempt(),
        binOperator(AND, wOrW("and"), arg1 = { expr(skipParsers = 2) }, arg2 = { expr(skipParsers = 1) }).attempt(),
        unaryOperator(NOT, wOrW("not"), arg = { expr(skipParsers = 2) }),
        binOperator(EQ, s("="), arg1 = { expr(skipParsers = 4) }, arg2 = { expr(skipParsers = 3) }).attempt(),
        binOperator(NEQ, s("!="), arg1 = { expr(skipParsers = 5) }, arg2 = { expr(skipParsers = 4) }).attempt(),

        binOperator(LTE, s("<="), arg1 = { expr(skipParsers = 6) }, arg2 = { expr(skipParsers = 5) }).attempt(),
        binOperator(LT, s("<"), arg1 = { expr(skipParsers = 7) }, arg2 = { expr(skipParsers = 6) }).attempt(),
        binOperator(GTE, s(">="), arg1 = { expr(skipParsers = 8) }, arg2 = { expr(skipParsers = 7) }).attempt(),
        binOperator(GT, s(">"), arg1 = { expr(skipParsers = 9) }, arg2 = { expr(skipParsers = 8) }).attempt(),
        binOperator(PLUS, s("+"), arg1 = { expr(skipParsers = 10) }, arg2 = { expr(skipParsers = 9) }).attempt(),

        binOperator(MINUS, s("-"), arg1 = { expr(skipParsers = 11) }, arg2 = { expr(skipParsers = 10) }).attempt(),
        binOperator(MULT, s("*"), arg1 = { expr(skipParsers = 12) }, arg2 = { expr(skipParsers = 11) }).attempt(),
        binOperator(DIV, s("/"), arg1 = { expr(skipParsers = 13) }, arg2 = { expr(skipParsers = 11) }).attempt(),
        binOperator(MOD, s("%"), arg1 = { expr(skipParsers = 14) }, arg2 = { expr(skipParsers = 11) }).attempt(),
        unaryOperator(UN_MINUS, s("-"), arg = { expr(skipParsers = 16) }),

        unaryOperator(UN_PLUS, s("+"), arg = { expr(skipParsers = 16) }),
        { expr(skipParsers = 0) }.inParentheses(),
        sqlNull.attempt(),
        double,
        int,
        quoted,
        boolean.attempt(),
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
        arg1: () -> Parser<Expr>,
        arg2: () -> Parser<Expr>
    ): Parser<Expr> {
        return (ws skipL arg1 skipR ws)
            .flatMap { leftSuccess ->
                typeParser
                    .map { Pair(leftSuccess, op2Type) }
                    .or { succeed(Pair(leftSuccess, null as Op2Type?)) }
            }
            .flatMap { (lhs, op) ->
                when(op) {
                   null -> succeed(lhs)
                   else -> (ws skipL arg2)
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
        return sqlId
            .and(args.inParentheses())
            .map { (sqlId, args) -> FunExpr(sqlId, args) }
    }

    private fun queryExpr(): Parser<QueryExpr> = TODO()
}