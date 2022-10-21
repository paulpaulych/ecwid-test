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
        op2Parser(OR, sOrS("or"), { expr(1)}).attempt() ,
        op2Parser(AND, sOrS("and"), { expr(2)}).attempt(),
        op1Parser(NOT, sOrS("not"), { expr(2)}),
        op2Parser(EQ, s("="), { expr(4)}).attempt(),
        op2Parser(NEQ, s("!="), { expr(5)}).attempt(),
        op2Parser(LTE, s("<="), { expr(6)}).attempt(),
        op2Parser(LT, s("<"), { expr(7)}).attempt(),
        op2Parser(GTE, s(">="), { expr(8)}).attempt(),
        op2Parser(GT, s(">"), { expr(9)}).attempt(),
        op2Parser(PLUS, s("+"), { expr(10)}).attempt(),
        op2Parser(MINUS, s("-"), { expr(11)}).attempt(),
        op2Parser(MULTIPLY, s("*"), { expr(12)}).attempt(),
        op2Parser(DIV, s("/"), { expr(13)}).attempt(),
        op1Parser(UN_MINUS, s("-"), { expr(14)}),
        op1Parser(UN_PLUS, s("+"), { expr(15)}),

        { expr(skipParsers = 0) }.inParentheses(),
        litExpr().attempt(),
        funExpr({ expr(skipParsers = 0) }).attempt(),
        selectableExpr()
    )

    fun expr(): Parser<Expr> =
        scoped("expression", "expression expected", expr(skipParsers = 0))

    private fun expr(skipParsers: Int): Parser<Expr> {
        val parsers = exprParsers.asSequence().drop(skipParsers)
        return notEof() skipL oneOf(parsers)
    }

    private fun op2Parser(
        op2Type: Op2Type,
        typeParser: Parser<String>,
        subExprParser: () -> Parser<Expr>
    ): Parser<Expr> {
        return (ws skipL subExprParser skipR ws)
            .flatMap { leftSuccess ->
                typeParser
                    .map { Pair(leftSuccess, op2Type) }
                    .or { succeed(Pair(leftSuccess, null as Op2Type?)) }
            }
            .flatMap { (lhs, op) ->
                when(op) {
                   null -> succeed(lhs)
                   else -> (ws skipL subExprParser)
                       .map { rhs -> Op2Expr(op, lhs, rhs) }
                }
            }
    }

    private fun op1Parser(
        type: Op1Type,
        typeParser: Parser<String>,
        subExprParser: () -> Parser<Expr>
    ): Parser<Expr> {
        return ((typeParser skipL ws).attempt() skipL subExprParser)
            .map { operand -> Op1Expr(type, operand) }
    }

    private fun litExpr(): Parser<Expr> =
        s(NULL.get).map { SqlNullExpr } or
                double.map(::DoubleExpr).defer() or
                int.map(::IntExpr).defer() or
                quoted.map(::StrExpr).defer() or
                boolean.map(::BoolExpr).defer()

    private fun selectableExpr(): Parser<Expr> = scoped(
        scope = SELECTABLE_EXPR.get,
        msg = "expected ${COLUMN.get}${" or ${WILDCARD.get}".takeIf { wildcardAllowed } ?: ""}",
        parser = when(wildcardAllowed) {
            true -> wildcard.attempt().map(::WildcardExpr) or column.map(::ColumnExpr).defer()
            false -> column.map(::ColumnExpr)
        }
    )

    private fun funExpr(subExprParser: () -> Parser<Expr>): Parser<Expr> {
        val arg = ws skipL subExprParser skipR ws
        val args = arg sepBy s(",")
        return ((latinWord skipR s(".")).optional() + latinWord)
            .and(args.inParentheses())
            .map { (func, args) ->
                val (schema, name) = func
                FunExpr(SqlId(schema, name), args)
            }
    }

    private fun queryExpr(): Parser<QueryExpr> = TODO()
}