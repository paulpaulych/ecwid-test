package io.github.paulpaulych.sql

import io.github.paulpaulych.parser.Parser
import io.github.paulpaulych.parser.TextParsers.attempt
import io.github.paulpaulych.parser.TextParsers.optional
import io.github.paulpaulych.parser.TextParsers.scoped
import io.github.paulpaulych.parser.TextParsersDsl.and
import io.github.paulpaulych.parser.TextParsersDsl.defer
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

    fun expr(): Parser<Expr> = scoped(
        scope = "expression",
        parser = litExpr() or
                ::selectableExpr or
                ::op1Expr
    )

    fun litExpr(): Parser<LitExpr> = scoped(
        scope = LITERAL.get,
        parser = s(NULL.get).map { SqlNullExpr } or
                double.map(::DoubleLitExpr).defer() or
                int.map(::IntLitExpr).defer() or
                quoted.map(::StrLitExpr).defer() or
                boolean.map(::BoolLitExpr).defer()
    )

    fun selectableExpr(): Parser<SelectableExpr> = scoped(
        scope = SELECTABLE_EXPR.get,
        msg = "expected ${COLUMN.get}${" or ${WILDCARD.get}".takeIf { wildcardAllowed } ?: ""}",
        parser = when(wildcardAllowed) {
            true -> wildcard.attempt().map(::WildcardExpr) or column.map(::ColumnExpr).defer()
            false -> column.map(::ColumnExpr)
        }
    )


    fun op1Expr(): Parser<Op1Expr> {
        val operator = Op1Type.values().map { op ->
            when(op) {
                UN_MINUS -> s("-")
                UN_PLUS -> s("+")
                NOT -> sOrS("not")
            }.map { op }
        }.reduce { a, b -> a or { b } }

        val inParentheses = (ws skipL ::expr skipR ws).inParentheses()
        val withoutParentheses = ws skipL ::expr
        return (operator + (inParentheses or withoutParentheses.defer()))
            .map { (op, operand) -> Op1Expr(op, operand) }
    }

    fun op2Expr(): Parser<Op2Expr> {
        val operator = op2OpsParsers()
            .reduce { a, b -> a or { b } }
        return ((ws skipL ::expr) + (ws skipL operator skipR ws) + ::expr)
            .map { (a, arg2) ->
                val (arg1, op) = a
                Op2Expr(op, arg1, arg2)
            }
    }

    private fun op2OpsParsers(): List<Parser<Op2Type>> {
        // TODO: extract to constant
        // to check more concrete templates firstly
        val fixedOrder = listOf(OR, AND, EQ, NEQ, GTE, GT, LTE, LT)

        Op2Type.values().forEach { op ->
            check(op in fixedOrder) {
                "order or operator $op not defined"
            }
        }

        return fixedOrder
            .map { op ->
                when(op) {
                    OR -> sOrS("or")
                    AND -> sOrS("and")
                    EQ -> s("=")
                    NEQ -> s("!=")
                    GT -> s(">")
                    GTE -> s(">=")
                    LTE -> s("<=")
                    LT -> s("<")
                }.map { op }
            }
    }

    fun funExpr(): Parser<FunExpr> =
        (latinWord skipR s(".")).optional()
            .and(latinWord)
            .and(((ws skipL ::expr skipR ws) sepBy s(",")).inParentheses())
            .map { (a, args) ->
                val (schema, name) = a
                FunExpr(SqlId(schema, name), args)
            }

    fun queryExpr(): Parser<QueryExpr> = TODO()
}