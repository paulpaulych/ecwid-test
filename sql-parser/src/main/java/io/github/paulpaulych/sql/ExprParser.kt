package io.github.paulpaulych.sql

import io.github.paulpaulych.parser.Parser
import io.github.paulpaulych.parser.TextParsers.attempt
import io.github.paulpaulych.parser.TextParsers.scoped
import io.github.paulpaulych.parser.TextParsersDsl.defer
import io.github.paulpaulych.parser.TextParsersDsl.map
import io.github.paulpaulych.parser.TextParsersDsl.or
import io.github.paulpaulych.sql.CommonSqlParsers.boolean
import io.github.paulpaulych.sql.CommonSqlParsers.column
import io.github.paulpaulych.sql.CommonSqlParsers.double
import io.github.paulpaulych.sql.CommonSqlParsers.int
import io.github.paulpaulych.sql.CommonSqlParsers.quoted
import io.github.paulpaulych.sql.CommonSqlParsers.s
import io.github.paulpaulych.sql.CommonSqlParsers.wildcard
import io.github.paulpaulych.sql.Expr.*
import io.github.paulpaulych.sql.Expr.LitExpr.*
import io.github.paulpaulych.sql.Expr.SelectableExpr.ColumnExpr
import io.github.paulpaulych.sql.Expr.SelectableExpr.WildcardExpr
import io.github.paulpaulych.sql.SqlScopes.*

class ExprParser(
    val wildcardAllowed: Boolean
) {

    fun expr(): Parser<Expr> = TODO()

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

    fun notExpr(): Parser<NotExpr> = TODO()

    fun andExpr(): Parser<AndExpr> = TODO()

    fun orExpr(): Parser<OrExpr> = TODO()

    fun eqExpr(): Parser<EqExpr> = TODO()

    fun neqExpr(): Parser<NeqExpr> = TODO()

    fun gExpr(): Parser<GExpr> = TODO()
    fun goeExpr(): Parser<GoeExpr> = TODO()

    fun lExpr(): Parser<LExpr> = TODO()
    fun loeExpr(): Parser<LoeExpr> = TODO()

    fun funExpr(): Parser<FunExpr> = TODO()

    fun queryExpr(): Parser<QueryExpr> = TODO()
}