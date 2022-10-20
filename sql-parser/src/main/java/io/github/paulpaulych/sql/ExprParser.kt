package io.github.paulpaulych.sql

import io.github.paulpaulych.parser.Parser
import io.github.paulpaulych.parser.TextParsers.scoped
import io.github.paulpaulych.parser.TextParsersDsl.defer
import io.github.paulpaulych.parser.TextParsersDsl.map
import io.github.paulpaulych.parser.TextParsersDsl.or
import io.github.paulpaulych.sql.CommonSqlParsers.boolean
import io.github.paulpaulych.sql.CommonSqlParsers.double
import io.github.paulpaulych.sql.CommonSqlParsers.int
import io.github.paulpaulych.sql.CommonSqlParsers.quoted
import io.github.paulpaulych.sql.CommonSqlParsers.s
import io.github.paulpaulych.sql.Expr.*
import io.github.paulpaulych.sql.Expr.LitExpr.*

class ExprParser(
    val wildcardAllowed: Boolean
) {

    fun expr(): Parser<Expr> = TODO()

    fun litExpr(): Parser<LitExpr> = scoped(
        scope = "SQL literal",
        parser = s("null").map { SqlNullExpr } or
                double.map(::DoubleLitExpr).defer() or
                int.map(::IntLitExpr).defer() or
                quoted.map(::StrLitExpr).defer() or
                boolean.map(::BoolLitExpr).defer()
    )

    fun selectableExpr(): Parser<SelectableExpr> = TODO()

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