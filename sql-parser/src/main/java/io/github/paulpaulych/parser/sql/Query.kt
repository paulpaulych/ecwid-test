package io.github.paulpaulych.parser.sql

import io.github.paulpaulych.formatting.fmt


data class Query(
    val columns: List<SelectedItem>,
    val source: Source,
    val where: Expr?,
    val groupBy: List<Column>,
    val having: Expr?,
    val sorts: List<Sort>,
    val limit: Int?,
    val offset: Int?,
) {
    override fun toString() = fmt()
}

data class SelectedItem(
    val expr: Expr,
    val alias: String?
)

enum class JoinType { CROSS, INNER, LEFT, RIGHT }

sealed interface Source {

    data class JoinSource(
        val type: JoinType,
        val condition: Expr?,
        val lhs: Source,
        val rhs: Source
    ): Source

    data class SubQuerySource(
        val query: Query,
        val alias: String
    ): Source

    data class SqlIdSource(
        val sqlId: SqlId,
        val alias: String?,
    ): Source
}

/**
 * represents qualified SQL name \[schema.]name
 */
data class SqlId(
    val schema: String?,
    val name: String
)

data class Column(
    val name: String,
    val source: SqlId?,
)

enum class Op1Type {
    UN_MINUS, UN_PLUS, NOT
}

enum class Op2Type {
    OR, AND,

    EQ, NEQ, GT, GTE, LT, LTE,

    PLUS, MINUS, MOD, DIV, MULT
}

sealed interface Expr {
    object SqlNullExpr: Expr
    data class IntExpr(val value: Int): Expr
    data class DoubleExpr(val value: Double): Expr
    data class StrExpr(val value: String): Expr
    data class BoolExpr(val value: Boolean): Expr
    data class ColumnExpr(val column: Column): Expr
    data class FunExpr(val function: SqlId, val args: List<Expr>): Expr
    data class SubQueryExpr(val query: Query): Expr
    data class Op1Expr(val op: Op1Type, val arg: Expr): Expr
    data class Op2Expr(val op: Op2Type, val lhs: Expr, val rhs: Expr): Expr
}

enum class SortOrder { ASC, DESC }

data class Sort(
    val expr: Expr,
    val direction: SortOrder
)
