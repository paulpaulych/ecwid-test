package io.github.paulpaulych.parser.sql

import io.github.paulpaulych.formatting.Indent
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
    override fun toString() = fmt(indent = Indent.empty())
}

data class SelectedItem(
    val expr: Expr,
    val alias: String?
) {
    override fun toString() = fmt(indent = Indent.empty())
}

enum class JoinType { CROSS, INNER, LEFT, RIGHT }

sealed interface Source {

    data class JoinSource(
        val type: JoinType,
        val condition: Expr?,
        val lhs: Source,
        val rhs: Source
    ): Source {
        override fun toString() = fmt(indent = Indent.empty())
    }

    data class SubQuerySource(
        val query: Query,
        val alias: String
    ): Source {
        override fun toString() = fmt(indent = Indent.empty())
    }

    data class SqlIdSource(
        val sqlId: SqlId,
        val alias: String?,
    ): Source {
        override fun toString() = fmt()
    }
}

/**
 * represents qualified SQL name \[schema.]name
 */
data class SqlId(
    val schema: String?,
    val name: String
) {
    override fun toString() = fmt()
}

data class Column(
    val name: String,
    val source: SqlId?,
) {
    override fun toString() = fmt()
}

enum class Op1Type {
    UN_MINUS, UN_PLUS, NOT;

    override fun toString() = fmt()
}

enum class Op2Type {
    OR, AND,

    EQ, NEQ, GT, GTE, LT, LTE,

    PLUS, MINUS, MOD, DIV, MULT;

    override fun toString() = fmt()
}

sealed interface Expr {

    data class IntExpr(val value: Int): Expr {
        override fun toString() = fmt()
    }

    data class DoubleExpr(val value: Double): Expr {
        override fun toString() = fmt()
    }

    data class StrExpr(val value: String): Expr {
        override fun toString() = fmt()
    }

    data class BoolExpr(val value: Boolean): Expr {
        override fun toString() = fmt()
    }

    object SqlNullExpr: Expr {
        override fun toString() = fmt()
    }

    data class ColumnExpr(val column: Column): Expr {
        override fun toString() = fmt()
    }

    data class FunExpr(val function: SqlId, val args: List<Expr>): Expr {
        override fun toString() = fmt(indent = Indent.empty())
    }

    data class SubQueryExpr(val query: Query): Expr {
        override fun toString() = fmt(indent = Indent.empty())
    }

    data class Op1Expr(val op: Op1Type, val arg: Expr): Expr {
        override fun toString() = fmt(indent = Indent.empty())
    }
    data class Op2Expr(val op: Op2Type, val lhs: Expr, val rhs: Expr): Expr {
        override fun toString() = fmt(indent = Indent.empty())
    }
}

enum class SortOrder { ASC, DESC }

data class Sort(
    val expr: Expr,
    val direction: SortOrder
) {
    override fun toString() = fmt(indent = Indent.empty())
}
