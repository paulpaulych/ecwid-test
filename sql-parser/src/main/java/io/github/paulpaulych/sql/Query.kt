package io.github.paulpaulych.sql

data class Query(
    private val columns: List<SelectableColumns>,
    private val source: Source,
    //TODO and as ','?
    private val whereClauses: List<Expr>,
    private val groupByColumns: List<String>,
    private val sortColumns: List<Sort>,
    private val limit: Int?,
    private val offset: Int?,
)

enum class JoinType { CROSS, INNER, LEFT, RIGHT }

sealed interface Source {

    data class JoinSource(
        val type: JoinType,
        val condition: Expr?,
        val lhs: Source,
        val rhs: Source
    ): Source {
        override fun toString() = "${type.name}_JOIN(l=$lhs,r=$rhs)"
    }

    data class QuerySource(
        val query: Query,
        val alias: String
    ): Source {
        override fun toString() = "TODO: implement Query.toString()"
    }

    data class SqlIdSource(
        val sqlId: SqlId,
        val alias: String?,
    ): Source {
        override fun toString() = alias?.let { "$sqlId $it" } ?: sqlId.toString()
    }
}

/**
 * represents qualified SQL name \[schema.]name
 */
data class SqlId(
    val schema: String?,
    val name: String
) {
    override fun toString() = schema?.let { "$it.$name" } ?: name
}

data class Column(
    val name: String,
    val source: String?
)

data class Wildcard(
    val source: String?,
): SelectableColumns

sealed interface SelectableColumns {

    data class ColumnsSet(
        val wildcard: Wildcard
    ): SelectableColumns

    data class ExprColumn(
        val expr: Expr,
        val alias: String,
    ): SelectableColumns
}

enum class Op1Type {
    UN_MINUS, UN_PLUS, NOT
}

enum class Op2Type {
    OR, AND,

    EQ, NEQ, GT, GTE, LT, LTE,

    PLUS, MINUS, MOD, DIV, MULT
}

sealed interface Expr {

    sealed interface ValueExpr: Expr

    sealed interface LitExpr: ValueExpr {
        data class IntExpr(val value: Int): LitExpr
        data class DoubleExpr(val value: Double): LitExpr
        data class StrExpr(val value: String): LitExpr
        data class BoolExpr(val value: Boolean): LitExpr
        object SqlNullExpr: LitExpr
    }

    sealed interface SelectableExpr: ValueExpr {
        data class ColumnExpr(val column: Column): SelectableExpr
        data class WildcardExpr(val wildcard: Wildcard?): SelectableExpr
    }

    data class FunExpr(val function: SqlId, val args: List<Expr>): ValueExpr
    data class QueryExpr(val query: Query): ValueExpr

    data class Op1Expr(val op: Op1Type, val arg: Expr): Expr
    data class Op2Expr(val op: Op2Type, val left: Expr, val right: Expr): Expr
}

enum class Direction { ASC, DESC }

data class Sort(
    val column: Column,
    val direction: Direction
)
