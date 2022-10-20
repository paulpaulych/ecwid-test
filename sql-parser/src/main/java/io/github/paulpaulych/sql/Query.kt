package io.github.paulpaulych.sql

data class Query(
    private val columns: List<SelectableColumns>,
    private val fromSources: List<Source>,
    private val joins: List<Join>,
    private val whereClauses: List<Expr>,
    private val groupByColumns: List<String>,
    private val sortColumns: List<Sort>,
    private val limit: Int?,
    private val offset: Int?,
)

sealed interface Source {

    data class QuerySource(
        val query: Query,
        val alias: String
    ): Source

    data class TableSource(
        val table: SqlId,
        val alias: String,
    ): Source
}

sealed interface Join {

    data class CrossJoin(val source: Source): Join

    data class SelectiveJoin(
        val source: Source,
        val expr: Expr
    ) {
        enum class JoinType { INNER, LEFT, RIGHT }
    }
}

data class SqlId(
    val schema: String?,
    val name: String
)

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
    OR, AND, EQ, NEQ, GT, GTE, LT, LTE
}

sealed interface Expr {

    sealed interface LitExpr: Expr {
        data class IntLitExpr(val value: Int): LitExpr
        data class DoubleLitExpr(val value: Double): LitExpr
        data class StrLitExpr(val value: String): LitExpr
        data class BoolLitExpr(val value: Boolean): LitExpr
        object SqlNullExpr: LitExpr
    }

    sealed interface SelectableExpr: Expr {
        data class ColumnExpr(val column: Column): SelectableExpr
        data class WildcardExpr(val wildcard: Wildcard?): SelectableExpr
    }

    data class Op1Expr(val op: Op1Type, val arg: Expr): Expr

    data class Op2Expr(val op: Op2Type, val left: Expr, val right: Expr): Expr

    data class FunExpr(val func: SqlId, val args: List<Expr>): Expr

    data class QueryExpr(val query: Query): Expr
}

enum class Direction { ASC, DESC }

data class Sort(
    val column: Column,
    val direction: Direction
)
