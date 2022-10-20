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

sealed interface SelectableColumns {

    data class Wildcard(
        val sourceAlias: String?,
    ): SelectableColumns

    data class ExprColumn(
        val expr: Expr,
        val alias: String,
    ): SelectableColumns
}

sealed interface Expr {

    sealed interface LitExpr: Expr {
        data class IntLitExpr(val value: Int): LitExpr
        data class DoubleLitExpr(val value: Double): LitExpr
        data class StrLitExpr(val value: String): LitExpr
        data class BoolLitExpr(val value: Boolean): LitExpr
        object SqlNullExpr
    }

    data class ColumnExpr(val column: Column): Expr

    data class NotExpr(val expr: Expr): Expr

    data class OrExpr(val left: Expr, val right: Expr): Expr
    data class AndExpr(val left: Expr, val right: Expr): Expr

    data class EqExpr(val left: Expr, val right: Expr): Expr
    data class NeqExpr(val left: Expr, val right: Expr): Expr
    data class GExpr(val left: Expr, val right: Expr): Expr
    data class GOEExpr(val left: Expr, val right: Expr): Expr
    data class LExpr(val left: Expr, val right: Expr): Expr
    data class LOEExpr(val left: Expr, val right: Expr): Expr

    data class FunExpr(val func: SqlId, val args: List<Expr>): Expr

    data class QueryExpr(val query: Query): Expr
}

enum class Direction { ASC, DESC }

data class Sort(
    val column: Column,
    val direction: Direction
)
