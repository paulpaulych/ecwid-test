package io.github.paulpaulych.sql

import io.github.paulpaulych.sql.Selectable.Column

data class Query(
    private val columns: List<Selectable>,
    private val fromSources: List<Source>,
    private val joins: List<Join>,
    private val whereClauses: List<Expr>,
    private val groupByColumns: List<String>,
    private val sortColumns: List<Sort>,
    private val limit: Int?,
    private val offset: Int?,
)

sealed interface Source {
    data class QuerySource(val query: Query): Source
    data class TableSource(val table: SqlId): Source
}

enum class JoinType {
    INNER, CROSS, LEFT, RIGHT
}

data class Join(
    val type: JoinType,
    val source: Source
)

data class SqlId(
    val schema: String?,
    val name: String
)

sealed interface Expr {

    sealed interface LitExpr: Expr {
        data class IntLitExpr(val value: Int): LitExpr
        data class DoubleLitExpr(val value: Double): LitExpr
        data class StrLitExpr(val value: String): LitExpr
        data class BoolLitExpr(val value: Boolean): LitExpr
        object SqlNullExpr
    }

    data class NotExpr(val expr: Expr): Expr

    data class OrExpr(val left: Expr, val right: Expr): Expr
    data class AndExpr(val left: Expr, val right: Expr): Expr

    data class EqExpr(val left: Expr, val right: Expr): Expr
    data class NeqExpr(val left: Expr, val right: Expr): Expr
    data class GExpr(val left: Expr, val right: Expr): Expr
    data class GOEExpr(val left: Expr, val right: Expr): Expr
    data class LExpr(val left: Expr, val right: Expr): Expr
    data class LOEExpr(val left: Expr, val right: Expr): Expr

    data class ColExpr(val name: String): Expr

    data class FunExpr(val func: SqlId, val args: List<Expr>): Expr

}

sealed interface Selectable {

    data class Wildcard(
        val sourceAlias: String?,
    ): Selectable

    data class Column(
        val name: String,
        val alias: String,
        val sourceAlias: String
    ): Selectable
}

enum class Direction {
    ASC, DESC
}

data class Sort(
    val column: Column,
    val direction: Direction
)
