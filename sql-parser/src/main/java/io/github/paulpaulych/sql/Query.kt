package io.github.paulpaulych.sql

import java.lang.System.lineSeparator

data class Query(
    private val columns: List<SelectedItem>,
    private val source: Source,
    private val where: Expr?,
    private val groupBy: List<Column>,
    private val having: Expr?,
    private val sorts: List<Sort>,
    private val limit: Int?,
    private val offset: Int?,
) {
    override fun toString() =
        "SELECT ${columns.joinToString(", ")}" + lineSeparator() +
        "FROM $source" + lineSeparator() +
        (where?.let { "WHERE $it" + lineSeparator() } ?: "") +
        (groupBy.takeIf { it.isNotEmpty() }?.let { "GROUP BY ${it.joinToString(", ")}}" + lineSeparator() } ?: "") +
        (having?.let { "HAVING $it" + lineSeparator() } ?: "") +
        (sorts.takeIf { it.isNotEmpty() }?.let { "ORDER BY ${it.joinToString(", ")}}" + lineSeparator() } ?: "") +
        (limit?.let { "LIMIT $it" + lineSeparator() } ?: "") +
        (offset?.let { "OFFSET $it" } ?: "")
}
data class SelectedItem(
    val expr: Expr,
    val alias: String?
) {
    override fun toString() = alias?.let { "$expr as $it" } ?: expr.toString()
}

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
        override fun toString() = "($query) as $alias"
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
    val source: SqlId?,
) {
    override fun toString() = source?.let { "$it.$name" } ?: name
}

data class Wildcard(
    val source: String?,
) {
    override fun toString() = source?.let { "$it.*" } ?: "*"
}

enum class Op1Type {
    UN_MINUS, UN_PLUS, NOT;

    override fun toString(): String = when(this) {
        UN_MINUS -> "-"
        UN_PLUS -> "+"
        NOT -> "NOT"
    }
}

enum class Op2Type {
    OR, AND,

    EQ, NEQ, GT, GTE, LT, LTE,

    PLUS, MINUS, MOD, DIV, MULT;

    override fun toString(): String = when(this) {
        OR -> "OR"
        AND -> "AND"
        EQ -> "="
        NEQ -> "!="
        GT -> ">"
        GTE -> ">="
        LT -> "<"
        LTE -> "<="
        PLUS -> "+"
        MINUS -> "-"
        MOD -> "%"
        DIV -> "/"
        MULT -> "*"
    }
}

sealed interface Expr {

    sealed interface ValueExpr: Expr

    sealed interface LitExpr: ValueExpr {
        data class IntExpr(val value: Int): LitExpr {
            override fun toString() = value.toString()
        }
        data class DoubleExpr(val value: Double): LitExpr {
            override fun toString() = value.toString()
        }
        data class StrExpr(val value: String): LitExpr {
            override fun toString() = value
        }
        data class BoolExpr(val value: Boolean): LitExpr {
            override fun toString() = value.toString()
        }
        object SqlNullExpr: LitExpr {
            override fun toString() = "null"
        }
    }

    sealed interface SelectableExpr: ValueExpr {
        data class ColumnExpr(val column: Column): SelectableExpr {
            override fun toString() = column.toString()
        }
        data class WildcardExpr(val wildcard: Wildcard): SelectableExpr {
            override fun toString() = wildcard.toString()
        }
    }

    data class FunExpr(val function: SqlId, val args: List<Expr>): ValueExpr {
        override fun toString() = "$function(${args.joinToString(", ")})"
    }
    data class QueryExpr(val query: Query): ValueExpr {
        override fun toString() = "($query)"
    }

    data class Op1Expr(val op: Op1Type, val arg: Expr): Expr {
        override fun toString() = "$op($arg)"
    }
    data class Op2Expr(val op: Op2Type, val lhs: Expr, val rhs: Expr): Expr {
        override fun toString() = "($lhs $op $rhs)"
    }
}

enum class SortOrder { ASC, DESC }

data class Sort(
    val expr: Expr,
    val direction: SortOrder
)
