package io.github.paulpaulych.formatting

import io.github.paulpaulych.parser.sql.*
import io.github.paulpaulych.parser.sql.Expr.*
import io.github.paulpaulych.parser.sql.Op2Type.*
import io.github.paulpaulych.parser.sql.Source.*

private val ls = System.lineSeparator()

fun Query.fmt(
    indent: Indent
): String {
    return indent + "SELECT" + ls +
            fmtColumns(indent, columns) +
            ls + indent + "FROM ${source.fmt(indent)}" +
            (where
                ?.let { ls + indent + "WHERE ${it.fmt(indent)}" }
                ?: "") +
            (groupBy.takeIf { it.isNotEmpty() }
                ?.let { ls + indent + "GROUP BY ${it.joinToString(", ")}" }
                ?: "") +
            (having
                ?.let { ls + indent + "HAVING ${it.fmt(indent)}" }
                ?: "") +
            (sorts.takeIf { it.isNotEmpty() }
                ?.let { sorts -> ls + indent + "ORDER BY ${ sorts.joinToString(", ") { sort -> sort.fmt(indent) }}" }
                ?: "") +
            (limit
                ?.let { ls + indent + "LIMIT $it"}
                ?: "") +
            (offset
                ?.let { ls + indent + "OFFSET $it" }
                ?: "")
}

fun SelectedItem.fmt(indent: Indent): String {
    val expr = expr.fmt(indent)
    return indent.append() + (alias?.let { "$expr as $it" } ?: expr)
}

fun JoinSource.fmt(indent: Indent): String =
    "${lhs.fmt(indent)}$ls${indent.append()}${type.name} JOIN ${rhs.fmt(indent)}" + (condition?.let { " ON $it" } ?: "")

fun SubQuerySource.fmt(indent: Indent): String =
    "${fmtSubQuery(indent, query)} AS $alias"

fun SqlIdSource.fmt(): String = alias?.let { "$sqlId $it" } ?: sqlId.toString()

fun SqlId.fmt(): String = schema?.let { "$it.$name" } ?: name

fun Column.fmt(): String = source?.let { "$it.$name" } ?: name

fun Op1Type.fmt(): String = when(this) {
    Op1Type.UN_MINUS -> "-"
    Op1Type.UN_PLUS -> "+"
    Op1Type.NOT -> "NOT"
}

fun Op2Type.fmt(): String = when(this) {
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

fun IntExpr.fmt(): String = value.toString()

fun DoubleExpr.fmt(): String = value.toString()

fun StrExpr.fmt(): String = value

fun BoolExpr.fmt(): String = when(this.value) {
    true -> Keyword.TRUE.value.lowercase()
    false -> Keyword.FALSE.value.lowercase()
}

@Suppress("unused")
fun SqlNullExpr.fmt(): String =
    Keyword.NULL.value.uppercase()

fun ColumnExpr.fmt(): String =
    column.fmt()

fun FunExpr.fmt(indent: Indent): String =
    "$function(${args.joinToString(", ") { it.fmt(indent) }})"

fun SubQueryExpr.fmt(indent: Indent): String =
    fmtSubQuery(indent, query)

fun Op1Expr.fmt(indent: Indent): String =
    "${op.fmt()}(${arg.fmt(indent)})"

fun Op2Expr.fmt(indent: Indent): String =
    when(this.op) {
        OR -> "${lhs.fmt(indent)}$ls${indent.append()}$op ${rhs.fmt(indent)}"
        AND -> "${lhs.fmt(indent)}$ls${indent.append().append()}$op ${rhs.fmt(indent)}"
        EQ, NEQ, GT, GTE, LT,
        LTE, PLUS, MINUS, MOD, DIV,
        MULT -> "${lhs.fmt(indent)} $op ${rhs.fmt(indent)}"
    }

fun Sort.fmt(indent: Indent) = "${expr.fmt(indent)} $direction"

private fun fmtSubQuery(indent: Indent, query: Query) = "($ls${query.fmt(indent.append().append())}$ls${indent.append()})"

private fun fmtColumns(
    indent: Indent,
    columns: List<SelectedItem>
): String {
    return columns.joinToString(",$ls") { it.fmt(indent) }
}

private fun Source.fmt(indent: Indent) = when(this) {
    is JoinSource -> fmt(indent)
    is SqlIdSource -> fmt()
    is SubQuerySource -> fmt(indent)
}

private fun Expr.fmt(indent: Indent) = when(this) {
    is BoolExpr -> fmt()
    is ColumnExpr -> fmt()
    is DoubleExpr -> fmt()
    is FunExpr -> fmt(indent)
    is IntExpr -> fmt()
    is Op1Expr -> fmt(indent)
    is Op2Expr -> fmt(indent)
    is SubQueryExpr -> fmt(indent)
    is SqlNullExpr -> fmt()
    is StrExpr -> fmt()
}