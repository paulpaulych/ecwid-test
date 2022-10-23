package io.github.paulpaulych.sql

import io.github.paulpaulych.sql.Expr.*
import io.github.paulpaulych.sql.Source.*
import java.lang.System.lineSeparator

data class Indent(
    val value: String
) {
    fun append() = Indent("$value  ")

    companion object {
        fun empty() = Indent("")
    }

    operator fun plus(indent: Indent): Indent =
        Indent(this.value + indent.value)

    operator fun plus(s: String): String =
        this.value + s

    override fun toString() = value
}

private operator fun String.plus(indent: Indent) = this + indent.value

fun Query.fmt(
    indent: Indent
): String {
    val ls = lineSeparator()
    val lsWithIndent = lineSeparator() + indent
    val lsWithIndent1 = lineSeparator() + indent + "\t"
    return indent + "SELECT ${columns.joinToString(", ")}" +
            lineSeparator() + indent + "FROM $source" +
            (where?.let { lineSeparator() + "WHERE $it" } ?: "") +
            (groupBy.takeIf { it.isNotEmpty() }?.let { lineSeparator() + "GROUP BY ${it.joinToString(", ")}" } ?: "") +
            (having?.let { lineSeparator() + "HAVING $it" } ?: "") +
            (sorts.takeIf { it.isNotEmpty() }?.let { lineSeparator() + "ORDER BY ${it.joinToString(", ")}" } ?: "") +
            (limit?.let { lineSeparator() + "LIMIT $it"} ?: "") +
            (offset?.let { lineSeparator() + "OFFSET $it" } ?: "")
}

fun SelectedItem.fmt(
    indent: Indent
): String {
    return alias?.let { "$expr as $it" } ?: expr.toString()
}

fun JoinSource.fmt(
    indent: Indent
) = "($lhs ${type.name} JOIN $rhs" + (condition?.let { " ON $it)" } ?: ")")

fun SubQuerySource.fmt(
    indent: Indent
) = "($query) as $alias"

fun SqlIdSource.fmt() = alias?.let { "$sqlId $it" } ?: sqlId.toString()

fun SqlId.fmt() = schema?.let { "$it.$name" } ?: name

fun Column.fmt() = source?.let { "$it.$name" } ?: name

fun Op1Type.fmt() = when(this) {
    Op1Type.UN_MINUS -> "-"
    Op1Type.UN_PLUS -> "+"
    Op1Type.NOT -> "NOT"
}

fun Op2Type.fmt() = when(this) {
    Op2Type.OR -> "OR"
    Op2Type.AND -> "AND"
    Op2Type.EQ -> "="
    Op2Type.NEQ -> "!="
    Op2Type.GT -> ">"
    Op2Type.GTE -> ">="
    Op2Type.LT -> "<"
    Op2Type.LTE -> "<="
    Op2Type.PLUS -> "+"
    Op2Type.MINUS -> "-"
    Op2Type.MOD -> "%"
    Op2Type.DIV -> "/"
    Op2Type.MULT -> "*"
}

fun IntExpr.fmt() = value.toString()

fun DoubleExpr.fmt() = value.toString()

fun StrExpr.fmt() = value

fun BoolExpr.fmt() = when(this.value) {
    true -> Keyword.TRUE.value.uppercase()
    false -> Keyword.FALSE.value.uppercase()
}

fun SqlNullExpr.fmt() = Keyword.NULL.value.uppercase()

fun ColumnExpr.fmt() = column.fmt()

fun FunExpr.fmt(indent: Indent) = "$function(${args.joinToString(", ")})"

fun QueryExpr.fmt(indent: Indent) = "($query)"

fun Op1Expr.fmt(indent: Indent) = "$op($arg)"

fun Op2Expr.fmt(indent: Indent) = "($lhs $op $rhs)"

fun Sort.fmt(indent: Indent) = "$expr $direction"
