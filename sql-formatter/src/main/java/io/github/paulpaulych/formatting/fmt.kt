package io.github.paulpaulych.formatting

import io.github.paulpaulych.parser.sql.*
import io.github.paulpaulych.parser.sql.Expr.*
import io.github.paulpaulych.parser.sql.Op2Type.*
import io.github.paulpaulych.parser.sql.Source.*

private const val JOIN_INDENT = "    "

fun sqlIndentBuffer(): IndentBuffer {
    return IndentBuffer.create(
        initialIndent = "",
        indentStep = "    "
    )
}

fun Query.fmt(): String {
    val buffer = sqlIndentBuffer()
    fmt(buffer)
    return buffer.getResult() + ";"
}

private fun Query.fmt(buffer: IndentBuffer) {
    buffer.appendText("SELECT")
        .appendIndented(resetColumn = true) { columnsBuffer ->
            columnsBuffer.appendSelectedItems(columns)
        }
        .newLine()
        .appendText("FROM ").apply(source::fmt)
    if (where != null) {
        buffer
            .newLine()
            .appendText("WHERE ")
            .apply(where::fmt)
    }
    if (groupBy.isNotEmpty()) {
        buffer
            .newLine()
            .appendText("GROUP BY ${groupBy.joinToString(", ") { it.fmt() }}")
    }
    if (having != null) {
        buffer
            .newLine()
            .appendText("HAVING ")
            .apply(having::fmt)
    }
    if (sorts.isNotEmpty()) {
        buffer
            .newLine()
            .appendText("ORDER BY ")
            .appendSorts(sorts)
    }
    if (limit != null) {
        buffer
            .newLine()
            .appendText("LIMIT $limit")
    }
    if (offset != null) {
        buffer
            .newLine()
            .appendText("OFFSET $offset")
    }
}

private fun JoinSource.fmt(buffer: IndentBuffer) {
    buffer
        .appendText("(")
        .apply(lhs::fmt)
        .newLine()
        .appendText(JOIN_INDENT)
        .appendText("${type.name} JOIN ")
        .apply(rhs::fmt)
    if (condition != null) {
        buffer
            .appendText(" ON ")
            .apply(condition::fmt)
    }
    buffer.appendText(")")
}

private fun SubQuerySource.fmt(buffer: IndentBuffer) {
    buffer
        .appendSubQuery(query)
        .appendText(" AS $alias")
}

private fun FunExpr.fmt(buffer: IndentBuffer) {
    buffer.appendText("${function.fmt()}(")
        .appendFunctionArgs(args)
        .appendText(")")
}

private fun SubQueryExpr.fmt(buffer: IndentBuffer) {
    buffer.appendSubQuery(query)
}

private fun Op1Expr.fmt(buffer: IndentBuffer) {
    buffer.appendText(op.fmt())
        .appendText("(")
        .apply(arg::fmt)
        .appendText(")")
}

private fun Op2Expr.fmt(buffer: IndentBuffer): IndentBuffer =
    when(this.op) {
        AND,
        OR -> {
            buffer
                .appendText("(")
                .apply(lhs::fmt)
                .appendIndented(resetColumn = true) { indented ->
                    indented
                        .appendText("${op.fmt()} ")
                        .apply(rhs::fmt)
                        .appendText(")")
                }
        }
        EQ, NEQ, GT, GTE, LT,
        LTE, PLUS, MINUS, MOD, DIV,
        MULT -> {
            buffer
                .appendText("(")
                .apply(lhs::fmt)
                .appendText(" ${op.fmt()} ")
                .apply(rhs::fmt)
                .appendText(")")
        }
    }

private fun IndentBuffer.appendSubQuery(query: Query): IndentBuffer {
    appendText("(")
        .shiftColumn(-1)
        .appendIndented(resetColumn = false) { indentBuffer ->
            indentBuffer
                .apply(query::fmt)
                .newLine()
        }.appendText(")")
    return this
}

private fun IndentBuffer.appendSorts(items: List<Sort>) =
    appendAll(
        items = items,
        append = { buf, item ->
            buf
                .apply(item.expr::fmt)
                .appendText(" ${item.direction}")
         },
        sep = { buf -> buf.appendText(", ") }
    )

private fun IndentBuffer.appendFunctionArgs(items: List<Expr>) =
    appendAll(
        items = items,
        append = { buf, item -> buf.apply(item::fmt) },
        sep = { buf -> buf.appendText(", ") }
    )

private fun IndentBuffer.appendSelectedItems(items: List<SelectedItem>) =
    appendAll(
        items = items,
        append = { buf, item ->
            buf.apply(item.expr::fmt)
                .appendText(item.alias?.let { " AS $it" } ?: "")
        },
        sep = { buf ->
            buf.appendText(",").newLine()
        }
    )

private fun Source.fmt(buffer: IndentBuffer) {
    when(this) {
        is JoinSource -> fmt(buffer)
        is SqlIdSource -> buffer.appendText(fmt())
        is SubQuerySource -> fmt(buffer)
    }
}

private fun Expr.fmt(buffer: IndentBuffer) {
    when(this) {
        is BoolExpr -> buffer.appendText(fmt())
        is ColumnExpr -> buffer.appendText(fmt())
        is DoubleExpr -> buffer.appendText(fmt())
        is IntExpr -> buffer.appendText(fmt())
        is SqlNullExpr -> buffer.appendText(fmt())
        is StrExpr -> buffer.appendText(fmt())
        is FunExpr -> fmt(buffer)
        is Op1Expr -> fmt(buffer)
        is Op2Expr -> fmt(buffer)
        is SubQueryExpr -> fmt(buffer)
    }
}

private fun SqlIdSource.fmt(): String = alias?.let { "${sqlId.fmt()} $it" } ?: sqlId.fmt()
private fun SqlId.fmt(): String = schema?.let { "$it.$name" } ?: name
private fun Column.fmt(): String = source?.let { "${it.fmt()}.$name" } ?: name
private fun Op1Type.fmt(): String = when(this) {
    Op1Type.UN_MINUS -> "-"
    Op1Type.UN_PLUS -> "+"
    Op1Type.NOT -> "NOT"
}

private fun Op2Type.fmt(): String = when(this) {
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

private fun IntExpr.fmt(): String = value.toString()
private fun DoubleExpr.fmt(): String = value.toString()
private fun StrExpr.fmt(): String = value
private fun BoolExpr.fmt(): String = when(this.value) {
    true -> Keyword.TRUE.value.lowercase()
    false -> Keyword.FALSE.value.lowercase()
}

private fun ColumnExpr.fmt(): String = column.fmt()
@Suppress("unused")
private fun SqlNullExpr.fmt(): String =
    Keyword.NULL.value.uppercase()
