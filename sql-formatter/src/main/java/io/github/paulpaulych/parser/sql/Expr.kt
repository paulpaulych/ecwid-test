package io.github.paulpaulych.parser.sql


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