package io.github.paulpaulych.sql

enum class SqlScopes(
    val get: String
) {
    LITERAL("SQL literal"),
    NULL("null"),

    SELECTABLE_EXPR("selectable expr"),
    COLUMN("column"),
    WILDCARD("wildcard")
}