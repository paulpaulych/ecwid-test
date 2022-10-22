package io.github.paulpaulych.sql

enum class Keyword(
    val value: String
) {

    SELECT("select"),
    AS("as"),

    FROM("from"),
    JOIN("join"),
    INNER("inner"),
    CROSS("cross"),
    FULL("full"),
    OUTER("outer"),
    LEFT("left"),
    RIGHT("right"),
    ON("on"),

    WHERE("where"),
    AND("and"),
    OR("or"),
    NOT("not"),

    TRUE("true"),
    FALSE("false"),

    BY("by"),
    ORDER("order"),
    GROUP("group"),
    ASC("asc"),
    DESC("desc"),

    HAVING("having"),
    LIMIT("limit"),
    OFFSET("offset");

    companion object {
        val ALL = (values().map { it.value.lowercase() } + values().map { it.value.uppercase() }).toSet()
    }
}