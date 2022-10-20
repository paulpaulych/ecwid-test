package io.github.paulpaulych.sql

// TODO: is required
enum class Keyword(
    val lowercase: String
) {

    SELECT("select"),
    FROM("from"),
    LEFT_JOIN("left join"),
    RIGHT_JOIN("right join"),
    JOIN("join"),
    INNER_JOIN("inner join"),
    CROSS_JOIN("cross join"),
    WHERE("where"),
    ORDER_BY("order by"),
    GROUP_BY("group by"),
    HAVING("having"),
    LIMIT("limit"),
    OFFSET("offset"),
    ASC("asc"),
    DESC("desc")
}