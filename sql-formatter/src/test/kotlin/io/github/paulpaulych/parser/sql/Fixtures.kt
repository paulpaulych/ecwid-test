package io.github.paulpaulych.parser.sql

import io.github.paulpaulych.parser.sql.JoinType.*
import io.github.paulpaulych.parser.sql.Source.JoinSource
import io.github.paulpaulych.parser.sql.Source.SqlIdSource

object Fixtures {

    fun crossJoin(l: Source, r: Source) = JoinSource(CROSS, null, l, r)
    fun innerJoin(e: Expr, l: Source, r: Source) = JoinSource(INNER, e, l, r)
    fun leftJoin(cond: Expr, l: Source, r: Source) = JoinSource(LEFT, cond, l, r)
    fun rightJoin(e: Expr, l: Source, r: Source) = JoinSource(RIGHT, e, l, r)

    fun tableA(alias: String) = SqlIdSource(SqlId(schema = null, name = "a"), alias = alias)

    fun tableSource(name: String) = SqlIdSource(SqlId(null, name), null)

    fun col(name: String) = Column(name, null)
}