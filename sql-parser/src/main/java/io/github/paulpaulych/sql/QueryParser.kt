package io.github.paulpaulych.sql

import io.github.paulpaulych.parser.Parser
import io.github.paulpaulych.parser.TextParsers.optional
import io.github.paulpaulych.parser.TextParsersDsl.map
import io.github.paulpaulych.parser.TextParsersDsl.plus
import io.github.paulpaulych.parser.TextParsersDsl.sepBy1
import io.github.paulpaulych.parser.TextParsersDsl.skipL
import io.github.paulpaulych.parser.TextParsersDsl.skipR
import io.github.paulpaulych.sql.CommonSqlParsers.anyWord
import io.github.paulpaulych.sql.CommonSqlParsers.columnsByComma
import io.github.paulpaulych.sql.CommonSqlParsers.comma
import io.github.paulpaulych.sql.CommonSqlParsers.excludingKeywords
import io.github.paulpaulych.sql.CommonSqlParsers.intValue
import io.github.paulpaulych.sql.CommonSqlParsers.sortOrder
import io.github.paulpaulych.sql.CommonSqlParsers.parser
import io.github.paulpaulych.sql.CommonSqlParsers.ws
import io.github.paulpaulych.sql.ExprParser.expr
import io.github.paulpaulych.sql.SourceParser.source

object QueryParser {

    private val selectedItems: Parser<List<SelectedItem>> =
        ((ws skipL expr skipR ws) + (Keyword.AS.parser() skipL ws skipL anyWord.excludingKeywords()).optional())
            .sepBy1(comma)
            .map { items -> items.map { (e, alias) -> SelectedItem(e, alias) } }

    private val sorts: Parser<List<Sort>> =
        ((ws skipL expr skipR ws) + sortOrder).sepBy1(comma)
            .map { items -> items.map { (e, order) -> Sort(e, order) } }

    private val select = Keyword.SELECT.parser() skipL ws skipL selectedItems
    private val from = Keyword.FROM.parser() skipL ws skipL source
    private val where = Keyword.WHERE.parser() skipL ws skipL expr
    private val groupBy = Keyword.GROUP.parser() skipL ws skipL Keyword.BY.parser() skipL ws skipL columnsByComma
    private val having = Keyword.HAVING.parser() skipL ws skipL expr
    private val orderBy = Keyword.GROUP.parser() skipL ws skipL Keyword.BY.parser() skipL ws skipL sorts
    private val limit = Keyword.LIMIT.parser() skipL ws skipL intValue
    private val offset = Keyword.OFFSET.parser() skipL ws skipL intValue

    val query: Parser<Query> =
        (ws skipL (select skipR ws) +
                (from skipR ws) +
                (where.optional() skipR ws) +
                (groupBy.optional() skipR ws) +
                (having.optional() skipR ws) +
                (orderBy.optional() skipR ws) +
                (limit.optional() skipR ws) +
                (offset.optional()))
            .map { (p1, offset) ->
                val (p2, limit) = p1
                val (p3, orderBy) = p2
                val (p4, having) = p3
                val (p5, groupBy) = p4
                val (p6, where) = p5
                val (select, from) = p6
                Query(
                    columns = select,
                    source = from,
                    where = where,
                    groupBy = groupBy ?: listOf(),
                    having = having,
                    sorts = orderBy ?: listOf(),
                    limit = limit,
                    offset = offset
                )
            }
}