package io.github.paulpaulych.parser.sql

import io.github.paulpaulych.parser.lib.Parser
import io.github.paulpaulych.parser.lib.TextParsers.attempt
import io.github.paulpaulych.parser.lib.TextParsers.optional
import io.github.paulpaulych.parser.lib.TextParsersDsl.ifPresent
import io.github.paulpaulych.parser.lib.TextParsersDsl.map
import io.github.paulpaulych.parser.lib.TextParsersDsl.plus
import io.github.paulpaulych.parser.lib.TextParsersDsl.sepBy1
import io.github.paulpaulych.parser.lib.TextParsersDsl.skipL
import io.github.paulpaulych.parser.lib.TextParsersDsl.skipR
import io.github.paulpaulych.parser.sql.CommonSqlParsers.anyWord
import io.github.paulpaulych.parser.sql.CommonSqlParsers.columnsByComma
import io.github.paulpaulych.parser.sql.CommonSqlParsers.comma
import io.github.paulpaulych.parser.sql.CommonSqlParsers.intValue
import io.github.paulpaulych.parser.sql.CommonSqlParsers.parser
import io.github.paulpaulych.parser.sql.CommonSqlParsers.sortOrder
import io.github.paulpaulych.parser.sql.CommonSqlParsers.ws
import io.github.paulpaulych.parser.sql.ExprParser.expr
import io.github.paulpaulych.parser.sql.SourceParser.source

object QueryParser {

    private val selectedItems: Parser<List<SelectedItem>> =
        ((ws skipL expr skipR ws) + (Keyword.AS.parser().optional().ifPresent { ws skipL anyWord }))
            .sepBy1(comma)
            .map { items -> items.map { (e, alias) -> SelectedItem(e, alias) } }

    private val sorts: Parser<List<Sort>> =
        ((ws skipL expr skipR ws) + sortOrder).sepBy1(comma)
            .map { items -> items.map { (e, order) -> Sort(e, order) } }

    private val select = Keyword.SELECT.parser().attempt() skipL ws skipL selectedItems
    private val from = Keyword.FROM.parser().attempt() skipL ws skipL source
    private val where: Parser<Expr?> = Keyword.WHERE.parser().optional().ifPresent { ws skipL expr }
    private val groupBy = (Keyword.GROUP.parser() skipL ws skipL Keyword.BY.parser()).optional().ifPresent { ws skipL columnsByComma }
    private val having = Keyword.HAVING.parser().optional().ifPresent { ws skipL expr }
    private val orderBy = (Keyword.GROUP.parser() skipL ws skipL Keyword.BY.parser()).optional().ifPresent { ws skipL sorts }
    private val limit = Keyword.LIMIT.parser().optional().ifPresent { ws skipL intValue }
    private val offset = Keyword.OFFSET.parser().optional().ifPresent { ws skipL intValue }

    val query: Parser<Query> =
        (ws skipL (select skipR ws) +
                (from skipR ws) +
                (where skipR ws) +
                (groupBy skipR ws) +
                (having skipR ws) +
                (orderBy skipR ws) +
                (limit skipR ws) +
                (offset skipR ws))
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