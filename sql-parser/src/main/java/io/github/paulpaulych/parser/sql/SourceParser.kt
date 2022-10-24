package io.github.paulpaulych.parser.sql

import io.github.paulpaulych.parser.lib.Parser
import io.github.paulpaulych.parser.lib.TextParsers.attempt
import io.github.paulpaulych.parser.lib.TextParsers.oneOf
import io.github.paulpaulych.parser.lib.TextParsers.optional
import io.github.paulpaulych.parser.lib.TextParsers.peekOnly
import io.github.paulpaulych.parser.lib.TextParsers.scoped
import io.github.paulpaulych.parser.lib.TextParsers.succeed
import io.github.paulpaulych.parser.lib.TextParsersDsl.flatMap
import io.github.paulpaulych.parser.lib.TextParsersDsl.many
import io.github.paulpaulych.parser.lib.TextParsersDsl.map
import io.github.paulpaulych.parser.lib.TextParsersDsl.or
import io.github.paulpaulych.parser.lib.TextParsersDsl.plus
import io.github.paulpaulych.parser.lib.TextParsersDsl.skipL
import io.github.paulpaulych.parser.lib.TextParsersDsl.skipR
import io.github.paulpaulych.parser.sql.CommonSqlParsers.anyWord
import io.github.paulpaulych.parser.sql.CommonSqlParsers.comma
import io.github.paulpaulych.parser.sql.CommonSqlParsers.inParentheses
import io.github.paulpaulych.parser.sql.CommonSqlParsers.parser
import io.github.paulpaulych.parser.sql.CommonSqlParsers.s
import io.github.paulpaulych.parser.sql.CommonSqlParsers.sqlId
import io.github.paulpaulych.parser.sql.CommonSqlParsers.ws
import io.github.paulpaulych.parser.sql.CommonSqlParsers.ws1
import io.github.paulpaulych.parser.sql.ExprParser.expr
import io.github.paulpaulych.parser.sql.JoinType.*
import io.github.paulpaulych.parser.sql.QueryParser.query
import io.github.paulpaulych.parser.sql.Source.*


object SourceParser {

    private val alias: Parser<String> = scoped(
        scope = "alias",
        parser = Keyword.AS.parser().optional() skipL ws skipL anyWord
    )

    private val crossJoinKeyword = comma or {
        (Keyword.CROSS.parser() or { Keyword.FULL.parser() skipL ws skipL Keyword.OUTER.parser() })
            .skipL(ws skipL Keyword.JOIN.parser())
    }
    private val leftJoinKeyword = Keyword.LEFT.parser() skipL ws skipL Keyword.OUTER.parser().optional() skipL ws skipL Keyword.JOIN.parser()
    private val rightJoinKeyword = Keyword.RIGHT.parser() skipL ws skipL Keyword.OUTER.parser().optional() skipL ws skipL Keyword.JOIN.parser()
    private val innerJoinKeyword = (Keyword.INNER.parser() skipL ws).optional() skipL Keyword.JOIN.parser()

    private val noCondition: Parser<Expr?> = succeed(null)
    private val joinCondition: Parser<Expr?> = scoped(
        scope = "join condition",
        parser = (Keyword.ON.parser() skipL ws1 skipL expr).map { it }
    )

    private val joinType: Parser<JoinType> = oneOf(sequenceOf(
        crossJoinKeyword.map { CROSS },
        innerJoinKeyword.map { INNER },
        rightJoinKeyword.map { RIGHT },
        leftJoinKeyword.map { LEFT },
    ))

    private fun joinCondition(type: JoinType): Parser<Expr?> =
        when (type) {
            CROSS -> noCondition
            INNER,
            LEFT,
            RIGHT -> joinCondition
        }

    private val querySource: Parser<Query> = ((s("(") skipL ws skipL Keyword.SELECT.parser().peekOnly()).attempt() skipL { query } skipR s(")"))

    private val sourceParsers: List<Parser<Source>> = listOf(
        joins(arg = { source(skipParsers = 1) }),
        scoped(
            scope = "derived query",
            msg = "derived query expected",
            parser = (querySource + (ws1 skipL alias)).map { (query, alias) -> SubQuerySource(query, alias) }
        ),
        scoped(
            scope = "source in parentheses",
            msg = "source in parentheses expected",
            parser = { source }.inParentheses()
        ),
        scoped(
            scope = "table or function",
            msg = "table or function expected",
            parser = (sqlId + (ws1 skipL alias).optional()).map { (sqlId, alias) -> SqlIdSource(sqlId, alias) }
        ),
    )

    val source: Parser<Source> = source(skipParsers = 0)

    private fun source(skipParsers: Int): Parser<Source> =
        scoped(
            scope = "source",
            msg = "source expected",
            parser = oneOf(sourceParsers.asSequence().drop(skipParsers))
        )

    private fun joins(arg: () -> Parser<Source>): Parser<Source> {
        val argParser = ws skipL arg

        val joinWithCondition =
            ((ws skipL joinType).attempt() + argParser)
                .flatMap { (type, rhs) ->
                    (ws skipL joinCondition(type))
                        .map { cond ->
                            Pair(Pair(type, cond), rhs)
                        }
                }

        return (argParser + joinWithCondition.many())
            .map { (first, joins) ->
                if (joins.isEmpty()) first
                else buildJoinSource(first, joins)
            }
    }

    private fun buildJoinSource(
        first: Source,
        joins: List<Pair<Pair<JoinType, Expr?>, Source>>
    ): Source {
        return joins.fold(first) { lhs, (operator, rhs) ->
            val (type, condition) = operator
            JoinSource(
                type = type,
                condition = condition,
                lhs = lhs,
                rhs = rhs
            )
        }
    }
}