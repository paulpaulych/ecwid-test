package io.github.paulpaulych.sql

import io.github.paulpaulych.parser.Parser
import io.github.paulpaulych.parser.TextParsers.attempt
import io.github.paulpaulych.parser.TextParsers.optional
import io.github.paulpaulych.parser.TextParsersDsl.map
import io.github.paulpaulych.parser.TextParsers.oneOf
import io.github.paulpaulych.parser.TextParsers.scoped
import io.github.paulpaulych.parser.TextParsers.succeed
import io.github.paulpaulych.parser.TextParsersDsl.flatMap
import io.github.paulpaulych.parser.TextParsersDsl.many
import io.github.paulpaulych.parser.TextParsersDsl.or
import io.github.paulpaulych.parser.TextParsersDsl.plus
import io.github.paulpaulych.parser.TextParsersDsl.skipL
import io.github.paulpaulych.sql.CommonSqlParsers.sqlId
import io.github.paulpaulych.sql.CommonSqlParsers.anyWord
import io.github.paulpaulych.sql.CommonSqlParsers.comma
import io.github.paulpaulych.sql.CommonSqlParsers.excludingKeywords
import io.github.paulpaulych.sql.CommonSqlParsers.inParentheses
import io.github.paulpaulych.sql.CommonSqlParsers.parser
import io.github.paulpaulych.sql.CommonSqlParsers.ws
import io.github.paulpaulych.sql.CommonSqlParsers.ws1
import io.github.paulpaulych.sql.JoinType.*
import io.github.paulpaulych.sql.Source.*
import io.github.paulpaulych.sql.SourceParser.SourceWithoutAlias.*


object SourceParser {

    //TODO: are subqueries allowed ON ?
    private val expr: () -> Parser<Expr> = ExprParser(wildcardAllowed = false)::expr

    private val alias: Parser<String> = scoped(
        scope = "alias",
        parser = anyWord.excludingKeywords()
    )

    private sealed interface SourceWithoutAlias {
        data class JoinWithoutAlias(val get: JoinSource): SourceWithoutAlias
        data class SqlIdWithoutAlias(val get: SqlId): SourceWithoutAlias
        data class QueryWithoutAlias(val get: Query): SourceWithoutAlias
    }

    private val sourceWithoutAlias: Parser<SourceWithoutAlias> = sqlId.map { SqlIdWithoutAlias(it) }
    private val sqlIdSource: Parser<Source> = sourceWithoutAlias.withAlias()

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

    private val joinOperator: Parser<JoinType> = oneOf(sequenceOf(
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

    private val sourceParsers: List<Parser<Source>> = listOf(
        joins(arg = { source(skipParsers = 1) }),
        { source }.inParentheses(),
        sqlIdSource
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
            (ws skipL joinOperator + argParser)
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

    private fun Parser<SourceWithoutAlias>.withAlias(): Parser<Source> =
        this.flatMap { source ->
            when(source) {
                is QueryWithoutAlias ->
                    (ws1 skipL alias)
                        .map { alias -> QuerySource(source.get, alias) }
                is SqlIdWithoutAlias ->
                    (ws1 skipL alias).optional()
                        .map { alias -> SqlIdSource(source.get, alias) }
                is JoinWithoutAlias -> succeed(source.get)
            }
        }
}