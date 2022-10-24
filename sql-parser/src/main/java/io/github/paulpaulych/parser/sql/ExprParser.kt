package io.github.paulpaulych.parser.sql

import io.github.paulpaulych.parser.lib.Parser
import io.github.paulpaulych.parser.lib.TextParsers.attempt
import io.github.paulpaulych.parser.lib.TextParsers.notEof
import io.github.paulpaulych.parser.lib.TextParsers.oneOf
import io.github.paulpaulych.parser.lib.TextParsers.peekOnly
import io.github.paulpaulych.parser.lib.TextParsers.scoped
import io.github.paulpaulych.parser.lib.TextParsersDsl.failed
import io.github.paulpaulych.parser.lib.TextParsersDsl.many
import io.github.paulpaulych.parser.lib.TextParsersDsl.map
import io.github.paulpaulych.parser.lib.TextParsersDsl.plus
import io.github.paulpaulych.parser.lib.TextParsersDsl.sepBy1
import io.github.paulpaulych.parser.lib.TextParsersDsl.skipL
import io.github.paulpaulych.parser.lib.TextParsersDsl.skipR
import io.github.paulpaulych.parser.sql.CommonSqlParsers.boolean
import io.github.paulpaulych.parser.sql.CommonSqlParsers.column
import io.github.paulpaulych.parser.sql.CommonSqlParsers.double
import io.github.paulpaulych.parser.sql.CommonSqlParsers.inParentheses
import io.github.paulpaulych.parser.sql.CommonSqlParsers.int
import io.github.paulpaulych.parser.sql.CommonSqlParsers.parser
import io.github.paulpaulych.parser.sql.CommonSqlParsers.quoted
import io.github.paulpaulych.parser.sql.CommonSqlParsers.s
import io.github.paulpaulych.parser.sql.CommonSqlParsers.sqlId
import io.github.paulpaulych.parser.sql.CommonSqlParsers.sqlNull
import io.github.paulpaulych.parser.sql.CommonSqlParsers.ws
import io.github.paulpaulych.parser.sql.Expr.*
import io.github.paulpaulych.parser.sql.Op1Type.*
import io.github.paulpaulych.parser.sql.Op2Type.*
import io.github.paulpaulych.parser.sql.QueryParser.query

object ExprParser {

    private val comparisonOperator: Parser<Op2Type> = oneOf(sequenceOf(
        s("=").map { EQ },
        s("!=").map { NEQ },
        s("<=").attempt().map { LTE },
        s("<").map { LT },
        s(">=").attempt().map { GTE },
        s(">").map { GT },
    ))

    private val plusOrMinus: Parser<Op2Type> = oneOf(sequenceOf(
        s("+").map { PLUS },
        s("-").map { MINUS }
    ))

    private val divModMult: Parser<Op2Type> = oneOf(sequenceOf(
        s("/").map { DIV },
        s("%").map { MOD },
        s("*").map { MULT }
    ))

    private val unMinus: Parser<Op1Type> = s("-").map { UN_MINUS }
    private val unPlus: Parser<Op1Type> = s("+").map { UN_PLUS }

    private val columnExpr: Parser<Expr> = column.map(::ColumnExpr)
    private val queryExpr: Parser<Expr> = ((s("(") skipL ws skipL Keyword.SELECT.parser().peekOnly()).attempt() skipL { query } skipR s(")")).map(::SubQueryExpr)

    private val exprParsers: List<Parser<Expr>> = listOf(
        samePrecedenceBinOps(Keyword.OR.parser().map { OR }, arg = { expr(skipParsers = 1) }).attempt(),
        samePrecedenceBinOps(Keyword.AND.parser().map { AND }, arg = { expr(skipParsers = 2) }).attempt(),
        unaryOp(Keyword.NOT.parser().map { NOT }, arg = { expr(skipParsers = 2) }),
        samePrecedenceBinOps(comparisonOperator, arg = { expr(skipParsers = 4) }),
        samePrecedenceBinOps(plusOrMinus, arg = { expr(skipParsers = 5) }),
        samePrecedenceBinOps(divModMult, arg = { expr(skipParsers = 6) }),
        unaryOp(unMinus, arg = { expr(skipParsers = 8) }),
        unaryOp(unPlus, arg = { expr(skipParsers = 8) }),
        queryExpr,
        { expr(skipParsers = 0) }.inParentheses(),
        sqlNull.attempt(),
        double,
        int,
        quoted,
        boolean.attempt(),
        functionCallWithoutArgs(),
        functionCall(arg = { expr(skipParsers = 0) }),
        columnExpr,
        failed("expression", "no expression found", isCommitted = true)
    )

    val expr: Parser<Expr> = scoped("expression", "expression expected", expr(skipParsers = 0))

    private fun expr(skipParsers: Int): Parser<Expr> {
        val parsers = exprParsers.asSequence().drop(skipParsers)
        return notEof() skipL oneOf(parsers)
    }

    private fun samePrecedenceBinOps(
        typeParser: Parser<Op2Type>,
        arg: () -> Parser<Expr>,
    ): Parser<Expr> {
        val argParser = ws skipL arg skipR ws
        return (argParser + (typeParser + argParser).many())
            .map { (first, tail) ->
                if (tail.isEmpty()) first
                else tail.fold(first) { lhs, (op, rhs) ->
                    Op2Expr(op, lhs, rhs)
                }
            }
    }

    private fun unaryOp(
        typeParser: Parser<Op1Type>,
        arg: () -> Parser<Expr>
    ): Parser<Expr> {
        return ((typeParser skipR ws).attempt() + arg)
            .map { (type, operand) -> Op1Expr(type, operand) }
    }

    private fun functionCall(arg: () -> Parser<Expr>): Parser<Expr> {
        val args = (ws skipL arg skipR ws) sepBy1 s(",")
        return ((sqlId skipR s("(")).attempt() + (args skipR s(")")))
            .map { (sqlId, args) -> FunExpr(sqlId, args) }
    }

    private fun functionCallWithoutArgs(): Parser<Expr> {
        return (sqlId skipR s("(") skipR ws skipR s(")")).attempt()
            .map { sqlId -> FunExpr(sqlId, listOf()) }
    }
}