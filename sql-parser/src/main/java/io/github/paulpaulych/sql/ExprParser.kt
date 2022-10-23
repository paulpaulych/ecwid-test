package io.github.paulpaulych.sql

import io.github.paulpaulych.parser.Parser
import io.github.paulpaulych.parser.TextParsers.attempt
import io.github.paulpaulych.parser.TextParsers.notEof
import io.github.paulpaulych.parser.TextParsers.oneOf
import io.github.paulpaulych.parser.TextParsers.scoped
import io.github.paulpaulych.parser.TextParsersDsl.and
import io.github.paulpaulych.parser.TextParsersDsl.many
import io.github.paulpaulych.parser.TextParsersDsl.map
import io.github.paulpaulych.parser.TextParsersDsl.plus
import io.github.paulpaulych.parser.TextParsersDsl.sepBy
import io.github.paulpaulych.parser.TextParsersDsl.skipL
import io.github.paulpaulych.parser.TextParsersDsl.skipR
import io.github.paulpaulych.sql.CommonSqlParsers.boolean
import io.github.paulpaulych.sql.CommonSqlParsers.column
import io.github.paulpaulych.sql.CommonSqlParsers.double
import io.github.paulpaulych.sql.CommonSqlParsers.inParentheses
import io.github.paulpaulych.sql.CommonSqlParsers.int
import io.github.paulpaulych.sql.CommonSqlParsers.parser
import io.github.paulpaulych.sql.CommonSqlParsers.quoted
import io.github.paulpaulych.sql.CommonSqlParsers.s
import io.github.paulpaulych.sql.CommonSqlParsers.sqlId
import io.github.paulpaulych.sql.CommonSqlParsers.sqlNull
import io.github.paulpaulych.sql.CommonSqlParsers.ws
import io.github.paulpaulych.sql.Expr.*
import io.github.paulpaulych.sql.Op1Type.*
import io.github.paulpaulych.sql.Op2Type.*

// TODO: optimize with constants
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

    private val exprParsers: List<Parser<Expr>> = listOf(
        samePrecedenceBinOps(Keyword.OR.parser().map { OR }, arg = { expr(skipParsers = 1) }).attempt(),
        samePrecedenceBinOps(Keyword.AND.parser().map { AND }, arg = { expr(skipParsers = 2) }).attempt(),
        unaryOp(Keyword.NOT.parser().map { NOT }, arg = { expr(skipParsers = 2) }),
        samePrecedenceBinOps(comparisonOperator, arg = { expr(skipParsers = 4) }).attempt(),
        samePrecedenceBinOps(plusOrMinus, arg = { expr(skipParsers = 5) }).attempt(),
        samePrecedenceBinOps(divModMult, arg = { expr(skipParsers = 6) }).attempt(),
        unaryOp(unMinus, arg = { expr(skipParsers = 8) }),
        unaryOp(unPlus, arg = { expr(skipParsers = 8) }),
        { expr(skipParsers = 0) }.inParentheses(),
        sqlNull.attempt(),
        double,
        int,
        quoted,
        boolean.attempt(),
        functionCall(arg = { expr(skipParsers = 0) }).attempt(),
        columnExpr
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
        val args = (ws skipL arg skipR ws) sepBy s(",")
        return sqlId
            .and(args.inParentheses())
            .map { (sqlId, args) -> FunExpr(sqlId, args) }
    }

    private fun queryExpr(): Parser<QueryExpr> = TODO()
}