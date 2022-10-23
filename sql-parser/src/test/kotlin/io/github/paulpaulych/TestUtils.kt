package io.github.paulpaulych

import io.github.paulpaulych.common.Either
import io.github.paulpaulych.common.Either.Left
import io.github.paulpaulych.common.Either.Right
import io.github.paulpaulych.parser.*
import io.github.paulpaulych.parser.fmt.fmt
import io.github.paulpaulych.sql.Query
import io.github.paulpaulych.sql.QueryParser
import io.kotest.assertions.fail
import io.kotest.assertions.withClue
import io.kotest.data.*
import io.kotest.matchers.collections.shouldContainExactly
import io.kotest.matchers.shouldBe
import io.kotest.matchers.types.instanceOf

object TestUtils {

    fun <A> expectSuccess(
        parser: Parser<A>,
        vararg cases: Pair<String, A>,
    ) {
        forAll(
            table(
                headers("input", "result"),
                *cases.map { (a, b) -> row(a, b) }.toTypedArray(),
            )
        ) { source, expected ->
            val res = TextParsers.run(parser, source)
            withClue(res) {
                res shouldBe instanceOf<Right<Success<A>>>()
                res as Right
                res.value.get shouldBe expected
            }
        }
    }

    fun <A> expectFailure(
        parser: Parser<A>,
        vararg cases: Pair<String, StackTrace.() -> Unit>
    ) {
        forAll(
            table(
                headers("input", "matcher"),
                *cases.map { (a, b) -> row(a, b) }.toTypedArray(),
            )
        ) { source, matcher ->
            val res = TextParsers.run(parser, source)
            withClue(res) {
                res shouldBe instanceOf<Left<StackTrace>>()
                res as Left
                matcher(res.value)
            }
        }
    }

    fun expectFmtSuccess(vararg cases: Pair<String, String>) {
        forAll(table(
            headers("given", "expected"),
            *cases.map { row(it.first, it.second) }.toTypedArray()
        )) { given, expected ->
            when(val result = TextParsers.run(QueryParser.query, given)) {
                is Left -> {
                    fail("expected success: ${fmt(result.value)}")
                }
                is Right -> {
                    val query: Query = result.value.get
                    query.toString().lines() shouldContainExactly expected.lines()
                }
            }
        }
    }

    fun expectFmtFailure(vararg cases: Pair<String, String>) {
        forAll(table(
            headers("given", "expected"),
            *cases.map { row(it.first, it.second) }.toTypedArray()
        )) { given, expected ->
            when(val result = TextParsers.run(QueryParser.query, given)) {
                is Left -> {
                    val stacktrace: StackTrace = result.value
                    fmt(stacktrace).lines() shouldContainExactly expected.lines()
                }
                is Right -> {
                    fail("expected failure: ${result.value.get}")
                }
            }
        }
    }

    fun <A> runParserTest(
        vararg cases: Row3<Parser<A>, String, Either<StackTrace, Success<A>>>
    ) {
        forAll(
            table(
                headers("parser", "input", "result"),
                *cases,
            )
        ) { parser, source, expected ->
            TextParsers.run(parser, source) shouldBe expected
        }
    }

    fun <A> ok(a: A, consumed: Int) = Right(Success(a, consumed))
    fun err(stack: StackTrace) = Left(stack)

}