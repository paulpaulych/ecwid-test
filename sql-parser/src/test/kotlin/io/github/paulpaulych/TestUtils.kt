package io.github.paulpaulych

import io.github.paulpaulych.parser.lib.*
import io.github.paulpaulych.parser.lib.ParseResult.Failure
import io.github.paulpaulych.parser.lib.ParseResult.Success
import io.github.paulpaulych.parser.sql.Query
import io.github.paulpaulych.parser.sql.QueryParser
import io.kotest.assertions.fail
import io.kotest.assertions.withClue
import io.kotest.data.*
import io.kotest.matchers.collections.shouldContainExactly
import io.kotest.matchers.shouldBe
import io.kotest.matchers.types.instanceOf
import java.lang.System.lineSeparator

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
                res shouldBe instanceOf<Success<A>>()
                res as Success
                res.get shouldBe expected
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
                res shouldBe instanceOf<Failure>()
                res as Failure
                matcher(res.get)
            }
        }
    }

    fun expectFmtSuccess(vararg cases: Pair<String, String>) {
        forAll(table(
            headers("given", "expected"),
            *cases.map { row(it.first, it.second) }.toTypedArray()
        )) { given, expected ->
            when(val result = TextParsers.run(QueryParser.query, given)) {
                is Failure -> {
                    fail("expected success: ${fmt(result.get)}")
                }
                is Success -> {
                    val query: Query = result.get
                    query.toString() shouldHaveSameLines expected
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
                is Failure -> {
                    val stacktrace: StackTrace = result.get
                    val stacktraceString = fmt(stacktrace)
                    withClue(lineSeparator() + stacktraceString + lineSeparator()) {
                        stacktraceString.lines() shouldContainExactly expected.lines()
                    }
                }
                is Success -> {
                    fail("expected failure: ${result.get}")
                }
            }
        }
    }

    infix fun String.shouldHaveSameLines(other: String) {
        this.lines() shouldContainExactly other.lines()
    }

    fun <A> runParserTest(
        vararg cases: Row3<Parser<A>, String, ParseResult<A>>
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

    fun <A> ok(a: A, consumed: Int) = Success(a, consumed)
    fun err(stack: StackTrace) = Failure(stack)

}