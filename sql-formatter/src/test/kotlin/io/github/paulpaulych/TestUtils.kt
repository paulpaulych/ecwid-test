package io.github.paulpaulych

import io.github.paulpaulych.parser.FormatResult
import io.github.paulpaulych.parser.QueryFormatter
import io.github.paulpaulych.parser.lib.ParseResult
import io.github.paulpaulych.parser.lib.ParseResult.Failure
import io.github.paulpaulych.parser.lib.ParseResult.Success
import io.github.paulpaulych.parser.lib.Parser
import io.github.paulpaulych.parser.lib.StackTrace
import io.github.paulpaulych.parser.lib.TextParsers
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
            val formatter = QueryFormatter()
            val results = given.lines().flatMap { formatter.appendInput(it) }
            when(val result = results.first()) {
                is FormatResult.Failure -> {
                    fail("expected success, but was: ${result.stacktrace}")
                }
                is FormatResult.Success -> {
                    withClue(result.sql) {
                        result.sql shouldHaveSameLines expected
                    }
                }
            }
        }
    }

    fun expectFmtFailure(vararg cases: Pair<String, String>) {
        forAll(table(
            headers("given", "expected"),
            *cases.map { row(it.first, it.second) }.toTypedArray()
        )) { given, expected ->
            val formatter = QueryFormatter()
            val results = given.lines().flatMap { formatter.appendInput(it) }
            when(val result = results.first()) {
                is FormatResult.Failure -> {
                    withClue(lineSeparator() + result.stacktrace + lineSeparator()) {
                        result.stacktrace.lines() shouldContainExactly expected.lines()
                    }
                }
                is FormatResult.Success -> {
                    fail("expected failure, but was: ${result.sql}")
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