package io.github.paulpaulych

import io.github.paulpaulych.common.Either
import io.github.paulpaulych.common.Either.Left
import io.github.paulpaulych.common.Either.Right
import io.github.paulpaulych.parser.*
import io.kotest.data.*
import io.kotest.matchers.shouldBe

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
            res as Right
            res.value.get shouldBe expected
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
            res as Left
            matcher(res.value)
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