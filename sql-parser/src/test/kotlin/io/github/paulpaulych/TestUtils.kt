package io.github.paulpaulych

import io.github.paulpaulych.common.Either
import io.github.paulpaulych.parser.*
import io.kotest.data.Row3
import io.kotest.data.forAll
import io.kotest.data.headers
import io.kotest.data.table
import io.kotest.matchers.shouldBe

object TestUtils {

    fun <A> runParserTest(
        vararg cases: Row3<Parser<A>, String, Either<Failure, Success<A>>>
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

    fun <A> ok(a: A, consumed: Int) = Either.Right(Success(a, consumed))
    fun err(isCommitted: Boolean, stack: List<Pair<State, String>>) =
        Either.Left(Failure(ParseError(stack.toList()), isCommitted))

}