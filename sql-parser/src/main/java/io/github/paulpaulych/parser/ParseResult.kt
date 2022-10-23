package io.github.paulpaulych.parser

import io.github.paulpaulych.parser.ParseResult.Failure
import io.github.paulpaulych.parser.ParseResult.Success

sealed interface ParseResult<out A> {

    data class Success<A>(
        val get: A,
        val consumed: Int
    ): ParseResult<A> {
        fun advanceConsumed(n: Int) = copy(consumed = consumed + n)
    }

    data class Failure(
        val get: StackTrace
    ): ParseResult<Nothing>
}

fun <A, B> ParseResult<A>.flatMap(f: (Success<A>) -> ParseResult<B>): ParseResult<B> =
    when(this) {
        is Failure -> this
        is Success -> f(this)
    }

fun <A> ParseResult<A>.mapFailure(f: (StackTrace) -> StackTrace): ParseResult<A> =
    when(this) {
        is Failure -> Failure(f(get))
        is Success -> this
    }