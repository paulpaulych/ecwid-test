package io.github.paulpaulych.parser

import io.github.paulpaulych.common.Either

fun interface Parser<A> {
    fun parse(state: State): Either<StackTrace, Success<A>>
}

data class Success<out A>(
    val get: A,
    val consumed: Int
) {
    fun advanceConsumed(n: Int) = copy(consumed = consumed + n)
}
