package io.github.paulpaulych.parser

import io.github.paulpaulych.common.Either

fun interface Parser<A> {
    fun parse(state: State): Either<ParseError, Success<A>>
}

data class Success<out A>(
    val get: A,
    val consumed: Int
) {
    fun advanceConsumed(n: Int) = copy(consumed = consumed + n)
}

data class Failure(
    val error: ParseError,
    val isCommitted: Boolean
) {
    fun uncommit(): Failure =
        if (!this.isCommitted) this
        else Failure(this.error, false)

    fun addCommit(commit: Boolean): Failure =
        this.copy(isCommitted = this.isCommitted || commit)
}
