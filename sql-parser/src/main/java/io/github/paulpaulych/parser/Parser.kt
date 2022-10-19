package io.github.paulpaulych.parser

import io.github.paulpaulych.common.Either

fun interface Parser<A> {
    fun parse(location: Location): Either<Failure, Success<A>>
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

data class ParseError(
    val stack: List<Pair<Location, String>>
)

fun ParseError.push(loc: Location, msg: String): ParseError =
    this.copy(stack = listOf(loc to msg) + this.stack)

fun ParseError.tag(tag: String): ParseError {
    val latest = this.stack.firstOrNull()
        ?.first
        ?.let { Pair(it, tag) }
    return this.copy(stack = listOfNotNull(latest) + stack)
}

data class Location(
    val input: String,
    val offset: Int
) {

    private val slice by lazy { input.slice(0..offset + 1) }

    val line by lazy { slice.count { it == '\n' } + 1 }

    val column by lazy {
        when (val n = slice.lastIndexOf('\n')) {
            -1 -> offset + 1
            else -> offset - n
        }
    }

    fun advanceBy(i: Int): Location {
        return this.copy(offset = offset + i)
    }
}

fun Location.toError(msg: String) =
    ParseError(listOf(this to msg))
