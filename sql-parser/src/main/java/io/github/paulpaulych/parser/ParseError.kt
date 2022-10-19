package io.github.paulpaulych.parser

data class ParseError(
    val stack: List<Pair<State, String>>
)

fun ParseError.push(loc: State, msg: String): ParseError =
    this.copy(stack = listOf(loc to msg) + this.stack)

fun ParseError.tag(tag: String): ParseError {
    val latest = this.stack.firstOrNull()
        ?.first
        ?.let { Pair(it, tag) }
    return this.copy(stack = listOfNotNull(latest) + stack)
}
