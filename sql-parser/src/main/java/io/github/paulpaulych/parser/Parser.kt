package io.github.paulpaulych.parser

fun interface Parser<A> {
    fun parse(state: State): ParseResult<A>
}
