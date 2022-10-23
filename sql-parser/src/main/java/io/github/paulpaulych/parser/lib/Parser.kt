package io.github.paulpaulych.parser.lib

fun interface Parser<A> {
    fun parse(state: State): ParseResult<A>
}
