package io.github.paulpaulych.parser.fmt

import io.github.paulpaulych.parser.State

data class InputLine(
    val line: Int,
    val column: Int,
    val content: String
) {
    companion object {
        private val ls = "\n"

        fun from(state: State): InputLine {
            val lines = state.input.splitToSequence(ls)
            var consumed = 0
            for ((idx, line) in lines.withIndex()) {
                if (consumed <= state.offset && consumed + line.length + ls.length > state.offset) {
                    return InputLine(
                        line = idx,
                        column = state.offset - consumed,
                        content = line
                    )
                }
                consumed += line.length + ls.length
            }
            //should never be reached
            throw IllegalStateException("line detection algorithm error: consumed=$consumed, state=$state")
        }
    }
}