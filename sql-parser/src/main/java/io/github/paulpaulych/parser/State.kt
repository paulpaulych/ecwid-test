package io.github.paulpaulych.parser

data class State(
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

    fun advanceBy(i: Int): State {
        return this.copy(offset = offset + i)
    }
}

fun State.toError(msg: String) =
    ParseError(listOf(this to msg))
