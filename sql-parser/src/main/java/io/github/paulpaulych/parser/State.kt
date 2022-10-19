package io.github.paulpaulych.parser

data class State(
    val input: String,
    val offset: Int
) {

    init {
        require(offset <= input.length) {
            "offset cannot be grater than input len but given offset=$offset for input=$input"
        }
    }

    private val slice by lazy { input.slice(0..offset + 1) }

    val line by lazy { slice.count { it == '\n' } }

    val column by lazy {
        when (val n = slice.lastIndexOf('\n')) {
            -1 -> offset
            else -> offset - n - 1
        }
    }

    fun advanceBy(i: Int): State {
        return this.copy(offset = offset + i)
    }
}