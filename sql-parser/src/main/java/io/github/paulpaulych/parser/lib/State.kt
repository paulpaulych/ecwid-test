package io.github.paulpaulych.parser.lib

data class State(
    val input: String,
    val offset: Int
) {

    init {
        require(offset <= input.length) {
            "offset cannot be grater than input len but given offset=$offset for input=$input"
        }
    }

    fun advanceBy(i: Int): State {
        return this.copy(offset = offset + i)
    }
}