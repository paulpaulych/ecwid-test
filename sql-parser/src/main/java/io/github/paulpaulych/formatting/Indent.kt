package io.github.paulpaulych.formatting

data class Indent(
    val value: String
) {
    fun append() = Indent("$value  ")

    companion object {
        fun empty() = Indent("")
    }

    operator fun plus(indent: Indent): Indent =
        Indent(this.value + indent.value)

    operator fun plus(s: String): String =
        this.value + s

    override fun toString() = value
}

private operator fun String.plus(indent: Indent) = this + indent.value
