package io.github.paulpaulych.formatting

class IndentBuffer private constructor(
    private val buffer: StringBuilder,
    private val indent: String,
    private val indentStep: String
) {

    private var columnAfterIndent = 0
    private var isLineIndented = false

    fun appendText(text: String): IndentBuffer {
        appendIndent()
        this.buffer.append(text)
        this.columnAfterIndent += text.length
        return this
    }

    fun newLine(): IndentBuffer {
        appendLineSeparator()
        this.columnAfterIndent = 0
        this.isLineIndented = false
        return this
    }

    fun appendIndented(
        resetColumn: Boolean,
        times: Int = 1,
        f: (IndentBuffer) -> Unit = {}
    ): IndentBuffer {
        appendLineSeparator()
        if (resetColumn) {
            this.columnAfterIndent = 0
        }
        val subQueryCursor = IndentBuffer(
            buffer = this.buffer,
            indent = this.indent + this.indentStep.repeat(times) + " ".repeat(this.columnAfterIndent),
            indentStep = this.indentStep
        )
        f(subQueryCursor)
        if (!resetColumn) {
            this.buffer.append(this.indent, " ".repeat(this.columnAfterIndent))
        }
        return this
    }

    fun getResult(): String = buffer.toString()

    fun shiftColumn(columns: Int): IndentBuffer {
        require(columnAfterIndent + columns >= 0) {
            error("cannot shift to negative position")
        }
        this.columnAfterIndent += columns
        return this
    }

    private fun appendIndent(): IndentBuffer {
        if (!isLineIndented) {
            this.buffer.append(indent)
            this.isLineIndented = true
        }
        return this
    }

    private fun appendLineSeparator() {
        this.buffer.append(System.lineSeparator())
    }

    companion object {

        fun create(
            initialIndent: String,
            indentStep: String
        ): IndentBuffer = IndentBuffer(
            buffer = StringBuilder(),
            indent = initialIndent,
            indentStep = indentStep
        )
    }
}

fun <A> IndentBuffer.appendAll(
    items: List<A>,
    append: (IndentBuffer, A) -> Unit,
    sep: (IndentBuffer) -> Unit
): IndentBuffer {
    val head = items.firstOrNull()
    val tail = items.drop(1)
    if (head == null) {
        return this
    }

    append(this, head)
    for (item in tail) {
        sep(this)
        append(this, item)
    }
    return this
}
