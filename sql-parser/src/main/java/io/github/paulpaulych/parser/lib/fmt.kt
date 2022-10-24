package io.github.paulpaulych.parser.lib

import io.github.paulpaulych.parser.lib.ErrorItem.ParseError
import io.github.paulpaulych.parser.lib.ErrorItem.ScopesTried
import java.lang.System.lineSeparator

internal fun fmt(stack: StackTrace): String {
    val stacktrace = stack.segments()
        .groupBy({ it.state }) { it.error }
        .map { (state, errors) ->
            val lineInput = InputLine.from(state)
            "[${lineInput.line + 1}:${lineInput.column + 1}] ${errors.joinToString(": ") { it.message() }}"
        }
        .joinToString(lineSeparator())
    val (lastMsgState, lastMsg) = stack.segments().last()
    val errorInputLine = InputLine.from(lastMsgState)
    val lineHeader = "[${errorInputLine.line + 1}:${errorInputLine.column + 1}]"
    val line = lastMsgState.input.lines()[errorInputLine.line]
    val highlight = "here--^".padStart(errorInputLine.column + lineHeader.length + 2)
    return "stacktrace:" + lineSeparator() +
            stacktrace + lineSeparator() +
            lineSeparator() +
            lineHeader + " " + line + lineSeparator() +
            highlight + lineSeparator() +
            "error: " + lastMsg.message()
}

private fun ErrorItem.message(): String = when(this) {
    is ParseError -> message
    is ScopesTried -> "expected one of [${scopes.joinToString(", ")}]"
}

private fun StackTrace.segments(): Sequence<StackTrace> {
    return sequence {
        var cur: StackTrace? = this@segments
        while (cur != null) {
            yield(cur)
            cur = cur.cause
        }
    }
}