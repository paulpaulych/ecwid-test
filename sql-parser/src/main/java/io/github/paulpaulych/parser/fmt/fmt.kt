package io.github.paulpaulych.parser.fmt

import io.github.paulpaulych.parser.ErrorItem
import io.github.paulpaulych.parser.ErrorItem.ParseError
import io.github.paulpaulych.parser.ErrorItem.ScopesTried
import io.github.paulpaulych.parser.StackTrace
import java.lang.System.lineSeparator

internal fun fmt(stack: StackTrace): String {
    val stacktrace = stack.segments()
        .groupBy({ it.state }) { it.error }
        .map { (state, errors) -> "[${state.line + 1}:${state.column + 1}] ${errors.joinToString(": ") { it.message() }}" }
        .joinToString(lineSeparator())
    val (lastMsgState, lastMsg) = stack.segments().last()
    val lineHeader = "[${lastMsgState.line + 1}:${lastMsgState.column + 1}]"
    val line = lastMsgState.input.lines()[lastMsgState.line]
    val highlight = "here --^".padStart(lastMsgState.column + lineHeader.length + 3)
    return "stacktrace:\n" +
            stacktrace + lineSeparator() +
            lineSeparator() +
            lineHeader + ": " + line + lineSeparator() +
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