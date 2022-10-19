package io.github.paulpaulych.parser

import io.github.paulpaulych.parser.ErrorItem.ParseError
import io.github.paulpaulych.parser.ErrorItem.ScopesTried

data class StackTrace(
    val state: State,
    val error: ErrorItem,
    val cause: StackTrace? = null
) {

    fun appendFailedScopes(scopes: List<String>): StackTrace {
        return this.copy(error = ScopesTried(scopes = error.failedScopes() + scopes))
    }

    fun addSegment(
        state: State,
        scope: String,
        message: String
    ): StackTrace {
        return StackTrace(
            state = state,
            error = ParseError(scope, message),
            cause = this
        )
    }
}

sealed interface ErrorItem {

    data class ParseError(
        val scope: String,
        val message: String
    ): ErrorItem

    data class ScopesTried(
        val scopes: List<String>
    ): ErrorItem

    fun failedScopes(): List<String> = when(this) {
        is ParseError -> listOf(scope)
        is ScopesTried -> scopes
    }
}