package io.github.paulpaulych.parser

import io.github.paulpaulych.parser.ErrorItem.ParseError
import io.github.paulpaulych.parser.ErrorItem.ScopesTried

data class StackTrace(
    val state: State,
    val error: ErrorItem,
    val isCommitted: Boolean = false,
    val cause: StackTrace? = null
) {

    fun appendFailedScopes(scopes: List<String>): StackTrace {
        return this.copy(
            error = ScopesTried(scopes = error.failedScopes() + scopes),
            cause = null
        )
    }

    fun uncommit(): StackTrace {
        return this.copy(isCommitted = false)
    }

    fun appendCommitted(isCommitted: Boolean): StackTrace {
        return this.copy(isCommitted = this.isCommitted || isCommitted)
    }

    fun addSegment(
        state: State,
        scope: String,
        message: String
    ): StackTrace {
        return StackTrace(
            state = state,
            isCommitted = this.isCommitted,
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