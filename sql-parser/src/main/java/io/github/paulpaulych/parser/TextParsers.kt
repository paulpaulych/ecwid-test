package io.github.paulpaulych.parser

import io.github.paulpaulych.common.*
import io.github.paulpaulych.common.Either.Left
import io.github.paulpaulych.common.Either.Right
import io.github.paulpaulych.parser.ErrorItem.ParseError
import io.github.paulpaulych.parser.TextParsersDsl.defer
import io.github.paulpaulych.parser.TextParsersDsl.or

object TextParsers {

    fun string(s: String): Parser<String> =
        Parser { state: State ->
            when (val idx = firstNonMatchingIndex(s, state.input, state.offset)) {
                null -> Right(Success(s, s.length))
                else -> {
                    val scope = "'$s'"
                    Left(StackTrace(
                        state = state.advanceBy(idx),
                        error = ParseError(scope = scope, message = "expected $scope"),
                        isCommitted = idx != 0,
                        cause = null
                    ))
                }
            }
        }

    fun regex(regex: Regex): Parser<String> =
        Parser { location ->
            when (val prefix = location.input.findPrefixMatching(regex, location.offset)) {
                null -> {
                    val scope = "expression matching regex '$regex'"
                    Left(StackTrace(
                        state = location,
                        error = ParseError(scope = scope, "expected $scope"),
                        isCommitted = false,
                        cause = null
                    ))
                }
                else -> Right(Success(prefix, prefix.length))
            }
        }

    // TODO: make regular expression
    fun notEof(): Parser<Unit> =
        Parser { state ->
            if (state.offset < state.input.length) {
                Right(Success(Unit, 0))
            } else {
                Left(StackTrace(
                    state = state,
                    error = ParseError("not end of input", "end of input found"),
                    isCommitted = false,
                    cause = null
                ))
            }
        }

    fun <A> succeed(a: A): Parser<A> =
        Parser { Right(Success(a, 0)) }

    fun <A> oneOf(parsers: Sequence<Parser<out A>>): Parser<A> =
        Parser { state ->
            var curErr: StackTrace? = null
            for (next in parsers) {
                if (curErr != null && curErr.isCommitted) {
                    return@Parser Left(curErr)
                }
                val nextRes = next.parse(state)
                if (nextRes !is Left) {
                    return@Parser nextRes
                }
                val nexErr = nextRes.value
                curErr = nexErr.takeIf { it.isCommitted }
                        ?: curErr
                        ?.appendFailedScopes(nextRes.value.error.failedScopes())
                        ?: nextRes.value
            }
            curErr?.let(::Left) ?: throw IllegalStateException("empty parser sequence given")
        }

    fun <A, B> flatMap(pa: Parser<A>, f: (A) -> Parser<B>): Parser<B> =
        Parser { location ->
            val aResult = pa.parse(location)
            aResult.flatMap { aSuccess ->
                val pb = f(aSuccess.get)
                val newLocation = location.advanceBy(aSuccess.consumed)
                val bResult = pb.parse(newLocation)
                bResult
                    .mapLeft { bErr ->
                        bErr.appendCommitted(isCommitted = aSuccess.consumed != 0)
                    }
                    .map { bSuccess ->
                        bSuccess.advanceConsumed(aSuccess.consumed)
                    }
            }
        }

    fun <A> scoped(
        scope: String,
        msg: String = "invalid $scope syntax",
        parser: Parser<A>
    ): Parser<A> = Parser { state ->
        parser
            .parse(state)
            .mapLeft { stackTrace ->
                stackTrace.addSegment(state, scope, msg)
            }
    }

    fun <A> Parser<A>.optional(): Parser<A?> =
        this.attempt() or succeed(null).defer()

    fun <A> Parser<A>.attempt(): Parser<A> =
        Parser { state -> this.parse(state).mapLeft { it.uncommit() }}

    fun <A> Parser<A>.peekOnly(): Parser<A> =
        Parser { state ->
            when(val res = this.parse(state)) {
                is Left -> res
                is Right -> Right(res.value.copy(consumed = 0))
            }
        }

    fun <A> run(p: Parser<A>, input: String): Either<StackTrace, Success<A>> {
        return p.parse(State(input = input, offset = 0))
    }
}
