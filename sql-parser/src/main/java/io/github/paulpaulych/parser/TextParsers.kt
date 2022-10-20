package io.github.paulpaulych.parser

import io.github.paulpaulych.common.*
import io.github.paulpaulych.common.Either.Left
import io.github.paulpaulych.common.Either.Right
import io.github.paulpaulych.parser.ErrorItem.ParseError

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

    fun <A> succeed(a: A): Parser<A> =
        Parser { Right(Success(a, 0)) }

    fun <A> or(pa: Parser<out A>, pb: () -> Parser<out A>): Parser<A> {
        return Parser { state ->
            val firstRes = pa.parse(state)
            firstRes.flatMapLeft { firstErr ->
                if (firstErr.isCommitted) {
                    firstRes
                } else {
                    val secondRes = pb().parse(state)
                    secondRes.mapLeft { secondErr ->
                        if (!secondErr.isCommitted) {
                            firstErr.appendFailedScopes(secondErr.error.failedScopes())
                        } else {
                            secondErr
                        }
                    }
                }
            }
        }
    }

    fun <A, B> flatMap(pa: Parser<A>, f: (A) -> Parser<B>): Parser<B> =
        Parser { location ->
            val aResult = pa.parse(location)
            aResult.flatMap { aSuccess ->
                val pb = f(aSuccess.get)
                val newLocation = location.advanceBy(aSuccess.consumed)
                pb.parse(newLocation)
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

    fun <A> optional(parser: Parser<A>): Parser<A?> =
        Parser { state ->
            parser.parse(state).flatMapLeft { succeed(null).parse(state) }
        }

    fun <A> Parser<A>.opt(): Parser<A?> = optional(this)

    fun <A> Parser<A>.attempt(): Parser<A> =
        Parser { state -> this.parse(state).mapLeft { it.uncommit() }}

    fun <A> run(p: Parser<A>, input: String): Either<StackTrace, Success<A>> {
        return p.parse(State(input = input, offset = 0))
    }
}
