package io.github.paulpaulych.parser

import io.github.paulpaulych.common.*
import io.github.paulpaulych.common.Either.Left
import io.github.paulpaulych.common.Either.Right

object TextParsers {

    fun string(s: String): Parser<String> =
        { location: Location ->
            when (val idx = firstNonMatchingIndex(s, location.input, location.offset)) {
                null -> Right(Success(s, s.length))
                else -> Left(Failure(
                    error = location.advanceBy(idx).toError("'$s'"),
                    isCommitted = idx != 0
                ))
            }
        }

    fun regex(regex: Regex): Parser<String> =
        { location ->
            when (val prefix = location.input.findPrefixMatching(regex)) {
                null -> Left(Failure(
                    error = location.toError("regex $regex"),
                    isCommitted = false
                ))
                else -> Right(Success(prefix.value, prefix.value.length))
            }
        }

    fun <A> succeed(a: A): Parser<A> =
        { Right(Success(a, 0)) }

    fun <A> slice(pa: Parser<A>): Parser<String> =
        { location ->
            when (val res = pa(location)) {
                is Left -> res
                is Right -> Right(Success(location.slice(res.value.consumed), res.value.consumed))
            }
        }

    fun <A> scope(msg: String, p: Parser<A>): Parser<A> =
        { state -> p(state)
            .mapLeft { failure -> failure.copy(error = failure.error.push(state, msg)) }
        }

    fun <A> tag(msg: String, p: Parser<A>): Parser<A> =
        { state ->
            p(state).mapLeft { failure -> failure.copy(error = failure.error.withNewTag(msg)) }
        }

    fun <A> attempt(p: Parser<A>): Parser<A> =
        { state -> p(state).mapLeft { it.uncommit() }}

    fun <A> or(pa: Parser<A>, pb: Parser<A>): Parser<A> {
        return { state ->
            val res = pa(state)
            res.flatMapLeft { failure ->
                if (failure.isCommitted) res
                else pa(state)
            }
        }
    }

    fun <A, B> flatMap(pa: Parser<A>, f: (A) -> Parser<B>): Parser<B> =
        { state ->
            val aResult = pa(state)
            aResult.flatMap { aSuccess ->
                val pb = f(aSuccess.a)
                pb(state.advanceBy(aSuccess.consumed))
                    .mapLeft { failure -> failure.addCommit(aSuccess.consumed != 0) }
                    .map { bSuccess -> bSuccess.advanceConsumed(bSuccess.consumed) }
            }
        }

    fun <A> run(p: Parser<A>, input: String): Either<Failure, Success<A>> {
        return p(Location(input = input, offset = 0))
    }
}
