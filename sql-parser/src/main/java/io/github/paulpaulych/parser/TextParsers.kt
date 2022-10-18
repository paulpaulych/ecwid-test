package io.github.paulpaulych.parser

import io.github.paulpaulych.common.*
import io.github.paulpaulych.common.Either.Left
import io.github.paulpaulych.common.Either.Right

object TextParsers {

    fun string(s: String): Parser<String> =
        Parser { location: Location ->
            when (val idx = firstNonMatchingIndex(s, location.input, location.offset)) {
                null -> Right(Success(s, s.length))
                else -> Left(Failure(
                    error = location.advanceBy(idx).toError("'$s'").tag("expected"),
                    isCommitted = false
                ))
            }
        }

    fun regex(regex: Regex): Parser<String> =
        Parser { location ->
            when (val prefix = location.input.findPrefixMatching(regex, location.offset)) {
                null -> Left(Failure(
                    error = location.toError("regex ($regex)").tag("expected expression matching"),
                    isCommitted = false
                ))
                else -> Right(Success(prefix, prefix.length))
            }
        }

    fun <A> succeed(a: A): Parser<A> =
        Parser { Right(Success(a, 0)) }

    fun <A> slice(pa: Parser<A>): Parser<String> =
        Parser { location ->
            when (val res = pa.parse(location)) {
                is Left -> res
                is Right -> Right(Success(location.slice(res.value.consumed), res.value.consumed))
            }
        }

    fun <A> or(pa: Parser<out A>, pb: () -> Parser<out A>): Parser<A> {
        return Parser { state ->
            val res = pa.parse(state)
            res.flatMapLeft { failure ->
                if (failure.isCommitted) res
                else pb().parse(state)
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
                    //TODO commit = aSuccess.consumed != 0
                    .mapLeft { failure -> failure.addCommit(false) }
                    .map { bSuccess ->
                        bSuccess.advanceConsumed(aSuccess.consumed)
                    }
            }
        }

    fun <A> scope(msg: String, p: Parser<A>): Parser<A> =
        Parser { state -> p.parse(state)
            .mapLeft { failure -> failure.copy(error = failure.error.push(state, msg)) }
        }

    fun <A> tag(msg: String, p: Parser<A>): Parser<A> =
        Parser { state ->
            p.parse(state).mapLeft { failure -> failure.copy(error = failure.error.tag(msg)) }
        }

    fun <A> attempt(p: Parser<A>): Parser<A> =
        Parser { state -> p.parse(state).mapLeft { it.uncommit() }}

    fun <A> run(p: Parser<A>, input: String): Either<Failure, Success<A>> {
        return p.parse(Location(input = input, offset = 0))
    }
}
