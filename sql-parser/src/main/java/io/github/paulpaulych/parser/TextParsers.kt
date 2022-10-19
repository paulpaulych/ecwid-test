package io.github.paulpaulych.parser

import io.github.paulpaulych.common.*
import io.github.paulpaulych.common.Either.Left
import io.github.paulpaulych.common.Either.Right

object TextParsers {

    fun string(s: String): Parser<String> =
        Parser { state: State ->
            when (val idx = firstNonMatchingIndex(s, state.input, state.offset)) {
                null -> Right(Success(s, s.length))
                else -> Left(state.advanceBy(idx).toError("'$s'").tag("expected"))
            }
        }

    fun regex(regex: Regex): Parser<String> =
        Parser { location ->
            when (val prefix = location.input.findPrefixMatching(regex, location.offset)) {
                null -> Left(location.toError("expression matching regex ($regex)").tag("expected"))
                else -> Right(Success(prefix, prefix.length))
            }
        }

    fun <A> succeed(a: A): Parser<A> =
        Parser { Right(Success(a, 0)) }

    fun <A> or(pa: Parser<out A>, pb: () -> Parser<out A>): Parser<A> {
        return Parser { state ->
            val res = pa.parse(state)
            res.flatMapLeft {
                pb().parse(state)
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
                    .map { bSuccess ->
                        bSuccess.advanceConsumed(aSuccess.consumed)
                    }
            }
        }

    fun <A> scope(msg: String, p: Parser<A>): Parser<A> =
        Parser { state -> p.parse(state)
            .mapLeft { error -> error.push(state, msg) }
        }

    fun <A> tag(msg: String, p: Parser<A>): Parser<A> =
        Parser { state ->
            p.parse(state).mapLeft { error -> error.tag(msg) }
        }

//    fun <A> attempt(p: Parser<A>): Parser<A> =
//        Parser { state -> p.parse(state).mapLeft { it.uncommit() }}

    fun <A> run(p: Parser<A>, input: String): Either<ParseError, Success<A>> {
        return p.parse(State(input = input, offset = 0))
    }
}
