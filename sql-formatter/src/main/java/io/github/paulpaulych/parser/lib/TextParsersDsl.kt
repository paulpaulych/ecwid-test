package io.github.paulpaulych.parser.lib

import io.github.paulpaulych.parser.lib.ErrorItem.ParseError
import io.github.paulpaulych.parser.lib.ParseResult.Failure
import io.github.paulpaulych.parser.lib.TextParsers.flatMap
import io.github.paulpaulych.parser.lib.TextParsers.oneOf
import io.github.paulpaulych.parser.lib.TextParsers.succeed
import io.github.paulpaulych.parser.sql.CommonSqlParsers.ws

object TextParsersDsl {

    fun <A> failed(
        scope: String,
        msg: String,
        isCommitted: Boolean = false
    ): Parser<A> =
        Parser { state ->
            Failure(StackTrace(state, ParseError(scope, msg), isCommitted))
        }

    @Suppress("UNCHECKED_CAST")
    fun <A: Any, B> Parser<A?>.ifPresent(f: (A) -> Parser<B>): Parser<B?> {
        return this.flatMap { it?.let { (ws skipL { f(it) }) as Parser<B?> } ?: succeed(null) }
    }

    fun <A> Parser<A>.defer(): () -> Parser<A> = { this }

    fun <A> Parser<A>.many(): Parser<List<A>> {
        val notEmptyList = map2(this, { this.many() }) { a, la -> listOf(a) + la }
        return notEmptyList or { succeed(listOf()) }
    }

    fun <A, B> Parser<A>.flatMap(f: (A) -> Parser<B>): Parser<B> {
        return flatMap(this, f)
    }

    fun <A> or(pa: Parser<out A>, pb: () -> Parser<out A>): Parser<A> {
        return oneOf(sequence {
            yield(pa)
            yield(pb())
        })
    }

    @JvmName("orExt")
    infix fun <A> Parser<out A>.or(pb: () -> Parser<out A>): Parser<A> {
        return or(this, pb)
    }

    fun <A, B> Parser<A>.map(f: (A) -> B): Parser<B> {
        return this.flatMap { a -> succeed(f(a)) }
    }

    fun <A, B, C> map2(
        pa: Parser<A>,
        pb: () -> Parser<B>,
        f: (A, B) -> C
    ): Parser<C> {
        return pa.flatMap { a ->
            pb().map { b -> f(a, b)  }
        }
    }

    infix fun <A, B> Parser<A>.and(pb: () -> Parser<B>): Parser<Pair<A, B>> = map2(this, pb) { a, b -> Pair(a, b) }

    infix fun <A, B> Parser<A>.and(pb: Parser<B>): Parser<Pair<A, B>> = map2(this, pb.defer()) { a, b -> Pair(a, b) }

    operator fun <A, B> Parser<A>.plus(pb: Parser<B>): Parser<Pair<A, B>> = map2(this, pb.defer()) { a, b -> Pair(a, b) }

    operator fun <A, B> Parser<A>.plus(pb: () -> Parser<B>): Parser<Pair<A, B>> = map2(this, pb) { a, b -> Pair(a, b) }

    infix fun <A, B> Parser<A>.skipR(p: Parser<B>): Parser<A> =
        (this + { p }).map { it.first }

    infix fun <A, B> Parser<A>.skipL(p: Parser<B>): Parser<B> =
        (this + { p }).map { it.second }

    infix fun <A, B> Parser<A>.skipL(p: () -> Parser<B>): Parser<B> =
        (this + p).map { it.second }

    fun <A> surround(
        start: Parser<*>,
        stop: Parser<*>,
        parser: () -> Parser<A>
    ): Parser<A> = start skipL parser skipR stop

    fun <A> surround(
        start: Parser<*>,
        stop: Parser<*>,
        parser: Parser<A>
    ): Parser<A> = surround(start, stop, parser.defer())

    infix fun <A> Parser<A>.sepBy(sep: Parser<*>): Parser<List<A>> {
        val notEmptyList = map2(this, { (sep skipL this).many() }) { a, la ->
            listOf(a) + la
        }
        return notEmptyList or { succeed(listOf()) }
    }

    infix fun <A> Parser<A>.sepBy1(sep: Parser<String>): Parser<List<A>> {
        val notEmptyList = map2(this, { (sep skipL this).many() }) { a, la ->
            listOf(a) + la
        }
        return notEmptyList
    }
}