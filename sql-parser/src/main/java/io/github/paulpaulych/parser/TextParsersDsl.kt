package io.github.paulpaulych.parser

import io.github.paulpaulych.parser.TextParsers.flatMap
import io.github.paulpaulych.parser.TextParsers.or
import io.github.paulpaulych.parser.TextParsers.string
import io.github.paulpaulych.parser.TextParsers.regex
import io.github.paulpaulych.parser.TextParsers.scope
import io.github.paulpaulych.parser.TextParsers.succeed
import java.util.regex.Pattern

object TextParsersDsl {

    fun <A> Parser<A>.defer(): () -> Parser<A> = { this }

    fun <A> Parser<A>.many(): Parser<List<A>> =
        or(
            map2(this, { this.many() }) { a, la -> listOf(a) + la },
            { succeed(listOf<A>()) }
        )

    fun <A, B> Parser<A>.flatMap(f: (A) -> Parser<B>): Parser<B> {
        return flatMap(this, f)
    }

    infix fun <A> Parser<A>.scope(scope: String): Parser<A> =
        scope(scope, parser = this)

    fun <A> Parser<A>.scope(scope: String, msg: String): Parser<A> =
        scope(scope, msg, this)

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

    infix fun <A, B> Parser<A>.skipR(p: Parser<B>): Parser<A> =
        (this and { p }).map { it.first }

    infix fun <A, B> Parser<A>.skipL(p: Parser<B>): Parser<B> =
        (this and { p }).map { it.second }

    val Regex.parser: Parser<String>
        get() = regex(this)

    val String.parser: Parser<String>
        get() = string(this)

    fun <A> surround(
        start: Parser<*>,
        stop: Parser<*>,
        parser: Parser<A>
    ): Parser<A> =
        start skipL parser skipR stop

    fun thru(s: String): Parser<String> =
        Regex(".*?${Pattern.quote(s)}").parser

    infix fun <A> Parser<A>.sepBy(sep: Parser<String>): Parser<List<A>> {
        val aWithSep = sep skipL this
        val notEmptyList = map2(this, { aWithSep.many() }) { a, la ->
            listOf(a) + la
        }
        return notEmptyList or { succeed(listOf()) }
    }
}