package io.github.paulpaulych.parser

import io.github.paulpaulych.parser.TextParsers.flatMap
import io.github.paulpaulych.parser.TextParsers.or
import io.github.paulpaulych.parser.TextParsers.string
import io.github.paulpaulych.parser.TextParsers.regex
import io.github.paulpaulych.parser.TextParsers.succeed
import java.util.regex.Pattern

object ParsersDsl {

    fun nRepeat(): Parser<Int> = regex(Regex("\\d+"))
        .flatMap { digit ->
            val count = digit.toInt()
            string("a").repeat(count).map { count }
        }

    fun <A> defer(pa: () -> Parser<A>): Parser<A> = pa()

    fun <A> Parser<A>.repeat(n: Int): Parser<List<A>> {
        if (n == 0) {
            return succeed(listOf())
        }
        return map2(this, { this.repeat(n - 1) }) { a, b -> listOf(a) + b }
    }

    fun <A> Parser<A>.many(): Parser<List<A>> {
        val parser = this
        val notEmptyList = map2(parser, { parser.many() }) { a, la -> listOf(a) + la }
        val emptyList = succeed(listOf<A>())
        return notEmptyList or emptyList
    }

    fun <A, B> Parser<A>.flatMap(f: (A) -> Parser<B>): Parser<B> {
        return flatMap(this, f)
    }

    infix fun <A> Parser<A>.or(pb: Parser<A>): Parser<A> {
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
            pb().map { b -> f(a, b) }
        }
    }

    fun <A> many1(p: Parser<A>): Parser<List<A>> {
        return map2(p, { p.many() } ) { a, b -> listOf(a) + b }
    }

    infix fun <A, B> Parser<A>.product(
        pb: Parser<B>
    ): Parser<Pair<A, B>> =
        this.flatMap { a ->
            pb.map { b -> Pair(a, b) }
        }

    infix fun <A, B> Parser<A>.skipR(p: Parser<B>): Parser<A> =
        (this product p).map { it.first }

    infix fun <A, B> Parser<A>.skipL(p: Parser<B>): Parser<B> =
        (this product p).map { it.second }


    val Regex.parser: Parser<String>
        get() = regex(this)

    val String.parser: Parser<String>
        get() = string(this)

    fun <A> surround(
        start: Parser<String>,
        stop: Parser<String>,
        parser: Parser<A>
    ): Parser<A> =
        start skipL parser skipR stop

    fun thru(s: String): Parser<String> =
        Regex(".*?${Pattern.quote(s)}").parser

    infix fun <A> Parser<A>.sep(sep: Parser<String>): Parser<List<A>> {
        val aWithSep = sep skipL this
        val notEmptyList = map2(this, { aWithSep.many() }) { a, la ->
            listOf(a) + la
        }
        return notEmptyList or succeed(listOf())
    }
}