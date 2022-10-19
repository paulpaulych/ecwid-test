package io.github.paulpaulych.common

import io.github.paulpaulych.common.Either.Left
import io.github.paulpaulych.common.Either.Right

sealed interface Either<out E, out A> {

    data class Left<L>(val value: L): Either<L, Nothing>

    data class Right<R>(val value: R): Either<Nothing, R>
}

fun <E, A, B> Either<E, A>.map(f: (A) -> B): Either<E, B> =
    when(this) {
        is Left -> this
        is Right -> Right(f(value))
    }

fun <E, A, B> Either<E, A>.flatMap(f: (A) -> Either<E, B>): Either<E, B> =
    when(this) {
        is Left -> this
        is Right -> f(value)
    }

fun <E, A, ER> Either<E, A>.mapLeft(f: (E) -> ER): Either<ER, A> =
    when(this) {
        is Left -> Left(f(value))
        is Right -> this
    }

fun <E, A, ER> Either<E, A>.flatMapLeft(f: (E) -> Either<ER, A>): Either<ER, A> =
    when(this) {
        is Left -> f(value)
        is Right -> this
    }