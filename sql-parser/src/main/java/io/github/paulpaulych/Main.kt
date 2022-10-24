package io.github.paulpaulych

import io.github.paulpaulych.parser.FormatResult.Failure
import io.github.paulpaulych.parser.FormatResult.Success
import io.github.paulpaulych.parser.QueryFormatter

fun main() {
    val formatter = QueryFormatter()

    println("Type or insert SQL query.. (Tip: don't forget ';') :")

    while (true) {
        val line = readLine() ?: error("error reading input")

        for (res in formatter.appendInput(line)) {
            when (res) {
                is Failure -> {
                    println("Oops.. Given invalid source:")
                    println()
                    println(res.source)
                    println()
                    println("ERROR:")
                    println()
                    println(res.stacktrace)
                    println()
                }
                is Success -> {
                    println("Your query was successfully formatted:")
                    println("SOURCE:")
                    println()
                    println(res.source)
                    println()
                    println("FORMATTED:")
                    println()
                    println(res.sql)
                    println()
                }
            }
        }
    }
}
