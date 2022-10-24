package io.github.paulpaulych

import io.github.paulpaulych.FormatResult.Failure
import io.github.paulpaulych.FormatResult.Success
import io.github.paulpaulych.formatting.fmt
import io.github.paulpaulych.parser.lib.ParseResult
import io.github.paulpaulych.parser.lib.TextParsers
import io.github.paulpaulych.parser.lib.TextParsers.string
import io.github.paulpaulych.parser.lib.TextParsersDsl.skipR
import io.github.paulpaulych.parser.lib.fmt
import io.github.paulpaulych.parser.sql.QueryParser

private const val QUERY_SEP = ';'
private const val LINE_SEP = "\n"

fun main() {

    val queryAcc = StringBuilder()

    println("Type or insert SQL query.. (Tip: don't forget ';') :")

    while (true) {
        val line = readLine() ?: error("error reading input")
        if (";" !in line) {
            queryAcc.append(line).append(LINE_SEP)
            continue
        }

        queryAcc.append(line.substringBefore(QUERY_SEP) + QUERY_SEP)

        when (val res = formatSql(queryAcc.toString())) {
            is Failure -> {
                println("Oops.. Enjoy the stacktrace)))")
                println()
                println(res.stacktrace)
            }
            is Success -> {
                println("Your query successfully formatted")
                println(res.sql)
            }
        }

        queryAcc.clear()
        queryAcc.append(line.substringAfter(QUERY_SEP)).append(LINE_SEP)
    }
}

sealed interface FormatResult {
    data class Success(val sql: String): FormatResult
    data class Failure(val stacktrace: String): FormatResult
}

fun formatSql(query: String): FormatResult {
    val parser = QueryParser.query skipR string(";")
    return when(val res = TextParsers.run(parser, query)) {
        is ParseResult.Failure -> Failure(fmt(res.get))
        is ParseResult.Success -> Success(res.get.fmt())
    }
}