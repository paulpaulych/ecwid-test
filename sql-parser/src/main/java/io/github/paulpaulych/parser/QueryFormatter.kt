package io.github.paulpaulych.parser

import io.github.paulpaulych.formatting.fmt
import io.github.paulpaulych.parser.lib.ParseResult
import io.github.paulpaulych.parser.lib.TextParsers
import io.github.paulpaulych.parser.lib.TextParsersDsl.skipR
import io.github.paulpaulych.parser.lib.fmt
import io.github.paulpaulych.parser.sql.QueryParser

private const val QUERY_SEP = ";"
private const val LINE_SEP = "\n"

class QueryFormatter {
    private val queryAcc = StringBuilder()

    fun appendInput(line: String): List<FormatResult> {
        val results = mutableListOf<FormatResult>()
        var lastIdx = -1
        var curIdx: Int?
        do {
            val searchStart = lastIdx + 1
            if (line.length <= searchStart) {
                break
            }
            curIdx = line.indexOf(QUERY_SEP, startIndex = searchStart)
            if (curIdx == -1) {
                queryAcc.append(line.substring(searchStart)).append(LINE_SEP).toString()
            } else {
                val query = queryAcc.append(line.substring(searchStart, curIdx)).toString()
                results += formatSql(query + QUERY_SEP)
                queryAcc.clear()
            }
            lastIdx = curIdx
        } while (curIdx != -1)
        return results
    }

    private fun formatSql(query: String): FormatResult {
        val parser = QueryParser.query skipR TextParsers.string(";")
        return when(val res = TextParsers.run(parser, query)) {
            is ParseResult.Failure -> FormatResult.Failure(fmt(res.get), source = query)
            is ParseResult.Success -> FormatResult.Success(res.get.fmt(), source = query)
        }
    }
}

sealed interface FormatResult {
    data class Success(val sql: String, val source: String): FormatResult
    data class Failure(val stacktrace: String, val source: String): FormatResult
}