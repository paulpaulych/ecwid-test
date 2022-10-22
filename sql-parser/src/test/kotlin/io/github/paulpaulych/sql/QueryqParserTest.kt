package io.github.paulpaulych.sql

import io.github.paulpaulych.common.Either.Left
import io.github.paulpaulych.common.Either.Right
import io.github.paulpaulych.parser.TextParsers
import io.github.paulpaulych.parser.fmt.fmt
import io.kotest.assertions.fail
import io.kotest.core.spec.style.DescribeSpec
import io.kotest.matchers.collections.shouldContainExactly

class QueryParserTest: DescribeSpec({

    fun fmtTest(given: String, expected: String) {
        val parser = QueryParser.query
        when(val result = TextParsers.run(parser, given)) {
            is Left -> {
                fail("expected success: ${fmt(result.value)}")
            }
            is Right -> {
                val query: Query = result.value.get
                query.toString().lines() shouldContainExactly expected.lines()
            }
        }
    }

    it("case1") {
        //language=SQL
        fmtTest(
            """
                SELECT author.name, count(book.id), sum(book.cost) 
                FROM author 
                LEFT JOIN book ON (author.id = book.author_id) 
                GROUP BY author.name 
                HAVING COUNT(*) > 1 AND SUM(book.cost) > 500
                LIMIT 10
                OFFSET 172
            """.trimIndent(),
            """
                SELECT author.name, count(book.id), sum(book.cost)
                FROM (author LEFT JOIN book ON (author.id = book.author_id))
                GROUP BY author.name
                HAVING ((COUNT(*) > 1) AND (SUM(book.cost) > 500))
                LIMIT 10
                OFFSET 172
            """.trimIndent(),
        )
    }

})