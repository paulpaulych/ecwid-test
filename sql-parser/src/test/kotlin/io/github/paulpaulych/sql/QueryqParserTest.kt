package io.github.paulpaulych.sql

import io.kotest.core.spec.style.DescribeSpec

class QueryParserTest: DescribeSpec({

    """
        SELECT author.name, count(book.id), sum(book.cost) 
        FROM author 
        LEFT JOIN book ON (author.id = book.author_id) 
        GROUP BY author.name 
        HAVING COUNT(*) > 1 AND SUM(book.cost) > 500
        LIMIT 10
    """.trimIndent()
})