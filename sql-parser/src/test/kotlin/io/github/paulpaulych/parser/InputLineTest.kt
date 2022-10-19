package io.github.paulpaulych.parser

import io.github.paulpaulych.parser.fmt.InputLine
import io.kotest.core.spec.style.DescribeSpec
import io.kotest.data.forAll
import io.kotest.data.headers
import io.kotest.data.row
import io.kotest.data.table
import io.kotest.matchers.shouldBe

internal class InputLineTest: DescribeSpec({

    val input = """
        aaaa bb
        
        ccc dd
    """.trimIndent()

    it("find input line by offset") {

        input.length shouldBe 15
        forAll(
            table(
                headers("state", "inputLine"),
                row(
                    State(input, offset = 0),
                    InputLine(0, 0, "aaaa bb")
                ),
                row(
                    State(input, offset = 6),
                    InputLine(0, 6, "aaaa bb")
                ),
                row(
                    State(input, offset = 7),
                    InputLine(0, 7, "aaaa bb")
                ),
                row(
                    State(input, offset = 8),
                    InputLine(1, 0, "")
                ),
                row(
                    State(input, offset = 9),
                    InputLine(2, 0, "ccc dd")
                ),
                row(
                    State(input, offset = 12),
                    InputLine(2, 3, "ccc dd")
                ),
                row(
                    State(input, offset = 14),
                    InputLine(2, 5, "ccc dd")
                ),
            )
        ) { state, expected ->
            InputLine.from(state) shouldBe expected
        }
    }

})