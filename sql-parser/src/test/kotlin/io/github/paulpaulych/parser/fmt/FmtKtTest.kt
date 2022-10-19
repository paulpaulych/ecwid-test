package io.github.paulpaulych.parser.fmt

import io.github.paulpaulych.parser.ErrorItem.ParseError
import io.github.paulpaulych.parser.StackTrace
import io.github.paulpaulych.parser.State
import io.kotest.core.spec.style.DescribeSpec
import io.kotest.data.forAll
import io.kotest.data.headers
import io.kotest.data.row
import io.kotest.data.table
import io.kotest.matchers.collections.shouldContainExactly

internal class FmtKtTest: DescribeSpec({

    it("error fmt") {
        forAll(
            table(
                headers("errors", "message"),
                row(
                    StackTrace(
                        state = State("""Hello world""", 0),
                        error = ParseError("greeting", "invalid greeting"),
                        cause = StackTrace(
                            state = State("""Hello world""", 5),
                            error = ParseError("', world!'", "expected ', world!'")
                        )
                    ),
                    """
                        stacktrace:
                        [1:1] invalid greeting
                        [1:6] expected ', world!'
                        
                        [1:6] Hello world
                            here --^
                        error: expected ', world!'
                    """.trimIndent()
                )
            )
        ) { stack, expected ->
            fmt(stack).lines() shouldContainExactly expected.lines()
        }
    }

})