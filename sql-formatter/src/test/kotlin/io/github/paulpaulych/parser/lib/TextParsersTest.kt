package io.github.paulpaulych.parser.lib

import io.github.paulpaulych.TestUtils.err
import io.github.paulpaulych.TestUtils.ok
import io.github.paulpaulych.TestUtils.runParserTest
import io.github.paulpaulych.parser.lib.ErrorItem.ParseError
import io.github.paulpaulych.parser.lib.ErrorItem.ScopesTried
import io.github.paulpaulych.parser.lib.TextParsers.flatMap
import io.github.paulpaulych.parser.lib.TextParsers.regex
import io.github.paulpaulych.parser.lib.TextParsers.scoped
import io.github.paulpaulych.parser.lib.TextParsers.string
import io.github.paulpaulych.parser.lib.TextParsers.succeed
import io.github.paulpaulych.parser.lib.TextParsersDsl.and
import io.github.paulpaulych.parser.lib.TextParsersDsl.defer
import io.github.paulpaulych.parser.lib.TextParsersDsl.or
import io.kotest.core.spec.style.DescribeSpec
import io.kotest.data.row

internal class TextParsersTest : DescribeSpec({

    it("string parser") {
        runParserTest(
            row(string("aa"), "aa", ok("aa", consumed = 2)),
            row(string("aa"), "aaa", ok("aa", consumed = 2)),
            row(string(""), "aaa", ok("", consumed = 0)),
            row(string("ab"), "bbb", err(
                stack = StackTrace(
                    state = State("bbb", 0),
                    error = ParseError("'ab'", "expected 'ab'"),
                )
            )),
            row(string("ab"), "aaa", err(
                stack = StackTrace(
                    isCommitted = true,
                    state = State("aaa", 1),
                    error = ParseError("'ab'", "expected 'ab'"),
                )
            )),
            row(string("aaa"), "aa", err(
                stack = StackTrace(
                    isCommitted = true,
                    state = State("aa", 2),
                    error = ParseError("'aaa'", "expected 'aaa'"),
                )
            )),
            row(string("aaa"), "bbb", err(
                stack = StackTrace(
                    state = State("bbb", 0),
                    error = ParseError("'aaa'", "expected 'aaa'"),
                )
            )),
            row(string("aaa"), "aab", err(
                stack = StackTrace(
                    isCommitted = true,
                    state = State("aab", 2),
                    error = ParseError("'aaa'", "expected 'aaa'"),
                )
            )),
        )
    }

    it("regex parser") {
        runParserTest(
            row(regex(Regex("\\d+")), "11aa", ok("11", consumed = 2)),
            row(regex(Regex("\\d+")), "11", ok("11", consumed = 2)),
            row(regex(Regex("\\d+")), "aa11", err(
                stack = StackTrace(
                    state = State("aa11", 0),
                    error = ParseError("expression matching regex '\\d+'", "expected expression matching regex '\\d+'"),
                )
            )),
            row(regex(Regex("\\d+")), "aa", err(
                stack = StackTrace(
                    state = State("aa", 0),
                    error = ParseError("expression matching regex '\\d+'", "expected expression matching regex '\\d+'"),
                )
            )),
        )
    }

    it("succeeds parser") {
        runParserTest(
            row(succeed(2), "", ok(2, consumed = 0)),
            row(succeed("1235"), "aasdasdff", ok("1235", consumed = 0)),
        )
    }

    it("or parser") {
        runParserTest(
            row(string("abc") or { string("aaa") }, "abc", ok("abc", consumed = 3)),
            row(string("abc") or { string("aaa") }, "aaavvv", err(
                stack = StackTrace(
                    isCommitted = true,
                    state = State("aaavvv", 1),
                    error = ParseError("'abc'", "expected 'abc'")
                )
            )),
            row(regex(Regex("\\d+")) or { string("aaa") }, "11aa", ok("11", consumed = 2)),
            row(regex(Regex("\\d+")) or { string("aaa") }, "aaa", ok("aaa", consumed = 3)),
            row(string("bb") or { string("aa") }, "ccaabb", err(
                stack = StackTrace(
                    state = State("ccaabb", 0),
                    error = ScopesTried(listOf("'bb'", "'aa'"))
                )
            )),
        )
    }

    it("flatMap parser - context dependent parsers") {
        val alphabeticallyNext: (String) -> Parser<String> = { prev ->
            string(prev.first().inc().toString())
        }
        runParserTest(
            row(flatMap(string("a"), alphabeticallyNext), "abc", ok("b", consumed = 2)),
            row(flatMap(string("a"), alphabeticallyNext), "acb", err(
                stack = StackTrace(
                    isCommitted = true,
                    state = State("acb", 1),
                    error = ParseError("'b'", "expected 'b'"),
                )
            )),
            row(flatMap(string("b"), alphabeticallyNext), "acb", err(
                stack = StackTrace(
                    state = State("acb", 0),
                    error = ParseError("'b'", "expected 'b'"),
                )
            )),
        )
    }

    it("scope combinator") {
        runParserTest(
            row(scoped("greeting", parser = string("hello, ") and string("world").defer()), "hello, world", ok(Pair("hello, ", "world"), consumed = 12)),
            row(scoped("greeting", parser = string("hello, ") and string("world").defer()), "hello, w0rld", err(
                StackTrace(
                    isCommitted = true,
                    state = State("hello, w0rld", 0),
                    error = ParseError("greeting", "invalid greeting syntax"),
                    cause = StackTrace(
                        isCommitted = true,
                        state = State("hello, w0rld", 8),
                        error = ParseError("'world'", "expected 'world'"),
                    )
                )
            )),
        )
    }
})