package io.github.paulpaulych.parser.lib

import io.github.paulpaulych.formatting.findPrefixMatching
import io.github.paulpaulych.formatting.firstNonMatchingIndex
import io.kotest.core.spec.style.DescribeSpec
import io.kotest.data.forAll
import io.kotest.data.headers
import io.kotest.data.row
import io.kotest.data.table
import io.kotest.matchers.shouldBe

class UtilKtTest: DescribeSpec({

    it("firstNonMatchingIndex") {
        forAll(table(
            headers("key", "source", "offset", "expected index"),
            row("aa", "aa", 0, null),

            row("", "aaa", 0, null),
            row("", "a", 0, null),
            row("", "", 0, null),

            row("a", "", 0, 0),
            row("a", "b", 0, 0),
            row("a", "aa", 0, null),

            row("abc", "abb", 0, 2),
            row("aa", "aaa", 0, null),
            row("aa", "aaa", 1, null),
            row("aa", "aaa", 2, 1),
            row("aa", "a", 0, 1),
        )) { key, source, offset, expect ->
            firstNonMatchingIndex(key, source, offset) shouldBe expect
        }
    }

    it("findPrefixMatching") {
        forAll(table(
            headers("source", "regex", "offset", "expected prefix"),
            row("aa11", Regex("\\d+"), 0, null),
            row("11aa", Regex("\\d+"), 0, "11"),
            row("11aa", Regex("\\d+"), 1, "1"),
            row("11aa", Regex("\\d+"), 2, null),
        )) { source, regex, offset, expected ->
            source.findPrefixMatching(regex, offset) shouldBe expected
        }
    }
})