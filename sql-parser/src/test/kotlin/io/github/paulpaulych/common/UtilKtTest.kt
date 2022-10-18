package io.github.paulpaulych.common

import io.kotest.core.spec.style.DescribeSpec
import io.kotest.data.forAll
import io.kotest.data.headers
import io.kotest.data.row
import io.kotest.data.table
import io.kotest.matchers.shouldBe

class UtilKtTest: DescribeSpec({

    describe(::firstNonMatchingIndex.name) {
        forAll(table(
            headers("key", "source", "offset", "expected index"),
            row("aa", "aa", 0, null),
            row("aa", "aaa", 0, null),
            row("aa", "aaa", 1, null),
            row("aa", "a", 2, 2),
        )) { key, source, offset, expect ->
            firstNonMatchingIndex(key, source, offset) shouldBe expect
        }
    }

})