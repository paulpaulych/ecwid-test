package io.github.paulpaulych.common

fun firstNonMatchingIndex(
    s1: String,
    s2: String,
    s2Offset: Int
): Int? {
    var pos = 0
    var offPos = pos + s2Offset
    while (!(pos > s1.length - 1 || offPos > s2.length - 1)) {
        if (s1[pos] != s2[pos]) {
            return pos
        }
        pos += 1
        offPos += 1
    }
    return null
}

fun String.findPrefixMatching(r: Regex): MatchResult? {
    return r.find(this)?.takeIf { it.range.first == 0 }
}
