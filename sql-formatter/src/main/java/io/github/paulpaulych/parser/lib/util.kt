package io.github.paulpaulych.formatting

fun firstNonMatchingIndex(
    s1: String,
    s2: String,
    s2Offset: Int
): Int? {
    var pos1 = 0
    var pos2 = s2Offset
    while (pos1 < s1.length && pos2 < s2.length) {
        if (s1[pos1] != s2[pos2]) {
            return pos1
        }
        pos1 += 1
        pos2 += 1
    }
    if (s2.length - s2Offset >= s1.length) {
        return null
    }
    return s2.length - s2Offset
}

fun String.findPrefixMatching(r: Regex, offset: Int): String? {
    return r.find(this.substring(offset))?.takeIf { it.range.first == 0 }?.value
}
