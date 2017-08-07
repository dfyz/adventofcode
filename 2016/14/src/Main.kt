import java.security.MessageDigest
import java.util.*

fun getTriple(s: String): Char? {
    val tripleIdx = s.indices.find {
        val ch = s[it]
        s.getOrNull(it + 1) == ch && s.getOrNull(it + 2) == ch
    }
    return if (tripleIdx != null) { s[tripleIdx] } else { null }
}

fun hasQuintuple(s: String, ch: Char): Boolean {
    return s.indices.any {
        idx -> (0..4).all { s.getOrNull(it + idx) == ch }
    }
}

val md = MessageDigest.getInstance("MD5")
fun md5(s: String): String = md.digest(s.toByteArray()).map { "%02x".format(it) }.joinToString("")

fun main(args: Array<String>) {
    val salt = args[0]
    val isHard = args.size > 1

    var index = -1
    val hashIterator = generateSequence {
        index += 1
        val firstMd5 = md5(salt + index)
        if (isHard) {
            (1..2016).fold(firstMd5) {
                s, _ -> md5(s)
            }
        } else {
            firstMd5
        }
    }.withIndex().iterator()

    val queueSize = 1001
    val queue = LinkedList<IndexedValue<String>>()
    (1..queueSize).forEach { queue.add(hashIterator.next()) }

    var foundKeys = 0
    while (foundKeys < 64) {
        val triple = getTriple(queue.first().value)
        if (triple != null) {
            if (queue.drop(1).any { hasQuintuple(it.value, triple) }) {
                foundKeys += 1
                println("key ${foundKeys}: index ${queue.first().index}")
            }
        }

        queue.pop()
        queue.add(hashIterator.next())
    }
}
