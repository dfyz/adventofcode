import java.io.File

data class Answer(val easy: Long?, val hard: Long)

fun cnt(from: Long, to: Long) = to - from

fun solve(ranges: List<List<Long>>, maxAllowed: Long): Answer {
    var minUnblocked = 0L
    var easyAnswer: Long? = null
    var hardAnswer = 0L
    for (range in ranges) {
        val curCnt = cnt(minUnblocked, range.first())
        if (curCnt > 0) {
            if (easyAnswer == null) {
                easyAnswer = minUnblocked
            }
            hardAnswer += curCnt
        }
        minUnblocked = Math.max(minUnblocked, range.get(1) + 1)
    }
    hardAnswer += cnt(minUnblocked, maxAllowed + 1)
    return Answer(easyAnswer, hardAnswer)
}

fun main(args: Array<String>) {
    val input = File(args.first())
        .readLines()
        .map { it.split("-").map { it.toLong() }.toList() }
        .toMutableList()
    val maxAllowed = args.get(1).toLong()

    input.sortBy { it[0] }
    println(solve(input, maxAllowed))
}
