import java.security.MessageDigest
import java.util.LinkedList

const val N: Int = 4
val md5 = MessageDigest.getInstance("MD5")

fun isDoorOpen(ch: Char) =
    when (ch) {
        in 'b'..'f' -> true
        else -> false
    }

fun isInside(x: Int, y: Int) =
    x in (0 until N) && y in (0 until N)

data class Direction(val dx: Int, val dy: Int, val symbol: Char)

val directions = listOf(
    Direction(0, -1, 'U'),
    Direction(0, 1, 'D'),
    Direction(-1, 0, 'L'),
    Direction(1, 0, 'R')
)

data class State(val x: Int, val y: Int, val passcode: String, val seq: String) {
    fun isFinal() = x == N && y == N

    fun nextStates(): List<State> {
        val result = ArrayList<State>()
        val hash = md5.digest((passcode + seq).toByteArray())
        val chars = (0..2).map { "%02x".format(hash[it]) }.joinToString("")

        for ((idx, dir) in directions.withIndex()) {
            val ch = chars[idx]
            if (isDoorOpen(ch)) {
                val nextX = x + dir.dx
                val nextY = y + dir.dy
                if (isInside(nextX, nextY)) {
                    result.add(State(nextX, nextY, passcode, seq + dir.symbol))
                }
            }
        }

        return result
    }
}

fun main(args: Array<String>) {
    val passcode = args[0]
    val initialState = State(0, 0, passcode, "")
    val queue = LinkedList<State>(listOf(initialState))

    var easyAnswer: String? = null
    var hardAnswer = 0

    while (!queue.isEmpty()) {
        val state = queue.poll()
        if (state.x == N - 1 && state.y == N - 1) {
            if (easyAnswer == null) {
                easyAnswer = state.seq
            }
            hardAnswer = Math.max(hardAnswer, state.seq.length)
        } else {
            for (nextState in state.nextStates()) {
                queue.add(nextState)
            }
        }
    }
    println("easy: ${easyAnswer}")
    println("hard: ${hardAnswer}")
}
