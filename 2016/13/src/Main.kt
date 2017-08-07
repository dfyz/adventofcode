import java.util.*
import kotlin.coroutines.experimental.buildSequence

data class Point(val x: Int, val y: Int) {
    fun adjacent() = buildSequence {
        if (x > 0) {
            yield(Point(x - 1, y))
        }
        if (y > 0) {
            yield(Point(x, y - 1))
        }
        yield(Point(x + 1, y))
        yield(Point(x, y + 1))
    }
}

data class Field(val n: Int) {
    fun isFree(p: Point): Boolean {
        val z = n + p.x*p.x + 3*p.x + 2*p.x*p.y + p.y + p.y*p.y
        return Integer.bitCount(z) % 2 == 0
    }
}

fun main(args: Array<String>) {
    val n = args[0].toInt()
    val dstX = args[1].toInt()
    val dstY = args[2].toInt()
    val isHard = args.size > 3

    val field = Field(n)

    val queue = LinkedList<Point>()
    val distances = mutableMapOf<Point, Int>()

    val start = Point(1, 1)
    queue.add(start)
    distances[start] = 0

    while (!queue.isEmpty()) {
        val current = queue.poll()
        val currentDistance = distances[current]!!

        if (!isHard && current.x == dstX && current.y == dstY) {
            println(currentDistance)
            break
        }

        for (adj in current.adjacent()) {
            if (field.isFree(adj) && adj !in distances && (!isHard || currentDistance < 50)) {
                distances[adj] = currentDistance + 1
                queue.add(adj)
            }
        }
    }

    if (isHard) {
        println(distances.size)
    }
}
