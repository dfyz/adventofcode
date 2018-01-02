import java.io.File
import java.util.LinkedList

data class Node(val used: Int, val size: Int) {
    val avail
        get() = size - used

    fun canAccomodate(other: Node) = other.used <= avail
    fun accomodate(other: Node) = copy(used = used + other.used)
    fun obliterate() = copy(used = 0)
}

data class Coords(val x: Int, val y: Int) {
    fun getAdj() =
        listOf(
            Coords(x - 1, y),
            Coords(x + 1, y),
            Coords(x, y - 1),
            Coords(x, y + 1)
        )
}

typealias Grid = Map<Coords, Node>

data class State(val emptyPos: Coords, val targetPos: Coords) {
    fun getMoves(grid: Grid): List<State> {
        val result = mutableListOf<State>()

        val toNode = grid.get(emptyPos)
        if (toNode != null) {
            for (fromCoords in emptyPos.getAdj()) {
                val fromNode = grid.get(fromCoords)
                if (fromNode != null && toNode.obliterate().canAccomodate(fromNode)) {
                    assert(targetPos != emptyPos)
                    val newTargetPos =
                        if (targetPos == fromCoords) {
                            emptyPos
                        } else {
                            targetPos
                        }
                    result.add(State(fromCoords, newTargetPos))
                }
            }
        }

        return result
    }
}

fun main(args: Array<String>) {
    val nodeRegex = Regex("""^/dev/grid/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T.*$""")

    val nodes = File(args.first())
        .readLines()
        .drop(2)
        .map {
            val (x, y, size, used) = nodeRegex.matchEntire(it)!!.destructured
            Pair(Coords(x.toInt(), y.toInt()), Node(used.toInt(), size.toInt()))
        }
        .toList()

    var easyAnswer = nodes.withIndex().flatMap {
        (i, n1) -> nodes.withIndex().map {
            (j, n2) ->
            if (i != j && n1.second.used > 0 && n2.second.canAccomodate(n1.second)) {
                1
            } else {
                0
            }
        }
    }.sum()
    println("easy: ${easyAnswer}")

    val emptyPos = nodes.find { it.second.used == 0 }!!.first
    val grid = nodes.toMap()
    val maxX = grid.keys.maxBy { it.x }!!.x
    val initialState = State(emptyPos, Coords(maxX, 0))
    val stateToDist = mutableMapOf(initialState to 0)
    val queue = LinkedList(listOf(initialState))

    while (!queue.isEmpty()) {
        val cur = queue.poll()
        val curDist = stateToDist[cur] ?: 0
        if (cur.targetPos == Coords(0, 0)) {
            println("hard: ${curDist}")
            break
        }

        for (adj in cur.getMoves(grid)) {
            if (adj !in stateToDist) {
                stateToDist.put(adj, curDist + 1)
                queue.add(adj)
            }
        }
    }
}
