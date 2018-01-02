import java.io.File
import java.util.LinkedList

typealias Grid = List<String>

data class Coords(val row: Int, val col: Int) {
    fun getAdj(grid: Grid): List<Coords> {
        return listOf(
            Coords(row + 1, col),
            Coords(row - 1, col),
            Coords(row, col + 1),
            Coords(row, col - 1)
        ).filter { it.isInside(grid) }
    }

    fun isInside(grid: Grid) =
        row >= 0 && col >= 0 && row < grid.size && col < grid[row].length && grid[row][col] != '#'
}

fun getDist(grid: Grid, from: Coords, to: Coords): Int {
    val queue = LinkedList(listOf(from))
    val dist = mutableMapOf(from to 0)
    while (!queue.isEmpty()) {
        val cur = queue.poll()
        val curDist = dist[cur]!!
        if (cur == to) {
            return curDist
        }
        for (adj in cur.getAdj(grid)) {
            if (adj !in dist) {
                dist[adj] = curDist + 1
                queue.add(adj)
            }
        }
    }
    throw IllegalStateException("no path from ${from} to ${to}")
}

fun solveTSP(cycle: Boolean, lastNode: Int, nums: Set<Int>, dists: Map<Pair<Int, Int>, Int>): Int {
    if (nums.isEmpty()) {
        return if (cycle) { dists[Pair(lastNode, 0)]!! } else { 0 }
    }

    var result: Int? = null
    for (nextNode in nums) {
        val nextAnswer = dists[Pair(lastNode, nextNode)]!! + solveTSP(cycle, nextNode, nums.minus(nextNode), dists)
        if (result == null || nextAnswer < result) {
            result = nextAnswer
        }
    }
    return result!!
}

fun main(args: Array<String>) {
    val grid = File(args.first()).readLines()

    val numbers = (0 until grid.size).flatMap { row ->
        (0 until grid[row].length).map { col ->
            Pair(grid[row][col], Coords(row, col))
        }
    }.filter {
        it.first.isDigit()
    }.map {
        Pair(it.first.toString().toInt(), it.second)
    }.toMap()

    val maxNum = numbers.keys.max()!!
    val dists = (0..maxNum).flatMap { from ->
        (0..maxNum).map { to ->
            val fromCoords = numbers[from]!!
            val toCoords = numbers[to]!!
            Pair(Pair(from, to), getDist(grid, fromCoords, toCoords))
        }
    }.toMap()

    val initialNums = (0..maxNum).toSet()
    println(solveTSP(false, 0, initialNums, dists))
    println(solveTSP(true, 0, initialNums, dists))
}