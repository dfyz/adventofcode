import java.io.File
import java.util.LinkedList
import kotlin.coroutines.experimental.buildSequence

fun isGoodFloor(floor: Set<String>): Boolean {
    val generators = floor.filter { it.endsWith("generator") }.map { it.split(" ")[0] }.toSet()
    if (generators.isEmpty()) {
        return true
    }
    val microchips = floor.filter { it.endsWith("microchip") }.map { it.split("-")[0] }.toSet()
    return microchips.all { it in generators }
}

data class GameState(val elevatorIndex: Int, val floors: List<Set<String>>) {
    fun getMoves() = buildSequence {
        for (delta in sequenceOf(-1, 1)) {
            val newElevatorIndex = elevatorIndex + delta
            if (newElevatorIndex !in floors.indices) {
                continue
            }

            val fromFloor = floors[elevatorIndex].toList()
            for (i in fromFloor.indices) {
                for (j in fromFloor.indices) {
                    val newFromFloor = fromFloor
                        .slice(fromFloor.indices.filter { it != i && it != j })
                        .toSet()
                    val toFloor = floors[newElevatorIndex].toMutableSet()
                    toFloor.add(fromFloor[i])
                    toFloor.add(fromFloor[j])

                    if (!isGoodFloor(toFloor) || !isGoodFloor(newFromFloor)) {
                        continue
                    }

                    val newFloors = mutableListOf<Set<String>>()
                    for (floorIdx in floors.indices) {
                        val floorToAdd = when (floorIdx) {
                            newElevatorIndex -> toFloor
                            elevatorIndex -> newFromFloor
                            else -> floors[floorIdx]
                        }
                        newFloors.add(floorToAdd)
                    }

                    yield(GameState(newElevatorIndex, newFloors))
                }
            }
        }
    }

    fun isFinal(): Boolean = floors.dropLast(1).all { it.isEmpty() }
}

fun main(args: Array<String>) {
    val lines = File(args[0]).readLines()

    val re = Regex("""a ((?:\w+ generator)|(?:\w+-compatible microchip))""")
    val floors = lines.map { re.findAll(it).map { it.groups[1]?.value!! }.toSet() }

    val initialState = GameState(0, floors)
    val distances = mutableMapOf(initialState to 0)
    val prev = mutableMapOf<GameState, GameState>()

    val queue = LinkedList<GameState>()
    queue.add(initialState)

    var maxDistance = 0
    while (!queue.isEmpty()) {
        val current = queue.poll()
        val currentDistance = distances[current]!!
        if (currentDistance > maxDistance) {
            println("dist: ${currentDistance}")
            maxDistance = currentDistance
        }

        if (current.isFinal()) {
            var state = current
            val winningSequence = mutableListOf<GameState>(state)
            while (state in prev) {
                val prevState = prev[state]!!
                winningSequence.add(prevState)
                state = prevState
            }
            winningSequence.reverse()
            winningSequence.withIndex().forEach { println("${it.index}: ${it.value}") }
            break
        }

        for (nextMove in current.getMoves()) {
            if (nextMove in distances) {
                continue
            }
            distances[nextMove] = currentDistance + 1
            prev[nextMove] = current
            queue.add(nextMove)
        }
    }
}
