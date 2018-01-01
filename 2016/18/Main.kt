enum class TileType {
    SAFE,
    TRAP,
}

fun main(args: Array<String>) {
    var currentRow = args[0]
    var iters = args[1].toInt()

    fun isPrevTileSafe(idx: Int) =
        if (idx < 0 || idx >= currentRow.length || currentRow[idx] == '.') {
            TileType.SAFE
        } else {
            TileType.TRAP
        }

    val trapPatterns = listOf(
        listOf(TileType.TRAP, TileType.TRAP, TileType.SAFE),
        listOf(TileType.SAFE, TileType.TRAP, TileType.TRAP),
        listOf(TileType.TRAP, TileType.SAFE, TileType.SAFE),
        listOf(TileType.SAFE, TileType.SAFE, TileType.TRAP)
    )

    var answer = 0
    repeat (times = iters) {
        answer += currentRow.count { it == '.' }

        val sb = StringBuilder()
        for (idx in 0 until currentRow.length) {
            val pattern = listOf(isPrevTileSafe(idx - 1), isPrevTileSafe(idx), isPrevTileSafe(idx + 1))
            val isCurTileSafe = pattern !in trapPatterns
            sb.append(if (isCurTileSafe) { '.' } else { '^' })
        }
        currentRow = sb.toString()
    }
    println(answer)
}
