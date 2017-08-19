import java.io.File

data class Disk(val offset: Int, val positions: Int)

fun solve(disks: List<Disk>): Int {
    var time = 1
    while (true) {
        val isGood = disks.withIndex().all {
            (idx, disk) -> (time + idx + 1 + disk.offset) % disk.positions == 0
        }
        if (isGood) {
            return time
        }
        time += 1
    }
}

fun main(args: Array<String>) {
    val re = Regex("""Disc #. has (\d+) positions; at time=0, it is at position (\d+).""")
    val disks = File(args[0]).readLines().map {
        val match = re.matchEntire(it)!!
        Disk(match.groups[2]!!.value.toInt(), match.groups[1]!!.value.toInt())
    }.toMutableList()

    println(solve(disks))
    disks.add(Disk(0, 11))
    println(solve(disks))
}
