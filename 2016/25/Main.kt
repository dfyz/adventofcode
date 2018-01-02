import java.io.File

fun initRegisters(aVal: Int): MutableMap<Char, Int> {
    val registers = mutableMapOf<Char, Int>()
    ('a'..'d').map { Pair(it, 0) }.toMap(registers)
    registers['a'] = aVal
    return registers
}

fun eval(registers: MutableMap<Char, Int>, program: List<List<String>>): List<Int> {
    var result = mutableListOf<Int>()
    var idx = 0

    val getVal = { v: String ->
        if (v.first().isLetter()) {
            registers[v.first()]!!
        } else {
            v.toInt()
        }
    }

    val change = { ch: Char, delta: Int ->
        registers[ch] = registers[ch]!! + delta
    }

    loop@ while (result.size < 100 && idx < program.size) {
        val cur = program[idx]
        when (cur.first()) {
            "cpy" -> registers[cur[2].first()] = getVal(cur[1])
            "inc" -> change(cur[1].first(), 1)
            "dec" -> change(cur[1].first(), -1)
            "jnz" -> {
                if (getVal(cur[1]) != 0) {
                    idx += getVal(cur[2])
                    continue@loop
                }
            }
            "out" -> result.add(getVal(cur[1]))
        }
        idx += 1
    }

    return result
}

fun isGood(output: List<Int>): Boolean {
    return output.withIndex().all { (idx, ch) -> ch == idx % 2 }
}

fun main(args: Array<String>) {
    val program = File(args.first()).readLines().map { it.split(' ') }

    for (i in generateSequence(1) { it + 1 }) {
        val candidate = eval(initRegisters(i), program)
        if (isGood(candidate)) {
            println(i)
            break
        }
    }
}
