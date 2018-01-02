import java.io.File

typealias Registers = MutableMap<Char, Int>

fun initRegisters(aVal: Int): Registers {
    val registers = mutableMapOf<Char, Int>()
    ('a'..'d').map { Pair(it, 0) }.toMap(registers)
    registers['a'] = aVal
    return registers
}

fun simulate(registers: Registers, programBlueprint: List<List<String>>): Int {
    val program = programBlueprint.map { it.toMutableList() }.toMutableList()

    val toRegRef = { v: String ->
        if (v.first() in registers) {
            v.first()
        } else {
            null
        }
    }

    val getVal = { v: String ->
        if (v.length == 1 && v.first().isLetter()) {
            registers[v.first()]!!
        } else {
            v.toInt()
        }
    }

    val changeVal = { cmd: List<String>, delta: Int ->
        if (cmd.size == 2) {
            val regRef = toRegRef(cmd[1])
            if (regRef != null) {
                registers[regRef] = registers[regRef]!! + delta
            }
        }
    }

    var idx = 0
    loop@ while (idx < program.size) {
        val cur = program[idx]
        when (cur.first()) {
            "cpy" -> {
                if (cur.size == 3) {
                    val regRef = toRegRef(cur[2])
                    if (regRef != null) {
                        registers[regRef] = getVal(cur[1])
                    }
                }
            }
            "inc" -> changeVal(cur, 1)
            "dec" -> changeVal(cur, -1)
            "jnz" -> {
                if (cur.size == 3) {
                    if (getVal(cur[1]) != 0) {
                        idx += getVal(cur[2])
                        continue@loop
                    }
                }
            }
            "tgl" -> {
                if (cur.size == 2) {
                    val toChangeIdx = idx + getVal(cur[1])
                    if (toChangeIdx < program.size) {
                        val toChange = program[toChangeIdx]
                        toChange[0] =
                            if (toChange.size == 2) {
                                if (toChange.first() == "inc") {
                                    "dec"
                                } else {
                                    "inc"
                                }
                            } else {
                                if (toChange.first() == "jnz") {
                                    "cpy"
                                } else {
                                    "jnz"
                                }
                            }
                    }
                }
            }
        }
        idx += 1
    }

    return registers['a']!!
}

fun main(args: Array<String>) {
    val program = File(args.first()).readLines().map { it.split(' ') }.toList()
    val easyRegisters = initRegisters(7)
    val easyAnswer = simulate(easyRegisters, program)
    println("easy: ${easyAnswer}")

    val hardRegisters = initRegisters(12)
    val hardAnswer = simulate(hardRegisters, program)
    println("hard: ${hardAnswer}")
}
