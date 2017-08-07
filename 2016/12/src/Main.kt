import java.io.File

fun main(args: Array<String>) {
    val lines = File(args[0]).readLines()
    val isHard = args.size > 1
    val regs = Array<Int>(4, {0})
    if (isHard) {
        regs[2] = 1
    }

    fun getRegIndex(s: String): Int {
        return (s[0] - 'a')
    }

    fun getRegOrValue(s: String): Int {
        return if (s[0].isDigit()) {
            s.toInt()
        } else {
            regs[getRegIndex(s)]
        }
    }

    var ip = 0
    while (ip < lines.size) {
        var delta = 1
        val tokens = lines[ip].split(" ")
        when (tokens[0]) {
            "cpy" -> {
                regs[getRegIndex(tokens[2])] = getRegOrValue(tokens[1])
            }
            "inc" -> {
                regs[getRegIndex(tokens[1])] += 1
            }
            "dec" -> {
                regs[getRegIndex(tokens[1])] -= 1
            }
            "jnz" -> {
                if (getRegOrValue(tokens[1]) != 0) {
                    delta = tokens[2].toInt()
                }
            }
        }
        ip += delta
    }
    println(regs[0])
}