import java.io.File

fun swap(arr: CharArray, idx1: Int, idx2: Int) {
    var tmp = arr[idx1]
    arr[idx1] = arr[idx2]
    arr[idx2] = tmp
}

fun doRotation(arr: CharArray, saveFrom: Int, saveTo: Int, range: IntProgression, diff: Int) {
    val saved = arr[saveFrom]
    for (i in range) {
        arr[i] = arr[i + diff]
    }
    arr[saveTo] = saved
}

fun rotateRight(arr: CharArray, shift: Int) {
    repeat(times = shift) {
        doRotation(arr, arr.size - 1, 0, arr.size - 1 downTo 1, -1)
    }
}

fun rotateLeft(arr: CharArray, shift: Int) {
    repeat(times = shift) {
        doRotation(arr, 0, arr.size - 1, 0 until arr.size - 1, 1)
    }
}

fun reverse(arr: CharArray, from: Int, to: Int) {
    var i = from
    var j = to
    while (i < j) {
        swap(arr, i, j)
        i += 1
        j -= 1
    }
}

fun doOneInstruction(password: String, line: List<String>): String {
    val chars = password.toCharArray()
    val getXY = { Pair(line.get(2), line.last()) }
    when (line.first()) {
        "swap" -> {
            val (x, y) = getXY()
            when (line.get(1)) {
                "position" -> swap(chars, x.toInt(), y.toInt())
                "letter" -> swap(chars, chars.indexOf(x.first()), chars.indexOf(y.first()))
            }
        }
        "rotate" -> {
            when (line.get(1)) {
                "left" -> rotateLeft(chars, line.get(2).toInt())
                "right" -> rotateRight(chars, line.get(2).toInt())
                "based" -> {
                    val idx = chars.indexOf(line.last().first())
                    rotateRight(chars, 1 + idx + if (idx >= 4) { 1 } else { 0 })
                }
            }
        }
        "reverse" -> {
            val (x, y) = getXY()
            reverse(chars, x.toInt(), y.toInt())
        }
        "move" -> {
            val (x, y) = getXY()
            val xInt = x.toInt()
            val yInt = y.toInt()
            if (xInt < yInt) {
                doRotation(chars, xInt, yInt, xInt until yInt, 1)
            } else {
                doRotation(chars, xInt, yInt, xInt downTo yInt + 1, -1)
            }
        }
    }
    return String(chars)
}

fun scramble(password: String, program: List<List<String>>): String {
    return program.fold(password) { pass, instr -> doOneInstruction(pass, instr) }
}

fun getPermutations(results: MutableList<String>, acc: String, left: String) {
    if (left.isEmpty()) {
        results.add(acc)
        return
    }
    for (idx in 0 until left.length) {
        getPermutations(results, acc + left[idx], left.substring(0, idx) + left.substring(idx + 1))
    }
}

fun main(args: Array<String>) {
    var password = args.first()
    var hardInput = args.get(1)
    var program = File(args.get(2)).readLines().map { it.split(' ') }

    println(scramble(password, program))
    val perms = mutableListOf<String>()
    getPermutations(perms, "", "abcdefgh")
    println(perms.find { p -> scramble(p, program) == hardInput })
}
