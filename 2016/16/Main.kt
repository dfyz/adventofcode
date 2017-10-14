fun invert(s: String): String {
    var result = StringBuilder()
    for (ch in s) {
        result.append(if (ch == '0') { "1" }  else { "0" })
    }
    return result.toString()
}

fun main(args: Array<String>) {
    val diskSize = args[0].toInt()
    var data = args[1]

    while (data.length < diskSize) {
        data += "0" + invert(data.reversed())
    }

    data = data.substring(0, diskSize)

    while (!data.isEmpty() && (data.length % 2 == 0)) {
        var newData = StringBuilder()
        for (i in 0 until data.length step 2) {
            newData.append(if (data[i] == data[i + 1]) {
                "1"
            } else {
                "0"
            })
        }
        data = newData.toString()
    }

    println(data)
}
