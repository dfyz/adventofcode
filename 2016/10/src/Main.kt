import java.io.File

interface Node {
    fun onValue(value: Int)
}

data class Bot(val botId: Int) : Node {
    override fun onValue(value: Int) {
        val otherValue = intermediateValue
        if (otherValue != null) {
            val lowValue = Math.min(otherValue, value)
            val highValue = Math.max(otherValue, value)
            if (lowValue == 17 && highValue == 61) {
                println("bot from the easy problem: ${this}")
            }
            lowOutput?.onValue(lowValue)
            highOutput?.onValue(highValue)
        } else {
            intermediateValue = value
        }
    }

    var intermediateValue: Int? = null
    var lowOutput: Node? = null
    var highOutput: Node? = null
}

data class Output(val outputId: Int) : Node {
    override fun onValue(value: Int) {
        this.value = value
    }

    var value: Int? = null
}

class Input(val value: Int, val node: Node) {
    fun trigger() {
        node.onValue(value)
    }
}

fun main(args: Array<String>) {
    val lines = File(args[0]).readLines()

    val inputs = mutableListOf<Input>()
    val bots = mutableMapOf<Int, Bot>()
    val outputs = mutableMapOf<Int, Output>()

    fun getNode(nodeType: String, nodeId: String): Node {
        val nodeIdInt = nodeId.toInt()
        return when (nodeType) {
            "bot" -> {
                bots.getOrPut(nodeIdInt, { Bot(nodeIdInt) })
            }
            "output" -> {
                outputs.getOrPut(nodeIdInt, { Output(nodeIdInt) })
            }
            else -> throw RuntimeException("unknown node type: ${nodeType}")
        }
    }

    for (line in lines) {
        val tokens = line.split(" ")
        when (tokens[0]) {
            "value" -> {
                val inputValue = tokens[1].toInt()
                inputs.add(Input(inputValue, getNode(tokens[4], tokens[5])))
            }
            else -> {
                val node = getNode(tokens[0], tokens[1]) as Bot
                assert(tokens[3] == "low")
                assert(tokens[8] == "high")
                node.lowOutput = getNode(tokens[5], tokens[6])
                node.highOutput = getNode(tokens[10], tokens[11])
            }
        }
    }

    inputs.forEach { it.trigger() }
    val hardAnswer = outputs.values
        .filter { it.outputId <= 2 }
        .fold(1, { acc, output -> acc * (output.value ?: 0) })
    println("answer for the hard problem: ${hardAnswer}")
}