import java.util.LinkedList

data class Node(val num: Int) {
    lateinit var next: Node
    lateinit var prev: Node
}

fun createElves(n: Int): Node {
    val first = Node(1)
    var prev = first
    for (i in 2..n) {
        val cur = Node(i)
        prev.next = cur
        cur.prev = prev
        prev = cur
    }
    prev.next = first
    first.prev = prev
    return first
}

fun killNext(killer: Node): Node {
    killer.next = killer.next.next
    return killer.next
}

fun solveEasy(start: Node, n: Int): Int {
    return generateSequence(start) { killNext(it) }.take(n).last().num
}

fun killCur(toKill: Node, stepTwice: Boolean): Node {
    val prev = toKill.prev
    val next = toKill.next
    prev.next = next
    next.prev = prev
    return if (stepTwice) { next.next } else { next }
}

fun solveHard(start: Node, n: Int): Int {
    var toKill = generateSequence(start) { it.next }.take(n / 2 + 1).last()
    var cur = start
    for (length in n downTo 2) {
        toKill = killCur(toKill, length % 2 == 1)
        cur = cur.next
    }
    return cur.num
}

fun main(args: Array<String>) {
    val n = args.first().toInt()
    println(solveEasy(createElves(n), n))
    println(solveHard(createElves(n), n))
}
