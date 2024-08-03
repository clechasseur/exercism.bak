class Forth {
    fun evaluate(vararg line: String): List<Int> {
        val stack = mutableListOf<Int>()
        ForthParser().parse(line.joinToString(" ")).forEach { it(stack) }
        return stack
    }
}
