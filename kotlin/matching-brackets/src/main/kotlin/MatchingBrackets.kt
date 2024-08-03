object MatchingBrackets {
    fun isValid(input: String): Boolean {
        val stack = mutableListOf<Char>()

        for (c in input) {
            when (c) {
                '(', '[', '{' -> stack.push(c)
                ')', ']', '}' -> {
                    if (stack.isEmpty() || stack.pop() != c.inverse) {
                        return false
                    }
                }
            }
        }

        return stack.isEmpty()
    }

    private fun<T> MutableList<T>.push(e: T) = add(e)

    private fun<T> MutableList<T>.pop(): T = removeLast()

    private val Char.inverse: Char
        get() = when (this) {
            ')' -> '('
            ']' -> '['
            '}' -> '{'
            else -> error("No inverse character for '$this'")
        }
}
