class ForthParser {
    companion object {
        private val DEFINITION_REGEX = Regex(": ([^ ]+) ([^ ]+( [^ ]+)*) ;")

        private fun <T> MutableList<T>.push(element: T) = add(element)
        private fun <T> MutableList<T>.pop() = removeAt(size - 1)
        private fun <T> List<T>.peek() = last()

        private fun checkStack(stack: List<Int>, needed: Int) {
            require(needed < 1 || stack.isNotEmpty()) { "empty stack" }
            require(needed < 2 || stack.size >= 2) { "only one value on the stack" }
        }

        fun add(stack: MutableList<Int>) {
            checkStack(stack, 2)
            stack.push(stack.pop() + stack.pop())
        }

        fun subtract(stack: MutableList<Int>) {
            checkStack(stack, 2)
            val b = stack.pop()
            val a = stack.pop()
            stack.push(a - b)
        }

        fun multiply(stack: MutableList<Int>) {
            checkStack(stack, 2)
            stack.push(stack.pop() * stack.pop())
        }

        fun divide(stack: MutableList<Int>) {
            checkStack(stack, 2)
            val denominator = stack.pop()
            val numerator = stack.pop()
            require(denominator > 0) { "divide by zero" }
            stack.push(numerator / denominator)
        }

        fun dup(stack: MutableList<Int>) {
            checkStack(stack, 1)
            stack.push(stack.peek())
        }

        fun drop(stack: MutableList<Int>) {
            checkStack(stack, 1)
            stack.pop()
        }

        fun swap(stack: MutableList<Int>) {
            checkStack(stack, 2)
            val first = stack.pop()
            val second = stack.pop()
            stack.push(first)
            stack.push(second)
        }

        fun over(stack: MutableList<Int>) {
            checkStack(stack, 2)
            val tmp = stack.pop()
            val toPush = stack.peek()
            stack.push(tmp)
            stack.push(toPush)
        }
    }

    private val instructions = mutableMapOf<String, (MutableList<Int>) -> Unit>(
            "+" to Companion::add,
            "-" to Companion::subtract,
            "*" to Companion::multiply,
            "/" to Companion::divide,
            "DUP" to Companion::dup,
            "DROP" to Companion::drop,
            "SWAP" to Companion::swap,
            "OVER" to Companion::over
    )

    private fun findRoutine(name: String) = instructions[name.toUpperCase()] ?: error("undefined operation")

    fun parse(programText: String): List<(MutableList<Int>) -> Unit> {
        val program = mutableListOf<(MutableList<Int>) -> Unit>()
        var marker = 0
        while (marker < programText.length) {
            val instructionEndIdx = programText.nextSpace(startIndex = marker)
            val instruction = programText.substring(marker until instructionEndIdx)
            when {
                instruction.all { it.isDigit() } -> {
                    val value = instruction.toInt()
                    program.add { it.push(value) }
                    marker = instructionEndIdx + 1
                }
                instruction != ":" -> {
                    val routine = findRoutine(instruction)
                    program.add(routine)
                    marker = instructionEndIdx + 1
                }
                else -> {
                    val endIdx = programText.indexOf(';', startIndex = marker)
                    val definitionExpression = programText.substring(marker..endIdx)
                    val match = DEFINITION_REGEX.matchEntire(definitionExpression)!!
                    val newName = match.groupValues[1]
                    require(newName.any { !it.isDigit() }) { "illegal operation" }
                    val routines = parse(match.groupValues[2])
                    instructions[newName.toUpperCase()] = { stack -> routines.forEach { it(stack) } }
                    marker = endIdx + 2
                }
            }
        }
        return program
    }

    private fun String.nextSpace(startIndex: Int) = when (val idx = indexOf(' ', startIndex = startIndex)) {
        -1 -> length
        else -> idx
    }
}
