object Bob {
    fun hey(input: String): String = if (input.silence) {
        "Fine. Be that way!"
    } else when (Pair(input.question, input.yelling)) {
        Pair(true, true) -> "Calm down, I know what I'm doing!"
        Pair(true, false) -> "Sure."
        Pair(false, true) -> "Whoa, chill out!"
        else -> "Whatever."
    }

    private val String.silence: Boolean
        get() = all { it.isWhitespace() }

    private val String.question: Boolean
        get() = trim().endsWith('?')

    private val String.yelling: Boolean
        get() = any { it.isUpperCase() } && none { it.isLowerCase() }
}
