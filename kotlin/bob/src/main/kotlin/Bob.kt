object Bob {
    fun hey(input: String): String = when {
        input.trim().endsWith('?') -> if (yelling(input)) "Calm down, I know what I'm doing!" else "Sure."
        silence(input) -> "Fine. Be that way!"
        else -> if (yelling(input)) "Whoa, chill out!" else "Whatever."
    }

    private fun yelling(input: String) = input.any { it.isUpperCase() } && input.none { it.isLowerCase() }

    private fun silence(input: String) = input.all { it.isWhitespace() }
}
