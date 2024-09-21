class RotationalCipher(val key: Int) {
    val cipher: Map<Char, Char> by lazy {
        ('a'..'z').plus('A'..'Z').map { it to cipherFor(it) }.toMap()
    }

    fun encode(input: String): String
        = input.map { cipher.get(it) ?: it }.joinToString("")

    private fun cipherFor(c: Char): Char = when {
        c.isUppercase() -> cipherFor(c.toLowerCase()).toUpperCase()
        (c + key) in 'a'..'z' -> c + key
        else -> c - (26 - key)
    }

    private fun Char.isUppercase() = this in 'A'..'Z'
}
