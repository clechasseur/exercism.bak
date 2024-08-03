import kotlin.random.Random

class Cipher(val key: String = randomKey()) {
    companion object {
        private fun randomKey(): String
            = List<Char>(100) { 'a' + Random.nextInt(26) }.joinToString("")

        private fun valid(str: String) = str.matches("[a-z]+".toRegex())
    }

    init {
        require(valid(key))
    }

    fun encode(msg: String): String {
        require(valid(msg))
        return msg.mapIndexed { idx, c -> c + (key[idx] - 'a') }.adjust().joinToString("")
    }

    fun decode(encodedMsg: String): String {
        require(valid(encodedMsg))
        return encodedMsg.mapIndexed { idx, c -> c - (key[idx] - 'a') }.adjust().joinToString("")
    }

    private fun Char.adjust(): Char = when {
        this < 'a' -> 'z' - ('a' - this) + 1
        this > 'z' -> 'a' + (this - 'z') - 1
        else -> this
    }

    private fun List<Char>.adjust() = this.map { it.adjust() }
}
