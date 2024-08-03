class ChainNotFoundException(msg: String) : RuntimeException(msg)

data class Domino(val left: Int, val right: Int) {
    fun reversed() = Domino(right, left)
}

object Dominoes {
    fun formChain(vararg dominoes: Domino) = formChain(dominoes.toList())

    fun formChain(dominoes: List<Domino>): List<Domino> {
        if (dominoes.isEmpty()) {
            return emptyList()
        }
        return continueChain(listOf(dominoes.first()), dominoes.drop(1))
                ?: throw ChainNotFoundException("No domino chain found.")
    }

    private fun continueChain(chain: List<Domino>, dominoes: List<Domino>): List<Domino>? {
        if (dominoes.isEmpty()) {
            if (chain.first().left == chain.last().right) {
                return chain
            }
            return null
        }

        val last = chain.last()
        dominoes.filter { it.left == last.right || it.right == last.right }.forEach { possibility ->
            val realPossibility = when (possibility.left == last.right) {
                true -> possibility
                false -> possibility.reversed()
            }
            val endChain = continueChain(chain + realPossibility, dominoes - possibility)
            if (endChain != null) {
                return endChain
            }
        }
        return null
    }
}
