class Dna(val strand: String) {
    companion object {
        val VALID_DNA = "[ACGT]*".toRegex()
    }

    init {
        require(strand.matches(VALID_DNA))
    }

    val nucleotideCounts: Map<Char, Int> by lazy {
        val counts = mutableMapOf(
            'A' to 0,
            'C' to 0,
            'G' to 0,
            'T' to 0
        )
        for (c in strand) {
            counts.put(c, counts.get(c)!! + 1)
        }
        counts
    }
}
