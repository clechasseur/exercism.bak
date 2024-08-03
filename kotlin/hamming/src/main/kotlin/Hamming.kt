object Hamming {
    fun compute(seq1: String, seq2: String): Int {
        if (seq1.length != seq2.length) {
            throw IllegalArgumentException("left and right strands must be of equal length.")
        }
        return seq1.zip(seq2).count { it.first != it.second }
    }
}
