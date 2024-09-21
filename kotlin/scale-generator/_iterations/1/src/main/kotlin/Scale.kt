class Scale(private val tonic: String) {
    companion object {
        private val sharps = listOf("A", "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#")
        private val flats = listOf("A", "Bb", "B", "C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab")
        private val betweenIntervals = mapOf("m" to 0, "M" to 1, "A" to 2)

        private fun endlessScaleFor(tonic: String): Sequence<String> {
            val scale = when (tonic) {
                "C", "a", "G", "D", "A", "E", "B", "F#", "e", "b", "f#", "c#", "g#", "d#" -> sharps
                "F", "Bb", "Eb", "Ab", "Db", "Gb", "d", "g", "c", "f", "bb", "eb" -> flats
                else -> error("no scale for $tonic")
            }
            val start = "${tonic[0].toUpperCase()}${tonic.substring(1)}"
            var idx = 0
            return generateSequence { scale[idx++ % scale.size] }.dropWhile { it != start }
        }
    }

    fun chromatic(): List<String> = endlessScaleFor(tonic).take(12).toList()

    fun interval(intervals: String): List<String> {
        val iter = endlessScaleFor(tonic).iterator()
        return listOf(iter.next()) + intervals.dropLast(1).map {
            repeat(betweenIntervals[it.toString()] ?: error("invalid interval $it")) { iter.next() }
            iter.next()
        }
    }
}
