class RailFenceCipher(val numRails: Int) {
    fun getEncryptedData(dat: String): String {
        val rails = List<MutableList<Char>>(numRails) { mutableListOf() }
        Oscillator(numRails).zip(dat.asSequence()).forEach {
            (railIdx, c) -> rails[railIdx].add(c)
        }
        return rails.flatMap { it }.joinToString("")
    }

    fun getDecryptedData(dat: String): String {
        val rails = Oscillator(numRails).take(dat.length)
                                        .groupBy { it }
                                        .map { (_, l) -> l.size }
                                        .fold(Pair(0, listOf<String>())) {
                                            pil, siz -> Pair(pil.first + siz,
                                                             pil.second.plus(dat.substring(pil.first until (pil.first + siz))))
                                        }.second.map { it.toMutableList() }
        return Oscillator(numRails).take(dat.length).map { rails[it].removeAt(0) }.joinToString("")
    }
}

private class Oscillator(max: Int, impl: Sequence<Int> = getImpl(max)): Sequence<Int> by impl {
    companion object {
        fun getImpl(max: Int): Sequence<Int> =
            generateSequence(Pair(0, true)) {
                (num, up) -> when {
                    num == (max - 1) && up -> Pair(num - 1, false)
                    num == 0 && !up -> Pair(1, true)
                    up -> Pair(num + 1, true)
                    else -> Pair(num - 1, false)
                }
            }.map { it.first }
    }
}
