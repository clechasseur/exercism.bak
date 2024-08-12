object Isogram {
    fun isIsogram(sen: String): Boolean =
        sen.toLowerCase()
           .filter { it != ' ' && it != '-' }
           .toList()
           .sortedBy { it }
           .zipWithNext { a, b -> a == b }
           .all { !it }
}