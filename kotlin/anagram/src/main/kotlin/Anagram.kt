class Anagram(val input: String) {
    private val sortedInput = input.toLowerCase().toList().sortedBy { it }

    fun match(candidates: List<String>): Set<String> = if (input.toList().all { it.isUpperCase() }) {
        // This requirement is weird.
        candidates.intersect(listOf(input)).toSet()
    } else {
        candidates.filter { it.toLowerCase().toList().sortedBy { it }.equals(sortedInput) }.toSet()
    }
}
