object BinarySearch {
    fun <T : Comparable<T>> search(items: List<T>, lookFor: T): Int
        = searchFrom(0, items, lookFor)

    private fun <T : Comparable<T>> searchFrom(indexDelta: Int, items: List<T>, lookFor: T): Int = when {
        items.isEmpty() -> -1
        else -> {
            val middleIdx = items.size / 2
            val comp = items[middleIdx].compareTo(lookFor)
            when {
                comp > 0 -> searchFrom(indexDelta, items.subList(0, middleIdx), lookFor)
                comp < 0 -> searchFrom(indexDelta + middleIdx + 1, items.subList(middleIdx + 1, items.size), lookFor)
                else -> middleIdx + indexDelta
            }
        }
    }
}
