object Flattener {
    fun flatten(nested: List<Any?>): List<Any> = nested.flatMap { flattenOne(it) }

    private fun flattenOne(one: Any?): List<Any> = when (one) {
        null -> emptyList()
        is List<Any?> -> flatten(one)
        else -> listOf(one)
    }
}
