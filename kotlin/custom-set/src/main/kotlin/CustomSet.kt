class CustomSet(vararg elements: Int) {
    private val _elements = mutableListOf<Int>().apply { elements.distinct().toCollection(this) }

    fun isEmpty(): Boolean = _elements.isEmpty()

    fun isSubset(other: CustomSet): Boolean = (this - other).isEmpty()

    fun isDisjoint(other: CustomSet): Boolean = intersection(other).isEmpty()

    fun contains(other: Int): Boolean = _elements.contains(other)

    fun intersection(other: CustomSet): CustomSet
            = CustomSet(*_elements.filter { other.contains(it) }.toIntArray())

    fun add(other: Int) {
        if (!contains(other)) {
            _elements.add(other)
        }
    }

    override fun equals(other: Any?): Boolean = when (other) {
        is CustomSet -> _elements.sorted() == other._elements.sorted()
        else -> false
    }

    override fun hashCode(): Int = _elements.sorted().hashCode()

    operator fun plus(other: CustomSet): CustomSet
            = CustomSet(*(_elements + other._elements).distinct().toIntArray())

    operator fun minus(other: CustomSet): CustomSet
            = CustomSet(*(_elements - other._elements).toIntArray())
}
