fun <T : Comparable<T>> List<T>.relationshipTo(other: List<T>): Relationship = when {
    size < other.size -> other.relationshipTo(this).let {
        if (it == Relationship.SUPERLIST) Relationship.SUBLIST else it
    }
    other.size == 0 -> when {
        size == 0 -> Relationship.EQUAL
        else -> Relationship.SUPERLIST
    }
    else -> when (windowed(other.size).any { it == other }) {
        true -> if (size == other.size) Relationship.EQUAL else Relationship.SUPERLIST
        false -> Relationship.UNEQUAL
    }
}
