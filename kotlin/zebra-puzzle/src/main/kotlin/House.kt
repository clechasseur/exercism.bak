class House(val idx: Int, val params: Map<String, MutableSet<String>> = defaultParams()) {
    companion object {
        private fun defaultParams() = mapOf(
                "owner" to mutableSetOf("Englishman", "Spaniard", "Ukrainian", "Norwegian", "Japanese"),
                "color" to mutableSetOf("red", "green", "ivory", "yellow", "blue"),
                "pet" to mutableSetOf("dog", "snails", "fox", "horse", "zebra"),
                "beverage" to mutableSetOf("coffee", "tea", "milk", "orange juice", "water"),
                "cigarettes" to mutableSetOf("Old Gold", "Kools", "Chesterfields", "Lucky Strike", "Parliaments")
        )
    }

    fun get(param: String): String? {
        val set = params[param]
        return when (set?.size) {
            1 -> set.first()
            else -> null
        }
    }

    fun couldBe(param: String, value: String) = params[param]?.contains(value) ?: false

    fun couldBeOtherThan(param: String, vararg values: String) = setOf(*values) != params[param]

    fun set(param: String, vararg values: String) {
        params[param]?.retainAll(values)
    }

    fun remove(param: String, value: String) {
        params[param]?.remove(value)
    }

    fun clone() = House(idx, params.map { it.key to it.value.toMutableSet() }.toMap())
}
