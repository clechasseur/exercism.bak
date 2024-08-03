class Allergies(val score: Int) {
    fun isAllergicTo(allergen: Allergen): Boolean = (score and allergen.score) != 0

    fun getList(): List<Allergen> = Allergen.values().filter { isAllergicTo(it) }.toList()
}
