class School {
    private val _db = mutableMapOf<Int, MutableList<String>>()

    fun db(): Map<Int, List<String>>
        = _db.mapValues { it.value.toList() }

    fun add(student: String, gradeNum: Int) {
        _db.getOrPut(gradeNum) { mutableListOf<String>() }.add(student)
    }

    fun grade(gradeNum: Int): List<String>
        = (_db.get(gradeNum) ?: mutableListOf<String>()).toList()

    fun sort(): Map<Int, List<String>>
        = _db.mapValues { it.value.sorted() }.toSortedMap()
}
