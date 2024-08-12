class KindergartenGarden(diagram: String) {
    companion object {
        private val plants = listOf("radishes", "clover", "grass", "violets")
    }

    private val garden = diagram.lineSequence().map {
        row -> row.asSequence().chunked(2) {
            chunk -> chunk.map { p -> plants.find { it.startsWith(p.toLowerCase()) }!! }
        }.mapIndexed { idx, chunk -> idx to chunk }.associate { it }
    }.fold(mutableMapOf<Int, MutableList<String>>()) {
        acc, m -> acc.apply { m.forEach { (studentIdx, studentPlants) ->
            getOrPut(studentIdx) { mutableListOf() }.addAll(studentPlants)
        } }
    }.asSequence().associate { it.key to it.value.toList() }

    fun getPlantsOfStudent(student: String): List<String> = garden[student[0] - 'A']!!
}
