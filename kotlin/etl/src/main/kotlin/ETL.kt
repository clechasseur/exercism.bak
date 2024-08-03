object ETL {
    fun transform(old: Map<Int, List<Char>>): Map<Char, Int>
        = old.flatMap { (score, letters) -> letters.map { l -> l.toLowerCase() to score } }.toMap()
}
