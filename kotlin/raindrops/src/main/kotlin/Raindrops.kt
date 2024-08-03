object Raindrops {
    fun convert(input: Int): String {
        val sb = StringBuilder()
        if (input % 3 == 0) {
            sb.append("Pling")
        }
        if (input % 5 == 0) {
            sb.append("Plang")
        }
        if (input % 7 == 0) {
            sb.append("Plong")
        }
        if (sb.isEmpty()) {
            sb.append(input)
        }
        return sb.toString()
    }
}
