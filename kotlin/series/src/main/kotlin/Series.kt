object Series {
    fun slices(len: Int, input: String): List<List<Int>> {
        require(len > 0) { "Slice length must be >= 0" }
        require(len <= input.length) { "Slice length must be <= input length" }
        require(!input.isEmpty()) { "Input cannot be empty" }

        return (0..(input.length - len)).map { input.substring(it until (it + len)) }
                                        .map { s -> s.map { c -> c - '0' } }
    }
}
