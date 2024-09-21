object BeerSong {
    fun verses(start: Int, end: Int): String =
        (start downTo end).map { verse(it) }.joinToString("\n")

    private fun verse(beers: Int): String =
"""${bottles(beers)} of beer on the wall, ${bottles(beers).toLowerCase()} of beer.
${action(beers)}, ${bottles(nextBottles(beers)).toLowerCase()} of beer on the wall.
"""

    private fun bottles(beers: Int): String = when(beers) {
        0 -> "No more bottles"
        1 -> "1 bottle"
        else -> "$beers bottles"
    }

    private fun action(beers: Int): String = when(beers) {
        0 -> "Go to the store and buy some more"
        1 -> "Take it down and pass it around"
        else -> "Take one down and pass it around"
    }

    private fun nextBottles(beers: Int): Int = when(beers) {
        0 -> 99
        else -> beers - 1
    }
}
