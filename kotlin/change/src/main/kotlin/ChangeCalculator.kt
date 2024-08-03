import kotlin.math.min

class ChangeCalculator(val coins: List<Int>) {
    init {
        require(!coins.isEmpty()) { "Must have at least one coin type" }
        require(coins.sorted() == coins) { "Coin types must be sorted" }
        require(coins.all { it > 0 }) { "Coin types must be greater than 0" }
        require(coins.distinct() == coins) { "Coin types must be unique" }
    }

    fun computeMostEfficientChange(amount: Int): List<Int> {
        var change: List<Int>? = null
        if (coins.first() == 1) {
            change = possiblyComputeMostEfficientChange(amount, coins.drop(1).reversed())
        }
        if (change == null) {
            change = possiblyComputeMostEfficientChange(amount, coins.reversed())
        }
        require(change != null) { "The total $amount cannot be represented in the given currency." }
        return change.sorted()
    }

    private fun possiblyComputeMostEfficientChange(amount: Int, withCoins: List<Int>, stepsLeft: Int = Int.MAX_VALUE): List<Int>? {
        require(amount >= 0) { "Negative totals are not allowed." }
        if (amount == 0) {
            return emptyList()
        }
        if (stepsLeft == 0) {
            return null
        }
        
        var change: List<Int>? = null
        for (coin in withCoins) {
            if (amount >= coin) {
                val subChange = possiblyComputeMostEfficientChange(
                    amount - coin,
                    withCoins.dropWhile { it > coin },
                    min(stepsLeft - 1, (change?.size ?: Int.MAX_VALUE) - 1))
                if (subChange != null && (change == null || (subChange.size + 1) < change.size)) {
                    change = listOf(coin) + subChange
                }
            }
        }
        return change
    }
}
