object Yacht {
    fun solve(category: YachtCategory, vararg dices: Int): Int {
        val match = matchers[category]?.find(dices.sorted().joinToString("")) ?: return 0
        return (scorers[category] ?: Yacht::defaultScorer)(match)
    }

    private val matchers = mapOf(
            YachtCategory.ONES to singleValueMatcher(1),
            YachtCategory.TWOS to singleValueMatcher(2),
            YachtCategory.THREES to singleValueMatcher(3),
            YachtCategory.FOURS to singleValueMatcher(4),
            YachtCategory.FIVES to singleValueMatcher(5),
            YachtCategory.SIXES to singleValueMatcher(6),
            YachtCategory.FULL_HOUSE to Regex("""(.)(\1{2})(?!\1)(.)(\3)|(.)(\5)(?!\5)(.)(\7{2})"""),
            YachtCategory.FOUR_OF_A_KIND to Regex("""(.)(\1{3})"""),
            YachtCategory.LITTLE_STRAIGHT to Regex("(12345)"),
            YachtCategory.BIG_STRAIGHT to Regex("(23456)"),
            YachtCategory.CHOICE to Regex("(.{5})"),
            YachtCategory.YACHT to Regex("""(.)(\1{4})""")
    )

    private val scorers = mapOf<YachtCategory, (MatchResult) -> Int>(
            YachtCategory.LITTLE_STRAIGHT to { _ -> 30 },
            YachtCategory.BIG_STRAIGHT to { _ -> 30 },
            YachtCategory.YACHT to { _ -> 50 }
    )

    private fun singleValueMatcher(value: Int) = Regex("($value{1,5})")

    private fun defaultScorer(match: MatchResult): Int
            = match.groupValues.drop(1).flatMap { gv -> gv.map { it.toString().toInt() } }.sum()
}