class LinkStatement(private val param1: String, private val value1: String,
                    private val param2: String, private val value2: String) : Statement {

    override fun apply(state: State): ApplyResult {
        var house = state.getWith(param1, value1)
        if (house != null) {
            state.set(house, param2, value2)
            return ApplyResult.DONE
        }

        house = state.getWith(param2, value2)
        if (house != null) {
            state.set(house, param1, value1)
            return ApplyResult.DONE
        }

        return state.houses.asSequence().map { h ->
            var res = ApplyResult.NONE
            if (!h.couldBe(param1, value1) && h.couldBe(param2, value2)) {
                state.remove(h, param2, value2)
                res = ApplyResult.APPLIED
            }
            if (!h.couldBe(param2, value2) && h.couldBe(param1, value1)) {
                state.remove(h, param1, value1)
                res = ApplyResult.APPLIED
            }
            res
        }.max()!!
    }

    override fun getCompatibility(state: State): Compatibility
            = state.houses.asSequence().map { getCompatibility(it) }.max()!!

    private fun getCompatibility(house: House): Compatibility {
        if (value1 == house.get(param1)) {
            if (value2 == house.get(param2)) {
                return Compatibility.PERFECT_MATCH
            } else if (!house.couldBe(param2, value2)) {
                return Compatibility.INCOMPATIBLE
            }
        }
        if (value2 == house.get(param2)) {
            if (value1 == house.get(param1)) {
                return Compatibility.PERFECT_MATCH
            } else if (!house.couldBe(param1, value1)) {
                return Compatibility.INCOMPATIBLE
            }
        }
        return Compatibility.COMPATIBLE
    }
}