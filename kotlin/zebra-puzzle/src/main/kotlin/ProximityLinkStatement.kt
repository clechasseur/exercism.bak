class ProximityLinkStatement(private val param1: String, private val value1: String,
                             private val param2: String, private val value2: String) : Statement {

    override fun apply(state: State): ApplyResult {
        val result = applyOneSided(state, param1, value1, param2, value2)
        if (result == ApplyResult.DONE) {
            return result
        }
        return sequenceOf(result, applyOneSided(state, param2, value2, param1, value1)).max()!!
    }

    override fun getCompatibility(state: State): Compatibility {
        var compatibility = Compatibility.COMPATIBLE
        state.houses.indices.forEach { houseIdx ->
            val house = state.houses[houseIdx]
            val toTheLeft = houseAt(state, houseIdx - 1)
            val toTheRight = houseAt(state, houseIdx + 1)
            if (value1 == house.get(param1)) {
                if (houseIs(toTheLeft, param2, value2) || houseIs(toTheRight, param2, value2)) {
                    compatibility = Compatibility.PERFECT_MATCH
                } else if (houseCoulntBe(toTheLeft, param2, value2) && houseCoulntBe(toTheRight, param2, value2)) {
                    return Compatibility.INCOMPATIBLE
                }
            }
            if (value2 == house.get(param2)) {
                if (houseIs(toTheLeft, param1, value1) || houseIs(toTheRight, param1, value1)) {
                    compatibility = Compatibility.PERFECT_MATCH
                } else if (houseCoulntBe(toTheLeft, param1, value1) && houseCoulntBe(toTheRight, param1, value1)) {
                    return Compatibility.INCOMPATIBLE
                }
            }
        }
        return compatibility
    }

    private fun applyOneSided(state: State, p1: String, v1: String, p2: String, v2: String): ApplyResult {
        var result = ApplyResult.NONE
        val house = state.getWith(p1, v1)
        if (house != null) {
            val houseIdx = state.houses.indexOf(house)
            var toTheLeft = houseAt(state, houseIdx - 1)
            var toTheRight = houseAt(state, houseIdx + 1)
            if ((toTheLeft != null && v2 == toTheLeft.get(p2)) || (toTheRight != null && v2 == toTheRight.get(p2))) {
                return ApplyResult.DONE
            }
            if (toTheLeft?.get(p2) != null) {
                toTheLeft = null
            }
            if (toTheRight?.get(p2) != null) {
                toTheRight = null
            }
            if (toTheLeft != null || toTheRight != null) {
                if (toTheLeft == null) {
                    state.set(toTheRight!!, p2, v2)
                    return ApplyResult.DONE
                } else if (toTheRight == null) {
                    state.set(toTheLeft, p2, v2)
                    return ApplyResult.DONE
                }
                state.houses.asSequence()
                        .filter { it != toTheLeft && it != toTheRight }
                        .forEach { state.remove(it, p2, v2) }
                result = ApplyResult.APPLIED
            }
        }

        return state.houses.asSequence().map { h ->
            var res = ApplyResult.NONE
            if (!h.couldBe(p1, v1)) {
                val hIdx = state.houses.indexOf(h)
                val toTheLeft = houseAt(state, hIdx - 1)
                val toTheRight = houseAt(state, hIdx + 1)
                if (toTheLeft != null && toTheLeft.couldBe(p2, v2)) {
                    state.remove(toTheLeft, p2, v2)
                    res = ApplyResult.APPLIED
                }
                if (toTheRight != null && toTheRight.couldBe(p2, v2)) {
                    state.remove(toTheRight, p2, v2)
                    res = ApplyResult.APPLIED
                }
            }
            res
        }.fold(result) { acc, r -> when {
            r > acc -> r
            else -> acc
        } }
    }

    private fun houseAt(state: State, houseIdx: Int): House? = when (houseIdx) {
        in state.houses.indices -> state.houses[houseIdx]
        else -> null
    }

    private fun houseIs(house: House?, param: String, value: String)
            = house != null && value == house.get(param)

    private fun houseCoulntBe(house: House?, param: String, value: String)
            = house == null || !house.couldBe(param, value)
}
