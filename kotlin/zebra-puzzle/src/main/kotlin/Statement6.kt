class Statement6 : Statement {
    // "The green house is immediately to the right of the ivory house."

    override fun apply(state: State): ApplyResult {
        var house = state.getWith("color", "green")
        if (house != null) {
            val houseIdx = state.houses.indexOf(house)
            state.set(state.houses[houseIdx - 1], "color", "ivory")
            return ApplyResult.DONE
        }

        house = state.getWith("color", "ivory")
        if (house != null) {
            val houseIdx = state.houses.indexOf(house)
            state.set(state.houses[houseIdx + 1], "color", "green")
            return ApplyResult.DONE
        }

        var result = ApplyResult.NONE
        if (state.houses.first().couldBe("color", "green")) {
            state.remove(state.houses.first(), "color", "green")
            result = ApplyResult.APPLIED
        }
        if (state.houses.last().couldBe("color", "ivory")) {
            state.remove(state.houses.last(), "color", "ivory")
            result = ApplyResult.APPLIED
        }
        result = state.houses.asSequence().map { h ->
            var res = ApplyResult.NONE
            if (!h.couldBe("color", "green")) {
                val hIdx = state.houses.indexOf(h)
                if (hIdx > state.houses.indices.first && state.houses[hIdx - 1].couldBe("color", "ivory")) {
                    state.remove(state.houses[hIdx - 1], "color", "ivory")
                    res = ApplyResult.APPLIED
                }
            }
            if (!h.couldBe("color", "ivory")) {
                val hIdx = state.houses.indexOf(h)
                if (hIdx < state.houses.indices.last && state.houses[hIdx + 1].couldBe("color", "green")) {
                    state.remove(state.houses[hIdx + 1], "color", "green")
                    res = ApplyResult.APPLIED
                }
            }
            res
        }.fold(result) { acc, r -> when {
            r > acc -> r
            else -> acc
        } }

        if (!state.houses[1].couldBe("color", "green")
                && !state.houses[1].couldBe("color", "ivory")
                && state.houses[3].couldBeOtherThan("color", "green", "ivory")) {
            state.houses[3].set("color", "green", "ivory")
            result = ApplyResult.APPLIED
        }
        if (!state.houses[3].couldBe("color", "green")
                && !state.houses[3].couldBe("color", "ivory")
                && state.houses[1].couldBeOtherThan("color", "gren", "ivory")) {
            state.houses[1].set("color", "green", "ivory")
            result = ApplyResult.APPLIED
        }

        return result
    }

    override fun getCompatibility(state: State): Compatibility {
        var compatibility = Compatibility.COMPATIBLE
        state.houses.indices.forEach { houseIdx ->
            val house = state.houses[houseIdx]
            val toTheLeft = houseAt(state, houseIdx - 1)
            val toTheRight = houseAt(state, houseIdx + 1)
            if ("green" == house.get("color")) {
                if ("ivory" == toTheLeft?.get("color")) {
                    compatibility = Compatibility.PERFECT_MATCH
                } else if (toTheLeft == null || !toTheLeft.couldBe("color", "ivory")) {
                    return Compatibility.INCOMPATIBLE
                }
            }
            if ("ivory" == house.get("color")) {
                if ("green" == toTheRight?.get("color")) {
                    compatibility = Compatibility.PERFECT_MATCH
                } else if (toTheRight == null || !toTheRight.couldBe("color", "green")) {
                    return Compatibility.INCOMPATIBLE
                }
            }
        }
        return compatibility
    }

    private fun houseAt(state: State, houseIdx: Int): House? = when (houseIdx) {
        in state.houses.indices -> state.houses[houseIdx]
        else -> null
    }
}