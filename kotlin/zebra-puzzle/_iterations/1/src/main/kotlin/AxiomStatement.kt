class AxiomStatement(private val houseIdx: Int, private val param: String, private val value: String) : Statement {
    override fun apply(state: State): ApplyResult {
        state.set(state.houses[houseIdx], param, value)
        return ApplyResult.DONE
    }

    override fun getCompatibility(state: State): Compatibility = when (value) {
        state.houses[houseIdx].get(param) -> Compatibility.PERFECT_MATCH
        else -> Compatibility.INCOMPATIBLE
    }
}
