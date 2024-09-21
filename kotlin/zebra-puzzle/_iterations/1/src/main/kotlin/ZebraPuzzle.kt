// Kotlin port of my half-assed Java solution:
// https://exercism.io/tracks/java/exercises/zebra-puzzle/solutions/d843c88030d3497c99ace798952f936b

class ZebraPuzzle {
    private val state: State?

    init {
        val state = State()
        val statements = getStatements()
        state.applyStatements(statements)
        this.state = state.guess(statements, mutableListOf("owner", "color", "pet", "beverage", "cigarettes"))
    }

    fun drinksWater(): String = state.drinksWater()

    fun ownsZebra(): String = state.ownsZebra()

    private fun State?.drinksWater(): String = ownerFor("beverage", "water")

    private fun State?.ownsZebra(): String = ownerFor("pet", "zebra")

    private fun State?.ownerFor(param: String, value: String) : String
            = this?.getWith(param, value)?.get("owner") ?: ""

    private fun State.solved(statements: List<Statement>): Boolean = statements.all {
        it.getCompatibility(this) == Compatibility.PERFECT_MATCH
    } && drinksWater().isNotEmpty() && ownsZebra().isNotEmpty()

    private fun State.applyStatements(statements: MutableList<Statement>) {
        while (statements.isNotEmpty()) {
            val statementsToRemove = mutableListOf<Statement>()
            val applied = statements.asSequence().map { statement ->
                val result = statement.apply(this)
                if (result == ApplyResult.DONE) {
                    statementsToRemove.add(statement)
                }
                result
            }.max()!! != ApplyResult.NONE
            statements.removeAll(statementsToRemove)
            if (!applied) {
                break
            }
        }
    }

    private fun State.guess(statements: List<Statement>, params: MutableList<String>): State? {
        if (params.isEmpty()) {
            return when {
                solved(statements) -> this
                else -> null
            }
        }
        val param = params.removeAt(0)

        val values = houses.map { house -> house.params[param]?.toList() ?: emptyList() }
        values[0].forEach { value0 ->
            values[1].forEach { value1 ->
                values[2].forEach { value2 ->
                    values[3].forEach { value3 ->
                        values[4].forEach { value4 ->
                            val clonedState = clone()
                            clonedState.set(clonedState.houses[0], param, value0)
                            clonedState.set(clonedState.houses[1], param, value1)
                            clonedState.set(clonedState.houses[2], param, value2)
                            clonedState.set(clonedState.houses[3], param, value3)
                            clonedState.set(clonedState.houses[4], param, value4)
                            val finalState = clonedState.guess(statements, params.toMutableList())
                            if (finalState != null) {
                                return finalState
                            }
                        }
                    }
                }
            }
        }
        return null
    }

    private fun getStatements(): MutableList<Statement> = mutableListOf(
            LinkStatement("owner", "Englishman", "color", "red"),
            LinkStatement("owner", "Spaniard", "pet", "dog"),
            LinkStatement("beverage", "coffee", "color", "green"),
            LinkStatement("owner", "Ukrainian", "beverage", "tea"),
            Statement6(),
            LinkStatement("cigarettes", "Old Gold", "pet", "snails"),
            LinkStatement("cigarettes", "Kools", "color", "yellow"),
            AxiomStatement(2, "beverage", "milk"),
            AxiomStatement(0, "owner", "Norwegian"),
            ProximityLinkStatement("cigarettes", "Chesterfields", "pet", "fox"),
            ProximityLinkStatement("cigarettes", "Kools", "pet", "horse"),
            LinkStatement("cigarettes", "Lucky Strike", "beverage", "orange juice"),
            LinkStatement("owner", "Japanese", "cigarettes", "Parliaments"),
            ProximityLinkStatement("owner", "Norwegian", "color", "blue")
    )
}
