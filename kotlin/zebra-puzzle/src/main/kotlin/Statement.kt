interface Statement {
    fun apply(state: State): ApplyResult
    fun getCompatibility(state: State): Compatibility
}
