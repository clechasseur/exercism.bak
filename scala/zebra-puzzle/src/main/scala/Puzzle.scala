import BitUtils.{firstBit, popCount}
import Domain.{AllPossibilities, InvalidValue, NumHouses, NumVariables, VariableIndex}

import scala.util.boundary
import scala.util.boundary.break

/**
 * Stores the state of the puzzle, with the variables of each house.
 */
class Puzzle:
  // Store the values of every variable for each house in one contiguous block, to ease traversal.
  private val values = Array.fill(NumHouses * NumVariables) {
    AllPossibilities
  }

  /**
   * Access for the values of variables in each house.
   */
  object value:
    /**
     * Returns the value of a variable for the given house.
     *
     * @param houseIdx index of house (0-based), from left to right
     * @param varIdx   index of variable (0-based)
     * @return variable value
     */
    def apply(houseIdx: Int, varIdx: Int): Int = values(houseIdx * NumHouses + varIdx)

    /**
     * Sets the value of a variable for the given house.
     *
     * @param houseIdx index of house (0-based), from left to right
     * @param varIdx   index of variable (0-based)
     * @param value    new variable value
     */
    def update(houseIdx: Int, varIdx: Int, value: Int): Unit = values(houseIdx * NumHouses + varIdx) = value

  /**
   * Solves the puzzle and leaves it in the valid configuration.
   */
  def solve(): Unit =
    if !backtrackingSolve() then
      throw RuntimeException("Could not solve puzzle")

  /**
   * Returns the owner of the house matching the given variable's value.
   *
   * @param varIdx   index of variable to use to identify owner
   * @param varValue value of variable to use to identify owner
   * @return owner nationality (see [[Domain.Nationality]])
   */
  def ownerMatching(varIdx: Int, varValue: Int): Int =
    (0 until NumHouses)
      .find(this.value(_, varIdx) == varValue)
      .map(this.value(_, VariableIndex.Owner))
      .getOrElse(throw RuntimeException("Could not find owner (is puzzle solved?)"))

  /**
   * Checks if the puzzle configuration is valid.
   * The configuration is valid if no two houses have the same value for a given variable
   * and if no variable is set to an invalid value.
   */
  private def valid: Boolean =
    boundary:
      val seenValues = Array.fill(NumVariables) {
        0
      }

      // Iterate all variables of each house.
      for
        houseIdx <- 0 until NumHouses
        varIdx <- 0 until NumVariables
      do
        val varValue = this.value(houseIdx, varIdx)

        // If the variable has an invalid value, the puzzle configuration is invalid.
        if varValue == InvalidValue then break(false)

        // If the variable has a single bit set (meaning it has a definitive value,
        // it's not just a bitfield of possibilities) and we've seen that value before,
        // the puzzle configuration is invalid.
        if popCount(varValue) == 1 then
          if (seenValues(varIdx) & varValue) != 0 then break(false)

          // Note the value of this variable as "seen".
          seenValues(varIdx) |= varValue

      // If we made it here, the puzzle configuration is valid.
      true

  /**
   * Checks if the given puzzle configuration is complete.
   * The configuration is complete if all variables have different values for each house.
   */
  private def complete: Boolean =
    boundary:
      val remainingValues = Array.fill(NumVariables) {
        AllPossibilities
      }

      // Iterate all variables of each house.
      for
        houseIdx <- 0 until NumHouses
        varIdx <- 0 until NumVariables
      do
        val varValue = this.value(houseIdx, varIdx)

        // If the variable does not have a single bit set (meaning it still
        // has multiple possibilities, or it has an invalid value), or if the
        // value of this variable was already used, then puzzle configuration
        // is not complete.
        if popCount(varValue) != 1 || (remainingValues(varIdx) & varValue) == 0 then break(false)

        // Remove this variable's value from the possibilities.
        remainingValues(varIdx) &= ~varValue

      // If we made it here, the puzzle configuration is complete.
      true

  /**
   * Attempts to solve the puzzle.
   * If we could solve the puzzle, returns true and puzzle contains the solution.
   * Otherwise, returns false and puzzle is unmodified.
   */
  private def backtrackingSolve(): Boolean =
    boundary:
      // The algorithm used to solve the puzzle has been inspired by this post:
      // https://www.baeldung.com/cs/csp

      // If this puzzle configuration is complete, return it.
      if complete then break(true)

      // Select a variable which has not been assigned yet.
      val valueIdx = nextUnassignedIdx

      // If we ran out of variables, it means the search is a dead end
      // in this branch. Return failure.
      if valueIdx == -1 then break(false)

      // Copy list of possible values for that variable.
      val valueBackup = values(valueIdx)
      var possibleValues = valueBackup

      // Go over possible values and try to solve.
      val valuesBackup = new Array[Int](NumHouses * NumVariables)
      while possibleValues != 0 do
        // Get next possible value and save it to the variable.
        val varValue = firstBit(possibleValues)
        possibleValues &= ~varValue
        values(valueIdx) = varValue

        // Make sure the puzzle configuration is valid with this value.
        if valid then
          // Attempt to add inferences to the puzzle config.
          // If it works, try solving the puzzle with this variable value.
          values.copyToArray(valuesBackup)
          if Inference.apply(this) && backtrackingSolve() then
            break(true)

          // Puzzle wasn't solved; remove inferences from the config.
          valuesBackup.copyToArray(values)

      // If we reach this point, it means we ran out of possible values
      // for the variable we selected. Thus, the search is a dead end in this branch.
      // Restore the variable to its original value and return failure.
      values(valueIdx) = valueBackup
      false

  /**
   * Selects the next unassigned variable in the puzzle and returns its index.
   */
  private def nextUnassignedIdx: Int =
    // We'll use the Minimum-Remaining-Values (MRV) heuristic to select the next variable:
    // we prioritize variables with the least remaining possible values.
    values
      .indices
      .map(i => (i, popCount(values(i))))
      .filter(_._2 > 1)
      .minBy(_._2)
      ._1
