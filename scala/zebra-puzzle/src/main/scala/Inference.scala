import BitUtils.popCount
import Domain.{Beverage, Color, Hobby, Nationality, NumHouses, Pet, VariableIndex}

import scala.util.boundary
import scala.util.boundary.break

/**
 * Handles applying inference for puzzle statements.
 */
object Inference:
  /**
   * Attempts to add inferences to a [[Puzzle]].
   * If the puzzle configuration was allowed by the statements, returns `true` and `puzzle` is updated.
   * Otherwise, returns `false` and the state of `puzzle` is undefined.
   *
   * @param puzzle puzzle to apply inference to
   * @return whether puzzle configuration was allowed by the statements
   */
  def apply(puzzle: Puzzle): Boolean = Statements.forall(statement => statement(puzzle))

  // Stores all puzzle statements.
  private val Statements = List(
    // 1. "There are five houses."
    // (No inference needed for this one)

    LinkStatement(
      "2. The Englishman lives in the red house.",
      VariableIndex.Owner,
      Nationality.Englishman,
      VariableIndex.Color,
      Color.Red,
    ),

    LinkStatement(
      "3. The Spaniard owns the dog.",
      VariableIndex.Owner,
      Nationality.Spaniard,
      VariableIndex.Pet,
      Pet.Dog,
    ),

    LinkStatement(
      "4. Coffee is drunk in the green house.",
      VariableIndex.Beverage,
      Beverage.Coffee,
      VariableIndex.Color,
      Color.Green,
    ),

    LinkStatement(
      "5. The Ukrainian drinks tea.",
      VariableIndex.Owner,
      Nationality.Ukrainian,
      VariableIndex.Beverage,
      Beverage.Tea,
    ),

    OrderedNeighbouringLinkStatement(
      "6. The green house is immediately to the right of the ivory house.",
      VariableIndex.Color,
      Color.Ivory,
      VariableIndex.Color,
      Color.Green,
    ),

    LinkStatement(
      "7. The snail owner likes to go dancing.",
      VariableIndex.Hobby,
      Hobby.Dancing,
      VariableIndex.Pet,
      Pet.Snails,
    ),

    LinkStatement(
      "8. The person in the yellow house is a painter.",
      VariableIndex.Hobby,
      Hobby.Painting,
      VariableIndex.Color,
      Color.Yellow,
    ),

    PositionStatement(
      "9. Milk is drunk in the middle house.",
      2,
      VariableIndex.Beverage,
      Beverage.Milk,
    ),

    PositionStatement(
      "10. The Norwegian lives in the first house.",
      0,
      VariableIndex.Owner,
      Nationality.Norwegian,
    ),

    NeighbouringLinkStatement(
      "11. The person who enjoys reading lives in the house next to the person with the fox.",
      VariableIndex.Hobby,
      Hobby.Reading,
      VariableIndex.Pet,
      Pet.Fox,
    ),

    NeighbouringLinkStatement(
      "12. The painter's house is next to the house with the horse.",
      VariableIndex.Hobby,
      Hobby.Painting,
      VariableIndex.Pet,
      Pet.Horse,
    ),

    LinkStatement(
      "13. The person who plays football drinks orange juice.",
      VariableIndex.Hobby,
      Hobby.Football,
      VariableIndex.Beverage,
      Beverage.OrangeJuice,
    ),

    LinkStatement(
      "14. The Japanese person plays chess.",
      VariableIndex.Owner,
      Nationality.Japanese,
      VariableIndex.Hobby,
      Hobby.Chess,
    ),

    NeighbouringLinkStatement(
      "15. The Norwegian lives next to the blue house.",
      VariableIndex.Owner,
      Nationality.Norwegian,
      VariableIndex.Color,
      Color.Blue,
    ),
  )

  /**
   * Trait representing a puzzle statement.
   */
  private sealed trait Statement:
    /**
     * Applies this statement to the given [[Puzzle]].
     * If the puzzle configuration was allowed by the statement, returns `true` and `puzzle` is updated.
     * Otherwise, returns `false` and the state of `puzzle` is undefined.
     *
     * @param puzzle puzzle to apply statement to
     * @return whether puzzle configuration was allowed by the statement
     */
    def apply(puzzle: Puzzle): Boolean

  /**
   * Inference for a puzzle statement that links two variable values
   * together for one house. For example:
   * '''The Englishman lives in the red house.'''
   *
   * @param description Statement description; used for debugging only
   * @param idx1   index of first linked variable
   * @param value1 value of first linked variable
   * @param idx2   index of second linked variable
   * @param value2 value of second linked variable
   */
  private class LinkStatement(description: String, idx1: Int, value1: Int, idx2: Int, value2: Int) extends Statement:
    /**
     * @inheritdoc
     */
    override def apply(puzzle: Puzzle): Boolean = doApply(puzzle, false)

    private def doApply(puzzle: Puzzle, reverse: Boolean): Boolean =
      boundary:
        // Determine what variable we're looking for this time around.
        val idx1 = if reverse then this.idx2 else this.idx1
        val value1 = if reverse then this.value2 else this.value1
        val idx2 = if reverse then this.idx1 else this.idx2
        val value2 = if reverse then this.value1 else this.value2

        // Attempt to find a house whose variable 1 has the correct value.
        for
          houseIdx <- 0 until NumHouses
          if puzzle.value(houseIdx, idx1) == value1
        do
          // If the house also has another value than value 2 set for variable 2, inference fails.
          val variable2 = puzzle.value(houseIdx, idx2)
          if popCount(variable2) == 1 && variable2 != value2 then break(false)

          // Set house's variable 2 to value 2.
          puzzle.value(houseIdx, idx2) = value2

          // Iterate the other houses.
          for
            otherHouseIdx <- 0 until NumHouses
            if otherHouseIdx != houseIdx
          do
            // If the other house has value 2 set for variable 2, inference fails.
            val otherVariable2 = puzzle.value(otherHouseIdx, idx2)
            val otherVariableIsSet = popCount(otherVariable2) == 1
            if otherVariableIsSet && otherVariable2 == value2 then break(false)

            // Remove value 2 from possible values of other house's variable 2
            // if its value is still undecided.
            if !otherVariableIsSet then
              puzzle.value(otherHouseIdx, idx2) = otherVariable2 & ~value2

        // If this is the first call, do the process in reverse.
        reverse || doApply(puzzle, true)

  /**
   * Inference for a puzzle statement that provides a fixed value
   * for a variable in a given house. For example:
   * '''Milk is drunk in the middle house.'''
   *
   * @param description Statement description; used for debugging only
   * @param houseIdx index of house for which to apply the value (0-based)
   * @param varIdx   index of variable to set
   * @param value    variable value
   */
  private class PositionStatement(description: String, houseIdx: Int, varIdx: Int, value: Int) extends Statement:
    /**
     * @inheritdoc
     */
    override def apply(puzzle: Puzzle): Boolean =
      boundary:
        // Such statements actually set the variable of a house to a specific value.
        val value = puzzle.value(houseIdx, varIdx)

        // If the variable is already set to some other value, inference fails.
        if popCount(value) == 1 && value != this.value then break(false)

        // Set the variable to its only valid value.
        puzzle.value(houseIdx, varIdx) = this.value
        true

  /**
   * Inference for a puzzle statement that links two variable values
   * for houses next to each other, regardless of order. For example:
   * '''The Norwegian lives next to the blue house.'''
   *
   * @param description Statement description; used for debugging only
   * @param varIdxNeighbour1 index of variable for first neighbour
   * @param valueNeighbour1  value of variable for first neighbour
   * @param varIdxNeighbour2 index of variable for second neighbour
   * @param valueNeighbour2  value of variable for second neighbour
   */
  private class NeighbouringLinkStatement(description: String,
                                          varIdxNeighbour1: Int, valueNeighbour1: Int,
                                          varIdxNeighbour2: Int, valueNeighbour2: Int) extends Statement:
    /**
     * @inheritdoc
     */
    override def apply(puzzle: Puzzle): Boolean = doApply(puzzle, false)

    private def doApply(puzzle: Puzzle, reverse: Boolean): Boolean =
      boundary:
        // Determine what variable we're looking for this time around.
        val varIdxNeighbour1 = if reverse then this.varIdxNeighbour2 else this.varIdxNeighbour1
        val valueNeighbour1 = if reverse then this.valueNeighbour2 else this.valueNeighbour1
        val varIdxNeighbour2 = if reverse then this.varIdxNeighbour1 else this.varIdxNeighbour2
        val valueNeighbour2 = if reverse then this.valueNeighbour1 else this.valueNeighbour2

        // Attempt to find a house whose variable 1 has the correct value.
        for
          houseIdx <- 0 until NumHouses
          if puzzle.value(houseIdx, varIdxNeighbour1) == valueNeighbour1
        do
          // This house has value 1 set for variable 1.

          // Check which other houses are neighbours of this house.
          val firstNeighbourIdx = if houseIdx > 0 then houseIdx - 1 else 1
          val secondNeighbourIdx = if houseIdx < (NumHouses - 1) then houseIdx + 1 else -1

          if secondNeighbourIdx == -1 then
            // House has only one neighbour.
            // If that neighbour has a different value than value 2 for variable 2, inference fails.
            val variable2 = puzzle.value(firstNeighbourIdx, varIdxNeighbour2)
            val variableIsSet = popCount(variable2) == 1
            if variableIsSet && variable2 != valueNeighbour2 then break(false)

            // Set the neighbour's variable 2 to value 2.
            puzzle.value(firstNeighbourIdx, varIdxNeighbour2) = valueNeighbour2
          else
            // House has two neighbours.
            val leftVariable2 = puzzle.value(firstNeighbourIdx, varIdxNeighbour2)
            val rightVariable2 = puzzle.value(secondNeighbourIdx, varIdxNeighbour2)
            val leftVariableIsSet = popCount(leftVariable2) == 1
            val rightVariableIsSet = popCount(rightVariable2) == 1

            if leftVariableIsSet then
              if leftVariable2 == valueNeighbour2 then
                // Left neighbour has value 2 set for variable 2 already,
                // so the right neighbour can't have it.
                if !rightVariableIsSet then
                  puzzle.value(secondNeighbourIdx, varIdxNeighbour2) = rightVariable2 & ~valueNeighbour2
              else
                // Left neighbour has a different value than value 2 for variable 2,
                // so the right neighbour must have value 2.
                if !rightVariableIsSet then
                  puzzle.value(secondNeighbourIdx, varIdxNeighbour2) = valueNeighbour2
                else if rightVariable2 != valueNeighbour2 then
                  // Right neighbour has a different value also; inference fails.
                  break(false)
            else if rightVariableIsSet then
              if rightVariable2 == valueNeighbour2 then
                // Right neighbour has value 2 for variable 2 already,
                // so the left neighbour can't have it.
                puzzle.value(firstNeighbourIdx, varIdxNeighbour2) = leftVariable2 & ~valueNeighbour2
              else
                // Right neighbour has a different value than value 2 for variable 2,
                // so the left neighbour must have value 2.
                puzzle.value(firstNeighbourIdx, varIdxNeighbour2) = valueNeighbour2

          // Iterate the other houses.
          for
            otherHouseIdx <- 0 until NumHouses
            if otherHouseIdx != houseIdx
            if otherHouseIdx != firstNeighbourIdx
            if otherHouseIdx != secondNeighbourIdx
          do
            // If the other house has value 2 set for variable 2, inference fails.
            val otherVariable2 = puzzle.value(otherHouseIdx, varIdxNeighbour2)
            val otherVariableIsSet = popCount(otherVariable2) == 1
            if otherVariableIsSet && otherVariable2 == valueNeighbour2 then break(false)

            // Remove value 2 from possible values of other house's variable 2
            // if its value is still undecided.
            if !otherVariableIsSet then
              puzzle.value(otherHouseIdx, varIdxNeighbour2) = otherVariable2 & ~valueNeighbour2

        // If this is the first call, do the process in reverse.
        reverse || doApply(puzzle, true)

  /**
   * Inference for a puzzle statement that links two variable values
   * for houses next to each other, but in a specific order. There's only
   * one statement like that:
   * '''The green house is immediately to the right of the ivory house.'''
   *
   * @param description Statement description; used for debugging only
   * @param leftVarIdx  index of variable for the house to the left
   * @param leftValue   value of variable for the house to the left
   * @param rightVarIdx index of variable for the house to the right
   * @param rightValue  value of variable for the house to the right
   */
  private class OrderedNeighbouringLinkStatement(description: String,
                                                 leftVarIdx: Int, leftValue: Int,
                                                 rightVarIdx: Int, rightValue: Int) extends Statement:
    /**
     * @inheritdoc
     */
    override def apply(puzzle: Puzzle): Boolean = doApply(puzzle, false)

    private def doApply(puzzle: Puzzle, reverse: Boolean): Boolean =
      boundary:
        // Determine what variable we're looking for this time around.
        val firstVarIdx = if reverse then this.rightVarIdx else this.leftVarIdx
        val firstValue = if reverse then this.rightValue else this.leftValue
        val secondVarIdx = if reverse then this.leftVarIdx else this.rightVarIdx
        val secondValue = if reverse then this.leftValue else this.rightValue
        val houseIdxOffset = if reverse then -1 else 1

        // Attempt to find a house whose first variable has the correct value.
        for
          houseIdx <- 0 until NumHouses
          if puzzle.value(houseIdx, firstVarIdx) == firstValue
        do
          // This house has the first value set for its first variable.
          // The other house (directly to its left or right) should have
          // the second value set for its second variable.
          val secondHouseIdx = houseIdx + houseIdxOffset

          // If there's no second house, inference fails.
          if secondHouseIdx < 0 || secondHouseIdx >= NumHouses then break(false)

          // If the second house has a value different from the second value
          // for its second variable, inference fails.
          val secondVariable = puzzle.value(secondHouseIdx, secondVarIdx)
          val secondVariableIsSet = popCount(secondVariable) == 1
          if secondVariableIsSet && secondVariable != secondValue then break(false)

          // Set the second house's second variable to the second value.
          puzzle.value(secondHouseIdx, secondVarIdx) = secondValue

          // Iterate the other houses.
          for
            otherHouseIdx <- 0 until NumHouses
            if otherHouseIdx != houseIdx
            if otherHouseIdx != secondHouseIdx
          do
            // If the other house has the second value set for its second variable, inference fails.
            val otherSecondVariable = puzzle.value(otherHouseIdx, secondVarIdx)
            val otherVariableIsSet = popCount(otherSecondVariable) == 1
            if otherVariableIsSet && otherSecondVariable == secondValue then break(false)

            // Remove the second value from possible values of other house's second variable
            // if its value is still undecided.
            if !otherVariableIsSet then
              puzzle.value(otherHouseIdx, secondVarIdx) = otherSecondVariable & ~secondValue

        // If this is the first call, do the process in reverse.
        reverse || doApply(puzzle, true)
