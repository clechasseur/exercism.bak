import Domain.{Beverage, Nationality, Pet, VariableIndex}

/**
 * This is a port of my C solution, available
 * [[https://exercism.org/tracks/c/exercises/zebra-puzzle/solutions/clechasseur here]].
 */
object ZebraPuzzle:
  val Englishman: Int = Nationality.Englishman
  val Spaniard: Int = Nationality.Spaniard
  val Ukrainian: Int = Nationality.Ukrainian
  val Norwegian: Int = Nationality.Norwegian
  val Japanese: Int = Nationality.Japanese

  case class Solution(waterDrinker: Int, zebraOwner: Int)

  lazy val solve: Solution =
    val puzzle = Puzzle()
    puzzle.solve()

    Solution(
      waterDrinker = puzzle.ownerMatching(VariableIndex.Beverage, Beverage.Water),
      zebraOwner = puzzle.ownerMatching(VariableIndex.Pet, Pet.Zebra),
    )
