/**
 * Provides information about the puzzle's domain values.
 *
 * @note As of this writing, Exercism's Scala track uses Scala 3.4.2
 *       which doesn't support binary literals, so I used hex literals
 *       instead. If ever the Scala track is updated to Scala 3.5+,
 *       they could be replaced for clarity.
 */
object Domain:
  /**
   * Number of houses in the puzzle.
   */
  val NumHouses = 5

  /**
   * Number of variables for each house.
   */
  val NumVariables = 5

  /**
   * Indexes of each variable in a house's data.
   */
  object VariableIndex:
    val Owner = 0
    val Color = 1
    val Beverage = 2
    val Pet = 3
    val Hobby = 4

  /**
   * Sentinel value representing an invalid configuration for a variable.
   */
  val InvalidValue = 0

  /**
   * Sentinel value representing all values combined.
   * Used to initialize a full bitfield of possible values.
   */
  val AllPossibilities = 0x1f // 0b11111

  /**
   * Nationalities of house occupants.
   */
  object Nationality:
    val Englishman = 0x1 // 0b00001
    val Spaniard = 0x2 // 0b00010
    val Ukrainian = 0x4 // 0b00100
    val Norwegian = 0x8 // 0b01000
    val Japanese = 0x10 // 0b10000

  /**
   * House colors
   */
  object Color:
    val Red = 0x1 // 0b00001
    val Green = 0x2 // 0b00010
    val Ivory = 0x4 // 0b00100
    val Yellow = 0x8 // 0b01000
    val Blue = 0x10 // 0b10000

  /**
   * Beverages
   */
  object Beverage:
    val Coffee = 0x1 // 0b00001
    val Tea = 0x2 // 0b00010
    val Milk = 0x4 // 0b00100
    val OrangeJuice = 0x8 // 0b01000
    val Water = 0x10 // 0b10000

  /**
   * Pets
   */
  object Pet:
    val Dog = 0x1 // 0b00001
    val Snails = 0x2 // 0b00010
    val Fox = 0x4 // 0b00100
    val Horse = 0x8 // 0b01000
    val Zebra = 0x10 // 0b10000

  /**
   * Hobbies
   */
  object Hobby:
    val Dancing = 0x1 // 0b00001
    val Painting = 0x2 // 0b00010
    val Reading = 0x4 // 0b00100
    val Football = 0x8 // 0b01000
    val Chess = 0x10 // 0b10000
