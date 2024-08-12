import "random" for Random

class Util {
  static abilityModifier(score) {
    if (score < 3) {
      Fiber.abort("Ability scores must be at least 3")
    } else if (score > 18) {
      Fiber.abort("Ability scores can be at most 18")
    }

    return ((score - 10) / 2).floor
  }
}

class Character {
  construct new() {
    _strength = Character.rollAbility()
    _dexterity = Character.rollAbility()
    _constitution = Character.rollAbility()
    _intelligence = Character.rollAbility()
    _wisdom = Character.rollAbility()
    _charisma = Character.rollAbility()
  }

  static rollAbility() {
    __rand = __rand || Random.new()

    return [__rand.int(1, 7), __rand.int(1, 7), __rand.int(1, 7), __rand.int(1, 7)].
      sort {|a, b| a > b }.
      take(3).
      reduce {|acc, d| acc + d }
  }

  strength { _strength }
  dexterity { _dexterity }
  constitution { _constitution }
  intelligence { _intelligence }
  wisdom { _wisdom }
  charisma { _charisma }

  hitpoints { 10 + Util.abilityModifier(constitution) }
}
