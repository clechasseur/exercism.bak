import std/[algorithm, random, sequtils]

type
  Character* = object
    strength*: int
    dexterity*: int
    constitution*: int
    intelligence*: int
    wisdom*: int
    charisma*: int
    hitpoints*: int

let baseHitPoints* = 10

proc ability*: int =
  var
    die = @[rand(1..6), rand(1..6), rand(1..6), rand(1..6)]
    sortedDie = die.sorted(Descending)
  sortedDie.delete(3)
  foldl(sortedDie, a + b)

proc modifier*(n: int): int =
  (n div 2) - 5

proc initCharacter*: Character =
  var
    c = Character(
      strength: ability(),
      dexterity: ability(),
      constitution: ability(),
      intelligence: ability(),
      wisdom: ability(),
      charisma: ability()
    )
  c.hitpoints = baseHitPoints + modifier(c.constitution)
  c

randomize()
