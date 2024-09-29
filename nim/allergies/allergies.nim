type
  Allergen* = enum
    Eggs, Peanuts, Shellfish, Strawberries, Tomatoes, Chocolate, Pollen, Cats
  Allergens* = set[Allergen]

proc allergies*(score: int): Allergens =
  cast[Allergens](score)

proc isAllergicTo*(score: int, allergen: Allergen): bool =
  allergen in allergies(score)
