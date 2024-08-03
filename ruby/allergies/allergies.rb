class Allergies
  ALLERGENS = %w[eggs peanuts shellfish strawberries tomatoes chocolate pollen cats]

  attr_reader :score

  def initialize(score)
    @score = score
  end

  def allergic_to?(allergen)
    score & (1 << ALLERGENS.index(allergen)) != 0
  end

  def list
    ALLERGENS.filter { allergic_to? _1 }
  end
end
