package lasagna

const (
	// DefaultTimePerLayer is the default time it takes to prepare one layer of lasagna.
	DefaultTimePerLayer = 2

	// NoodleAmount is the amount of noodles to use for one portion of noodles, in grams.
	NoodleAmount = 50

	// SauceAmount is the amount of sauce to use for one portion of sauce, in litres.
	SauceAmount = 0.2

	// DefaultPortions is the number of portions yielded by one lasagna recipe.
	DefaultPortions = 2.0
)

// PreparationTime calculates the total preparation time for the lasagna.
func PreparationTime(layers []string, timePerLayer int) int {
	if timePerLayer == 0 {
		timePerLayer = DefaultTimePerLayer
	}

	return len(layers) * timePerLayer
}

// Quantities returns the quantity of noodles and sauce needed to prepare the lasagna.
func Quantities(layers []string) (noodles int, sauce float64) {
	for _, layer := range layers {
		switch layer {
		case "noodles":
			noodles += NoodleAmount
		case "sauce":
			sauce += SauceAmount
		}
	}
	return
}

// AddSecretIngredient takes the secret ingredient from an awesome lasagna recipe
// and copies it to your standard, boring recipe.
func AddSecretIngredient(awesomeRecipe, boringRecipe []string) {
	boringRecipe[len(boringRecipe)-1] = awesomeRecipe[len(awesomeRecipe)-1]
}

// ScaleRecipe scales a lasagna recipe, so it yields the given number of potions.
// The quantities provided are expected to be for a recipe yielding the default number of portions.
func ScaleRecipe(quantities []float64, portions int) (scaledQuantities []float64) {
	scale := float64(portions) / DefaultPortions
	scaledQuantities = make([]float64, len(quantities))
	for i, q := range quantities {
		scaledQuantities[i] = q * scale
	}
	return
}
