#include "zebra_puzzle.h"

#include "domain.h"
#include "solver.h"

#include <stddef.h>

// Maps owner values to the actual owner names as strings,
// as expected by the tests.
static const char *owner_name(uint8_t owner)
{
    switch (owner)
    {
    case ENGLISHMAN:
        return "Englishman";
    case SPANIARD:
        return "Spaniard";
    case UKRAINIAN:
        return "Ukrainian";
    case NORWEGIAN:
        return "Norwegian";
    case JAPANESE:
        return "Japanese";
    }

    // Not a valid owner name.
    return NULL;
}

// Pointer to a predicate that can identify a valid house config.
typedef bool (*house_pred_t)(const house_t *);

// Returns the name of the owner whose house matches the given predicate.
static const char *owner_for(const puzzle_t *puzzle, house_pred_t pred)
{
    for (const house_t *house = puzzle->houses; house != puzzle->houses + NUM_HOUSES; ++house) {
        if (pred(house)) {
            return owner_name(house->owner);
        }
    }

    // Owner not found.
    return NULL;
}

// Predicate identifying the house where the water drinker lives.
static bool houses_water_drinker(const house_t *house)
{
    return house->beverage == WATER;
}

// Predicate identifying the house where the zebra owner lives.
static bool houses_zebra(const house_t *house)
{
    return house->pet == ZEBRA;
}

solution_t solve_puzzle(void)
{
    puzzle_t puzzle = solve();

    solution_t solution = {
        .drinks_water = owner_for(&puzzle, houses_water_drinker),
        .owns_zebra   = owner_for(&puzzle, houses_zebra)
    };
    return solution;
}

