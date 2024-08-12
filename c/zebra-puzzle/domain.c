#include "domain.h"

#include "utils.h"

#include <stddef.h>

// Returns a house with all variables accepting all possibilities.
static house_t init_house(void)
{
    house_t house = {
        .owner           = ALL_POSSIBILITIES,
        .color           = ALL_POSSIBILITIES,
        .beverage        = ALL_POSSIBILITIES,
        .pet             = ALL_POSSIBILITIES,
        .cigarette_brand = ALL_POSSIBILITIES
    };

    return house;
}

puzzle_t init_puzzle(void)
{
    puzzle_t puzzle = {
        .houses = {
            init_house(),
            init_house(),
            init_house(),
            init_house(),
            init_house()
        }
    };

    return puzzle;
}

bool is_valid(const puzzle_t *puzzle)
{
    house_t seen_values = {
        .owner           = 0,
        .color           = 0,
        .beverage        = 0,
        .pet             = 0,
        .cigarette_brand = 0
    };

    // Iterate each house to check their values.
    for (const house_t *house = puzzle->houses; house != puzzle->houses + NUM_HOUSES; ++house) {
        // Iterate each variable for this house.
        for (size_t var_i = 0; var_i != NUM_VARIABLES; ++var_i) {
            uint8_t var_value = *(&house->owner + var_i);
            uint8_t *seen_var_values = &seen_values.owner + var_i;

            // If the variable has an invalid value, the puzzle configuration is invalid.
            if (var_value == INVALID_VALUE) {
                return false;
            }

            // If the variable has a single bit set (meaning it has a definitive value,
            // it's not just a bitfield of possibilities) and we've seen that value before,
            // the puzzle configuration is invalid.
            if (popcount(var_value) == 1) {
                if ((*seen_var_values & var_value) != 0) {
                    return false;
                }

                // Note the value of this variable as "seen".
                *seen_var_values |= var_value;
            }
        }
    }

    // If we made it here, the puzzle configuration is valid.
    return true;
}

bool is_complete(const puzzle_t *puzzle)
{
    house_t remaining_values = init_house();

    // Iterate each house to check their values.
    for (const house_t *house = puzzle->houses; house != puzzle->houses + NUM_HOUSES; ++house) {
        // Iterate each variable for this house.
        for (size_t var_i = 0; var_i != NUM_VARIABLES; ++var_i) {
            uint8_t var_value = *(&house->owner + var_i);
            uint8_t *remaining_var_values = &remaining_values.owner + var_i;

            // If the variable does not have a single bit set (meaning it still
            // has multiple possibilities or it has an invalid value),
            // then puzzle configuration is not complete.
            if (popcount(var_value) != 1) {
                return false;
            }

            // If the value of this variable was already used,
            // then puzzle configuration is invalid (and thus not complete).
            if ((*remaining_var_values & var_value) == 0) {
                return false;
            }

            // Remove this variable's value from the possibilities.
            *remaining_var_values &= ~var_value;
        }
    }

    // If we made it here, puzzle configuration is complete.
    return true;
}
