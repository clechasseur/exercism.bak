#include "solver.h"

#include "inference.h"
#include "utils.h"

#include <stddef.h>

// The algorithm used to solve the puzzle has been inspired by this post:
// https://www.baeldung.com/cs/csp

// Selects the next unassigned variable in the puzzle and returns a pointer to it.
static uint8_t *select_unassigned_variable(puzzle_t *puzzle)
{
    // We'll use the Minimum-Remaining-Values (MRV) heuristic to select the next variable:
    // we prioritize variables with the least remaining possible values.
    uint8_t *variable = NULL;
    uint8_t num_values = UINT8_MAX;

    // Iterate houses to find an unassigned variable.
    for (house_t *house = puzzle->houses; house != puzzle->houses + NUM_HOUSES; ++house) {
        // Iterate variables for this house.
        for (size_t var_i = 0; var_i != NUM_VARIABLES; ++var_i) {
            // Only select variables that still have more than one possible value.
            const uint8_t var_num_values = popcount(*(&house->owner + var_i));
            if (var_num_values > 1 && var_num_values < num_values) {
                // This variable has fewer remaining values than the one we had, use it.
                variable = &house->owner + var_i;
                num_values = var_num_values;
            }
        }
    }

    return variable;
}

// Attempts to solve the puzzle via backtracking.
//
// If we could solve the puzzle, returns `true` and `puzzle` contains the solution.
// Otherwise, returns `false` and `puzzle` is unmodified.
static bool backtracking_solve(puzzle_t *puzzle)
{
    // If this puzzle configuration is complete, return it.
    if (is_complete(puzzle)) {
        return true;
    }

    // Select a variable which has not been assigned yet.
    uint8_t *variable = select_unassigned_variable(puzzle);

    // If we ran out of variables, it means the search is a dead end
    // in this branch. Return failure.
    if (variable == NULL) {
        return false;
    }

    // Copy list of possible values for that variable.
    const uint8_t variable_bak = *variable;
    uint8_t possible_values = *variable;

    // Go over possible values and try to solve.
    while (possible_values != 0) {
        // Get next possible value and save it to the variable.
        const uint8_t value = pop_bit(&possible_values);
        *variable = value;

        // Make sure the puzzle configuration is valid with this value.
        if (is_valid(puzzle)) {
            // Attempt to add inferences to the puzzle config.
            // If it works, try solving the puzzle with this variable value.
            const puzzle_t puzzle_bak = *puzzle;
            if (add_inferences(puzzle) && backtracking_solve(puzzle)) {
                return true;
            }

            // Puzzle wasn't solved; remove inferences from the config.
            *puzzle = puzzle_bak;
        }

        // Puzzle wasn't solved with this value; we need to try the next one.
    }

    // If we reach this point, it means we ran out of possible values
    // for the variable we selected. Thus, the search is a dead end in this branch.
    // Restore the variable to its original value and return failure.
    *variable = variable_bak;
    return false;
}

puzzle_t solve(void)
{
    puzzle_t puzzle = init_puzzle();
    backtracking_solve(&puzzle);
    return puzzle;
}
