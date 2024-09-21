#include "inference.h"

#include "utils.h"

#include <assert.h>
#include <stddef.h>

// Typedef for a function that applies inference to a puzzle config.
//
// The second parameter is an implementation-specific data passed to
// the function and can be `NULL`.
//
// The third parameter allows the function to call itself recursively;
// when called initially, will always be set to `false`.
typedef bool (*inference_fn_t)(puzzle_t *, const void *, bool);

// Struct storing a link between two variables.
// For example: "The Ukrainian drinks tea."
typedef struct {
    size_t  variable_offset_1;
    size_t  variable_offset_2;
    uint8_t variable_value_1;
    uint8_t variable_value_2;
} link_statement_data_t;

// Struct storing the position of a particular variable value.
// For example: "Milk is drunk in the middle house."
typedef struct {
    size_t  house_offset;
    size_t  variable_offset;
    uint8_t variable_value;
} position_statement_data_t;

// Struct storing information about two houses next to each other.
// For example: "Kools are smoked in the house next to the house where the horse is kept."
typedef struct {
    size_t  neighbour_1_variable_offset;
    size_t  neighbour_2_variable_offset;
    uint8_t neighbour_1_variable_value;
    uint8_t neighbour_2_variable_value;
} neighbouring_link_statement_data_t;

// Struct storing information about two houses next to each other, in a specific order.
// For example: "The green house is immediately to the right of the ivory house."
typedef struct {
    size_t  left_variable_offset;
    size_t  right_variable_offset;
    uint8_t left_variable_value;
    uint8_t right_variable_value;
} ordered_neighbouring_link_statement_data_t;

// Attempts to add inference for a link between two variables to a puzzle config.
// `statement_data` is assugmed to point to a `link_statement_data_t`.
static bool add_link_statement_inferences(puzzle_t *puzzle, const void *statement_data, bool reverse)
{
    const link_statement_data_t *link_data = (const link_statement_data_t *) statement_data;

    // Attempt to find a house whose variable_1 has the correct value.
    for (house_t *house = puzzle->houses; house != puzzle->houses + NUM_HOUSES; ++house) {
        if (*(&house->owner + link_data->variable_offset_1) == link_data->variable_value_1) {
            // This house has value_1 set for variable_1.

            // If the house also has another value than value_2 set for variable_2, inference fails.
            uint8_t *variable_2 = &house->owner + link_data->variable_offset_2;
            if (popcount(*variable_2) == 1 && *variable_2 != link_data->variable_value_2) {
                return false;
            }

            // Set house's variable_2 to value_2.
            *variable_2 = link_data->variable_value_2;

            // Iterate the other houses.
            for (house_t *other_house = puzzle->houses; other_house != puzzle->houses + NUM_HOUSES; ++other_house) {
                if (other_house != house) {
                    // If the other house has value_2 set for variable_2, inference fails.
                    uint8_t *other_variable_2 = &other_house->owner + link_data->variable_offset_2;
                    const bool other_variable_is_set = popcount(*other_variable_2) == 1;
                    if (other_variable_is_set && *other_variable_2 == link_data->variable_value_2) {
                        return false;
                    }

                    // Remove value_2 from possible values of other house's variable_2
                    // if its value is still undecided.
                    if (!other_variable_is_set) {
                        *other_variable_2 &= ~link_data->variable_value_2;
                    }
                }
            }
        }
    }

    // If this is the first call, do the process in reverse.
    if (!reverse) {
        const link_statement_data_t reverse_link_data = {
            .variable_offset_1  = link_data->variable_offset_2,
            .variable_offset_2  = link_data->variable_offset_1,
            .variable_value_1   = link_data->variable_value_2,
            .variable_value_2   = link_data->variable_value_1
        };

        if (!add_link_statement_inferences(puzzle, &reverse_link_data, true)) {
            return false;
        }
    }

    // If we reach this point, inference is successful.
    return true;
}

// Attempts to add inference for the position of a particular variable value.
// `statement_data` is assugmed to point to a `position_statement_data_t`.
static bool add_position_statement_inferences(puzzle_t *puzzle, const void *statement_data, bool reverse)
{
    const position_statement_data_t *position_data = (const position_statement_data_t *) statement_data;

    // Such statements actually set the variable of a house to a specific value.
    house_t *house = puzzle->houses + position_data->house_offset;
    uint8_t *variable = &house->owner + position_data->variable_offset;

    // If the variable is already set to some other value, inference fails.
    if (popcount(*variable) == 1 && *variable != position_data->variable_value) {
        return false;
    }

    // Set the variable to its only valid value.
    *variable = position_data->variable_value;

    // We don't need to perform a reverse operation.
    assert(!reverse);

    // Inference is a success.
    return true;
}

// Attempts to add inference for information about two houses next to each other.
// `statement_data` is assugmed to point to a `neighbouring_link_statement_data_t`.
static bool add_neigbourging_link_statement_inferences(puzzle_t *puzzle, const void *statement_data, bool reverse)
{
    const neighbouring_link_statement_data_t *link_data = (const neighbouring_link_statement_data_t *) statement_data;

    // Attempt to find a house whose variable_1 has the correct value.
    for (house_t *house = puzzle->houses; house != puzzle->houses + NUM_HOUSES; ++house) {
        if (*(&house->owner + link_data->neighbour_1_variable_offset) == link_data->neighbour_1_variable_value) {
            // This house has value_1 set for variable_1.

            // Check which other houses are neighbours of this house.
            house_t *first_neighbour = (house != puzzle->houses) ? house - 1 : house + 1;
            house_t *second_neighbour = (house != (puzzle->houses + NUM_HOUSES - 1)) ? house + 1 : NULL;

            if (second_neighbour == NULL) {
                // House has only one neighbour.
                // If that neighbour has a different value than value_2 for variable_2, inference fails.
                uint8_t *variable_2 = &first_neighbour->owner + link_data->neighbour_2_variable_offset;
                const bool variable_2_is_set = popcount(*variable_2) == 1;
                if (variable_2_is_set && *variable_2 != link_data->neighbour_2_variable_value) {
                    return false;
                }

                // Set the neighbour's variable_2 to value_2.
                *variable_2 = link_data->neighbour_2_variable_value;
            } else {
                // House has two neighbours.
                uint8_t *left_variable_2 = &first_neighbour->owner + link_data->neighbour_2_variable_offset;
                uint8_t *right_variable_2 = &second_neighbour->owner + link_data->neighbour_2_variable_offset;
                const bool left_variable_2_is_set = popcount(*left_variable_2) == 1;
                const bool right_variable_2_is_set = popcount(*right_variable_2) == 1;

                if (left_variable_2_is_set) {
                    if (*left_variable_2 == link_data->neighbour_2_variable_value) {
                        // Left neighbour has value_2 set for variable_2 already,
                        // so the right neighbour can't have it.
                        if (!right_variable_2_is_set) {
                            *right_variable_2 &= ~link_data->neighbour_2_variable_value;
                        }
                    } else {
                        // Left neighbour has a different value than value_2 for variable_2,
                        // so the right neighbour must have value_2.
                        if (!right_variable_2_is_set) {
                            *right_variable_2 = link_data->neighbour_2_variable_value;
                        } else if (*right_variable_2 != link_data->neighbour_2_variable_value) {
                            // Right neighbour has a different value also; inference fails.
                            return false;
                        }
                    }
                } else if (right_variable_2_is_set) {
                    if (*right_variable_2 == link_data->neighbour_2_variable_value) {
                        // Right neighbour has value_2 for variable_2 already,
                        // so the left neighbour can't have it.
                        *left_variable_2 &= ~link_data->neighbour_2_variable_value;
                    } else {
                        // Right neighbour has a different value than value_2 for variable_2,
                        // so the left neighbour must have value_2.
                        *left_variable_2 = link_data->neighbour_2_variable_value;
                    }
                }
            }

            // Iterate the other houses.
            for (house_t *other_house = puzzle->houses; other_house != puzzle->houses + NUM_HOUSES; ++other_house) {
                if (other_house != house && other_house != first_neighbour && other_house != second_neighbour) {
                    // If the other house has value_2 set for variable_2, inference fails.
                    uint8_t *other_variable_2 = &other_house->owner + link_data->neighbour_2_variable_offset;
                    const bool other_variable_is_set = popcount(*other_variable_2) == 1;
                    if (other_variable_is_set && *other_variable_2 == link_data->neighbour_2_variable_value) {
                        return false;
                    }

                    // Remove value_2 from possible values of other house's variable_2
                    // if its value is still undecided.
                    if (!other_variable_is_set) {
                        *other_variable_2 &= ~link_data->neighbour_2_variable_value;
                    }
                }
            }
        }
    }

    // If this is the first call, do the process in reverse.
    if (!reverse) {
        const neighbouring_link_statement_data_t reverse_link_data = {
            .neighbour_1_variable_offset    = link_data->neighbour_2_variable_offset,
            .neighbour_2_variable_offset    = link_data->neighbour_1_variable_offset,
            .neighbour_1_variable_value     = link_data->neighbour_2_variable_value,
            .neighbour_2_variable_value     = link_data->neighbour_1_variable_value
        };

        if (!add_neigbourging_link_statement_inferences(puzzle, &reverse_link_data, true)) {
            return false;
        }
    }

    // If we reach this point, inference is successful.
    return true;
}

// Attempts to add inference for information about two houses next to each other, in a specific order.
// `statement_data` is assugmed to point to a `ordered_neighbouring_link_statement_data_t`.
static bool add_ordered_neigbourging_link_statement_inferences(puzzle_t *puzzle, const void *statement_data, bool reverse)
{
    const ordered_neighbouring_link_statement_data_t *link_data = (const ordered_neighbouring_link_statement_data_t *) statement_data;

    // Determine what variable we're looking for this time around.
    size_t first_variable_offset = link_data->left_variable_offset;
    size_t second_variable_offset = link_data->right_variable_offset;
    uint8_t first_variable_value = link_data->left_variable_value;
    uint8_t second_variable_value = link_data->right_variable_value;
    ptrdiff_t other_house_offset = 1;
    if (reverse) {
        first_variable_offset = link_data->right_variable_offset;
        second_variable_offset = link_data->left_variable_offset;
        first_variable_value = link_data->right_variable_value;
        second_variable_value = link_data->left_variable_value;
        other_house_offset = -1;
    }

    // Attempt to find a house whose first_variable has the correct value.
    for (house_t *first_house = puzzle->houses; first_house != puzzle->houses + NUM_HOUSES; ++first_house) {
        if (*(&first_house->owner + first_variable_offset) == first_variable_value) {
            // This house has first_value set for first_variable.
            // The other house (directly to its left or right) should have second_value set for second_variable.
            house_t *second_house = first_house + other_house_offset;

            // If there's no second house, inference fails
            if (second_house < puzzle->houses || second_house == puzzle->houses + NUM_HOUSES) {
                return false;
            }

            // If the second house has a value different from second_value for second_variable, inference fails.
            uint8_t *second_variable = &second_house->owner + second_variable_offset;
            const bool second_variable_is_set = popcount(*second_variable) == 1;
            if (second_variable_is_set && *second_variable != second_variable_value) {
                return false;
            }

            // Set the second house's second_variable to second_value.
            *second_variable = second_variable_value;

            // Iterate the other houses.
            for (house_t *other_house = puzzle->houses; other_house != puzzle->houses + NUM_HOUSES; ++other_house) {
                if (other_house != second_house) {
                    // If the other house has second_value set for second_variable, inference fails.
                    uint8_t *other_second_variable = &other_house->owner + second_variable_offset;
                    const bool other_variable_is_set = popcount(*other_second_variable) == 1;
                    if (other_variable_is_set && *other_second_variable == second_variable_value) {
                        return false;
                    }

                    // Remove second_value from possible values of other house's second_variable
                    // if its value is still undecided.
                    if (!other_variable_is_set) {
                        *other_second_variable &= ~second_variable_value;
                    }
                }
            }
        }
    }

    // If this is the first call, do the process in reverse.
    if (!reverse && !add_ordered_neigbourging_link_statement_inferences(puzzle, link_data, true)) {
        return false;
    }

    // If we reach this point, inference is successful.
    return true;
}

// initialize the data for each puzzle statement.
static const link_statement_data_t statement_2_data = {
    // "The Englishman lives in the red house."
    .variable_offset_1  = offsetof(house_t, owner) / sizeof(uint8_t),
    .variable_offset_2  = offsetof(house_t, color) / sizeof(uint8_t),
    .variable_value_1   = ENGLISHMAN,
    .variable_value_2   = RED
};
static const link_statement_data_t statement_3_data = {
    // "The Spaniard owns the dog."
    .variable_offset_1  = offsetof(house_t, owner) / sizeof(uint8_t),
    .variable_offset_2  = offsetof(house_t, pet) / sizeof(uint8_t),
    .variable_value_1   = SPANIARD,
    .variable_value_2   = DOG
};
static const link_statement_data_t statement_4_data = {
    // "Coffee is drunk in the green house."
    .variable_offset_1  = offsetof(house_t, beverage) / sizeof(uint8_t),
    .variable_offset_2  = offsetof(house_t, color) / sizeof(uint8_t),
    .variable_value_1   = COFFEE,
    .variable_value_2   = GREEN
};
static const link_statement_data_t statement_5_data = {
    // "The Ukrainian drinks tea."
    .variable_offset_1  = offsetof(house_t, owner) / sizeof(uint8_t),
    .variable_offset_2  = offsetof(house_t, beverage) / sizeof(uint8_t),
    .variable_value_1   = UKRAINIAN,
    .variable_value_2   = TEA
};
static const ordered_neighbouring_link_statement_data_t statement_6_data = {
    // "The green house is immediately to the right of the ivory house."
    .left_variable_offset   = offsetof(house_t, color) / sizeof(uint8_t),
    .right_variable_offset  = offsetof(house_t, color) / sizeof(uint8_t),
    .left_variable_value    = IVORY,
    .right_variable_value   = GREEN
};
static const link_statement_data_t statement_7_data = {
    // "The Old Gold smoker owns snails."
    .variable_offset_1  = offsetof(house_t, cigarette_brand) / sizeof(uint8_t),
    .variable_offset_2  = offsetof(house_t, pet) / sizeof(uint8_t),
    .variable_value_1   = OLD_GOLD,
    .variable_value_2   = SNAILS
};
static const link_statement_data_t statement_8_data = {
    // "Kools are smoked in the yellow house."
    .variable_offset_1  = offsetof(house_t, cigarette_brand) / sizeof(uint8_t),
    .variable_offset_2  = offsetof(house_t, color) / sizeof(uint8_t),
    .variable_value_1   = KOOLS,
    .variable_value_2   = YELLOW
};
static const position_statement_data_t statement_9_data = {
    // "Milk is drunk in the middle house."
    .house_offset       = 2,
    .variable_offset    = offsetof(house_t, beverage) / sizeof(uint8_t),
    .variable_value     = MILK
};
static const position_statement_data_t statement_10_data = {
    .house_offset       = 0,
    .variable_offset    = offsetof(house_t, owner) / sizeof(uint8_t),
    .variable_value     = NORWEGIAN
};
static const neighbouring_link_statement_data_t statement_11_data = {
    // "The man who smokes Chesterfields lives in the house next to the man with the fox."
    .neighbour_1_variable_offset    = offsetof(house_t, cigarette_brand) / sizeof(uint8_t),
    .neighbour_2_variable_offset    = offsetof(house_t, pet) / sizeof(uint8_t),
    .neighbour_1_variable_value     = CHESTERFIELDS,
    .neighbour_2_variable_value     = FOX
};
static const neighbouring_link_statement_data_t statement_12_data = {
    // "Kools are smoked in the house next to the house where the horse is kept."
    .neighbour_1_variable_offset    = offsetof(house_t, cigarette_brand) / sizeof(uint8_t),
    .neighbour_2_variable_offset    = offsetof(house_t, pet) / sizeof(uint8_t),
    .neighbour_1_variable_value     = KOOLS,
    .neighbour_2_variable_value     = HORSE
};
static const link_statement_data_t statement_13_data = {
    // "The Lucky Strike smoker drinks orange juice."
    .variable_offset_1  = offsetof(house_t, cigarette_brand) / sizeof(uint8_t),
    .variable_offset_2  = offsetof(house_t, beverage) / sizeof(uint8_t),
    .variable_value_1   = LUCKY_STRIKE,
    .variable_value_2   = ORANGE_JUICE
};
static const link_statement_data_t statement_14_data = {
    // "The Japanese smokes Parliaments."
    .variable_offset_1  = offsetof(house_t, owner) / sizeof(uint8_t),
    .variable_offset_2  = offsetof(house_t, cigarette_brand) / sizeof(uint8_t),
    .variable_value_1   = JAPANESE,
    .variable_value_2   = PARLIAMENTS
};
static const neighbouring_link_statement_data_t statement_15_data = {
    // "The Norwegian lives next to the blue house."
    .neighbour_1_variable_offset    = offsetof(house_t, owner) / sizeof(uint8_t),
    .neighbour_2_variable_offset    = offsetof(house_t, color) / sizeof(uint8_t),
    .neighbour_1_variable_value     = NORWEGIAN,
    .neighbour_2_variable_value     = BLUE
};

// Stores information about one statement, with its inference function and data.
typedef struct {
    inference_fn_t  inference_fn;
    const void      *statement_data;
} statement_t;

// Initialize the list of statements.
#define STATEMENT(statement_fn, statement_num) \
    { \
        .inference_fn   = statement_fn, \
        .statement_data = &statement_ ## statement_num ## _data \
    }
#define LINK_STATEMENT(statement_num) \
    STATEMENT(add_link_statement_inferences, statement_num)
#define POSITION_STATEMENT(statement_num) \
    STATEMENT(add_position_statement_inferences, statement_num)
#define NEIGHBOURING_LINK_STATEMENT(statement_num) \
    STATEMENT(add_neigbourging_link_statement_inferences, statement_num)
#define ORDERED_NEIGHBOURING_LINK_STATEMENT(statement_num) \
    STATEMENT(add_ordered_neigbourging_link_statement_inferences, statement_num)

static const statement_t statements[] = {
    LINK_STATEMENT(2),
    LINK_STATEMENT(3),
    LINK_STATEMENT(4),
    LINK_STATEMENT(5),
    ORDERED_NEIGHBOURING_LINK_STATEMENT(6),
    LINK_STATEMENT(7),
    LINK_STATEMENT(8),
    POSITION_STATEMENT(9),
    POSITION_STATEMENT(10),
    NEIGHBOURING_LINK_STATEMENT(11),
    NEIGHBOURING_LINK_STATEMENT(12),
    LINK_STATEMENT(13),
    LINK_STATEMENT(14),
    NEIGHBOURING_LINK_STATEMENT(15)
};
static const size_t num_statements = sizeof(statements) / sizeof(statement_t);

bool add_inferences(puzzle_t *puzzle)
{
    // Add inference for each statement, bailing out if one fails.
    for (const statement_t *statement = statements; statement != statements + num_statements; ++statement) {
        if (!statement->inference_fn(puzzle, statement->statement_data, false)) {
            return false;
        }
    }

    // If we reach this point, inference is successful.
    return true;
}
