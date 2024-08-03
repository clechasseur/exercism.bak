#ifndef DOMAIN_H
#define DOMAIN_H

#include <stdbool.h>
#include <stdint.h>

// Number of houses in the puzzle.
#define NUM_HOUSES      5

// Number of variables for each house.
#define NUM_VARIABLES   5

// Possible values of the variables in the puzzle.
// They are laid out as bit values so that we can represent
// possible values as a bitfield.
enum {
    // Sentinel value representing an invalid configuration
    INVALID_VALUE = 0,

    // Nationalities of house occupants
    ENGLISHMAN = 0x01,
    SPANIARD   = 0x02,
    UKRAINIAN  = 0x04,
    NORWEGIAN  = 0x08,
    JAPANESE   = 0x10,

    // House colors
    RED    = 0x01,
    GREEN  = 0x02,
    IVORY  = 0x04,
    YELLOW = 0x08,
    BLUE   = 0x10,

    // Beverages
    COFFEE       = 0x01,
    TEA          = 0x02,
    MILK         = 0x04,
    ORANGE_JUICE = 0x08,
    WATER        = 0x10,

    // Pets
    DOG    = 0x01,
    SNAILS = 0x02,
    FOX    = 0x04,
    HORSE  = 0x08,
    ZEBRA  = 0x10,

    // Cigarette brands
    OLD_GOLD      = 0x01,
    KOOLS         = 0x02,
    CHESTERFIELDS = 0x04,
    LUCKY_STRIKE  = 0x08,
    PARLIAMENTS   = 0x10,

    // Sentinel value representing all values combined.
    // Used to initialize a full bitfield of possible values.
    ALL_POSSIBILITIES = 0x1F
};

// Storage for the configuration of one house in the puzzle.
// Possible values for each variable are stored as bitfields.
// When only one bit remains, it's the value associated with the variable.
typedef struct {
    uint8_t owner;
    uint8_t color;
    uint8_t beverage;
    uint8_t pet;
    uint8_t cigarette_brand;
} house_t;

// Storage for a puzzle configuration. Contains one `house_t` per house.
typedef struct {
    house_t houses[NUM_HOUSES];
} puzzle_t;

// Initializes a blank puzzle configuration.
// Initially, each variable of each house accepts all possible values.
puzzle_t init_puzzle(void);

// Checks if the given puzzle configuration is valid.
// The configuration is valid if no two houses have the same value for a given variable
// and if no variable is set to an `INVALID_VALUE`.
bool is_valid(const puzzle_t *puzzle);

// Checks if the given puzzle configuration is complete.
// The configuration is complete if all variables have different values for each house.
bool is_complete(const puzzle_t *puzzle);

#endif // DOMAIN_H
