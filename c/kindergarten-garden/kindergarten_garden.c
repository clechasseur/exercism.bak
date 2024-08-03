#include "kindergarten_garden.h"

#include <stddef.h>
#include <string.h>

static ptrdiff_t student_offset(const char *student)
{
    // Conveniently, the school only accepts students if
    // their name are in perfect alphabetical order.
    return *student - 'A';
}

static plant_t to_plant(char c)
{
    switch (c) {
        case 'C':
            return CLOVER;
        case 'G':
            return GRASS;
        case 'R':
            return RADISHES;
        case 'V':
            return VIOLETS;
    }

    return INVALID_PLANT;
}

static void row_to_plants(const char *row, plant_t *plants)
{
    plants[0] = to_plant(row[0]);
    plants[1] = to_plant(row[1]);
}

plants_t plants(const char *diagram, const char *student)
{
    ptrdiff_t plants_offset = student_offset(student) * 2;
    const char *first_row = diagram + plants_offset;
    const char *second_row = strchr(diagram, '\n') + 1 + plants_offset;

    plants_t result;
    row_to_plants(first_row, result.plants);
    row_to_plants(second_row, result.plants + 2);
    
    return result;
}
