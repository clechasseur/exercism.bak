#include "allergies.h"

bool is_allergic_to(allergen_t allergen, int score)
{
    return (score & (1 << allergen)) != 0;
}

allergen_list_t get_allergens(int score)
{
    allergen_list_t allergens = { 0 };
    for (allergen_t allergen = ALLERGEN_EGGS; allergen != ALLERGEN_COUNT; ++allergen) {
        if (is_allergic_to(allergen, score)) {
            ++allergens.count;
            allergens.allergens[allergen] = true;
        }
    }

    return allergens;
}
