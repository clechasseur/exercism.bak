#include "allergies.h"

#include <algorithm>
#include <array>
#include <iterator>

namespace allergies {

namespace {

const std::array<std::string_view, 8> allergens{
    "eggs",
    "peanuts",
    "shellfish",
    "strawberries",
    "tomatoes",
    "chocolate",
    "pollen",
    "cats",
};

} // anonymous namespace

allergy_test::allergy_test(std::size_t score)
    : score_{score}
{
}

auto allergy_test::is_allergic_to(std::string_view allergen) const -> bool
{
    return is_allergic_to(std::find(allergens.cbegin(), allergens.cend(), allergen) - allergens.cbegin());
}

auto allergy_test::get_allergies() const -> std::unordered_set<std::string>
{
    std::unordered_set<std::string> allergies;

    for (std::size_t allergen_score = 0; allergen_score < allergens.size(); ++allergen_score) {
        if (is_allergic_to(allergen_score)) {
            allergies.emplace(allergens[allergen_score]);
        }
    }

    return allergies;
}

}  // namespace allergies
