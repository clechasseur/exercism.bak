#if !defined(ALLERGIES_H)
#define ALLERGIES_H

#include <cstddef>
#include <string>
#include <string_view>
#include <unordered_set>

namespace allergies {

class allergy_test final
{
public:
    explicit allergy_test(std::size_t score);

    auto is_allergic_to(std::string_view allergen) const -> bool;
    auto get_allergies() const -> std::unordered_set<std::string>;

private:
    std::size_t score_;

    [[nodiscard]] auto is_allergic_to(std::size_t allergen_score) const -> bool {
        return (score_ & (1 << allergen_score)) != 0;
    }
};

}  // namespace allergies

#endif // ALLERGIES_H