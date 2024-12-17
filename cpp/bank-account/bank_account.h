#if !defined(BANK_ACCOUNT_H)
#define BANK_ACCOUNT_H

#include <atomic>
#include <cstdint>

namespace Bankaccount {

class Bankaccount
{
public:
    Bankaccount();

    // Disable copy/move support
    Bankaccount(const Bankaccount&) = delete;
    Bankaccount(Bankaccount&&) = delete;
    Bankaccount& operator=(const Bankaccount&) = delete;
    Bankaccount& operator=(Bankaccount&&) = delete;

    auto open() -> void;
    auto close() -> void;

    auto deposit(std::int_least64_t amount) -> void;
    auto withdraw(std::int_least64_t amount) -> void;
    auto balance() const -> std::int_least64_t;

private:
    std::atomic_int_least64_t balance_;

    auto update(std::int_least64_t balance_diff) -> void;
    auto desired_amount(std::int_least64_t balance,
                        std::int_least64_t balance_diff) const -> std::int_least64_t;
};

}  // namespace Bankaccount

#endif  // BANK_ACCOUNT_H