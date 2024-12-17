#include "bank_account.h"

#include <stdexcept>

namespace Bankaccount {

namespace {

constexpr std::int_least64_t closed_sentinel = -1;

} // anonymous namespace

// Constructor. Account is initially closed.
Bankaccount::Bankaccount()
    : balance_{closed_sentinel}
{
}

auto Bankaccount::open() -> void
{
    auto expected = closed_sentinel; 
    if (!balance_.compare_exchange_strong(expected, 0, std::memory_order_relaxed)) {
        throw std::runtime_error{"Cannot open an account that is already open"};
    }
}

auto Bankaccount::close() -> void
{
    if (balance_.exchange(closed_sentinel) == closed_sentinel) {
        throw std::runtime_error{"Cannot close an account that isn't open"};
    }
}

auto Bankaccount::deposit(std::int_least64_t amount) -> void
{
    if (amount < 0) {
        throw std::runtime_error{"Cannot deposit a negative amount; use withdraw() instead"};
    }
    update(amount);
}

auto Bankaccount::withdraw(std::int_least64_t amount) -> void
{
    if (amount < 0) {
        throw std::runtime_error{"Cannot withdraw a negative amount; use deposit() instead"};
    }
    update(-amount);
}

auto Bankaccount::balance() const -> std::int_least64_t
{
    auto bal = balance_.load();
    if (bal == closed_sentinel) {
        throw std::runtime_error{"Cannot get balance of an account that isn't open"};
    }
    return bal;
}

auto Bankaccount::update(std::int_least64_t balance_diff) -> void
{
    auto bal = balance();
    auto desired = desired_amount(bal, balance_diff);
    while (!balance_.compare_exchange_weak(bal, desired, std::memory_order_relaxed)) {
        if (bal == closed_sentinel) {
            throw std::runtime_error{"Cannot update balance of account that isn't open"};
        }
        desired = desired_amount(bal, balance_diff);
    }
}

auto Bankaccount::desired_amount(std::int_least64_t balance,
                                 std::int_least64_t balance_diff) const -> std::int_least64_t
{
    auto desired = balance + balance_diff;
    if (desired < 0) {
        throw std::runtime_error{"Cannot withdraw more money that the current account balance"};
    }
    return desired;
}

} // namespace Bankaccount
