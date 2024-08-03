#include "parallel_letter_frequency.h"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <execution>
#include <iterator>
#include <thread>
#include <utility>

namespace parallel_letter_frequency {

// A wrapper for a vector of `std::string_view`s that presents a unified view as if one long string.
class strings_view
{
    const string_views& strings_;

public:
    class const_iterator;

    explicit strings_view(const string_views& strings) : strings_(strings) {}
    strings_view(const strings_view&) = delete;
    strings_view(strings_view&&) = delete;
    strings_view& operator=(const strings_view&) = delete;
    strings_view& operator=(strings_view&&) = delete;

    std::size_t size() const {
        return std::accumulate(strings_.cbegin(), strings_.cend(), std::size_t{0},
                               [](auto siz, auto&& s) { return siz + s.size(); });
    }
    bool empty() const {
        return size() == 0;
    }

    auto begin() const -> const_iterator {
        return {strings_, false};
    }
    auto cbegin() const -> const_iterator {
        return begin();
    }

    auto end() const -> const_iterator {
        return {strings_, true};
    }
    auto cend() const -> const_iterator {
        return end();
    }

    // Note: this iterator implementation presents itself as a random-access iterator,
    // but technically there are some missing pieces for it to be fully-compliant,
    // notably support for going backwards (e.g. `operator--` and such). Because these
    // are much harder to implement and weren't needed for the exercise, I skipped them.
    class const_iterator
    {
        string_views::const_iterator strs_it_{}, strs_end_{};
        std::string_view::const_iterator it_{}, end_{};

        void align() {
            while (strs_it_ != strs_end_ && it_ == end_) {
                it_ = strs_it_->cbegin();
                end_ = strs_it_->cend();
                ++strs_it_;
            }
        }

    public:
        using iterator_category = std::random_access_iterator_tag;
        using value_type = std::string_view::value_type;
        using difference_type = std::ptrdiff_t;
        using pointer = std::string_view::const_pointer;
        using reference = std::string_view::const_reference;
    
        const_iterator() = default;
        const_iterator(const string_views& strings, bool is_end)
            : strs_end_{strings.cend()}
        {
            if (!is_end) {
                strs_it_ = strings.cbegin();
                align();
            } else {
                strs_it_ = strs_end_;
                if (strs_it_ != strings.cbegin()) {
                    it_ = end_ = (strs_it_ - 1)->cend();
                }
            }
        }

        auto operator*() const -> reference { return *it_; }
        auto operator->() const -> pointer { return &**this; }

        friend bool operator==(const const_iterator& lhs, const const_iterator& rhs) {
            return lhs.strs_it_ == rhs.strs_it_ && lhs.it_ == rhs.it_;
        }
        friend bool operator!=(const const_iterator& lhs, const const_iterator& rhs) {
            return !(lhs == rhs);
        }
        friend bool operator<(const const_iterator& lhs, const const_iterator& rhs) {
            return lhs.strs_it_ < rhs.strs_it_ ||
                (lhs.strs_it_ == rhs.strs_it_ && lhs.it_ < rhs.it_);
        }
        friend bool operator<=(const const_iterator& lhs, const const_iterator& rhs) {
            return !(rhs < lhs);
        }
        friend bool operator>(const const_iterator& lhs, const const_iterator& rhs) {
            return rhs < lhs;
        }
        friend bool operator>=(const const_iterator& lhs, const const_iterator& rhs) {
            return !(lhs < rhs);
        }

        auto operator++() -> const_iterator& {
            ++it_;
            align();
            return *this;
        }
        auto operator++(int) -> const_iterator {
            auto it{*this};
            ++*this;
            return it;
        }

        auto operator+=(difference_type n) -> const_iterator& {
            while (n > 0) {
                auto max_move = std::min(n, std::distance(it_, end_));
                it_ += max_move;
                align();
                n -= max_move;
            }
            return *this;
        }
        friend auto operator+(const const_iterator& lhs, difference_type rhs) -> const_iterator {
            auto it{lhs};
            it += rhs;
            return it;
        }
        friend auto operator+(difference_type lhs, const const_iterator& rhs) -> const_iterator {
            return rhs + lhs;
        }

        friend difference_type operator-(const const_iterator& lhs, const const_iterator& rhs) {
            if (lhs >= rhs) {
                auto it{rhs};
                difference_type diff = 0;
                while (it.strs_it_ != lhs.strs_it_) {
                    diff += std::distance(it.it_, it.end_);
                    it.it_ = it.end_;
                    it.align();
                }
                return diff + std::distance(it.it_, lhs.it_);
            } else {
                return -(rhs - lhs);
            }
        }
    };
};

auto frequency(const string_views& texts) -> frequencies {
    // Determine level of concurrency we can achieve on this platform.
    auto concurrency = static_cast<std::ptrdiff_t>(std::thread::hardware_concurrency());
    if (concurrency == 0) {
        // Sometimes we can't determine hardware concurrency.
        // When that happens, do our best.
        concurrency = 4;
    }

    // Create a unified view of all the text, then split it in a
    // number of ranges equal to the possible concurrency.
    strings_view view{texts};
    const auto range_size = static_cast<std::ptrdiff_t>(
        std::ceil(static_cast<double>(view.size()) / concurrency)
    );
    std::vector<std::pair<strings_view::const_iterator, strings_view::const_iterator>> ranges;
    ranges.reserve(concurrency);
    const auto end = view.cend();
    for (auto it = view.cbegin(); it != end; ) {
        const auto range_beg = it;
        it += std::min(std::distance(it, end), range_size);
        ranges.emplace_back(range_beg, it);
    }

    // Use `std::transform` to process each range of characters, computing the frequencies
    // of letters in the range into a map. Ask for "parallel unsequenced policy", which asks
    // the runtime to parallelize the computing of ranges, possibly in an unsequenced fashion.
    // (Note: since this will possibly execute in parallel, we need to pre-allocate a vector
    // to store the resulting maps, otherwise if we allocate as we finish ranges, we'll hit
    // problems because of concurrent vector reallocations.)
    std::vector<frequencies> freqs;
    freqs.resize(ranges.size());
    std::transform(std::execution::par_unseq, ranges.cbegin(), ranges.cend(), freqs.begin(),
                   [](auto&& range) {
                       frequencies freqs;
                       for (auto it = range.first; it != range.second; ++it) {
                           if (std::isalpha(static_cast<unsigned char>(*it))) {
                               ++freqs[static_cast<char>(std::tolower(static_cast<unsigned char>(*it)))];
                           }
                       }
                       return freqs;
                   });

    // Now that we've computed the frequencies for each range, merge the maps into one.
    return std::accumulate(freqs.cbegin(), freqs.cend(), frequencies{},
                           [](auto&& freqs, auto&& partial_freqs) {
                               for (auto [c, count] : partial_freqs) {
                                   freqs[c] += count;
                               }
                               return freqs;
                           });
}

}
