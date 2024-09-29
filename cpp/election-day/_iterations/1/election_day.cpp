#include <algorithm>
#include <string>
#include <vector>

namespace election {

// The election result struct is already created for you:
struct ElectionResult {
    // Name of the candidate
    std::string name{};
    // Number of votes the candidate has
    int votes{};
};

// vote_count takes a reference to an `ElectionResult` as an argument and will
// return the number of votes in the `ElectionResult.
auto vote_count(const ElectionResult& result) -> int
{
    return result.votes;
}

// increment_vote_count takes a reference to an `ElectionResult` as an argument
// and a number of votes (int), and will increment the `ElectionResult` by that
// number of votes.
auto increment_vote_count(ElectionResult& result, int votes) -> void
{
    result.votes += votes;
}

// determine_result receives the reference to a final_count and returns a
// reference to the `ElectionResult` of the new president. It also changes the
// name of the winner by prefixing it with "President". The final count is given
// in the form of a `reference` to `std::vector<ElectionResult>`, a vector with
// `ElectionResults` of all the participating candidates.
auto determine_result(std::vector<ElectionResult>& results) -> ElectionResult&
{
    auto votes_comp = [](auto&& result1, auto&& result2) {
        return result1.votes < result2.votes;
    };
    auto winner = std::max_element(results.begin(), results.end(), votes_comp);

    winner->name = "President " + winner->name;
    return *winner;
}

}  // namespace election