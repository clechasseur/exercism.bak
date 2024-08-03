#include "anagram.h"

#include <ctype.h>
#include <stdbool.h>

// A list of 32 prime numbers. We will map each alphabetic character to one of those
// by computing `letter % 32`. Because 'A' == 65 and 'a' == 97, this means that lowercase
// and uppercase letters will get the same primes.
// Idea stolen from https://stackoverflow.com/a/36310820
#define NUM_PRIMES 32
const unsigned long long PRIMES[NUM_PRIMES] = {
    2, 3, 5, 7, 11, 13, 17, 19,
    23, 29, 31, 37, 41, 43, 47, 53,
    59, 61, 67, 71, 73, 79, 83, 89,
    97, 101, 103, 107, 109, 113, 127, 131
};

// A large enough prime number to use as modulus when computing hashes.
// Also stolen from https://stackoverflow.com/a/36310820
const unsigned long long PRIME_MODULUS = (1ull << 58) - 27;

// Struct used to return a word's hash from the `compute_word_hash` function.
typedef struct {
    unsigned long long hash;
    bool is_subject;
} hash_result;

// A "safe" version of toupper that casts the character to unsigned char
// since if we don't do that, we get undefined behavior.
// See https://en.cppreference.com/w/c/string/byte/toupper
#define toupper_s(c) toupper((unsigned char) (c))

// Computes the hash of a word. The hash will be the same regardless of the
// characters' positions, so it can be used to identify anagrams.
// If subject is provided, also checks if the word is the same as the subject,
// doing a case-insensitive check.
static hash_result compute_word_hash(const char *word, const char *subject)
{
    hash_result result = { .hash = 1, .is_subject = true };
    
    for (; *word != '\0'; ++word) {
        // To compute the hash, we map each character to a prime number
        // and multiply those together. Because of the way we map characters
        // to primes, we don't need to differentiate upper- and lowercase
        // characters (see note above).
        result.hash = (result.hash * PRIMES[*word % NUM_PRIMES]) % PRIME_MODULUS;

        if (subject != NULL) {
            if (*subject != '\0') {
                result.is_subject = result.is_subject && toupper_s(*word) == toupper_s(*subject++);
            } else {
                result.is_subject = false;
            }
        }
    }

    result.is_subject = result.is_subject && subject != NULL && *subject == '\0';

    return result;
}

void find_anagrams(const char *subject, struct candidates *candidates)
{
    // First compute the hash of the subject.
    unsigned long long subject_hash = compute_word_hash(subject, NULL).hash;

    // Scan candidates and mark anagrams that are different than subject.
    for (size_t i = 0; i < candidates->count; ++i) {
        struct candidate *candidate = candidates->candidate + i;
        if (candidate->is_anagram == UNCHECKED) {
            hash_result candidate_hash = compute_word_hash(candidate->word, subject);
            candidate->is_anagram = (candidate_hash.hash == subject_hash && !candidate_hash.is_subject) ? IS_ANAGRAM : NOT_ANAGRAM;
        }
    }
}
