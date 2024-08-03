#include "high_scores.h"

#include <string.h>

int32_t latest(const int32_t *scores, size_t scores_len)
{
    return scores[scores_len - 1];
}

int32_t personal_best(const int32_t *scores, size_t scores_len)
{
    int32_t best = 0;
    for (const int32_t *end = scores + scores_len; scores < end; ++scores) {
        if (*scores > best) {
            best = *scores;
        }
    }

    return best;
}

size_t personal_top_three(const int32_t *scores, size_t scores_len, int32_t *output)
{
    // This solution is a bit complex, but saves on additional memory allocation.
    // A simpler solution would be to duplicate the input array and sort it
    // using qsort, then copy the first 3 (or less) scores to the output.
    // The simpler solution might even be faster, we'd have to bench...
    
    size_t output_len = scores_len < 3 ? scores_len : 3;
    int32_t *last_output = output + (output_len - 1);
    memset(output, 0, output_len * sizeof(int32_t));

    for (const int32_t *scores_end = scores + scores_len; scores < scores_end; ++scores) {
        if (*scores > *last_output) {
            // Again, here, the technique used is to find the insertion point,
            // move down any values as needed, then insert. But another technique
            // would be to always insert at the last_output point, then swap
            // values to move the new score up until it reaches the proper
            // position. Not sure which is faster TBH, we'd again have to bench.
            
            int32_t *insert_pos = last_output;
            while (insert_pos != output && *scores > *(insert_pos - 1)) {
                --insert_pos;
            }

            memmove(insert_pos + 1, insert_pos, (last_output - insert_pos) * sizeof(int32_t));
            *insert_pos = *scores;
        }
    }

    return output_len;
}
