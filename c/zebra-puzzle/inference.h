#ifndef INFERENCE_H
#define INFERENCE_H

#include "domain.h"

// Attempts to add inferences to a puzzle.
// If the puzzle configuration was allowed by the statements, returns `true` and `puzzle` is updated.
// Otherwise, returns `false` and the value of `puzzle` is undefined.
bool add_inferences(puzzle_t *puzzle);

#endif // INFERENCE_H
