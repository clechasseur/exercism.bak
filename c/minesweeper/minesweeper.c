#include "minesweeper.h"

#include <stdlib.h>
#include <string.h>

// Computes the pointer to start at for surrounding squares.
// Will be `cur - 1` unless we're at `start`, in which case will be `start`.
#define SURROUND_START(cur, start) \
    ((cur) != (start) ? (cur) - 1 : (start))

// Computes the pointer to end at for surrounding squares.
// Will be `cur + 2` unless we're at `start + count`, in which case will be `cur + 1`.
// (It's always one past the next square, since this is the end pointer.)
#define SURROUND_END(cur, start, count) \
    (((cur) != ((start) + (count) - 1) ? (cur) + 1 : (cur)) + 1)

static char **allocate_annotation(const size_t rows, const size_t cols)
{
    // Allocate enough memory for everything in one go.
    size_t row_size = cols + 1;
    char **annotation = (char **) malloc(
        sizeof(char *) * rows + // Enough room for pointers to rows
        row_size * rows         // Enough room for each row, including terminating null
    );

    // Initialize the rows array, filling each row with empty squares.
    char *row_storage = (char *) (annotation + rows);
    for (char **row = annotation; row != (annotation + rows); ++row) {
        *row = row_storage;
        *(*row + cols) = '\0';
        memset(*row, ' ', cols);
        row_storage += row_size;
    }

    return annotation;
}

void free_annotation(char **annotation)
{
    // Since we allocated everything in a single block of memory...
    free(annotation);
}

char **annotate(const char **minefield, const size_t rows)
{
    // If we don't have any row, return null.
    if (rows == 0 || minefield == NULL) {
        return NULL;
    }

    // Compute the number of columns.
    size_t cols = strlen(*minefield);

    // Allocate memory for the annotated field.
    char **annotation = allocate_annotation(rows, cols);

    // Scan the input field to locate mines.
    char **out_row = annotation;
    for (const char **in_row = minefield; in_row != (minefield + rows); ++in_row, ++out_row) {
        char *out_square = *out_row;
        for (const char *in_square = *in_row; *in_square != '\0'; ++in_square, ++out_square) {
            if (*in_square == '*') {
                // Found a mine. First copy it to the annotated field.
                *out_square = '*';

                // Next, increment counts of all surrounding squares.
                // First compute the mine's position in the row.
                size_t mine_pos = in_square - *in_row;

                // Iterate surrounding rows.
                char **out_s_row = SURROUND_START(out_row, annotation);
                for (const char **in_s_row = SURROUND_START(in_row, minefield);
                        in_s_row != SURROUND_END(in_row, minefield, rows);
                        ++in_s_row, ++out_s_row) {

                    // Iterate surrounding squares (in the surrounding row).
                    char *out_s_square = SURROUND_START(*out_s_row + mine_pos, *out_s_row);
                    for (const char *in_s_square = SURROUND_START(*in_s_row + mine_pos, *in_s_row);
                            in_s_square != SURROUND_END(*in_s_row + mine_pos, *in_s_row, cols);
                            ++in_s_square, ++out_s_square) {

                        // Skip square if it has a mine. This also skips the square at the
                        // center of this surrounding square search since it has a mine in it already. 
                        if (*out_s_square != '*') {
                            // If this is the first time this square has a mine around it,
                            // set square to '1', otherwise increment it.
                            *out_s_square = (*out_s_square == ' ' ? '1' : *out_s_square + 1);
                        }
                    }
                }
            }
        }
    }

    // We're done, return annotated field.
    return annotation;
}
