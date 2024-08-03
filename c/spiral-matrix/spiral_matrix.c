#include "spiral_matrix.h"

#include <stdbool.h>
#include <stdlib.h>

typedef struct {
    int x;
    int y;
} point;

static bool in_bounds(point pt, int matrix_size)
{
    return pt.x >= 0 && pt.x < matrix_size && pt.y >= 0 && pt.y < matrix_size;
}

static point turn_right(point displacement)
{
    const point new_displacement = { -displacement.y, displacement.x };
    return new_displacement;
}

static point move(point pt, point displacement)
{
    const point new_pt = { pt.x + displacement.x, pt.y + displacement.y };
    return new_pt;
}

static int *square_at(spiral_matrix_t *spiral, point pt)
{
    return spiral->matrix[pt.y] + pt.x;
}

spiral_matrix_t *spiral_matrix_create(int size)
{
    // Allocate memory
    spiral_matrix_t *spiral = (spiral_matrix_t *) calloc(
        sizeof(spiral_matrix_t) +  // Space for the struct
        size * sizeof(int *) +     // Space for the array of rows
        size * size * sizeof(int), // Space for the actual matrix
        sizeof(char)
    );
    if (spiral == NULL || size == 0) {
        return spiral;
    }

    // Initialize array of row pointers
    spiral->size = size;
    spiral->matrix = (int **) (spiral + 1);
    for (size_t y = 0; y < (size_t) size; ++y) {
        spiral->matrix[y] = ((int *) (spiral->matrix + size)) + (y * size);
    }

    // Merry go round
    int next = 1;
    point pt = { 0, 0 };
    point displacement = { 1, 0 };
    while (in_bounds(pt, size) && *square_at(spiral, pt) == 0) {
        *square_at(spiral, pt) = next++;
        point next_pt = move(pt, displacement);
        if (!in_bounds(next_pt, size) || *square_at(spiral, next_pt) != 0) {
            displacement = turn_right(displacement);
            next_pt = move(pt, displacement);
        }
        pt = next_pt;
    }

    return spiral;
}

void spiral_matrix_destroy(spiral_matrix_t *spiral)
{
    free(spiral);
}
