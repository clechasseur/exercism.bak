#include "darts.h"

#include <math.h>

#define POW2(x) ((x) * (x))

#define INNER_CIRCLE_RADIUS 1.0f
#define MIDDLE_CIRCLE_RADIUS 5.0f
#define OUTER_CIRCLE_RADIUS 10.0f

#define INNER_CIRCLE_SCORE 10
#define MIDDLE_CIRCLE_SCORE 5
#define OUTER_CIRCLE_SCORE 1

uint8_t score(coordinate_t landing_position)
{
    float distance = sqrtf(POW2(landing_position.x) + POW2(landing_position.y));
    
    if (distance <= INNER_CIRCLE_RADIUS) {
        return INNER_CIRCLE_SCORE;
    } else if (distance <= MIDDLE_CIRCLE_RADIUS) {
        return MIDDLE_CIRCLE_SCORE;
    } else if (distance <= OUTER_CIRCLE_RADIUS) {
        return OUTER_CIRCLE_SCORE;
    }
    return 0;
}
