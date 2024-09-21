#include "queen_attack.h"

#include <stdbool.h>
#include <stdlib.h>

inline static bool invalid(position_t queen)
{
    return queen.row > 7 || queen.column > 7;
}

inline static bool same_position(position_t queen_1, position_t queen_2)
{
    return queen_1.row == queen_2.row && queen_1.column == queen_2.column;
}

inline static bool same_diagonal(position_t queen_1, position_t queen_2)
{
    return abs(queen_1.row - queen_2.row) == abs(queen_1.column - queen_2.column);
}

attack_status_t can_attack(position_t queen_1, position_t queen_2)
{
    if (invalid(queen_1) || invalid(queen_2) || same_position(queen_1, queen_2)) {
        return INVALID_POSITION;
    }

    bool can_attack = queen_1.row == queen_2.row
                   || queen_1.column == queen_2.column
                   || same_diagonal(queen_1, queen_2);
    return can_attack ? CAN_ATTACK : CAN_NOT_ATTACK;
}
