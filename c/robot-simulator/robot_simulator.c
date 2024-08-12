#include "robot_simulator.h"

#include <stddef.h>

const robot_position_t DISPLACEMENTS[DIRECTION_MAX] = {
    // NORTH
    { .x = 0, .y = 1 },

    // EAST
    { .x = 1, .y = 0 },

    // SOUTH
    { .x = 0, .y = -1 },

    // WEST
    { .x = -1, .y = 0 }
};

static void displace(robot_position_t *position, robot_position_t delta)
{
    position->x += delta.x;
    position->y += delta.y;
}

static void advance(robot_status_t *robot)
{
    displace(&robot->position, DISPLACEMENTS[robot->direction]);
}

static void turn_left(robot_status_t *robot)
{
    robot->direction = (robot->direction + 3) % DIRECTION_MAX;
}

static void turn_right(robot_status_t *robot)
{
    robot->direction = (robot->direction + 1) % DIRECTION_MAX;
}

typedef void (*robot_command_func_t)(robot_status_t *);
typedef struct {
    char command;
    robot_command_func_t func;
} robot_command_t;

#define NUM_ROBOT_COMMANDS 3
const robot_command_t ROBOT_COMMANDS[NUM_ROBOT_COMMANDS] = {
    { 'A', advance },
    { 'L', turn_left },
    { 'R', turn_right }
};

static void execute_command(robot_status_t *robot, char command)
{
    for (const robot_command_t *robot_command = ROBOT_COMMANDS; robot_command != (ROBOT_COMMANDS + NUM_ROBOT_COMMANDS); ++robot_command) {
        if (robot_command->command == command) {
            robot_command->func(robot);
            return;
        }
    }
}

robot_status_t robot_create(robot_direction_t direction, int x, int y)
{
    robot_status_t robot = {
        .direction = direction,
        .position = {
            .x = x,
            .y = y
        }
    };
    return robot;
}

void robot_move(robot_status_t *robot, const char *commands)
{
    if (robot == NULL || commands == NULL) {
        return;
    }
    
    for (; *commands != '\0'; ++commands) {
        execute_command(robot, *commands);
    }
}
