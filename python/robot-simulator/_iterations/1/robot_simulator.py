from functools import reduce

EAST = complex(1, 0)
NORTH = complex(0, 1)
WEST = complex(-1, 0)
SOUTH = complex(0, -1)

class Robot:
    def __init__(self, direction=NORTH, x_pos=0, y_pos=0):
        self.position = complex(x_pos, y_pos)
        self.direction = direction

    @property
    def coordinates(self):
        return self.position.real, self.position.imag

    def move(self, instructions):
        self.position, self.direction = reduce(
            lambda state, instruction: MOVES[instruction](*state),
            instructions,
            (self.position, self.direction),
        )

MOVES = {
    'L': lambda position, direction: (position, direction * 1j),
    'R': lambda position, direction: (position, direction * -1j),
    'A': lambda position, direction: (position + direction, direction),
}
