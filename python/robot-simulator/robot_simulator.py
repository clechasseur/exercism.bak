EAST = complex(1, 0)
NORTH = complex(0, 1)
WEST = complex(-1, 0)
SOUTH = complex(0, -1)

class Robot:
    def __init__(self, direction=NORTH, x_pos=0, y_pos=0):
        self.position = complex(x_pos, y_pos)
        self.direction = direction

        self._moves = {
            'L': self.turn_left,
            'R': self.turn_right,
            'A': self.advance,
        }

    @property
    def coordinates(self):
        return self.position.real, self.position.imag

    def turn_left(self):
        self.direction *= 1j

    def turn_right(self):
        self.direction *= -1j

    def advance(self):
        self.position += self.direction

    def move(self, instructions):
        for instruction in instructions:
            self._moves[instruction]()
