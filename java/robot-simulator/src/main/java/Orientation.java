enum Orientation {
    NORTH, EAST, SOUTH, WEST;

    Orientation right() {
        if (this == NORTH) {
            return EAST;
        } else if (this == EAST) {
            return SOUTH;
        } else if (this == SOUTH) {
            return WEST;
        } else {
            return NORTH;
        }
    }

    Orientation left() {
        if (this == NORTH) {
            return WEST;
        } else if (this == WEST) {
            return SOUTH;
        } else if (this == SOUTH) {
            return EAST;
        } else {
            return NORTH;
        }
    }
}
