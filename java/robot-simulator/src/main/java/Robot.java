import java.util.Arrays;

class Robot {
    private GridPosition gridPosition;
    private Orientation orientation;

    Robot(GridPosition gridPosition, Orientation orientation) {
        this.gridPosition = gridPosition;
        this.orientation = orientation;
    }

    GridPosition getGridPosition() {
        return this.gridPosition;
    }

    Orientation getOrientation() {
        return this.orientation;
    }

    void turnLeft() {
        orientation = orientation.left();
    }

    void turnRight() {
        orientation = orientation.right();
    }

    void advance() {
        gridPosition = gridPosition.move(orientation);
    }

    void simulate(String steps) {
        Arrays.stream(steps.split("")).forEach(step -> {
            switch (step) {
                case "L": {
                    turnLeft();
                    break;
                }
                case "R": {
                    turnRight();
                    break;
                }
                case "A": {
                    advance();
                    break;
                }
                default:
                    throw new IllegalArgumentException("Invalid step: " + step);
            }
        });
    }
}
