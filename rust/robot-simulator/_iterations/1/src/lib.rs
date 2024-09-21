use std::ops::{Add, AddAssign};

/// A point in 2D space, representing the location of a [`Robot`].
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Point {
    pub x: i32,
    pub y: i32,
}

impl Point {
    /// Creates a new `Point` from its `x` and `y` coordinates.
    pub fn new(x: i32, y: i32) -> Self {
        Self { x, y }
    }

    /// Returns a `Point` at (0, 0).
    pub fn zero() -> Self {
        Self::default()
    }
}

impl From<Point> for (i32, i32) {
    /// Converts a [`Point`] into a tuple of its coordinates.
    ///
    /// # Examples
    ///
    /// ```
    /// # use robot_simulator::Point;
    /// let pt = Point::new(7, 3);
    /// let tuple: (i32, i32) = pt.into();
    /// assert_eq!((7, 3), tuple);
    /// ```
    fn from(value: Point) -> Self {
        (value.x, value.y)
    }
}

impl From<(i32, i32)> for Point {
    /// Creates a [`Point`] from a tuple of its coordinates.
    ///
    /// # Examples
    ///
    /// ```
    /// # use robot_simulator::Point;
    /// let tuple = (7, 3);
    /// let pt: Point = tuple.into();
    /// assert_eq!(Point::new(7, 3), pt);
    /// ```
    fn from(value: (i32, i32)) -> Self {
        Self::new(value.0, value.1)
    }
}

impl Add for Point {
    type Output = Point;

    /// Adds two [`Point`]s by adding their `x` and `y` coordinates.
    ///
    /// # Examples
    ///
    /// ```
    /// # use robot_simulator::Point;
    /// let pt_a = Point::new(7, 3);
    /// let pt_b = Point::new(2, -4);
    /// assert_eq!(Point::new(9, -1), pt_a + pt_b);
    /// ```
    fn add(mut self, rhs: Self) -> Self::Output {
        self += rhs;
        self
    }
}

impl AddAssign for Point {
    fn add_assign(&mut self, rhs: Self) {
        self.x += rhs.x;
        self.y += rhs.y;
    }
}

/// Direction that a [`Robot`] can face.
#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Direction {
    North,
    East,
    South,
    West,
}

impl Direction {
    /// Returns the `Direction` we'd face if we turned left.
    pub fn left(&self) -> Self {
        match self {
            Self::North => Self::West,
            Self::East => Self::North,
            Self::South => Self::East,
            Self::West => Self::South,
        }
    }

    /// Returns the `Direction` we'd face if we turned right.
    pub fn right(&self) -> Self {
        match self {
            Self::North => Self::East,
            Self::East => Self::South,
            Self::South => Self::West,
            Self::West => Self::North,
        }
    }

    /// Returns the displacement of a [`Robot`], were it to move in this `Direction`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use robot_simulator::{Direction, Point};
    /// let position = Point::new(0, 0);
    ///
    /// let to_the_north = position + Direction::North.displacement();
    /// assert_eq!(Point::new(0, 1), to_the_north);
    ///
    /// let to_the_west = position + Direction::West.displacement();
    /// assert_eq!(Point::new(-1, 0), to_the_west);
    /// ```
    pub fn displacement(&self) -> Point {
        match self {
            Self::North => Point::new(0, 1),
            Self::East => Point::new(1, 0),
            Self::South => Point::new(0, -1),
            Self::West => Point::new(-1, 0),
        }
    }
}

/// A robot that can travel in a 2D space.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Robot {
    position: Point,
    direction: Direction,
}

impl Robot {
    /// Creates a new `Robot` at the given starting position, facing in the given [`Direction`].
    pub fn new(x: i32, y: i32, direction: Direction) -> Self {
        Self {
            position: Point::new(x, y),
            direction,
        }
    }

    /// Turns the `Robot` to the right.
    #[must_use]
    pub fn turn_right(self) -> Self {
        Self {
            direction: self.direction.right(),
            ..self
        }
    }

    /// Turns the `Robot` to the left.
    #[must_use]
    pub fn turn_left(self) -> Self {
        Self {
            direction: self.direction.left(),
            ..self
        }
    }

    /// Moves the `Robot` forward one step in the [`direction`] it is currently facing.
    ///
    /// [`direction`]: Self::direction
    #[must_use]
    pub fn advance(self) -> Self {
        Self {
            position: self.position + self.direction.displacement(),
            ..self
        }
    }

    /// Has the `Robot` follow the given set of instructions, returning its final state.
    ///
    /// # Valid instructions
    ///
    /// | Instruction | Effect                           |
    /// |-------------|----------------------------------|
    /// | `L`         | Turns the robot to the [`left`]  |
    /// | `R`         | Turns the robot to the [`right`] |
    /// | `A`         | [`Advances`] the robot one step  |
    ///
    /// # Panics
    ///
    /// Panics if an invalid instruction is encountered.
    ///
    /// # Examples
    ///
    /// ```
    /// # use robot_simulator::{Direction, Robot};
    /// let robot = Robot::new(0, 0, Direction::North);
    /// let final_robot = robot.instructions("LAARALARRA");
    /// assert_eq!(Robot::new(-2, 1, Direction::East), final_robot);
    /// ```
    ///
    /// ```should_panic
    /// # use robot_simulator::{Direction, Robot};
    /// let _ = Robot::new(0, 0, Direction::North).instructions("FOO"); // Panics!
    /// ```
    ///
    /// [`left`]: Self::turn_left
    /// [`right`]: Self::turn_right
    /// [`Advances`]: Self::advance
    #[must_use]
    pub fn instructions(self, instructions: &str) -> Self {
        instructions
            .chars()
            .fold(self, |robot, instruction| match instruction {
                'L' => robot.turn_left(),
                'R' => robot.turn_right(),
                'A' => robot.advance(),
                c => panic!("invalid instruction: {c}"),
            })
    }

    /// Returns the `Robot`'s current position.
    pub fn position(&self) -> (i32, i32) {
        self.position.into()
    }

    /// Returns the [`Direction`] the `Robot` is currently facing.
    pub fn direction(&self) -> &Direction {
        &self.direction
    }
}
