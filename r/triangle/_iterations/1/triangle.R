triangle <- function(x, y, z) {
  sides <- sort(c(x, y, z))
  stopifnot(all(sides > 0) & sides[1] + sides[2] >= sides[3])
  kind <- 42
  if (sides[1] == sides[2] & sides[2] == sides[3]) {
    class(kind) <- c(class(kind), "equilateral")
  }
  if (sides[1] == sides[2] | sides[2] == sides[3]) {
    class(kind) <- c(class(kind), "isosceles")
  }
  if (sides[1] != sides[2] & sides[2] != sides[3]) {
    class(kind) <- c(class(kind), "scalene")
  }
  kind
}
