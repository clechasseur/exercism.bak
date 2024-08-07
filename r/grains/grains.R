square <- function(n) {
  stopifnot(n >= 1 & n <= 64)
  2 ^ (n - 1)
}

total <- function() {
  (2 ^ 64) - 1 # Total is the number of grains that would be on square 65, minus one
}
