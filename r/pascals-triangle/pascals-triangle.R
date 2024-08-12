pascals_triangle <- function(n) {
  stopifnot(!is.null(n))
  stopifnot(n >= 0)
  triangle <- list()
  if (n > 0) {
    triangle <- c(triangle, list(1))
    if (n > 1) {
      for (i in 2:n) {
        last.row <- triangle[[length(triangle)]]
        new.row <- c(1)
        if (i > 2) {
          for (j in 2:(i - 1)) {
            new.row <- c(new.row, last.row[j - 1] + last.row[j])
          }
        }
        new.row <- c(new.row, 1)
        triangle <- c(triangle, list(new.row))
      }
    }
  }
  triangle
}
