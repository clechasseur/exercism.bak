prime_factors <- function(number) {
  factor <- 2
  factors <- c()
  while (number > 1) {
    if (number %% factor == 0) {
      number <- number / factor;
      factors <- c(factors, factor)
    } else {
      factor <- factor + 1
    }
  }
  factors
}
