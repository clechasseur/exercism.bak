number_type <- function(n){
  if (n > 1) {
    possible.factors <- c(1:(n - 1))
    aliquot.sum <- sum(possible.factors[n %% possible.factors == 0])
  } else if (n == 1) {
    aliquot.sum <- 0
  } else {
    stop("Invalid number: ", n)
  }
  if (aliquot.sum < n) {
    "deficient"
  } else if (aliquot.sum > n) {
    "abundant"
  } else {
    "perfect"
  }
}
