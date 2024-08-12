sum_of_multiples <- function(factors, limit) {
  seq.of.mutiples <- function(n) {
    seq(from = 0, to = limit - 1, by = n)
  }
  sum(unique(unlist(sapply(factors, seq.of.mutiples))))
}
