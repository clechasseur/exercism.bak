sieve <- function(limit) {
  if (limit >= 2) {
    primes <- 2:limit
    n <- 2
    while (n < limit) {
      primes <- primes[!primes %in% seq(from = n * 2, to = limit * 2, by = n)]
      repeat {
        n <- n + 1
        if (n >= limit | n %in% primes) {
          break()
        }
      }
    }
    primes
  } else {
    c()
  }
}
