handshake <- function(n) {
  actions <- c(1, 2, 4, 8)
  names(actions) <- c("wink", "double blink", "close your eyes", "jump")
  output <- names(actions)[bitwAnd(n, actions) != 0]
  if (length(output) == 0) {
    c()
  } else if (bitwAnd(n, 16) != 0) {
    rev(output)
  } else {
    output
  }
}
