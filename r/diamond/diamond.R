diamond <- function(letter) {
  max.idx <- match(letter, LETTERS)
  get.spaces <- function(n) paste0(rep_len(" ", n), collapse = "")
  get.lines <- function(from, to) {
    sapply(from:to, function(i) {
      paste0(get.spaces(max.idx - i),
             LETTERS[i],
             get.spaces((max.idx - 1) * 2 - 1 - 2 * (max.idx- i)),
             LETTERS[i],
             get.spaces(max.idx - i))
    })
  }
  line.a <- paste0(get.spaces(max.idx - 1), "A", get.spaces(max.idx - 1))
  lines <- c(line.a)
  if (max.idx > 1) {
    lines <- c(lines, get.lines(2, max.idx))
    if (max.idx > 2) {
      lines <- c(lines, get.lines(max.idx - 1, 2))
    }
    lines <- c(lines, line.a)
  }
  paste0(lines, collapse = "\n")
}
