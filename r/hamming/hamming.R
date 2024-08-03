hamming <- function(strand1, strand2) {
  stopifnot(nchar(strand1) == nchar(strand2))
  sum(unlist(strsplit(strand1, "")) != unlist(strsplit(strand2, "")))
}
