normalized_plaintext <- function(input) {
  gsub("[^[:alnum:]]", "", tolower(input))
}

plaintext_segments <- function(input) {
  input <- normalized_plaintext(input)
  num.cols <- ceiling(sqrt(nchar(input)))
  num.rows <- ceiling(nchar(input) / num.cols)
  segs <- c()
  while (nchar(input) > 0) {
    segs <- c(segs, substr(input, 1, num.cols))
    input <- substr(input, num.cols + 1, nchar(input))
  }
  if (is.null(segs)) {
    ""
  } else {
    segs
  }
}

encoded <- function(input) {
  gsub("[[:blank:]]", "", ciphertext(input))
}

ciphertext <- function(input) {
  segs <- plaintext_segments(input)
  if (length(segs) == 1 & nchar(segs[1]) == 0) {
    segs <- c()
  }
  if (length(segs) > 0) {
    num.cols = nchar(segs[1])
    paste0(sapply(1:num.cols, function(col) {
      paste0(sapply(1:length(segs), function(row) {
        if (nchar(segs[row]) >= col) {
          substr(segs[row], col, col)
        } else {
          " "
        }
      }), collapse = "")
    }), collapse = " ")
  } else {
    ""
  }
}
