# Determine whether the number is valid.
is_valid <- function(input) {
  if (grepl("[^[:digit:][:blank:]]", input) | nchar(trimws(input)) <= 1) {
    FALSE
  } else {
    rev.digits <- rev(as.integer(unlist(strsplit(gsub("\\s", "", input), ""))))
    every.second <- seq.int(2, length(rev.digits), 2)
    rev.digits[every.second] <- rev.digits[every.second] * 2
    rev.digits[rev.digits > 9] <- rev.digits[rev.digits > 9] - 9
    sum(rev.digits) %% 10 == 0
  }
}
