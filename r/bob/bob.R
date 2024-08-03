bob <- function(input) {
  input <- unlist(strsplit(trimws(input), ""))
  if (length(input) == 0) {
    "Fine. Be that way!"
  } else {
    is.a.question <- tail(input, n = 1) == "?"
    is.yelling <- length(input[grepl("[[:lower:]]", input)]) == 0 & length(input[grepl("[[:upper:]]", input)]) != 0
    if (is.a.question & is.yelling) {
      "Calm down, I know what I'm doing!"
    } else if (is.a.question) {
      "Sure."
    } else if (is.yelling) {
      "Whoa, chill out!"
    } else {
      "Whatever."
    }
  }
}
