scrabble_score <- function(input){
  scores        <- c( 1,   3,   3,   2,   1,   4,   2,   4,   1,   8,   5,   1,   3,   1,   1,   3,  10,   1,   1,   1,   1,   4,   4,   8,   4,  10)
  names(scores) <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")
  u.v.input     <- unlist(strsplit(toupper(input), ""))
  input.scores  <- sapply(u.v.input, function(c) scores[names(scores) == c])
  if (length(input.scores) != 0) {
    sum(input.scores)
  } else {
    0
  }
}
