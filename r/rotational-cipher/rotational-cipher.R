rotate <- function(text, key) {
  chars <- unlist(strsplit(text, ""))
  output <- c()
  for (char in chars) {
    if (char %in% letters) {
      output <- c(output, letters[((match(char, letters) + key - 1) %% 26) + 1])
    } else if (char %in% LETTERS) {
      output <- c(output, LETTERS[((match(char, LETTERS) + key - 1) %% 26) + 1])
    } else {
      output <- c(output, char)
    }
  }
  paste0(output, collapse = "")
}
