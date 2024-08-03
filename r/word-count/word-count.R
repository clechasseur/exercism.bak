word_count <- function(input) {
  words <- unlist(strsplit(gsub("[^a-z0-9 ]+", "", tolower(trimws(input))), " +"))
  sapply(unique(words), function(word) length(words[words == word]), simplify = FALSE)
}
