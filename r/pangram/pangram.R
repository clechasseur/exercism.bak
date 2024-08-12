is_pangram <- function(input) {
  length(unique(unlist(strsplit(gsub("[^a-z]", "", tolower(input)), "")))) == 26
}
