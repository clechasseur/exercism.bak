anagram <- function(subject, candidates) {
  sort.str.i <- function(s) {
    paste0(sort(unlist(strsplit(tolower(s), ""))), collapse = "")
  }
  is.anagram <- function(s) {
    sort.str.i(s) == sort.str.i(subject) & tolower(s) != tolower(subject)
  }
  anagrams <- candidates[unlist(lapply(candidates, is.anagram))]
  if (length(anagrams) != 0) {
    anagrams[!duplicated(tolower(anagrams))]
  } else {
    c()
  }
}
