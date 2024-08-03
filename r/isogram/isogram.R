is_isogram <- function(word) {
  cleaned.word = gsub("[ -]", "", word)
  sorted.word = paste0(sort(unlist(strsplit(tolower(cleaned.word), ""))), collapse = "")
  !grepl("(\\w)\\1", sorted.word)
}
