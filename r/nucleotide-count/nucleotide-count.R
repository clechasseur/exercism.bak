nucleotide_count <- function(input) {
  stopifnot(!grepl("[^ACGT]", input))
  counts <- as.list(table(unlist(strsplit(input, ""))))
  for (n in c("A", "C", "G", "T")) {
    if (length(counts[names(counts) == n]) == 0) {
      counts[n] <- 0
    }
  }
  counts
}
