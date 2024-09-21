transcriptions <- c(G = "C", C = "G", T = "A", A = "U")
transcribe <- function(nucleotide) {
  output <- unname(transcriptions[nucleotide])
  stopifnot(!is.na(output))
  output
}

to_rna <- function(dna) {
  paste0(sapply(unlist(strsplit(dna, "")), transcribe), collapse = "")
}
