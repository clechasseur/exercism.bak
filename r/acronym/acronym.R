acronym <- function(input) {
  toupper(gsub("([[:alnum:]'])[[:alnum:]']*[^[:alnum:]']*", "\\1", input))
}
