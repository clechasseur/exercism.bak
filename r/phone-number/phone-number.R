parse_phone_number <- function(number_string) {
  phone.regex <- "^[[:blank:]]*\\+?1?[[:blank:].-]*\\(?[[:blank:]]*([2-9]\\d{2})[[:blank:]]*\\)?[[:blank:].-]*([2-9]\\d{2})[[:blank:].-]*(\\d{4})\\s*$"
  if (grepl(phone.regex, number_string)) {
    gsub(phone.regex, "\\1\\2\\3", number_string, perl = TRUE)
  } else {
    NULL
  }
}
