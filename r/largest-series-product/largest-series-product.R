largest_series_product <- function(digits, span){
  stopifnot(!grepl("\\D", digits))
  stopifnot(span >= 0)
  if (span > 0) {
    nums <- as.numeric(unlist(strsplit(digits, "")))
    stopifnot(span <= length(nums))
    max(sapply(1:(length(nums) - span + 1), function(i) prod(nums[i:(i + span - 1)], na.rm = TRUE)))
  } else {
    1
  }
}
