lyrics <- function(first, last) {
  paste0(sapply(c(first:last), verse), collapse = "\n")
}

verse <- function(number) {
  num.str <- paste0(number)
  paste0(switch(num.str, "0" = "No more bottles", "1" = "1 bottle", paste0(number, " bottles")),
         " of beer on the wall, ",
         switch(num.str, "0" = "no more bottles", "1" = "1 bottle", paste0(number, " bottles")),
         " of beer.\n",
         switch(num.str, "0" = "Go to the store and buy some more",
                         "1" = "Take it down and pass it around",
                         "Take one down and pass it around"),
         ", ",
         switch(num.str, "0" = "99 bottles", "1" = "no more bottles", "2" = "1 bottle", paste0(number - 1, " bottles")),
         " of beer on the wall.\n")
}
