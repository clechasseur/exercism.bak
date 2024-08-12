allergy <- function(num) {
  allergies        <- c(  1,        2,          4,             8,           16,          32,        64,     128)
  names(allergies) <- c("eggs", "peanuts", "shellfish", "strawberries", "tomatoes", "chocolate", "pollen", "cats")
  allergies[bitwAnd(num, allergies) != 0]
}

allergic_to <- function(allergy_object, allergy) {
  any(names(allergy_object) == allergy)
}

list_allergies <- function(allergy_object) {
  names(allergy_object)
}
