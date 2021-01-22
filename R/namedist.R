#' @include replace_letters.R
#' @import stringdist
#' @title namedist
#' @description Apply stringdist function to two names in which the option
#' replace_letters is available
#' @param name1 string
#' @param name2 string
#' @param r_letters vector of letters to be replaced if necessary
#'
#' @return distance between the two names as integer value
#' @export
namedist <- function(name1,
                     name2,
                     r_letters = c("K" = "C",
                                   "W" = "V",
                                   "Y" = "I",
                                   "Z" = "S")) {
  return(
    stringdist(
      replace_letters(name1, r_letters),
      replace_letters(name2, r_letters)
  ))
}
