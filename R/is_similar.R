
#' @include namedist.R
#' @title is_similar
#' @description Get if two names are similar based on the distance between them.
#' If the difference is superior of the threshold, then there is no match
#' @param name1 vector of names (character type)
#' @param name2 vector of names (character type)
#' @param threshold integer, max difference between two words (see namedist)
#' @param r_letters named vector of letter to replace (see replace_letters)
#'
#' @return TRUE or false based on the threshold
#' @export
is_similar <-function(name1,
                      name2,
                      threshold = 1,
                      r_letters = c("K" = "C",
                                    "W" = "V",
                                    "Y" = "I",
                                    "Z" = "S")){
  return(namedist(name1, name2, r_letters) <= threshold)
}

