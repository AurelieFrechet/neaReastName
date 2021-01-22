#' @import stringr
#' @title replace_letters
#' @description Replace selected letters by substitute
#'
#' @param word : string in which replace letters to normalize the name
#' @param r_letters : named character vector of letters to replace
#'
#' @return same word but with replaced letters
replace_letters <- function(word,
                            r_letters = c("K" = "C",
                                          "W" = "V",
                                          "Y" = "I",
                                          "Z" = "S")) {
  for (i in 1:length(r_letters)) {
    word <- str_replace_all(
      string = word,
      pattern = names(r_letters)[i],
      replacement = r_letters[i]
    )
  }
  return(word)
}








