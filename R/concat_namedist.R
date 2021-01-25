concat_namedist <- function(name1,
                            name2,
                            r_letters = c(
                              "K" = "C",
                              "W" = "V",
                              "Y" = "I",
                              "Z" = "S"
                            )) {
  return(namedist(
    name1 = paste(name1, collapse = ""),
    name2 = paste(name2, collapse = ""),
    r_letters = r_letters
  ))
}
