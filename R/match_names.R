match_names <- function(name,
                        list_of_names,
                        threshold = 1,
                        r_letters = c(
                          "K" = "C",
                          "W" = "V",
                          "Y" = "I",
                          "Z" = "S"
                        ),
                        a = c("ordered", "non-ordered", "both"),
                        b = c('full', "partial", "both")) {

  pouet <- lapply(list_of_names, common_names, name)
  lapply(pouet, pertinence)

}
