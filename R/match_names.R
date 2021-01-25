match_names <- function(name,
                        list_of_names,
                        threshold = 1,
                        r_letters = c(
                          "K" = "C",
                          "W" = "V",
                          "Y" = "I",
                          "Z" = "S"
                        ),
                        center_names_w = 0.1) {

  score <- sapply(
    X = list_of_names,
    FUN = function(n) {
        # Full name, ordered
        0.4 * pertinence(common_names(
          name1 = name,
          name2 = n,
          initials_only = FALSE,
          order = TRUE,
          threshold = threshold,
          r_letters = r_letters
        )) +

        0.4 * pertinence(common_names(
          name1 = name,
          name2 = n,
          initials_only = FALSE,
          order = TRUE,
          threshold = threshold,
          r_letters = r_letters
        )) +

        # Initials only, ordered
        0.1 * pertinence(common_names(
          name1 = name,
          name2 = n,
          initials_only = TRUE,
          order = TRUE,
          threshold = threshold,
          r_letters = r_letters
        )) +

        # Initials only, non-ordered
        0.1 * pertinence(common_names(
          name1 = name,
          name2 = n,
          initials_only = TRUE,
          order = TRUE,
          threshold = threshold,
          r_letters = r_letters
        ))
         })

return(list_of_names[score > 0.5])

}
