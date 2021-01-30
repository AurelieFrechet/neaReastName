#' @include common_names.R
#' @include is_similar.R
#' @title match_names
#' @description match names
#'
#' @param name name to match
#' @param list_of_names list of names
#' @param threshold threshold of distance
#' @param r_letters letters to change
#' @param last_name TRUE if last name is in last position,
#' else, last name is in first position
#'
#' @return list of matches
#' @export
match_names <- function(name,
                        list_of_names,
                        threshold = 1,
                        last_name = TRUE,
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
      # If all names concat are exactly the same
      if (is_similar(
        name1     = paste(name, collapse = ""),
        name2     = paste(n, collapse = ""),
        threshold = 0,
        r_letters = r_letters
      )) {
        return(1)

      } else {
        # Exact Names non-matched
        exact_names_ordered <- common_names(
          name1 = name,
          name2 = n,
          order = TRUE,
          threshold = 0,
          r_letters = r_letters
        )
        exact_names_NA <- sum(is.na(exact_names_ordered))

        # Exact Names non-matched
        similar_names_ordered <- common_names(
          name1 = name,
          name2 = n,
          order = TRUE,
          threshold = threshold,
          r_letters = r_letters
        )
        similar_names_NA <- sum(is.na(similar_names_ordered))

        # Number of names
        n_names <- length(exact_names_ordered)
        # Position of last name
        if (last_name) {
          last_name_pos <- n_names
        } else {
          last_name_pos <- 1
        }

        # If lastname exactly the same (threshold = 0)
        if (!is.na(exact_names_ordered[last_name_pos])) {
          return(1 - (exact_names_NA + similar_names_NA) / (n_names * 2))
        } else {
          # If last name is similar
          if (!is.na(similar_names_ordered[last_name_pos])) {
            return(1 - ((exact_names_NA - 1) + similar_names_NA) / ((n_names - 1) * 2))
          }
          return(0)
        }
      }


    }
  )
  return(list_of_names[score >= 0.5])

}
