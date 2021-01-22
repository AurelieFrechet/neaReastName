#' @title common_names
#' @description Look for commons initials between two names
#' @param name1 vector of strings
#' @param name2 vector of strings
#' @param order if TRUE, initials should be in the same ordre
#'
#' @return character vector of correspondances betweens initiales of the two names
#' @export
common_names <-
  function(name1,
           name2,
           initials_only = FALSE,
           order = TRUE,
           threshold = 1,
           r_letters = c("K" = "C",
                         "W" = "V",
                         "Y" = "I",
                         "Z" = "S")) {

  # Look for biggest name (b_name)
  if(length(name1) >= length(name2)){
    large_name <- name1
    small_name <- name2
  } else {
    large_name <- name2
    small_name <- name1
  }

  # Prepare result vector (big_name length)
  result <- rep(NA_character_, length(large_name))

  if(initials_only) {
    # Get Initials from names
    large_name <- substring(large_name, 1, 1)
    small_name <- substring(small_name, 1, 1)
  }

  # # Browse all values in small name vector
  # i_result <- 0
  # for (i_sn in 1:length(small_name)) {
  #   # Initialize values
  #   i_ln <- 1
  #   found <- FALSE
  #   # While not found in large name vector
  #   while (!found & i_ln <= length(large_name)) {
  #     # Differents conditions if initials only
  #     if (initials_only) {
  #       condition <- small_name[i_sn] == large_name[i_ln]
  #     } else {
  #       condition <- is_similar(
  #         name1 = small_name[i_sn],
  #         name2 = large_name[i_ln],
  #         threshold = threshold,
  #         r_letters = r_letters
  #       )
  #     }
  #
  #     # If names or initials matches
  #     if (condition) {
  #       found <- TRUE
  #       i_result <- i_result + i_ln
  #       result[i_result]  <- large_name[i_ln]
  #       large_name <- large_name[-c(1:i_ln)]
  #     }
  #     i_ln <- i_ln + 1
  #   }
  # }

  # Browse all values in small name vector
  for (i in 1:length(large_name)) {
    # Initialize values
    j <- 1
    found <- FALSE
    # While not found in large name vector
    while (!found & j <= length(small_name)) {
      # Differents conditions if initials only
      if (initials_only) {
        condition <- small_name[j] == large_name[i]
      } else {
        condition <- is_similar(
          name1 = small_name[j],
          name2 = large_name[i],
          threshold = threshold,
          r_letters = r_letters
        )
      }

      # If names or initials matches
      if (condition) {
        found <- TRUE
        result[i]  <- large_name[i]
        if(order){
          small_name <- small_name[-c(1:j)]
        } else {
          small_name <- small_name[-j]
        }
      }
      j <- j + 1
    }
  }

  return(result)
  }




