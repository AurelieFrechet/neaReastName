#' @include is_similar.R
#' @title common_names
#' @description Look for commons initials between two names
#' @param name1 vector of strings
#' @param name2 vector of strings
#' @param order if TRUE, initials should be in the same ordre
#' @return character vector of correspondances betweens initiales of the two names
#' @export
common_names <-
  function(name1,
           name2,
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

  # Browse all values in small name vector
  for (i in 1:length(large_name)) {
    # Initialize values
    j <- 1
    found <- FALSE
    # While not found in large name vector
    while (!found & j <= length(small_name)) {
      # If there is only a initial, compare with first letter of other name
      if (nchar(small_name[j]) == 1 | nchar(large_name[j]) == 1) {
        condition <- substring(large_name[j], 1, 1) == substring(small_name[j], 1, 1)
      } else {
        condition <- is_similar(
          name1 = small_name[j],
          name2 = large_name[i],
          threshold = threshold,
          r_letters = r_letters
        )
      }

      # If names matches
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




