#' pertinence
#' @description This is the section where I'm supposed
#' to describe this f** functions I've spend my evening on
#' @param vector result of common_names
#'
#' @return pertinence score between 0 and 1
#' @export
pertinence <- function(names_found, center_names_w = 0.1){
  n = length(names_found)
  names_weight    = rep(center_names_w, n)
  names_weight[1] = (1-(n-2)*center_names_w)/2
  names_weight[n] = (1-(n-2)*center_names_w)/2

  return(sum(names_weight * !is.na(names_found)))
}







