#' Title
#'
#' @param vector result of common_names
#'
#' @return
#' @export
#'
#' @examples
pertinence <- function(names_found, center_names_w = 0.1){
  n = length(names_found)
  names_weight    = rep(center_names_w, n)
  names_weight[1] = (1-(n-2)*center_names_w)/2
  names_weight[n] = (1-(n-2)*center_names_w)/2

  return(sum(names_weight * !is.na(names_found)))
}







