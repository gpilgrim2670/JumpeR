#' Removes duplicate splits
#'
#' @importFrom stringr str_detect
#'
#' @param x a list of splits, in which position 2 and position 3 might be duplicates
#' @return a list with duplicated value in position 2 removed
#'
#' @seealso \code{remove_duplicate_splits} is a helper function inside \code{splits_parse}

remove_duplicate_splits <- function(x) {
  #### testing ####
  # x <- data_1_splits[[1]]

  if (all(length(x) >= 3, stringr::str_detect(x[3], x[2]) == TRUE)) {
    x <- x[-2]
  }

  return(x)

}
