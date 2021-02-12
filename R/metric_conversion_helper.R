#' Converts distances in feet-inches to meters
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @importFrom stringr str_detect
#' @importFrom stringr str_split_fixed
#'
#' @param x A character vector of distance(s) to be converted from feet-inches to meters
#' @return a numeric value representing a number of meters.  Units are not included

metric_conversion_helper <- function(x) {

  x <- as.character(x)

  if(is.na(x) == TRUE) return(NA)
  if (str_detect(x, "-") == TRUE) {
    feet <- as.numeric(stringr::str_split_fixed(x, "-", n = 2)[, 1])
    inches <-
      as.numeric(stringr::str_split_fixed(x, "-", n = 2)[, 2])
    if (inches >= 12)
      stop("Inches must be less than 12 in a field formatted length")
    x <- (feet * 12) + inches
    x <- round(x * 0.0254, 2) # convert to meters
    x <- paste0(x, "m") # append m
  } else {
    x
  }
  return(x)

}
