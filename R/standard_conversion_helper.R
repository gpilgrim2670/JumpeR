#' Converts distances in meters to feet-inches
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @importFrom stringr str_detect
#' @importFrom stringr str_detect
#'
#' @param x A character vector of distance(s) to be converted from meters to feet-inches
#' @return a character vector in feet-inches

standard_conversion_helper <- function(x) {

  x <- as.character(x)

  if (str_detect(x, "m") == TRUE) {
    x <- as.numeric(str_remove(x, "m"))
    x <- round(x * 39.37, 2) # convert from meters to inches

    inches <- round(x%%12, 2)
    inches <- base::sprintf("%05.2f", inches) # add leading 0 as needed
    feet <- x%/%12

    x <- paste0(feet, "-", inches) # stick together in proper format

  } else {
    x
  }
  return(x)

}
