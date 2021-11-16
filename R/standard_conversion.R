#' Formatting meters lengths as feet-inches
#'
#' Takes a character string (or list) representing a length in meters format
#' (e.g. "3.85m") and converts it to a distance in feet-inches ("12-07.45")
#'
#' @importFrom purrr map_chr
#'
#' @param x A character vector of distance(s) in meters format ("3.85m") , to be
#'   converted to meters ("12-07.45")
#' @return returns the value of the string \code{x} which represents a distance
#'   in feet-inches

standard_conversion <- function(x) {
  x <- purrr::map_chr(x, standard_conversion_helper)
  return(x)
}

#' Converts distances in meters to feet-inches
#'
#' @importFrom stringr str_detect
#' @importFrom stringr str_detect
#'
#' @param x A character vector of distance(s) to be converted from meters to
#'   feet-inches
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
