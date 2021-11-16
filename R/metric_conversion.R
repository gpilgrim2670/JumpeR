#' Formatting feet-inches lengths as meters
#'
#' Takes a character string (or list) representing a length in feet-inches
#' format (e.g. "12-07.45") and converts it to a distance in meters ("3.85m").
#'
#' @importFrom purrr map_chr
#'
#' @param x A character vector of distance(s) in feet-inches format (e.g.
#'   "12-07.45"), to be converted to meters ("3.85m")
#' @return returns the value of the string \code{x} which represents a distance
#'   in meters, as a character, with unit "m" included
#'
#' @examples
#' distances <- c("1.23m", "5-02.34", "43.45", "6.89", NA)
#' metric_conversion(distances)
#' math_format(metric_conversion(distances))
#' metric_conversion("5.45m")
#' @export


metric_conversion <- function(x) {
  x <- purrr::map_chr(x, metric_conversion_helper)
  return(x)
}

#' Converts distances in feet-inches to meters
#'
#' @importFrom stringr str_detect
#' @importFrom stringr str_split_fixed
#'
#' @param x A character vector of distance(s) to be converted from feet-inches
#'   to meters
#' @return a numeric value representing a number of meters.  Units are not
#'   included

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
