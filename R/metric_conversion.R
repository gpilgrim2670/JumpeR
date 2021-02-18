#' Formatting feet-inches lengths as meters
#'
#' Takes a character string (or list) representing a length in feet-inches format (e.g. "12-07.45") and converts it to a distance in meters ("3.85m")
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @importFrom purrr map_chr
#'
#' @param x A character vector of distance(s) in feet-inches format (e.g. "12-07.45"), to be converted to meters ("3.85m")
#' @return returns the value of the string \code{x} which represents a distance in meters, as a character, with unit "m" included
#'
#' @examples
#' distances <- c("1.23m", "5-02.34", "43.45", "6.89", NA)
#' metric_conversion(distances)
#' math_format(metric_conversion(distances))
#' metric_conversion("5.45m")
#' @export


metric_conversion <- function(x) {
  x <- map_chr(x, metric_conversion_helper)
  return(x)
}
