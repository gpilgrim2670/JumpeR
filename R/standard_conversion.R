#' Formatting meters lengths as feet-inches
#'
#' Takes a character string (or list) representing a length in meters format (e.g. "3.85m") and converts it to a distance in feet-inches ("12-07.45")
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @importFrom purrr map_chr
#'
#' @param x A character vector of distance(s) in meters format ("3.85m") , to be converted to meters ("12-07.45")
#' @return returns the value of the string \code{x} which represents a distance in feet-inches

standard_conversion <- function(x) {
  x <- map_chr(x, standard_conversion_helper)
  return(x)
}
