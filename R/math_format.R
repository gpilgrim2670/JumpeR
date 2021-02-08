#' Formatting mm:ss.th times as seconds
#'
#' Takes a character string (or list) representing time in track format (e.g. 1:35.37) and converts it to a numeric value (95.37) or a list of values representing seconds.
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @importFrom purrr map_dbl
#'
#' @param x A character vector of time(s) in track format (e.g. 1:35.93, as minutes:seconds.tenths hundreths) to be converted to seconds (95.93)
#' @return returns the value of the string \code{x} which represents a time in track format (mm:ss.th) and converts it to seconds
#'
#' @examples math_format("1:35.93")
#' math_format("16:45.19")
#' math_format("25.43")
#' math_format(c("1:35.93", "16:45.19", NA, "25.43"))
#'
#' @export


math_format <- function(x) {
  x <- map_dbl(x, math_format_helper)
  return(x)
}
