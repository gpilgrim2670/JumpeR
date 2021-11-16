#' Formatting mm:ss.th times as seconds
#'
#' Takes a character string (or list) representing time in track format (e.g.
#' 1:35.37) and converts it to a numeric value (95.37) or a list of values
#' representing seconds.
#'
#' @importFrom purrr map_dbl
#'
#' @param x A character vector of time(s) in track format (e.g. 1:35.93, as
#'   minutes:seconds.tenths hundreths) to be converted to seconds (95.93)
#' @return returns the value of the string \code{x} which represents a time in
#'   track format (mm:ss.th) and converts it to seconds
#'
#' @examples math_format("1:35.93")
#' math_format("16:45.19")
#' math_format("25.43")
#' math_format(c("1:35.93", "16:45.19", NA, "25.43"))
#'
#' @export


math_format <- function(x) {
  x <- purrr::map_dbl(x, math_format_helper)
  return(x)
}

#' Helper function for formatting mm:ss.th times as seconds
#'
#' @importFrom stringr str_detect
#' @importFrom stringr str_split_fixed
#' @importFrom stringr str_remove
#'
#' @param x A character vector of time(s) in track format (e.g. 1:35.93) to be
#'   converted to seconds (95.93)
#' @return a numeric value representing a time or distance.  Units are not
#'   included


math_format_helper <- function(x) {

  x <- as.character(x)
  if(is.na(x) == TRUE) return(NA)
  if(x %in% c("DNS", "DNF", "NH", "DQ", "SCR", "FOUL") == TRUE) return(NA)
  if (stringr::str_detect(x, ":") == TRUE) {
    min <- as.numeric(stringr::str_split_fixed(x, ":", n = 2)[,1])
    sec <- as.numeric(stringr::str_split_fixed(x, ":", n = 2)[,2])
    if (sec >= 60) stop("Seconds must be less than 60 in a track formatted time")
    x <- (min*60) + sec
  } else if(str_detect(x, "-") == TRUE) {
    feet <- as.numeric(stringr::str_split_fixed(x, "-", n = 2)[,1])
    inches <- as.numeric(stringr::str_split_fixed(x, "-", n = 2)[,2])
    if (inches >= 12) stop("Inches must be less than 12 in a field formatted length")
    x <- round((feet*12) + inches, 2)
  } else if(str_detect(x, "m$") == TRUE) {
    x <- as.numeric(stringr::str_remove(x, "m$"))
  } else {
    as.numeric(x)
  }
  return(as.numeric(x))

}
