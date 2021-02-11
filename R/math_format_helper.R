#' Helper function for formatting mm:ss.th times as seconds
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @importFrom stringr str_detect
#' @importFrom stringr str_split_fixed
#'
#' @param x A character vector of time(s) in track format (e.g. 1:35.93) to be converted to seconds (95.93)
#' @return a numeric value representing a time or distance.  Units are not included


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
    x <- as.numeric(str_remove(x, "m$"))
  } else {
    as.numeric(x)
  }
  return(as.numeric(x))

}
