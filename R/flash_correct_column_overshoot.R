#' Corrects column index overshoots when naming columns based on their contents
#'
#' When naming columns based on the contents of a data frame the position of a
#' particular term, e.g. "Athlete" is used to name a column "Athlete".  If there
#' is a blank row at the top of the data frame then the position of "Athlete"
#' will be offset by the number of columns in the data frame.  This function
#' corrects for that.
#'
#' @importFrom purrr map
#'
#' @param x a column position index
#' @param df a data frame with missing column names
#' @return a correct index for column x
#'
#' @seealso \code{flash_correct_column_overshoot} is a helper function inside
#'   \code{\link{flash_parse_table}}

flash_correct_column_overshoot <- function(x = NA, df) {
  x <- purrr::map(x, flash_correct_column_overshoot_helper, df = df) %>%
    unlist()

  return(x)
}

#' Vectorizes \code{flash_correct_column_overshoot}
#'
#' When naming columns based on the contents of a data frame the position of a
#' particular term, eg "Athlete" is used to name a column "Athlete".  If there
#' is a blank row at the top of the data frame then the position of "Athlete"
#' will be offset by the number of columns in the data frame.  This function
#' corrects for that.
#'
#' @param x a column position index
#' @param df a data frame with missing column names
#' @return a correct index for column x
#'
#' @seealso \code{flash_correct_column_overshoot} is a helper function inside
#'   \code{\link{flash_parse_table}}

flash_correct_column_overshoot_helper <- function(x, df) {
  if (is.na(x)) {
    return(x)
  } else if (identical(x, integer(0))) {
    return(integer(0))
  } else {
    multiplier <- floor(x / ncol(df))
    x <- ifelse(x > ncol(df), x - (ncol(df) * multiplier) , x)

    return(x)
  }
}
