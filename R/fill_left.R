#' Shifts non-NA values to left in data frame
#'
#' Moves non-NA data left into NA spaces, then removes all columns that contain
#' only NA values
#'
#' @param df a data frame having some NA values
#' @return a data frame where all values have been pushed left, replacing NAs,
#'   and all columns containing only \code{NA} values have been removed
#'
#' @seealso \code{fill_left} is a helper function inside \code{lines_sort}

fill_left <- function(df) {
  df <- as.data.frame(t(apply(df, 1, function(x) {
    return(c(x[!is.na(x)], x[is.na(x)]))
  })))

  df <- Filter(function(x)
    ! all(is.na(x)), df)

}
