#' Transform list of lists into dataframe
#'
#' Converts list of lists, with all sub-lists having the same number of elements into a dataframe where each sub-list is a row and each element a column
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @param x a list of lists, with all sub-lists having the same length
#' @return a dataframe where each sub-list is a row and each element of that sub-list is a column
#'
#' @seealso \code{list_transform} is a helper function used inside of \code{tf_parse} and \code{event_parse}


list_transform <- function(x) {
  df <- as.data.frame(t(as.data.frame(x, stringsAsFactors = FALSE)),
                      row.names = FALSE,
                      stringsAsFactors = FALSE)
  return(df)
}
