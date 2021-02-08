#' Removes unneeded flights columns within \code{tf_parse}
#'
#' Inside of \code{tf_parse} & \code{tf_parse}, removes flight columns that do not have an associated flight_attempts column
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @importFrom dplyr select
#' @importFrom dplyr all_of
#' @importFrom stringr str_remove
#' @importFrom stringr str_subset
#'
#' @param x dataframe with columns called both "Flight_X" and "Flight_X_Results" where X is a number
#' @return returns a dataframe where Flight_X columns that do not have a corresponding Flight_X_Results have been removed
#'
#' @seealso \code{remove_unneeded_flights} runs inside \code{\link{flash_parse}} & \code{\link{tf_parse}}

remove_unneeded_flights <- function(x) {

  attempt_cols <-
    stringr::str_remove(stringr::str_subset(names(x), "^Flight_"), "_Attempts?_?\\d{0,}")
  keep_cols <- attempt_cols[duplicated(attempt_cols)]
  remove_cols <- attempt_cols[!attempt_cols %in% keep_cols]
  x <- x %>%
    dplyr::select(-dplyr::all_of(remove_cols))

  return(x)

}
