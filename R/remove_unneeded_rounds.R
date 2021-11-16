#' Removes unneeded rounds columns within \code{tf_parse}
#'
#' Inside of \code{tf_parse} & \code{tf_parse}, removes round columns that do
#' not have an associated round_attempts column
#'
#' @importFrom dplyr select
#' @importFrom dplyr all_of
#' @importFrom stringr str_remove
#' @importFrom stringr str_subset
#'
#' @param x data frame with columns called both "Round_X" and "Round_X_Results"
#'   where X is a number
#' @return returns a data frame where Round_X columns that do not have a
#'   corresponding Round_X_Results have been removed
#'
#' @seealso \code{remove_unneeded_rounds} runs inside \code{\link{flash_parse}}
#'   & \code{\link{tf_parse}}

remove_unneeded_rounds <- function(x) {
  attempt_cols <-
    stringr::str_remove(stringr::str_subset(names(x), "^Round_"),
                        "_Attempts?_?\\d{0,}")

  keep_cols <- attempt_cols[duplicated(attempt_cols)]

  remove_cols <- attempt_cols[!attempt_cols %in% keep_cols]

  x <- x %>%
    dplyr::select(-dplyr::all_of(remove_cols))

  return(x)

}
