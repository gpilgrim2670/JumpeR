#' Collects removes unneeded attempts columns within \code{tf_parse}
#'
#' Inside of \code{tf_parse} & \code{tf_parse}, removes attempts columns that do not have an assoicated attempt_result
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @importFrom dplyr select
#' @importFrom dplyr all_of
#' @importFrom stringr str_remove
#' @importFrom stringr str_subset
#'
#' @param x dataframe with columns called both "Attempt_X" and "Attempt_X_Result" where X is a number
#' @return returns a dataframe where Attempt_X columns that do no have a corresponding Attempt_X_Result have been removed
#'
#' @seealso \code{remove_unneeded_attempts} runs inside \code{\link{flash_parse}} & \code{\link{tf_parse}}

remove_unneeded_attempts <- function(x) {

  attempt_cols <-
    stringr::str_remove(stringr::str_subset(names(x), "^Attempt_"), "_Result")
  keep_these <- attempt_cols[duplicated(attempt_cols)]
  drop_these <- attempt_cols[!attempt_cols %in% keep_these]
  x <- x %>%
    dplyr::select(-dplyr::all_of(drop_these))

  return(x)

}
