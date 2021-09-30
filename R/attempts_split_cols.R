#' Creates new columns for splitting attempts strings
#'
#' Given a data frame with columns "Round_1_Attempts" it will produce three
#' columns, for each of the attempts in round 1 (Round_1_Attempt_1,
#' Round_1_Attempt_2 etc.)
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_split_fixed
#'
#' @param i iterative value
#' @param data output from \code{tf_parse}
#' @param new_cols a list of new column names to make
#' @param old_cols a list of old columns to split
#' @return returns a data frame with Round_X_Attempts columns split into
#'   individual attempts inside \code{tf_parse}
#'
#' @seealso \code{attempts_split_cols} is a helper function inside
#'   \code{attempts_split}

attempts_split_cols <- function(i, data, new_cols, old_cols) {
  # new_cols <- cols_to_create
  # old_cols <- cols_to_split
  # i <- 1
  # data <- flash_data

  i_c <- (3 * i) - 2

  data <- data %>%
    mutate(!!new_cols[i_c] := stringr::str_split_fixed(!!as.name(old_cols[i]), "", n = 3)[, 1]) %>%
    mutate(!!new_cols[i_c + 1] := stringr::str_split_fixed(!!as.name(old_cols[i]), "", n = 3)[, 2]) %>%
    mutate(!!new_cols[i_c + 2] := stringr::str_split_fixed(!!as.name(old_cols[i]), "", n = 3)[, 3])

  return(data)

}
