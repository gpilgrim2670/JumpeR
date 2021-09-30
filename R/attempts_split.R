#' Creates new columns of split attempts strings
#'
#' Given a data frame with columns "Round_1_Attempts" it will output three
#' columns, for each of the attempts in round 1 (Round_1_Attempt_1,
#' Round_1_Attempt_2 etc.)
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @importFrom dplyr all_of
#' @importFrom dplyr select
#' @importFrom stringr str_remove
#' @importFrom stringr str_subset
#' @importFrom stringr str_detect
#' @importFrom stringr str_sort
#' @importFrom purrr map
#' @importFrom purrr reduce
#'
#' @param data_to_split output from \code{read_results} followed by
#'   \code{add_row_numbers}
#' @return returns a data frame with Round_X_Attempts columns split into
#'   individual attempts inside \code{tf_parse}
#'
#' @seealso \code{attempts_split} is a helper function inside
#'   \code{\link{tf_parse}}

attempts_split <- function(data_to_split) {
  # data_to_split <- df

  #### get names of all Round_X_Attempts columns ####
  cols_to_split <-
    stringr::str_subset(names(data_to_split), "^Round_\\d{1,}_Attempts")
  if (length(cols_to_split) >= 1) {
    #### create ending for new split columns ####
    ending <- paste0("_", seq(1, 3, 1))

    #### create names for new split columns from Round_X_Attempts columns and ending ####
    cols_to_create <-
      as.vector(outer(cols_to_split, ending, paste0)) %>%
      unique() %>%
      stringr::str_remove("s") %>%
      stringr::str_sort(numeric = TRUE)

    #### map attempts_split_cols function over lists of names ####
    df_list <-
      purrr::map(
        seq(1, length(cols_to_split), 1),
        attempts_split_cols,
        data = data_to_split,
        new_cols = cols_to_create,
        old_cols = cols_to_split
      )

    #### collapse mapped results into one data frame ####
    df <- df_list %>%
      purrr::reduce(dplyr::left_join) %>%
      # purrr::reduce(cbind) %>%
      dplyr::select(-dplyr::all_of(cols_to_split))

    # df <- df %>%
    #   dplyr::select(colnames(.)[stringr::str_detect(names(.), "^Round", negate = TRUE)], sort(colnames(.)[stringr::str_detect(names(.), "^Round")]))

    return(df)

  } else {
    return(data_to_split)
  }


}

