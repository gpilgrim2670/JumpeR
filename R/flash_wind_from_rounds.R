#' Pulls Wind Data by Round from Horizontal Flash Table Results
#'
#' In some Flash Table results for horizontal events (long jump, triple jump,
#' throwing events), a wind value is listed for each round/attempt.  This
#' function pulls out those wind values into columns called "Round_1_Wind" (if
#' the round data is in a column called Round_1)
#'
#' @importFrom dplyr left_join
#' @importFrom purrr map
#' @importFrom purrr reduce
#' @importFrom stringr str_sort
#' @importFrom stringr str_detect
#'
#' @param df a data frame containing results with wind data included in round
#'   columns.
#' @return a data frame with all wind data in separate (tidy) columns
#' @seealso \code{wind_from_rounds} is a helper function inside
#'   \code{\link{flash_clean_horizontal_events}}


wind_from_rounds <- function(df) {
  #### Error Messages ####

  if (is.data.frame(df) == FALSE) {
    stop("df must be a data frame")
  }

  #### testing ####
  # df <- clean_horizontal_data

  #### Actual Function ####

  #### Remove Empty Columns ####
  df <- df[, colSums(is.na(df)) != nrow(df)]

  #### Locate Split Columns ####
  round_cols <- names(df)[stringr::str_detect(names(df), "^R")]

  if (length(round_cols) > 0) {
    round_cols <- stringr::str_sort(round_cols, numeric = TRUE)
    i <- seq(1, length(round_cols), by = 1)

    #### Run Helper Function ####
    suppressMessages(
      df_corrected <- purrr::map(
        i,
        wind_from_rounds_helper,
        df = df,
        round_cols = round_cols,
      ) %>%
        purrr::reduce(dplyr::left_join) %>%
        unique()
    )

    return(df_corrected)
  } else {
    return(df)
  }
}

#' Helper function for extracting wind data from round columns
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr na_if
#' @importFrom stringr str_extract
#' @importFrom stringr str_remove
#'
#' @param df a data frame containing round columns with both results and wind
#'   data
#' @param i list of values to iterate along
#' @param round_cols list of columns containing results and wind values by round
#' @param ... other arguments as needed
#' @return a list of data frames with all wind data for each round in a separate
#'   (tidy) column

wind_from_rounds_helper <- function(df = df, i, round_cols, ...) {


  # df <- clean_horizontal_data

  #### testing ####
  # df <- clean_horizontal_data

  # round_cols <- names(clean_horizontal_data)[
  #   stringr::str_detect(names(clean_horizontal_data), "^R")]

  #### wind string definition ####
  wind_string <- "(?<!\\d)(\\+|\\-)\\d\\.\\d|w\\:\\+?\\-?\\d\\.\\d"

  #### actual function ####

  wind_cols <- paste(round_cols, "Wind", sep = "_")
  # define positions of column ranges

  df <- df %>%
    dplyr::mutate(!!as.name(wind_cols[i]) := stringr::str_extract(!!as.name(round_cols[i]), wind_string)) %>%
    dplyr::mutate(!!as.name(wind_cols[i]) := stringr::str_remove(!!as.name(wind_cols[i]), "w\\:?")) %>%
    dplyr::na_if("NA")

  df <- df[, colSums(is.na(df)) != nrow(df)]

  return(df)
}
