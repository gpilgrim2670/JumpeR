#' Cleans distance events
#'
#' Cleans distance event results pulled from Flash Results html tables.  Distance events are generally those with lengths of 400m or greater.  Can present cleaned data in wide or long format.
#'
#' @author Gregory A. Pilgrim \email{gpilgrim2670@@gmail.com} and George M. Perry
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom dplyr rename
#' @importFrom stringr str_trim
#' @importFrom stringr str_remove
#'
#' @param df a data frame of distance event data from Flash Results
#' @param wide_format_clean should df be presented in wide format (default is \code{FALSE})?
#' @return a cleaned version of df
#'
#' @seealso \code{flash_clean_distance_events} is a helper function inside \code{\link{flash_parse_table}}

flash_clean_distance_events <- function(df, wide_format_distance = wide_format_clean) {

  # testing
  # url_1500 <- "https://flashresults.com/2015_Meets/Outdoor/05-28_NCAAEast/005-1-03.htm"
  # df <- flash_parse_table(url_1500)

  df <- df %>%
    data.frame() %>%
    dplyr::mutate(Time = stringr::str_remove(Time, "Q")) %>%
    dplyr::mutate(dplyr::across(matches("[0-9]"), ~stringr::str_remove(.x, " ?\\[(.*)\\]")))

  if (wide_format_distance == FALSE) {

    varying_cols <- names(df)[grep("^X\\d", names(df))] # determine names of varying columns

    df <- df %>%
      reshape(
        direction = "long",
        # varying = grep("^X\\d", names(df)),
        varying = varying_cols,
        sep = "",
        timevar = "Split_Distance",
        ids = row.names(df),
        v.names = "Split_Time"
      ) %>%
      dplyr::select(-id) %>%
      dplyr::mutate(Split_Distance = varying_cols[Split_Distance], # reshape converts varying cols to indexes for some reason, this is a workaround
                    Split_Distance = stringr::str_remove(Split_Distance, "^X"),
                    Split_Distance = stringr::str_remove(Split_Distance, "[m|M]$"))

    # old version, requires tidyr
    # clean_distance_data <- df %>%
    #   tidyr::pivot_longer(matches("[0-9]"), names_to = "SplitDistance", values_to = "SplitTime") %>%
    #   mutate(Time = str_remove(Time, "Q"), SplitTime = str_remove(SplitTime, "\\[(.*)\\]")) %>%
    #   rename("Result" = "Time")

  }

  clean_distance_data <- df %>%
    dplyr::rename("Result" = "Time") %>%
    dplyr::mutate(dplyr::across(where(is.character), stringr::str_trim)) # remove whitespaces

  # drops all-NA columns
  clean_distance_data <- Filter(function(x)
    ! all(is.na(x)), clean_distance_data)

  return(clean_distance_data)
}
