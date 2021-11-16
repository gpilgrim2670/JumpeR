#' Cleans relay events
#'
#' Cleans results pulled from Flash Results html tables for relay events. Can
#' present cleaned data in wide or long format.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr across
#' @importFrom dplyr na_if
#' @importFrom dplyr select
#' @importFrom dplyr matches
#' @importFrom stringr str_trim
#' @importFrom stringr str_remove
#' @importFrom stringr str_split_fixed
#' @importFrom stringr str_extract
#' @importFrom stringr str_detect
#'
#' @param df a data frame of relay event data from Flash Results
#' @param wide_format_relay should df be presented in wide format (default is
#'   \code{FALSE})?
#' @return a cleaned version of df
#'
#' @seealso \code{flash_clean_relay_events} is a helper function inside
#'   \code{\link{flash_parse_table}}


flash_clean_relay_events <- function(df, wide_format_relay) {

  # testing
  # url_200semis <- "https://flashresults.com/2017_Meets/Outdoor/06-22_USATF/004-2-02.htm"
  # url_2017_200 <- "https://flashresults.com/2015_Meets/Outdoor/06-25_USATF/004-3-01.htm"
  # DMR <- "https://www.flashresults.com/2018_Meets/Indoor/03-09_NCAA/026-1-01.htm"
  # df <- DMR %>%
  #   flash_parse_table()

  if (wide_format_relay == FALSE) {

    df <- df %>%
      data.frame() %>%
      dplyr::select(-dplyr::matches("X\\.\\d+")) %>%
      dplyr::mutate(Time = stringr::str_remove(Time, "(?<!D)[Q|q]")) %>%
      dplyr::mutate(dplyr::across(
        dplyr::matches("[0-9]"),
        ~ stringr::str_remove(.x, " ?\\[(.*)\\]")
      ))


    varying_cols <-
      names(df)[grep("(^X\\d)|(^Lap)|(^L\\d)", names(df))] # determine names of varying columns

    # only attempt to convert to long format if there are actual split columns present
    if (length(varying_cols) > 0) {
      df <- flash_pivot_longer(df, varying = varying_cols)
    }
  } else {
    df <- df %>%
      dplyr::select(-dplyr::matches("X\\.\\d+")) %>%
      flash_col_names() %>%
      dplyr::mutate(dplyr::across(
        dplyr::matches("Split_"),
        ~ stringr::str_remove(.x, " ?\\[(.*)\\]")
      ))
  }

  # begin actual function
  clean_relay_data <- df %>%
    dplyr::mutate(Time = stringr::str_remove(Time, "(?<!D)[Q|q]")) %>%
    dplyr::rename("Result" = "Time") %>%
    dplyr::mutate(
      Tiebreaker = dplyr::case_when(
        stringr::str_detect(Result, "\\(\\d{1,2}\\.\\d{3}\\)") == TRUE ~ stringr::str_extract(Result, "\\d{1,2}\\.\\d{3}"),
        # pull tiebreakers out of Results column
        TRUE ~ "NA"
      )
    ) %>%
    dplyr::mutate(Result = stringr::str_remove(Result, "\\(\\d{1,2}\\.\\d{3}\\)")) %>% # remove tiebreakers
    dplyr::mutate(Result = stringr::str_remove(Result, "\\\n[:upper:]{1,2}")) %>% # remove PB, SB type strings
    dplyr::na_if("NA") %>%
    dplyr::mutate(dplyr::across(where(is.character), stringr::str_trim))  # remove whitespaces

  # if("Team" %in% names(clean_relay_data) == FALSE){
  #   clean_relay_data <- clean_relay_data %>%
  #     dplyr::mutate(
  #       Team = Name
  #       Team = stringr::str_split_fixed(Name, "\\\n", 3)[, 2],
  #       Name = stringr::str_split_fixed(Name, "\\\n", 3)[, 1]
  #     )
  # }

  return(clean_relay_data)

}

#' @rdname flash_clean_relay_events
#' @export
relay_events <- flash_clean_relay_events
