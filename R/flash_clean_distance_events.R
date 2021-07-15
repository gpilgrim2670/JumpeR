#' Cleans distance events
#'
#' Cleans distance event results pulled from Flash Results html tables.
#' Distance events are generally those with lengths of 400m or greater.  Can
#' present cleaned data in wide or long format.
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom dplyr rename
#' @importFrom dplyr matches
#' @importFrom dplyr na_if
#' @importFrom stringr str_trim
#' @importFrom stringr str_remove
#'
#' @param df a data frame of distance event data from Flash Results
#' @param wide_format_distance should df be presented in wide format (default is
#'   \code{FALSE})?
#' @return a cleaned version of df
#'
#' @seealso \code{flash_clean_distance_events} is a helper function inside
#'   \code{\link{flash_parse_table}}

flash_clean_distance_events <- function(df, wide_format_distance = wide_format_clean) {

  # testing
  # url_1500 <- "https://flashresults.com/2015_Meets/Outdoor/05-28_NCAAEast/005-1-03.htm"
  # df <- flash_parse_table(url_1500)
  # url_steeple <- "https://www.flashresults.com/2019_Meets/Outdoor/07-25_USATF_CIS/022-1_compiled.htm"
  # url_10000 <- "https://www.flashresults.com/2019_Meets/Outdoor/07-25_USATF_CIS/013-1_compiled.htm"
  # url_racewalk <- "https://www.flashresults.com/2019_Meets/Outdoor/07-25_USATF_CIS/019-1_compiled.htm"
  #
  # df <- url_10000 %>%
  #   flash_parse_table()

  df <- df %>%
    data.frame() %>%
    dplyr::select(-dplyr::matches("X\\.\\d+")) %>%
    dplyr::mutate(Time = stringr::str_remove(Time, "(?<!D)[Q|q]")) %>%
    dplyr::mutate(dplyr::across(dplyr::matches("[0-9]"), ~stringr::str_remove(.x, " ?\\[(.*)\\]")))

  if (wide_format_distance == FALSE) {

    varying_cols <- names(df)[grep("(^X\\d)|(^Lap)|(^L\\d)", names(df))] # determine names of varying columns

    # only attempt to convert to long format if there are actual split columns present
    if(length(varying_cols) > 0) {

      df <- flash_pivot_longer(df, varying = varying_cols)

    }

  } else {
    df <- df %>%
      flash_col_names() %>%
      dplyr::mutate(dplyr::across(dplyr::matches("Split_"), ~stringr::str_remove(.x, " ?\\[(.*)\\]")))
  }

  clean_distance_data <- df %>%
    dplyr::rename("Result" = "Time") %>%
    dplyr::mutate(
      Tiebreaker = dplyr::case_when(
        stringr::str_detect(Result, "\\(\\d{1,2}\\.\\d{3}\\)") == TRUE ~ stringr::str_extract(Result, "\\d{1,2}\\.\\d{3}"),
        TRUE ~ "NA"
      )
    ) %>%
    dplyr::na_if("NA") %>%
    dplyr::mutate(Result = stringr::str_remove(Result, "\\(\\d{1,2}\\.\\d{3}\\)")) %>%
    dplyr::mutate(Result = stringr::str_remove(Result, "\\\n[:upper:]{1,2}")) %>%
    dplyr::mutate(dplyr::across(where(is.character), stringr::str_trim)) # remove whitespaces

  if ("Team" %in% names(clean_distance_data) == FALSE) {
    clean_distance_data  <- clean_distance_data  %>%
      dplyr::mutate(
        Team = stringr::str_split_fixed(Name, "\\\n", 3)[, 2],
        Name = stringr::str_split_fixed(Name, "\\\n", 3)[, 1]
      )
  }

  # drops all-NA columns
  clean_distance_data <- Filter(function(x)
    ! all(is.na(x)), clean_distance_data)

  return(clean_distance_data)
}

#' @rdname flash_clean_distance_events
#' @export
distance_events <- flash_clean_distance_events
