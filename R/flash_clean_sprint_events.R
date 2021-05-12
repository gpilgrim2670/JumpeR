#' Cleans sprint events
#'
#' Cleans results pulled from Flash Results html tables for sprint events.
#' Sprint events are generally those with lengths of less than 400m.  Can
#' present cleaned data in wide or long format.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr across
#' @importFrom dplyr na_if
#' @importFrom stringr str_trim
#' @importFrom stringr str_remove
#' @importFrom stringr str_split_fixed
#' @importFrom stringr str_extract
#' @importFrom stringr str_detect
#'
#' @param df a data frame of sprint event data from Flash Results
#' @return a cleaned version of df
#'
#' @seealso \code{flash_clean_sprint_events} is a helper function inside
#'   \code{\link{flash_parse_table}}


flash_clean_sprint_events <- function(df) {

  # testing
  # url_200semis <- "https://flashresults.com/2017_Meets/Outdoor/06-22_USATF/004-2-02.htm"
  # url_2017_200 <- "https://flashresults.com/2015_Meets/Outdoor/06-25_USATF/004-3-01.htm"
  # df <- url_200semis %>%
  #   flash_parse_table()

  # begin actual function
  clean_sprint_data <- df %>%
    dplyr::mutate(Time = stringr::str_remove(Time, "Q|q")) %>%
    dplyr::rename("Result" = "Time") %>%
    dplyr::mutate(Tiebreaker = dplyr::case_when(stringr::str_detect(Result, "\\(\\d{1,2}\\.\\d{3}\\)") == TRUE ~ stringr::str_extract(Result, "\\d{1,2}\\.\\d{3}"), # pull tiebreakers out of Results column
                                                TRUE ~ "NA")) %>%
    dplyr::mutate(Result = stringr::str_remove(Result, "\\(\\d{1,2}\\.\\d{3}\\)")) %>% # remove tiebreakers
    dplyr::mutate(Result = stringr::str_remove(Result, "\\\n[:upper:]{1,2}")) %>% # remove PB, SB type strings
    dplyr::na_if("NA") %>%
    dplyr::mutate(dplyr::across(where(is.character), stringr::str_trim))  # remove whitespaces

  if("Team" %in% names(clean_sprint_data) == FALSE){
    clean_sprint_data <- clean_sprint_data %>%
    dplyr::mutate(
      Team = stringr::str_split_fixed(Name, "\\\n", 3)[, 2],
      Name = stringr::str_split_fixed(Name, "\\\n", 3)[, 1]
    )
  }

  return(clean_sprint_data)

}

#' @rdname flash_clean_sprint_events
#' @export
sprint_events <- flash_clean_sprint_events
