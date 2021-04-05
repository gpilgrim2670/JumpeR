#' Cleans sprint events
#'
#' Cleans sprint event results pulled from Flash Results html tables.  Distance events are generally those with lengths of less than 400m.  Can present cleaned data in wide or long format.
#'
#' @author Gregory A. Pilgrim \email{gpilgrim2670@@gmail.com} and George M. Perry
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr across
#' @importFrom stringr str_trim
#' @importFrom stringr str_remove
#'
#' @param df a data frame of sprint event data from Flash Results
#' @param wide_format_clean should df be presented in wide format (default is \code{FALSE})?
#' @return a cleaned version of df
#'
#' @seealso \code{flash_clean_sprint_events} is a helper function inside \code{\link{flash_parse_table}


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
    dplyr::mutate(dplyr::across(where(is.character), stringr::str_trim)) # remove whitespaces

  return(clean_sprint_data)

}