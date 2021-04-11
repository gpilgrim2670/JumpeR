#' Cleans horizontal events
#'
#' Cleans horizontal event results pulled from Flash Results html tables.  Can present cleaned data in wide or long format.
#'
#' @author Gregory A. Pilgrim \email{gpilgrim2670@@gmail.com} and George M. Perry
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr everything
#' @importFrom dplyr na_if
#' @importFrom dplyr contains
#' @importFrom stringr str_split_fixed
#' @importFrom stringr str_remove
#' @importFrom stringr str_extract
#'
#' @param df a data frame of horizontal event data from Flash Results
#' @param wide_format_horizontal should df be presented in wide format (default is \code{FALSE})?
#' @return a cleaned version of df
#'
#' @seealso \code{flash_clean_horizontal_events} is a helper function inside \code{\link{flash_parse_table}}

flash_clean_horizontal_events <- function(df, wide_format_horizontal = wide_format_clean) {

  # testing
  # url_TJ <- "https://flashresults.com/2017_Meets/Outdoor/06-22_USATF/029-1_compiledSeries.htm"
  # url_SP <- "https://flashresults.com/2015_Meets/Outdoor/05-28_NCAAEast/017-1_compiledSeries.htm"

  df <- df %>%
    data.frame() %>%
    dplyr::select(Place, Name, dplyr::starts_with("R"), Event, Gender, dplyr::contains("Order"), dplyr::contains("Wind"))

  if (wide_format_horizontal == FALSE) {

    # need this as a separate step because `varying` reaches for names(df) and those are changed by `select` above
    df <- df %>%
      reshape(
        direction = "long",
        varying = grep("^R", names(df)),
        sep = "",
        timevar = "Round",
        ids = row.names(df),
        v.names = "Result"
      ) %>%
      dplyr::select(-id)

    rownames(df) <- NULL # reshape sets row names, remove them
  }

  clean_horizontal_data <- df %>%
    dplyr::mutate(
      # Flight = stringr::str_split_fixed(Name, "\\\n", 3)[, 3],
      Flight = stringr::str_extract(Name, "(?<=Flight\\:\\s{1,3})\\d{1,}"),
      Team = stringr::str_split_fixed(Name, "\\\n", 3)[, 2],
      Name = stringr::str_split_fixed(Name, "\\\n", 3)[, 1]
    ) %>%
    dplyr::mutate(
      Wind = stringr::str_extract(Result, "(?<=w\\:)(\\+|\\-)?\\d\\.\\d"),
      Standard = stringr::str_split_fixed(Result, "\\\n", 3)[, 2],
      Result = stringr::str_split_fixed(Result, "\\\n", 3)[, 1]
    ) %>%
    # dplyr::mutate(Flight = stringr::str_remove(Flight, "Flight\\:\\s+")) %>%
    # dplyr::mutate(Wind = stringr::str_remove(Wind, "\\\n"),
    #               Wind = stringr::str_remove(Wind, "w\\:")
    #               ) %>%
    dplyr::na_if("") %>%
    dplyr::select(-Standard)

  # Drops all-NA wind column from throws and indoor meets
  clean_horizontal_data <- Filter(function(x)
    ! all(is.na(x)), clean_horizontal_data)

  return(clean_horizontal_data)
}

#' @rdname flash_clean_horizontal_events
#' @export
horizontal_events <- flash_clean_horizontal_events
