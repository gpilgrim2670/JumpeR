#' Cleans vertical events
#'
#' Cleans vertical event results pulled from Flash Results html tables.  Can present cleaned data in wide or long format.
#'
#' @author Gregory A. Pilgrim \email{gpilgrim2670@@gmail.com} and George M. Perry
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr everything
#' @importFrom stringr str_split_fixed
#'
#' @param df a data frame of vertical event data from Flash Results
#' @param wide_format_clean should df be presented in wide format (default is \code{FALSE})?
#' @return a cleaned version of df
#'
#' @seealso \code{flash_clean_vertical_events} is a helper function inside \code{\link{flash_parse_table}}

flash_clean_vertical_events <- function(df, wide_format_vertical = wide_format_clean) {

  # url_PV <- "https://flashresults.com/2015_Meets/Outdoor/06-25_USATF/025-1-01.htm"
  # url_2017_PV <- "https://flashresults.com/2017_Meets/Outdoor/06-22_USATF/026-1-01.htm"
  # url_2017_HJ <- "https://flashresults.com/2017_Meets/Outdoor/06-22_USATF/024-1-01.htm"
  # url_ncaa_PV <- "https://flashresults.com/2015_Meets/Outdoor/05-28_NCAAEast/033-1-01.htm"
  # df <- flash_parse_table(url_PV)

  df <- df[-1, ] %>%
    data.frame() # remove tibble class because it doesn't work well with reshape

  if (wide_format_vertical == FALSE) {
    df <- df %>%
      reshape(
        direction = "long",
        varying = grepl("[0-9]", names(df)),
        sep = "",
        timevar = "Height",
        ids = row.names(df)
      ) %>%
      dplyr::select(dplyr::everything(), "Result" = "X",-id)

    rownames(df) <- NULL # reshape sets row names, remove them
  }

  df <- df %>%
    dplyr::mutate(Team = stringr::str_split_fixed(Name, "\\\n", 2)[,2],
                  Name = stringr::str_split_fixed(Name, "\\\n", 2)[,1],)

  # original version, uses tidyr
  # clean_vertical_data <- df %>%
  #   tidyr::pivot_longer(matches("[0-9]"),
  #                names_to = "Height",
  #                values_to = "Result") %>%
  #   tidyr::separate(Name, c("Name", "Affiliation"), "\\n")


  # if (tidy_table == "table") {

    # original version, uses tidyr
    # clean_vertical_data <- clean_vertical_data %>%
    #   tidyr::pivot_wider(names_from = Height, values_from = Result)

    # return(clean_vertical_data)
  # }

  return(df)
}

#' @rdname flash_clean_vertical_events
#' @export
vertical_events <- flash_clean_vertical_events
