#' Cleans vertical events
#'
#' Cleans vertical event results pulled from Flash Results html tables.  Can
#' present cleaned data in wide or long format.
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr everything
#' @importFrom dplyr starts_with
#' @importFrom dplyr rename_with
#' @importFrom stringr str_remove
#' @importFrom stringr str_split_fixed
#'
#' @param df a data frame of vertical event data from Flash Results
#' @param wide_format_vertical should df be presented in wide format (default is
#'   \code{FALSE})?
#' @return a cleaned version of df
#'
#' @seealso \code{flash_clean_vertical_events} is a helper function inside
#'   \code{\link{flash_parse_table}}

flash_clean_vertical_events <- function(df, wide_format_vertical = wide_format_clean) {

  #### testing ####
  # url_PV <- "https://flashresults.com/2015_Meets/Outdoor/06-25_USATF/025-1-01.htm"
  # url_2017_PV <- "https://flashresults.com/2017_Meets/Outdoor/06-22_USATF/026-1-01.htm"
  # url_2017_HJ <- "https://flashresults.com/2017_Meets/Outdoor/06-22_USATF/024-1-01.htm"
  # url_ncaa_PV <- "https://flashresults.com/2015_Meets/Outdoor/05-28_NCAAEast/033-1-01.htm"
  # df <- flash_parse_table(url_PV)
  # df <- "https://www.flashresults.com/2021_Meets/Indoor/03-11_NCAA/033-6_compiled.htm" %>%
  #   flash_parse_table()
  # df <- "https://flashresults.com/2017_Meets/Indoor/01-13_AggieTeam/011-1-01.htm" %>%
  #   flash_parse_table()

  #### begin actual function ####

  if (any(stringr::str_detect(df[1,], "(^Place$)|(^Athlete$)|(^Order$)|(^Jump$)"),
          na.rm = TRUE)) {
    df <- df[-1, ] # remove first row if it contains header information
  }

  df <- df %>% # remove tibble class because it doesn't work well with reshape
    data.frame()

  # clean up column names
  # names(df) <- str_replace(names(df), "Best\\.Jump", "Best")
  names(df) <- str_replace(names(df), "Fl\\.\\.Pl\\.", "Flight_Place")

  # convert to long format, requires columns like 3.31m etc.



  if (any(stringr::str_detect(names(df), "[:lower:]\\d\\.\\d\\dm"))) {
    wide_format_vertical <- TRUE
    # if("Finals_Result" %in% names(df))
    message("Duplicate column names fixed in results.  Please check column names.")
  }

  # if (all(wide_format_vertical == FALSE & any(stringr::str_detect(names(df), "[0-9]")))) {
  if (wide_format_vertical == FALSE) {
    varying_cols <- names(df)[grepl("[0-9]", names(df))]

    # only attempt to convert to long format if there are actual round columns present
    if (length(varying_cols) > 0) {
      df <- df %>%
        reshape(
          direction = "long",
          varying = varying_cols,
          sep = "",
          timevar = "Height",
          ids = row.names(df)
        ) %>%
        dplyr::select(dplyr::everything(), "Result" = "X",-id) %>%
        dplyr::filter(is.na(Result) == FALSE) # remove rows without a result

      rownames(df) <- NULL # reshape sets row names, remove them
    }
  }

  # df <- df %>%
  #   dplyr::rename_with(cols = dplyr::starts_with("Best"), ~stringr::str_remove(., "(?<=(Best)).*"))

  # break out team names, which are sometimes included in Name
  if ("Team" %!in% names(df)) {
    df <- df %>%
      dplyr::mutate(
        Team = stringr::str_split_fixed(Name, "\\\n", 2)[, 2],
        Name = stringr::str_split_fixed(Name, "\\\n", 2)[, 1],
      )
  }

  # remove standard unit heights from Best
  if ("Finals_Result" %in% names(df)) {
    df <- df %>%
      dplyr::mutate(
        Finals_Result = stringr::str_split_fixed(Finals_Result, "\\\n", 2)[, 1],
        Finals_Result = stringr::str_split_fixed(Finals_Result, "\\s", 2)[, 1]
      )
  }

  # remove standard unit heights from Height
  if ("Height" %in% names(df)) {
    df <- df %>%
      dplyr::mutate(Height = stringr::str_replace_all(Height, "m\\d{0,}\\-?\\.?\\d{0,}\\.?\\d{0,}", "m"))
  }

  # remove residual standard units from names
  # they're sometimes present in pole vault results

  names(df) <- stringr::str_remove(names(df), "(?<=\\dm)\\.\\d{1,2}$")

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
