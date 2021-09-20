#' Cleans horizontal events
#'
#' Cleans horizontal event results pulled from Flash Results html tables.  Can
#' present cleaned data in wide or long format.
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr everything
#' @importFrom dplyr na_if
#' @importFrom dplyr contains
#' @importFrom dplyr filter
#' @importFrom dplyr rename_with
#' @importFrom dplyr starts_with
#' @importFrom stringr str_split_fixed
#' @importFrom stringr str_remove
#' @importFrom stringr str_extract
#'
#' @param df a data frame of horizontal event data from Flash Results
#' @param wide_format_horizontal should df be presented in wide format (default
#'   is \code{FALSE})?
#' @return a cleaned version of df
#'
#' @seealso \code{flash_clean_horizontal_events} is a helper function inside
#'   \code{\link{flash_parse_table}}

flash_clean_horizontal_events <- function(df, wide_format_horizontal = wide_format_clean) {

  #### testing ####
  # url_TJ <- "https://flashresults.com/2017_Meets/Outdoor/06-22_USATF/029-1_compiledSeries.htm"
  # url_SP <- "https://flashresults.com/2015_Meets/Outdoor/05-28_NCAAEast/017-1_compiledSeries.htm"
  # df <- "https://www.flashresults.com/2021_Meets/Outdoor/04-16_VirginiaChallenge/035-1_compiledSeries.htm" %>%
  #   flash_parse_table()

  # df <- "https://www.flashresults.com/2021_Meets/Indoor/03-11_NCAA/033-3_compiledSeries.htm" %>%
  #   flash_parse_table()

  # df <-
  #   "https://flashresults.com/2015_Meets/Outdoor/05-28_NCAAEast/017-1_compiledSeries.htm" %>%
  #   flash_parse_table(wide_format = FALSE)


  # df <-
  #   "https://www.flashresults.com/2019_Meets/Outdoor/06-05_NCAAOTF-Austin/015-1_compiledSeries.htm" %>%
  #   flash_parse_table(wide_format = TRUE)

  df <- df %>%
    data.frame() %>%
    dplyr::select(
      Place,
      Name,
      dplyr::starts_with("R"),
      Event,
      Gender,
      dplyr::contains("Order"),
      dplyr::contains("Wind"),
      # dplyr::contains("Best"),
      dplyr::contains("Final"),
      dplyr::contains("Points"), # for decathlon points
      dplyr::contains("Date"),
    )

  if (wide_format_horizontal == FALSE) {

    varying_cols <- grep("^R", names(df))

    # only attempt to convert to long format if there are actual round columns present
    if(length(varying_cols) > 0) {

    # need this as a separate step because `varying` reaches for names(df) and those are changed by `select` above
    df <- df %>%
      reshape(
        direction = "long",
        varying = varying_cols,
        sep = "",
        timevar = "Round",
        ids = row.names(df),
        v.names = "Result"
      ) %>%
      dplyr::select(-id)

    rownames(df) <- NULL # reshape sets row names, remove them
    }
  }

  clean_horizontal_data <- df %>%
    # dplyr::rename_with(cols = dplyr::starts_with("Best"), ~stringr::str_remove(., "(?<=(Best)).*")) %>%
    dplyr::mutate(
      # Flight = stringr::str_split_fixed(Name, "\\\n", 3)[, 3],
      Flight = stringr::str_extract(Name, "(?<=Flight\\:\\s{1,3})\\d{1,}"),
      Team = stringr::str_split_fixed(Name, "\\\n", 3)[, 2],
      Name = stringr::str_split_fixed(Name, "\\\n", 3)[, 1]
    )

  if("Result" %in% names(clean_horizontal_data)){
    clean_horizontal_data <- clean_horizontal_data %>%
    dplyr::mutate(
      Wind = stringr::str_extract(Result, "(?<=w\\:)(\\+|\\-)?\\d\\.\\d"),
      # Wind = dplyr::case_when(is.na(Wind) == TRUE ~ stringr::str_extract(Result, "(?<=\\\n)(\\+|\\-).*$"), # sometimes wind isn't denoted with a w:
      #                         TRUE ~ ""),
      Wind = dplyr::case_when(is.na(Wind) == TRUE ~ stringr::str_extract(Result, "(?<=\\\n)(\\+|\\-)?\\d\\.\\d(\\[\\d{1,2}\\])?$"), # sometimes wind isn't denoted with a w:
                              TRUE ~ ""),
      Wind = str_remove(Wind, "\\[\\d{1,2}\\]"),
      Standard = stringr::str_split_fixed(Result, "\\\n", 3)[, 2],
      Result = stringr::str_split_fixed(Result, "\\\n", 3)[, 1]
    ) %>%
    # dplyr::mutate(Flight = stringr::str_remove(Flight, "Flight\\:\\s+")) %>%
    # dplyr::mutate(Wind = stringr::str_remove(Wind, "\\\n"),
    #               Wind = stringr::str_remove(Wind, "w\\:")
    #               ) %>%
    dplyr::na_if("") %>%
    dplyr::filter(is.na(Result) == FALSE) %>%
    dplyr::select(-Standard)
  }

  if("Finals_Result" %in% names(clean_horizontal_data)){

    clean_horizontal_data <- clean_horizontal_data %>%
      dplyr::mutate(Finals_Result = stringr::str_split_fixed(Finals_Result, "\\\n", 2)[, 1]) %>%
      wind_from_rounds() %>%
      dplyr::mutate(Finals_Result = stringr::str_remove(Finals_Result, " \\(\\d{1,3}\\-\\d{1,2}\\.?\\d{0,2}")) %>%  # remove results in standard units
      dplyr::mutate(dplyr::across(dplyr::matches("^R"), ~ stringr::str_remove(.x, "\n.*\n?.*\r?.*\n?.*\n?.*\n?.*\n?.*\n?.*\n?.*"))) %>%
      dplyr::mutate(dplyr::across(!dplyr::matches("Wind"), ~ stringr::str_remove(.x, "\\s?w\\:\\+?\\-?\\d?\\d?\\.?\\d?\\s?")))
      # dplyr::rename("Finals_Result" = Best)

    if(wide_format_horizontal == TRUE){
      clean_horizontal_data <- clean_horizontal_data %>%
        dplyr::select(-dplyr::matches("^Result"))
    }

  }

  #### renaming columns ####
  names(clean_horizontal_data) <- stringr::str_replace_all(names(clean_horizontal_data), "Rnd\\.(\\d)", "Round_\\1")
  names(clean_horizontal_data) <- stringr::str_replace_all(names(clean_horizontal_data), "Round\\.(\\d)", "Round_\\1")

  # reorder wind and round columns #
  clean_horizontal_data <- clean_horizontal_data %>%
    dplyr::select(!dplyr::matches("R.*\\_"), stringr::str_sort(names(.), numeric = TRUE))

  # Drops all-NA wind column from throws and indoor meets
  clean_horizontal_data <- Filter(function(x)
    ! all(is.na(x)), clean_horizontal_data)

  return(clean_horizontal_data)
}

#' @rdname flash_clean_horizontal_events
#' @export
horizontal_events <- flash_clean_horizontal_events
