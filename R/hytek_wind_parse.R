#' Collects splits within \code{tf_parse}
#'
#' Takes the output of \code{read_results} and, inside of \code{tf_parse},
#' extracts split times and associated row numbers
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr rename_at
#' @importFrom dplyr mutate_at
#' @importFrom dplyr rename
#' @importFrom dplyr vars
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_split
#' @importFrom stringr str_detect
#' @importFrom purrr map_lgl
#' @importFrom purrr map
#'
#' @param text output of \code{read_results} with row numbers appended by
#'   \code{add_row_numbers}
#' @return returns a data frame with wind speeds and row numbers
#'
#' @seealso \code{wind_parse_hytek} runs inside \code{\link{hytek_parse}} on the
#'   output of \code{\link{read_results}} with row numbers from
#'   \code{\link{add_row_numbers}}

wind_parse_hytek <- function(text) {

  #### Testing ####

  # text <- read_results("http://results.deltatiming.com/ncaa/tf/2019-florida-relays/print/190328F005") %>%
  #   add_row_numbers()

  # text <- read_results("http://results.deltatiming.com/tf/2019-tiger-track-classic/190405F032") %>%
  #   add_row_numbers()

  #### Actual Function ####
  ### collect row numbers from rows containing splits ###
  ### define strings ###

  wind_string <- "(?<=[:alpha:]\\s?\\()(\\+|\\-)?\\d\\.\\d|(?<=[:alpha:]\\s?\\()w\\:\\+?\\-?\\d\\.\\d"

  row_numbs <- text %>%
    .[stringr::str_detect(.,
                     wind_string)] %>%
    stringr::str_extract_all("\\d{1,}$")

  #### if there are still no valid splits return blank dataframe ####
  if (length(row_numbs) > 0) {
    minimum_row <- min(as.numeric(row_numbs))
    maximum_row <- as.numeric(length(text))

    #### help out a little, in case there are wind results that only have one space between them ####
    text <- stringr::str_replace_all(text, "(\\)) (\\d)", "\\1  \\2")

    text <- stringr::str_replace_all(text, "(?<=\\sX)\\s{5,}(?=\\d)", "  NA  ")

    #### pull out rows containing wind results, which will remove row numbers ####

      suppressWarnings(
        data_1_wind <- text %>%
          .[stringr::str_detect(.,
                           paste0(wind_string, "|\\s{2}NA\\s{2}"))] %>%
          stringr::str_replace_all("\n", "") %>%
          stringr::str_extract_all(
           wind_string
          ) %>%
          stringr::str_remove_all('\\"') %>% # to coerce into atomic vector
          stringr::str_replace_all("\\(", " ") %>%
          stringr::str_replace_all("\\)", " ") %>%
          stringr::str_remove_all("c") %>%
          stringr::str_replace_all(",", "   ") %>%
          purrr::map(trimws)
      )



    #### add row numbers back in since they were removed ####
    data_1_wind <- paste(row_numbs, data_1_wind, sep = "   ")

    #### break out by length ####
    data_1_wind <-
      unlist(purrr::map(data_1_wind, stringr::str_split, "\\s{2,}"),
             recursive = FALSE)

    data_wind_length_2 <- data_1_wind[purrr::map(data_1_wind, length) == 2]
    data_wind_length_3 <- data_1_wind[purrr::map(data_1_wind, length) == 3]
    data_wind_length_4 <- data_1_wind[purrr::map(data_1_wind, length) == 4]
    data_wind_length_5 <- data_1_wind[purrr::map(data_1_wind, length) == 5]
    data_wind_length_6 <- data_1_wind[purrr::map(data_1_wind, length) == 6]
    data_wind_length_7 <- data_1_wind[purrr::map(data_1_wind, length) == 7]
    data_wind_length_8 <- data_1_wind[purrr::map(data_1_wind, length) == 8]
    data_wind_length_9 <- data_1_wind[purrr::map(data_1_wind, length) == 9]
    data_wind_length_10 <- data_1_wind[purrr::map(data_1_wind, length) == 10]

    #### transform all lists to dataframes ####
    if (length(data_wind_length_10) > 0) {
      df_10_wind <- data_wind_length_10 %>%
        list_transform()
    } else {
      df_10_wind <- data.frame(Row_Numb = character(),
                                 stringsAsFactors = FALSE)
    }

    if (length(data_wind_length_9) > 0) {
      df_9_wind <- data_wind_length_9 %>%
        list_transform()
    } else {
      df_9_wind <- data.frame(Row_Numb = character(),
                                stringsAsFactors = FALSE)
    }

    if (length(data_wind_length_8) > 0) {
      df_8_wind <- data_wind_length_8 %>%
        list_transform()
    } else {
      df_8_wind <- data.frame(Row_Numb = character(),
                                stringsAsFactors = FALSE)
    }

    if (length(data_wind_length_7) > 0) {
      df_7_wind <- data_wind_length_7 %>%
        list_transform()
    } else {
      df_7_wind <- data.frame(Row_Numb = character(),
                                stringsAsFactors = FALSE)
    }

    if (length(data_wind_length_6) > 0) {
      df_6_wind <- data_wind_length_6 %>%
        list_transform()
    } else {
      df_6_wind <- data.frame(Row_Numb = character(),
                                stringsAsFactors = FALSE)
    }

    if (length(data_wind_length_5) > 0) {
      df_5_wind <- data_wind_length_5 %>%
        list_transform()
    } else {
      df_5_wind <- data.frame(Row_Numb = character(),
                                stringsAsFactors = FALSE)
    }

    if (length(data_wind_length_4) > 0) {
      df_4_wind <- data_wind_length_4 %>%
        list_transform()
    } else {
      df_4_wind <- data.frame(Row_Numb = character(),
                                stringsAsFactors = FALSE)
    }

    if (length(data_wind_length_3) > 0) {
      df_3_wind <- data_wind_length_3 %>%
        list_transform()
    } else {
      df_3_wind <- data.frame(Row_Numb = character(),
                                stringsAsFactors = FALSE)
    }

    if (length(data_wind_length_2) > 0) {
      df_2_wind <- data_wind_length_2 %>%
        list_transform()
    } else {
      df_2_wind <- data.frame(Row_Numb = character(),
                                stringsAsFactors = FALSE)
    }

    #### bind up results ####
    # results are bound before going to lines_sort so that in cases where there are multiple rows with splits for the same race,
    # like in longer events with many splits, those splits can be collected and treated together
    data_wind <-
      dplyr::bind_rows(
        df_10_wind,
        df_9_wind,
        df_8_wind,
        df_7_wind,
        df_6_wind,
        df_5_wind,
        df_4_wind,
        df_3_wind,
        df_2_wind
      ) %>%
      lines_sort(min_row = minimum_row) %>%
      dplyr::mutate(Row_Numb = as.numeric(Row_Numb) - 1) # make row number of split match row number of performance

    #### rename columns V1, V2 etc. by 50 ####
    old_names <- names(data_wind)[grep("^V", names(data_wind))]
    new_names <-
      paste("Round", seq(1, length(old_names)), "Wind", sep = "_")

    data_wind <- data_wind %>%
      dplyr::rename_at(dplyr::vars(old_names), ~ new_names) %>%
      dplyr::na_if("NA")

  } else { # if there are no rows with valid splits return blank data frame
    data_wind <- data.frame(Row_Numb = as.numeric())
  }
  return(data_wind)

}
