#' Collects flights within \code{tf_parse}
#'
#' Takes the output of \code{read_results} and, inside of \code{tf_parse}, extracts jump/throw flights and associated row numbers
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr rename_at
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr vars
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_split
#' @importFrom stringr str_detect
#' @importFrom purrr map_lgl
#' @importFrom purrr map
#'
#' @param text output of \code{read_results} with row numbers appended by \code{add_row_numbers}
#' @return returns a dataframe with split times and row numbers
#'
#' @seealso \code{flights_parse} runs inside \code{\link{tf_parse}} on the output of \code{\link{read_results}} with row numbers from \code{\link{add_row_numbers}}

flights_parse <- function(text) {

  #### Testing ####
  # file <- "http://leonetiming.com/2019/Indoor/GregPageRelays/Results.htm"
  # file <-
  #    system.file("extdata", "Results-IVP-Track-Field-Championship-2019-20-v2.pdf", package = "JumpeR")
  # file <- read_results(file)
  # text <- add_row_numbers(file)

  #### Actual Function ####
  ### collect row numbers from rows containing flights ###
  ### define strings ###

  attempt_string <- "\\d{1,2}\\.\\d{2}m?|PASS|FOUL|\\d{1,3}\\-\\d{2}\\.?\\d{2}?" # for metric and imperial units

    #### pull out rows containing flights ####

      suppressWarnings(
        data_1 <- text %>%
          .[purrr::map_lgl(., stringr::str_detect, attempt_string)] %>%
          .[purrr::map_lgl(., ~ stringr::str_detect(., "^\n\\s*\\d+\\s|^\n\\s*--", negate = TRUE))] %>% # removes rows that start with a place, to remove main results
          .[purrr::map_lgl(., ~ !any(stringr::str_detect(., "[:lower:]{2,}")))] %>% # removes rows that have two lower case letters in a row
          .[purrr::map_lgl(., ~ !any(stringr::str_detect(., ":")))] %>% # helps with removing records like "NYS: 1.45m"
          stringr::str_replace_all("\\(\\+?\\-?\\d{1,3}\\.\\d{1,3}\\)", "  ") %>%  # remove anything in parenthesis, replace with spaces
          stringr::str_replace_all("\\(NWI\\)", "  ") %>%  # remove NWI in parenthesis, replace with spaces
          stringr::str_replace_all(" ", "  ") %>% # put multiple spaces between flights
          trimws()
      )

    #### break out by length ####
    data_1 <-
      unlist(purrr::map(data_1, stringr::str_split, "\\s{2,}"),
             recursive = FALSE)

    data_length_2 <- data_1[purrr::map(data_1, length) == 2]
    data_length_3 <- data_1[purrr::map(data_1, length) == 3]
    data_length_4 <- data_1[purrr::map(data_1, length) == 4]
    data_length_5 <- data_1[purrr::map(data_1, length) == 5]
    data_length_6 <- data_1[purrr::map(data_1, length) == 6]
    data_length_7 <- data_1[purrr::map(data_1, length) == 7]
    data_length_8 <- data_1[purrr::map(data_1, length) == 8]
    data_length_9 <- data_1[purrr::map(data_1, length) == 9]
    data_length_10 <- data_1[purrr::map(data_1, length) == 10]
    data_length_11 <- data_1[purrr::map(data_1, length) == 11]
    data_length_12 <- data_1[purrr::map(data_1, length) == 12]

    #### transform all lists to dataframes ####
    if (length(data_length_12) > 0) {
      df_12 <- data_length_12 %>%
        list_transform() %>%
        dplyr::rename("Row_Numb" = V12)
    } else {
      df_12 <- data.frame(Row_Numb = character(),
                          stringsAsFactors = FALSE)
    }

    if (length(data_length_11) > 0) {
      df_11 <- data_length_11 %>%
        list_transform() %>%
        dplyr::rename("Row_Numb" = V11)
    } else {
      df_11 <- data.frame(Row_Numb = character(),
                          stringsAsFactors = FALSE)
    }

    if (length(data_length_10) > 0) {
      df_10 <- data_length_10 %>%
        list_transform() %>%
        dplyr::rename("Row_Numb" = V10)
    } else {
      df_10 <- data.frame(Row_Numb = character(),
                          stringsAsFactors = FALSE)
    }

    if (length(data_length_9) > 0) {
      df_9 <- data_length_9 %>%
        list_transform() %>%
        dplyr::rename("Row_Numb" = V9)
    } else {
      df_9 <- data.frame(Row_Numb = character(),
                         stringsAsFactors = FALSE)
    }

    if (length(data_length_8) > 0) {
      df_8 <- data_length_8 %>%
        list_transform() %>%
        dplyr::rename("Row_Numb" = V8)
    } else {
      df_8 <- data.frame(Row_Numb = character(),
                         stringsAsFactors = FALSE)
    }

    if (length(data_length_7) > 0) {
      df_7 <- data_length_7 %>%
        list_transform() %>%
        dplyr::rename("Row_Numb" = V7)
    } else {
      df_7 <- data.frame(Row_Numb = character(),
                         stringsAsFactors = FALSE)
    }

    if (length(data_length_6) > 0) {
      df_6 <- data_length_6 %>%
        list_transform() %>%
        dplyr::rename("Row_Numb" = V6)
    } else {
      df_6 <- data.frame(Row_Numb = character(),
                         stringsAsFactors = FALSE)
    }

    if (length(data_length_5) > 0) {
      df_5 <- data_length_5 %>%
        list_transform() %>%
        dplyr::rename("Row_Numb" = V5)
    } else {
      df_5 <- data.frame(Row_Numb = character(),
                         stringsAsFactors = FALSE)
    }

    if (length(data_length_4) > 0) {
      df_4 <- data_length_4 %>%
        list_transform() %>%
        dplyr::rename("Row_Numb" = V4)
    } else {
      df_4 <- data.frame(Row_Numb = character(),
                         stringsAsFactors = FALSE)
    }

    if (length(data_length_3) > 0) {
      df_3 <- data_length_3 %>%
        list_transform() %>%
        dplyr::rename("Row_Numb" = V3)
    } else {
      df_3 <- data.frame(Row_Numb = character(),
                         stringsAsFactors = FALSE)
    }

    if (length(data_length_2) > 0) {
      df_2 <- data_length_2 %>%
        list_transform() %>%
        dplyr::rename("Row_Numb" = V2)
    } else {
      df_2 <- data.frame(Row_Numb = character(),
                         stringsAsFactors = FALSE)
    }

    #### bind up results ####
    # results are bound with named column "Row_Numb" retained
    data <-
      dplyr::bind_rows(df_12, df_11, df_10, df_9, df_8, df_7, df_6, df_5, df_4, df_3, df_2) %>%
      dplyr::mutate(Row_Numb = as.numeric(Row_Numb) - 1) # make row number of split match row number of performance

    #### rename columns V1, V2 etc. at Attempt_1, Attempt_2 etc. ####
    old_names <- names(data)[grep("^V", names(data))]
    new_names <-
      paste("Flight", seq(1, length(names(data)) - 1), sep = "_")

    data <- data %>%
      dplyr::rename_at(dplyr::vars(dplyr::all_of(old_names)), ~ new_names) %>%
      dplyr::arrange(Row_Numb)

    return(data)

}

