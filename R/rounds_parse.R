#' Collects rounds within \code{tf_parse}
#'
#' Takes the output of \code{read_results} and, inside of \code{tf_parse},
#' extracts jump/throw rounds and associated row numbers.
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
#' @param text output of \code{read_results} with row numbers appended by
#'   \code{add_row_numbers}
#' @return returns a data frame with split times and row numbers
#'
#' @seealso \code{rounds_parse} runs inside \code{\link{tf_parse}} on the
#'   output of \code{\link{read_results}} with row numbers from
#'   \code{\link{add_row_numbers}}

rounds_parse <- function(text) {

  #### Testing ####
  # file <- "http://leonetiming.com/2019/Indoor/GregPageRelays/Results.htm"
  # file <-
  #    system.file("extdata", "Results-IVP-Track-Field-Championship-2019-20-v2.pdf", package = "JumpeR")
  # file <- "http://tfresultsdata.deltatiming.com/2019-sun-belt-outdoor-championships/190510F029.htm"
  # file <- read_results(file)
  # text <- add_row_numbers(file)

  #### Actual Function ####
  ### collect row numbers from rows containing rounds ###
  ### define strings ###

  attempt_string <- "\\d{1,2}\\.\\d{2}m?|PASS|FOUL|  X  |\\d{1,3}\\-\\d{2}\\.?\\d{2}?" # for metric and imperial units

    #### pull out rows containing rounds ####

      suppressWarnings(
        data_rounds_1 <- text %>%
          .[stringr::str_detect(., attempt_string)] %>%
          .[purrr::map_lgl(., ~ stringr::str_detect(., "^\n\\s*\\d+\\s|^\n\\s*--", negate = TRUE))] %>% # removes rows that start with a place, to remove main results
          .[purrr::map_lgl(., ~ !any(stringr::str_detect(., "[:lower:]{2,}")))] %>% # removes rows that have two lower case letters in a row
          .[purrr::map_lgl(., ~ !any(stringr::str_detect(., ":")))] %>% # helps with removing records like "NYS: 1.45m"
          stringr::str_replace_all("\\(\\+?\\-?\\d{1,3}\\.\\d{1,3}\\)", "  ") %>%  # remove anything in parenthesis, replace with spaces
          stringr::str_replace_all("\\(NWI\\)", "  ") %>%  # remove NWI in parenthesis, replace with spaces
          stringr::str_replace_all(" ", "  ") %>% # put multiple spaces between rounds
          trimws()
      )

    #### break out by length ####
    data_rounds_1 <-
      unlist(purrr::map(data_rounds_1, stringr::str_split, "\\s{2,}"),
             recursive = FALSE)

    data_rounds_length_2 <- data_rounds_1[purrr::map(data_rounds_1, length) == 2]
    data_rounds_length_3 <- data_rounds_1[purrr::map(data_rounds_1, length) == 3]
    data_rounds_length_4 <- data_rounds_1[purrr::map(data_rounds_1, length) == 4]
    data_rounds_length_5 <- data_rounds_1[purrr::map(data_rounds_1, length) == 5]
    data_rounds_length_6 <- data_rounds_1[purrr::map(data_rounds_1, length) == 6]
    data_rounds_length_7 <- data_rounds_1[purrr::map(data_rounds_1, length) == 7]
    data_rounds_length_8 <- data_rounds_1[purrr::map(data_rounds_1, length) == 8]
    data_rounds_length_9 <- data_rounds_1[purrr::map(data_rounds_1, length) == 9]
    data_rounds_length_10 <- data_rounds_1[purrr::map(data_rounds_1, length) == 10]
    data_rounds_length_11 <- data_rounds_1[purrr::map(data_rounds_1, length) == 11]
    data_rounds_length_12 <- data_rounds_1[purrr::map(data_rounds_1, length) == 12]

    #### transform all lists to dataframes ####
    if (length(data_rounds_length_12) > 0) {
      df_rounds_12 <- data_rounds_length_12 %>%
        list_transform() %>%
        dplyr::rename("Row_Numb" = V12)
    } else {
      df_rounds_12 <- data.frame(Row_Numb = character(),
                          stringsAsFactors = FALSE)
    }

    if (length(data_rounds_length_11) > 0) {
      df_rounds_11 <- data_rounds_length_11 %>%
        list_transform() %>%
        dplyr::rename("Row_Numb" = V11)
    } else {
      df_rounds_11 <- data.frame(Row_Numb = character(),
                          stringsAsFactors = FALSE)
    }

    if (length(data_rounds_length_10) > 0) {
      df_rounds_10 <- data_rounds_length_10 %>%
        list_transform() %>%
        dplyr::rename("Row_Numb" = V10)
    } else {
      df_rounds_10 <- data.frame(Row_Numb = character(),
                          stringsAsFactors = FALSE)
    }

    if (length(data_rounds_length_9) > 0) {
      df_rounds_9 <- data_rounds_length_9 %>%
        list_transform() %>%
        dplyr::rename("Row_Numb" = V9)
    } else {
      df_rounds_9 <- data.frame(Row_Numb = character(),
                         stringsAsFactors = FALSE)
    }

    if (length(data_rounds_length_8) > 0) {
      df_rounds_8 <- data_rounds_length_8 %>%
        list_transform() %>%
        dplyr::rename("Row_Numb" = V8)
    } else {
      df_rounds_8 <- data.frame(Row_Numb = character(),
                         stringsAsFactors = FALSE)
    }

    if (length(data_rounds_length_7) > 0) {
      df_rounds_7 <- data_rounds_length_7 %>%
        list_transform() %>%
        dplyr::rename("Row_Numb" = V7)
    } else {
      df_rounds_7 <- data.frame(Row_Numb = character(),
                         stringsAsFactors = FALSE)
    }

    if (length(data_rounds_length_6) > 0) {
      df_rounds_6 <- data_rounds_length_6 %>%
        list_transform() %>%
        dplyr::rename("Row_Numb" = V6)
    } else {
      df_rounds_6 <- data.frame(Row_Numb = character(),
                         stringsAsFactors = FALSE)
    }

    if (length(data_rounds_length_5) > 0) {
      df_rounds_5 <- data_rounds_length_5 %>%
        list_transform() %>%
        dplyr::rename("Row_Numb" = V5)
    } else {
      df_rounds_5 <- data.frame(Row_Numb = character(),
                         stringsAsFactors = FALSE)
    }

    if (length(data_rounds_length_4) > 0) {
      df_rounds_4 <- data_rounds_length_4 %>%
        list_transform() %>%
        dplyr::rename("Row_Numb" = V4)
    } else {
      df_rounds_4 <- data.frame(Row_Numb = character(),
                         stringsAsFactors = FALSE)
    }

    if (length(data_rounds_length_3) > 0) {
      df_rounds_3 <- data_rounds_length_3 %>%
        list_transform() %>%
        dplyr::rename("Row_Numb" = V3)
    } else {
      df_rounds_3 <- data.frame(Row_Numb = character(),
                         stringsAsFactors = FALSE)
    }

    if (length(data_rounds_length_2) > 0) {
      df_rounds_2 <- data_rounds_length_2 %>%
        list_transform() %>%
        dplyr::rename("Row_Numb" = V2)
    } else {
      df_rounds_2 <- data.frame(Row_Numb = character(),
                         stringsAsFactors = FALSE)
    }

    #### bind up results ####
    # results are bound with named column "Row_Numb" retained
    data_rounds <-
      dplyr::bind_rows(df_rounds_12, df_rounds_11, df_rounds_10, df_rounds_9, df_rounds_8, df_rounds_7, df_rounds_6, df_rounds_5, df_rounds_4, df_rounds_3, df_rounds_2) %>%
      dplyr::mutate(Row_Numb = as.numeric(Row_Numb) - 1) # make row number of split match row number of performance

    #### rename columns V1, V2 etc. at Attempt_1, Attempt_2 etc. ####
    old_names <- names(data_rounds)[grep("^V", names(data_rounds))]
    new_names <-
      paste("Round", seq(1, length(names(data_rounds)) - 1), sep = "_")

    data_rounds <- data_rounds %>%
      dplyr::rename_at(dplyr::vars(dplyr::all_of(old_names)), ~ new_names) %>%
      dplyr::arrange(Row_Numb)
      # dplyr::distinct(dplyr::across(dplyr::contains("Round")), .keep_all = TRUE)

    return(data_rounds)

}

