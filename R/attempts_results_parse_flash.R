#' Collects results of high jump & pole vault attempts within \code{tf_parse}
#'
#' Takes the output of \code{read_results} and, inside of \code{tf_parse}, extracts jump/throw attempts and associated row numbers
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr rename_at
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr vars
#' @importFrom dplyr bind_cols
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
#' @seealso \code{attempts_results_parse} runs inside \code{\link{tf_parse}} on the output of \code{\link{read_results}} with row numbers from \code{\link{add_row_numbers}}

attempts_results_parse_flash <- function(text) {

  #### Testing ####
  # file <- "http://leonetiming.com/2019/Indoor/GregPageRelays/Results.htm"
  # file <-
  #    system.file("extdata", "Results-IVP-Track-Field-Championship-2019-20-v2.pdf", package = "JumpeR")
  file <- "https://www.flashresults.com/2019_Meets/Outdoor/04-27_VirginiaGrandPrix/014-1.pdf"
  file <- read_results(file)
  text <- add_row_numbers(file)

  ### define strings ###
  attempt_results_string_flash <- " P | PPP | O | X | XO | XXO | XX | XXX | \\-{3} " # for metric and imperial units

  #### Actual Function ####
  ### collect row numbers from rows containing attempts ###
  row_numbs <- text %>%
    .[purrr::map_lgl(., stringr::str_detect, attempt_results_string_flash)] %>%
    str_extract("\\d{1,}$")

  #### pull out rows containing attempts ####
  suppressWarnings(
    data_1 <- text %>%
      .[purrr::map_lgl(., stringr::str_detect, attempt_results_string_flash)] %>%
          stringr::str_extract_all(attempt_results_string_flash)
  )

  #### break out by length ####


  # theoretically there can be any number of attempts, as long as
  # one athlete keeps clearing heights
  data_length_1 <- data_1[purrr::map(data_1, length) == 1]
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
      list_transform()
  }  else {
    df_12 <- data.frame(V1 = character(),
                        stringsAsFactors = FALSE)
  }

  if (length(data_length_11) > 0) {
    df_11 <- data_length_11 %>%
      list_transform()
  }  else {
    df_11 <- data.frame(V1 = character(),
                       stringsAsFactors = FALSE)
  }

  if (length(data_length_10) > 0) {
    df_10 <- data_length_10 %>%
      list_transform()
  }  else {
    df_10 <- data.frame(V1 = character(),
                        stringsAsFactors = FALSE)
  }

  if (length(data_length_9) > 0) {
    df_9 <- data_length_9 %>%
      list_transform()
  }  else {
    df_9 <- data.frame(V1 = character(),
                        stringsAsFactors = FALSE)
  }

  if (length(data_length_8) > 0) {
    df_8 <- data_length_8 %>%
      list_transform()
  }  else {
    df_8 <- data.frame(V1 = character(),
                        stringsAsFactors = FALSE)
  }

  if (length(data_length_7) > 0) {
    df_7 <- data_length_7 %>%
      list_transform()
  }  else {
    df_7 <- data.frame(V1 = character(),
                        stringsAsFactors = FALSE)
  }

  if (length(data_length_6) > 0) {
    df_6 <- data_length_6 %>%
      list_transform()
  }  else {
    df_6 <- data.frame(V1 = character(),
                        stringsAsFactors = FALSE)
  }

  if (length(data_length_5) > 0) {
    df_5 <- data_length_5 %>%
      list_transform()
  }  else {
    df_5 <- data.frame(V1 = character(),
                        stringsAsFactors = FALSE)
  }

  if (length(data_length_4) > 0) {
    df_4 <- data_length_4 %>%
      list_transform()
  }  else {
    df_4 <- data.frame(V1 = character(),
                        stringsAsFactors = FALSE)
  }

  if (length(data_length_3) > 0) {
    df_3 <- data_length_3 %>%
      list_transform
  }  else {
    df_3 <- data.frame(V1 = character(),
                        stringsAsFactors = FALSE)
  }

  if (length(data_length_2) > 0) {
    df_2 <- data_length_2 %>%
      list_transform()
  }  else {
    df_2 <- data.frame(V1 = character(),
                        stringsAsFactors = FALSE)
  }

  if (length(data_length_1) > 0) {
    df_1 <- data_length_1 %>%
      list_transform()
  }  else {
    df_1 <- data.frame(V1 = character(),
                        stringsAsFactors = FALSE)
  }

  #### bind up results ####
  # results are bound with named column "Row_Numb" retained
  data <-
    dplyr::bind_rows(df_12, df_11, df_10, df_9, df_8, df_7, df_6, df_5, df_4, df_3, df_2, df_1) %>%
    dplyr::bind_cols(row_numbs)

  names(data)[ncol(data)] <- "Row_Numb" # to rename last column since we don't know how many columns there will be

  data <- data %>%
    dplyr::mutate(Row_Numb = as.numeric(Row_Numb)) # make row number of split match row number of performance

  #### rename columns V1, V2 etc. at Attempt_1, Attempt_2 etc. ####
  old_names <- names(data)[grep("^V", names(data))]
  new_names <-
    paste("Attempt", seq(1, length(names(data)) - 1), "Result", sep = "_")

  data <- data %>%
    dplyr::rename_at(dplyr::vars(dplyr::all_of(old_names)), ~ new_names) %>%
    dplyr::arrange(Row_Numb)

  return(data)

}

