#' Collects results of high jump & pole vault round attempts within
#' \code{tf_parse}
#'
#' Takes the output of \code{read_results} and, inside of \code{tf_parse},
#' extracts vertical jump round attempts (XXO etc) and associated row numbers
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr rename_at
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr vars
#' @importFrom dplyr lag
#' @importFrom dplyr arrange
#' @importFrom dplyr across
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_split
#' @importFrom stringr str_detect
#' @importFrom stringr str_trim
#' @importFrom purrr map_lgl
#' @importFrom purrr map
#'
#' @param text output of \code{read_results} with row numbers appended by
#'   \code{add_row_numbers}
#' @return returns a data frame with split times and row numbers
#'
#' @seealso \code{flash_round_attempts_parse} runs inside
#'   \code{\link{flash_parse}} on the output of \code{\link{read_results}} with
#'   row numbers from \code{\link{add_row_numbers}}

flash_round_attempts_parse <- function(text) {
  #### Testing ####
  # file <- "http://leonetiming.com/2019/Indoor/GregPageRelays/Results.htm"
  # file <-
  #    system.file("extdata", "Results-IVP-Track-Field-Championship-2019-20-v2.pdf", package = "JumpeR")
  # file <- "https://www.flashresults.com/2019_Meets/Outdoor/05-23_NCAAEast-Jacksonville/014-1.pdf"
  # file <- read_results(file)
  # text <- add_row_numbers(file)
  # text <- flash_file

  #### Actual Function ####

  #### define strings ####
  flash_attempts_string <-
    " P  | ?PPP ?| O | X | XO ?| ?XXO ?| XX | ?XX\\- | ?XX\U2013 | ?X\\-{2} | ?X\U2013{2} | ?XXX | XR | ?\\-{3} | ?\U2013{3} " # for metric and imperial units, also has special dash "em-dash", code is \U2013


  #### Clean up incoming text ####
  text <- text %>%
    stringr::str_remove_all("\n\\s*") %>%
    .[purrr::map_lgl(., ~ !any(stringr::str_detect(., "^[A-Z][a-z].{1,}$")))]  # remove records

  #### collect row numbers from rows containing rounds ####
  # row_numbs <- text %>%
  #   .[purrr::map_lgl(., stringr::str_detect, attempt_results_string_flash)] %>%
  #   str_extract("\\d{1,}$")

  #### pull out rows containing rounds ####
  suppressWarnings(
    data_1 <- text %>%
      stringr::str_replace_all(" ", "  ") %>% # if attempts are close together "XO XO" to "XO   XO"
      .[stringr::str_detect(., flash_attempts_string)] %>%
      stringr::str_replace_all("\U2013", "\\-") %>% # replace em dashes with regular dashes
      stringr::str_extract_all(paste0(flash_attempts_string, "|\\d{1,}$"))
  )


  #### break out by length ####
  # theoretically there can be any number of rounds, as long as
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
  data_length_13 <- data_1[purrr::map(data_1, length) == 13]

  #### transform all lists to dataframes ####
  if (length(data_length_13) > 0) {
    df_13 <- data_length_13 %>%
      list_transform() %>%
      dplyr::rename(Row_Numb = V13)
  }  else {
    df_13 <- data.frame(Row_Numb = character(),
                        stringsAsFactors = FALSE)
  }

  if (length(data_length_12) > 0) {
    df_12 <- data_length_12 %>%
      list_transform() %>%
      dplyr::rename(Row_Numb = V12)
  }  else {
    df_12 <- data.frame(Row_Numb = character(),
                        stringsAsFactors = FALSE)
  }

  if (length(data_length_11) > 0) {
    df_11 <- data_length_11 %>%
      list_transform() %>%
      dplyr::rename(Row_Numb = V11)
  }  else {
    df_11 <- data.frame(Row_Numb = character(),
                        stringsAsFactors = FALSE)
  }

  if (length(data_length_10) > 0) {
    df_10 <- data_length_10 %>%
      list_transform() %>%
      dplyr::rename(Row_Numb = V10)
  }  else {
    df_10 <- data.frame(Row_Numb = character(),
                        stringsAsFactors = FALSE)
  }

  if (length(data_length_9) > 0) {
    df_9 <- data_length_9 %>%
      list_transform() %>%
      dplyr::rename(Row_Numb = V9)
  }  else {
    df_9 <- data.frame(Row_Numb = character(),
                       stringsAsFactors = FALSE)
  }

  if (length(data_length_8) > 0) {
    df_8 <- data_length_8 %>%
      list_transform() %>%
      dplyr::rename(Row_Numb = V8)
  }  else {
    df_8 <- data.frame(Row_Numb = character(),
                       stringsAsFactors = FALSE)
  }

  if (length(data_length_7) > 0) {
    df_7 <- data_length_7 %>%
      list_transform() %>%
      dplyr::rename(Row_Numb = V7)
  }  else {
    df_7 <- data.frame(Row_Numb = character(),
                       stringsAsFactors = FALSE)
  }

  if (length(data_length_6) > 0) {
    df_6 <- data_length_6 %>%
      list_transform() %>%
      dplyr::rename(Row_Numb = V6)
  }  else {
    df_6 <- data.frame(Row_Numb = character(),
                       stringsAsFactors = FALSE)
  }

  if (length(data_length_5) > 0) {
    df_5 <- data_length_5 %>%
      list_transform() %>%
      dplyr::rename(Row_Numb = V5)
  }  else {
    df_5 <- data.frame(Row_Numb = character(),
                       stringsAsFactors = FALSE)
  }

  if (length(data_length_4) > 0) {
    df_4 <- data_length_4 %>%
      list_transform() %>%
      dplyr::rename(Row_Numb = V4)
  }  else {
    df_4 <- data.frame(Row_Numb = character(),
                       stringsAsFactors = FALSE)
  }

  if (length(data_length_3) > 0) {
    df_3 <- data_length_3 %>%
      list_transform() %>%
      dplyr::rename(Row_Numb = V3)
  }  else {
    df_3 <- data.frame(Row_Numb = character(),
                       stringsAsFactors = FALSE)
  }

  if (length(data_length_2) > 0) {
    df_2 <- data_length_2 %>%
      list_transform() %>%
      dplyr::rename(Row_Numb = V2)
  }  else {
    df_2 <- data.frame(Row_Numb = character(),
                       stringsAsFactors = FALSE)
  }

  #### bind up results ####
  # results are bound with named column "Row_Numb" retained
  suppressMessages(
    data_round_attempts <-
      dplyr::bind_rows(
        df_13,
        df_12,
        df_11,
        df_10,
        df_9,
        df_8,
        df_7,
        df_6,
        df_5,
        df_4,
        df_3,
        df_2
      )
  )
  if ("V1" %in% names(data_round_attempts)) {
    data_round_attempts <- data_round_attempts %>%
      dplyr::mutate(Row_Numb = as.numeric(Row_Numb)) %>%
      dplyr::rename(V1a = V1,
                    V1 = Row_Numb)

    # suppressMessages(
    # data_round_attempts <- row_numbs %>%
    #   dplyr::bind_cols(data_round_attempts)
    # )

    data_round_attempts <- data_round_attempts %>%
      arrange(V1) %>%
      dplyr::mutate(V1 = dplyr::case_when(
        stringr::str_detect(dplyr::lag(.[, ncol(.)], default = "NA"), "O ?$|\U2013 ?$|- ?$") == TRUE &
          V1 - dplyr::lag(V1) <= 1 ~ V1 - 1,
        # checks for strings in the last column which mean the athlete can keep jumping and adjusts row numbs to reflect athlete having multiple rows of rounds
        TRUE ~ V1
      )) %>%
      lines_sort(min_row = min(data_round_attempts$V1)) %>%
      dplyr::mutate(Row_Numb = as.numeric(Row_Numb)) %>%
      dplyr::arrange(Row_Numb)

    #### rename columns V1, V2 etc. at Attempt_1, Attempt_2 etc. ####
    old_names <-
      names(data_round_attempts)[grep("^V", names(data_round_attempts))]
    new_names <-
      paste("Round", seq(1, length(names(
        data_round_attempts
      )) - 1), "Attempts", sep = "_")

    data_round_attempts <- data_round_attempts %>%
      dplyr::rename_at(dplyr::vars(dplyr::all_of(old_names)), ~ new_names) %>%
      # dplyr::arrange(Row_Numb) %>%
      dplyr::mutate(dplyr::across(new_names, stringr::str_trim))
  }

  if (sum(suppressWarnings(str_detect(data_round_attempts, "O"))) >= 1) {
    # some results, like long jump will have X for faults, but not O for pass - these are not the kind of results we want for round_attempts, rather they will be captured as rounds

    row.names(data_round_attempts) <- NULL

    return(data_round_attempts)
  } else {
    data_round_attempts <- data.frame(Row_Numb = character(),
                                      stringsAsFactors = FALSE)
    return(data_round_attempts)
  }

}
