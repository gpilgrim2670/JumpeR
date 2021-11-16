#' Collects relay athletes as a data frame within \code{tf_parse}
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr bind_rows
#' @importFrom dplyr na_if
#' @importFrom dplyr select
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_split
#' @importFrom stringr str_detect
#' @importFrom purrr map_lgl
#' @importFrom purrr map
#'
#' @param x output from \code{read_results} followed by \code{add_row_numbers}
#' @return returns a data frame of relay athletes and the associated performance
#'   row number
#'
#' @seealso \code{collect_relay_athletes_data} runs inside of \code{tf_parse}

collect_relay_athletes <- function(x){

  #### testing ####
  # x <- read_results("http://leonetiming.com/2019/Indoor/GregPageRelays/Results.htm") %>%
  # add_row_numbers()
  # x <- as_lines_list_2


  #### define strings ####
  relay_athlete_string <- "\n\\s*[1-4]\\)"
  score_string <- "\\d{3}\\.?\\d?\\d?\\s|\\s{4,}\\d{1,}\\s"

  #### find row numbers of relay athletes ####
  row_numbs_relay_athlete <- x %>%
    .[stringr::str_detect(.,
                     relay_athlete_string)] %>%
    .[stringr::str_detect(.,
                     score_string, negate = TRUE)] %>%
    stringr::str_extract_all("\\d{1,}$")

  #### if there are some rows with relay athletes pull them out ####
  if (length(row_numbs_relay_athlete) > 0) {
    minimum_row <- min(as.numeric(row_numbs_relay_athlete))

    #### clean up incoming data, mostly to remove grade/age strings and reaction times ####
    suppressWarnings(
      data_1_relay_athlete <- x %>%
        .[stringr::str_detect(.,
                         relay_athlete_string)] %>%
        .[stringr::str_detect(.,
                         score_string, negate = TRUE)] %>%
        stringr::str_remove_all("\n") %>%
        stringr::str_replace_all("\\s(?=\\d)", "  ") %>% # make to sure have enough spaces between athlete names
        stringr::str_replace_all("(?<=[1-4]\\))   ", " NA  ") %>%
        # stringr::str_replace_all(stats::setNames(replacement_2, typo_2)) %>%
        stringr::str_remove_all("\\)") %>%
        stringr::str_remove_all("[A-Z]\\d{1,3}") %>% # for M25 designations in masters - Male 25
        stringr::str_remove_all(" M?FR | M?SO | M?JR | M?SR | F?FR | F?SO | F?JR | F?SR | W?FR | W?SO | W?JR | W?SR ") %>% # for gender/grade designations
        stringr::str_remove_all("r\\:\\+?\\-?\\d?\\.\\d\\d?") %>% # for reaction pad outputs
        stringr::str_remove_all("r\\:NRT") %>% # for reaction time fail to register
        stringr::str_remove_all("\\d+|\\:|\\.|DQ|\\=\\=|\\*\\*") %>% # all digits or colons or periods (times, DQ, record designators)
        # stringr::str_replace_all() %>% # all digits
        stringr::str_remove_all("r\\:\\+?\\-?\\.") %>%
        stringr::str_remove_all("\\+\\+|\\*\\*") %>%
        trimws() %>%
        # stringr::str_remove_all(" SR$| SR | JR$| JR | SO$| SO | FR$| FR ") %>% # grade designators
        trimws()
    )

    #### add row numbers back in as first column since lines_sort requires row numbers to be in V1 ####
    data_1_relay_athlete <- paste(row_numbs_relay_athlete, data_1_relay_athlete, sep = "   ")

    #### split strings ####
    data_1_relay_athlete <-
      unlist(purrr::map(data_1_relay_athlete, stringr::str_split, "\\s{2,}"),
             recursive = FALSE)

    data_length_5_relay_athlete <- data_1_relay_athlete[purrr::map(data_1_relay_athlete, length) == 5] # all four athletes on one line
    data_length_4_relay_athlete <- data_1_relay_athlete[purrr::map(data_1_relay_athlete, length) == 4] # all four athletes on one line but one is missing
    data_length_3_relay_athlete <- data_1_relay_athlete[purrr::map(data_1_relay_athlete, length) == 3] # for two-line relays, two athletes per line
    data_length_2_relay_athlete <- data_1_relay_athlete[purrr::map(data_1_relay_athlete, length) == 2] %>%  # for two-line relays, two athletes per line, but one is missing
      .[purrr::map_lgl(., ~
                         any(stringr::str_detect(.,
                                                 "\\s|\\,")))] # to differentiate names from teams (like in circa 2005 NCAA results) - must have space or comma for separating names

    if (length(data_length_5_relay_athlete) > 0) {
      # splits from 100M relay legs
      df_5_relay_athlete <- data_length_5_relay_athlete %>%
        list_transform()

    } else {
      df_5_relay_athlete <- data.frame(Row_Numb = character(),
                                       stringsAsFactors = FALSE)
    }

    if (length(data_length_4_relay_athlete) > 0) {
      df_4_relay_athlete <- data_length_4_relay_athlete %>%
        list_transform()

    } else {
      df_4_relay_athlete <- data.frame(Row_Numb = character(),
                                       stringsAsFactors = FALSE)
    }

    if (length(data_length_3_relay_athlete) > 0) {
      df_3_relay_athlete <- data_length_3_relay_athlete %>%
        list_transform()

    } else {
      df_3_relay_athlete <- data.frame(Row_Numb = character(),
                                       stringsAsFactors = FALSE)
    }

    if (length(data_length_2_relay_athlete) > 0) {
      df_2_relay_athlete <- data_length_2_relay_athlete %>%
        list_transform() %>%
        dplyr::filter((as.numeric(V1) + 1) %!in% as.numeric(unlist(row_numbs_relay_athlete)) & (as.numeric(V1) + 2) %!in% as.numeric(unlist(row_numbs_relay_athlete))) # sometimes team names get caught up in relay data - this removes them by making sure no relay covers more than two rows

    } else {
      df_2_relay_athlete <- data.frame(Row_Numb = character(),
                                       stringsAsFactors = FALSE)
    }

    #### bind up results ####
    # results are bound before going to lines_sort so that in cases where there are multiple rows with splits for the same race,
    # like in results where relays athletes are reported on two lines, the results can be collected together
    relay_athletes_data <-
      dplyr::bind_rows(df_5_relay_athlete, df_4_relay_athlete, df_3_relay_athlete, df_2_relay_athlete)

    relay_athletes_data <- relay_athletes_data %>%
      lines_sort(min_row = min(as.numeric(relay_athletes_data$V1) - 2)) %>%
      dplyr::mutate(Row_Numb = as.numeric(Row_Numb)) %>%   # make row number of relay match row number of performance
      dplyr::select(
        "Relay_Athlete_1" = V2,
        "Relay_Athlete_2" = V3,
        "Relay_Athlete_3" = V4,
        "Relay_Athlete_4" = V5,
        Row_Numb
      ) %>%
      dplyr::na_if("NA")

  } else {
    relay_athletes_data <- data.frame(Row_Numb = as.numeric())
  }

  return(relay_athletes_data)
}
