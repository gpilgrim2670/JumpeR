#' Collects attempts within \code{tf_parse}
#'
#' Takes the output of \code{read_results} and, inside of \code{tf_parse},
#' extracts jump/throw attempts and associated row numbers
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
#' @param text output of \code{read_results} with row numbers appended by
#'   \code{add_row_numbers}
#' @return returns a data frame with split times and row numbers
#'
#' @seealso \code{rounds_parse_flash} runs inside \code{\link{flash_parse}} on
#'   the output of \code{\link{read_results}} with row numbers from
#'   \code{\link{add_row_numbers}}

flash_rounds_parse <- function(text) {
  #### Testing ####
  # file <- "http://leonetiming.com/2019/Indoor/GregPageRelays/Results.htm"
  # file <-
  #    system.file("extdata", "Results-IVP-Track-Field-Championship-2019-20-v2.pdf", package = "JumpeR")
  # file <- "https://www.flashresults.com/2019_Meets/Outdoor/06-30_PreClassic/001-1.pdf"
  # file <- "https://www.flashresults.com/2019_Meets/Outdoor/04-27_VirginiaGrandPrix/036-1.pdf"
  # file <- "https://www.flashresults.com/2019_Meets/Outdoor/04-12_TamuInvite/014-1.pdf"
  # file <-
  #   "https://www.flashresults.com/2018_Meets/Outdoor/05-05_A10/015-1.pdf"
  # file <- read_results(file)
  # text <- add_row_numbers(file)
  # file <-
  #   "https://www.flashresults.com/2019_Meets/Outdoor/04-12_TamuInvite/014-1.pdf"
  # file <- read_results(file)
  # text <- add_row_numbers(file)
  # text <- raw_results
  # text <- flash_file

  #### Actual Function ####
  ### define strings ###

  attempt_string_flash <-
    "\\d{1,2}\\.\\d{2} |\\d{1,3}\\-\\d{2}\\.?\\d{2}?|  X  |  \U2013  " # special dash character "em-dash" is \U2013

  text <- text %>%
    stringr::str_remove_all("\n\\s*") %>%
    .[purrr::map_lgl(., ~ !any(stringr::str_detect(., "^[A-Z][a-km-z].{1,}$")))] %>%   # remove records, don't want to exclude rows beginning with "Pl "
    .[purrr::map_lgl(., ~ !any(stringr::str_detect(., "(?<=[\U2013]) +X ")))] %>%  # remove special case where in vertical jumps someone passes a round then fails leaving "XX-  X" and that X gets picked up like it would for a horizontal event
    .[purrr::map_lgl(., ~ !any(stringr::str_detect(., "(?<=O) +X ")))] # remove special case where in vertical jumps someone passes a round then makes one attempt and passes leaving "O  X" and that X gets picked up like it would for a horizontal event

  #### collect row numbers from rows containing rounds ####
  row_numbs <- text %>%
    .[stringr::str_detect(., attempt_string_flash)] %>%
    stringr::str_extract("\\d{1,}$")

  #### pull out rows containing rounds ####

  suppressWarnings(
    data_rounds <- text %>%
      .[stringr::str_detect(., attempt_string_flash)] %>%
      # .[purrr::map_lgl(., ~ stringr::str_detect(., "^\\d", negate = TRUE))] %>% # removes rows that start with a place, to remove main results and scores with decimal places (5 Alfred U. 2.50 etc.)
      stringr::str_extract_all(attempt_string_flash, simplify = TRUE) %>%
      trimws()
  )

  #### reattach row numbers ####
  data_rounds <- cbind(row_numbs, data_rounds) %>%
    as.data.frame() %>%
    dplyr::na_if("") %>%
    dplyr::rename(V1 = row_numbs) # for list_sort, needs V1 to be row numbers, but named V1

  if (any(stringr::str_detect(text, "Scored")) == TRUE) {
    # gets rid of team scores in vertical jump events
    row_score <-
      min(as.numeric(stringr::str_extract(text[stringr::str_detect(text, "Scored")], "\\d{1,}$")))
    data_rounds <- data_rounds %>%
      dplyr::filter(as.numeric(row_numbs) < row_score)
  }

  #### reattach row numbers ####

  data_rounds <- data_rounds %>%
    lines_sort(min_row = min(as.numeric(row_numbs))) %>%
    dplyr::mutate(Row_Numb = as.numeric(Row_Numb))

  #### rename columns V1, V2 etc. at Attempt_1, Attempt_2 etc. ####
  old_names <-
    names(data_rounds)[grep("^V", names(data_rounds))]
  new_names <-
    paste("Round", seq(1, length(names(data_rounds)) - 1), sep = "_")

  data_rounds <- data_rounds %>%
    dplyr::rename_at(dplyr::vars(dplyr::all_of(old_names)), ~ new_names)

  if (sum(suppressWarnings(str_detect(text, "\\d\\.\\d{2}m"))) >= 1) {
    # keeps running times like 10.34 from getting into rounds
    return(data_rounds)
  } else {
    data_rounds <- data.frame(Row_Numb = character(),
                              stringsAsFactors = FALSE)
    return(data_rounds)
  }

}
