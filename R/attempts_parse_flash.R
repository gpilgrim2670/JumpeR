#' Collects attempts within \code{tf_parse}
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
#' @seealso \code{attempts_parse_flash} runs inside \code{\link{flash_parse}} on the output of \code{\link{read_results}} with row numbers from \code{\link{add_row_numbers}}

attempts_parse_flash <- function(text) {
  #### Testing ####
  # file <- "http://leonetiming.com/2019/Indoor/GregPageRelays/Results.htm"
  # file <-
  #    system.file("extdata", "Results-IVP-Track-Field-Championship-2019-20-v2.pdf", package = "JumpeR")
  # file <- "https://www.flashresults.com/2019_Meets/Outdoor/06-30_PreClassic/001-1.pdf"
  # file <- "https://www.flashresults.com/2019_Meets/Outdoor/04-27_VirginiaGrandPrix/036-1.pdf"
  # file <- read_results(file)
  # text <- add_row_numbers(file)
  # text <- raw_results


  #### Actual Function ####
  ### define strings ###

  attempt_string_flash <-
    "\\d{1,2}\\.\\d{2} |\\d{1,3}\\-\\d{2}\\.?\\d{2}?|  X  |  \\–  " # special dash character

  #### collect row numbers from rows containing attempts ####
  row_numbs <- text %>%
    .[purrr::map_lgl(., stringr::str_detect, attempt_string_flash)] %>%
    str_extract("\\d{1,}$")

  #### pull out rows containing attempts ####

  suppressWarnings(
    data_attempts <- text %>%
      .[purrr::map_lgl(., stringr::str_detect, attempt_string_flash)] %>%
      stringr::str_extract_all(attempt_string_flash, simplify = TRUE) %>%
      trimws()
  )

  #### reattach row numbers ####
  data_attempts <- cbind(row_numbs, data_attempts) %>%
    as.data.frame() %>%
    dplyr::na_if("") %>%
    dplyr::rename(V1 = row_numbs) # for list_sort, needs V1 to be row numbers, but named V1

  #### reattach row numbers ####

  data_attempts <- data_attempts %>%
    lines_sort(min_row = min(as.numeric(row_numbs))) %>%
    dplyr::mutate(Row_Numb = as.numeric(Row_Numb))

  #### rename columns V1, V2 etc. at Attempt_1, Attempt_2 etc. ####
  old_names <-
    names(data_attempts)[grep("^V", names(data_attempts))]
  new_names <-
    paste("Attempt", seq(1, length(names(data_attempts)) - 1), sep = "_")

  data_attempts <- data_attempts %>%
    dplyr::rename_at(dplyr::vars(dplyr::all_of(old_names)), ~ new_names)

  if (sum(suppressWarnings(str_detect(text, "\\d\\.\\d{2}m"))) >= 1) {
    # keeps running times like 10.34 from getting into attempts
    return(data_attempts_results)
  } else {
    data_attempts <- data.frame(Row_Numb = character(),
                                stringsAsFactors = FALSE)
    return(data_attempts)
  }

}
