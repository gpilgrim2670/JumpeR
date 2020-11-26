#' Reads track and field results into a list of strings in preparation for parsing with \code{tf_parse}
#'
#' Outputs list of strings to be processed by \code{tf_parse}
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @importFrom SwimmeR add_row_numbers
#' @importFrom SwimmeR event_parse
#' @importFrom dplyr mutate
#' @importFrom stringr str_remove
#'
#' @param file a .pdf or .html file (could be a url) where containing swimming results.  Must be formatted in a "normal" fashion - see vignette
#' @param avoid xxx
#' @param typo xxx
#' @param replacement xxx
#'
#' @return a dataframe
#'
#' @seealso \code{tf_parse} is meant to be preceeded by \code{\link{read_results}}
#'
#' @export

tf_parse <-
  function(file,
           avoid = avoid_default,
           typo = typo_default,
           replacement = replacement_default) {


    #### default typo and replacement strings ####
    typo_default <- c("typo")

    replacement_default <- c("typo")

    if(length(typo) != length(replacement)) {
      stop("typo and replacement must have the same number of elements (be the same length)")
    }

    #### strings that if a line begins with one of them the line is ignored ####
    avoid_default <-
      c("Record\\:")

    #### define avoid_minimal ####
    avoid_minimal <- c("^\\s{1,}r\\:")

    #### testing setup ####
    file <-
      system.file("extdata", "Results-IVP-Track-Field-Championship-2019-20-v2.pdf", package = "JumpeR")

    file <- read_results(file)
    avoid <- avoid_default
    typo <- typo_default
    replacement <- replacement_default

    #### assign row numbers ####
    as_lines_list_2 <- SwimmeR:::add_row_numbers(text = file)

      #### Pulls out event labels from text ####
    events <- SwimmeR:::event_parse(as_lines_list_2) %>%
      dplyr::mutate(Event = stringr::str_remove(Event, " Women$| Men$"))


      #### set up strings ####
      Name_String <-
        "_?[:alpha:]+\\s?\\'?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\'\\.]*,?\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:]*\\s?[:alpha:]*\\s?[:alpha:]*\\.?,? [:alpha:]+\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\']*\\s?[:alpha:]*\\s?[:alpha:]*\\s?[:alpha:\\.]*"
      Time_Score_String <- "\\d{0,2}\\:?\\-\\d{1,3}\\.\\d{2}m?"
      Time_Score_Specials_String <- paste0(Time_Score_String, "|^NT$|^NP$|^DQ$|^DNS$|^DNF$|^FOUL$")
      Wind_String <- "\\+\\d\\.\\d|\\-\\d\\.\\d|^NWS$|^\\d\\.\\d$"
      Age_String <- "^SR$|^JR$|^SO$|^FR$|^[:digit:]{1,3}$"

      #### clean input data ####
      suppressWarnings(
        data_1 <- as_lines_list_2 %>%
          # stringr::str_replace_all(stats::setNames(replacement, typo)) %>% # moved to top 8/26
          stringr::str_replace_all("\\*(\\d{1,})", replacement = "\\1") %>%  # removes * placed in front of place number in ties
          stringr::str_extract_all(
            "\n\\s*\\d*\\s* #? \\d{0,3} \\*?[:alpha:].*|\n\\s*\\d* \\d*\\-[:alpha:].*|\n\\s*-{2,5}\\s* [:alpha:].*|\n\\s*\\d* \\d*\\-[:alpha:].*"
          ) %>%
          .[purrr::map(., length) > 0] %>%
          .[purrr::map(., stringr::str_length) > 50] %>%
          .[purrr::map_lgl(., stringr::str_detect, paste0(Time_Score_String,"|DQ"))] %>% # must have \\.\\d\\d because all swimming and diving times do
          # .[purrr::map_lgl(., stringr::str_detect, "\\.\\d\\d")] %>% # must have \\.\\d\\d because all swimming and diving times do
          .[purrr::map_lgl(., stringr::str_detect, "[:alpha:]{2,}")] %>% # must have at least two letters in a row
          .[purrr::map_lgl(., ~ !any(stringr::str_detect(., avoid)))] %>%
          stringr::str_remove_all("\n") %>%
          # trimws() %>%
          stringr::str_replace_all(stats::setNames(replacement, typo)) %>% # moved to top of pipeline 8/26
          # remove 'A', 'B' etc. relay designators - should this go in typo instead?
          stringr::str_replace_all("  \\'[A-Z]\\'  ", "  ") %>%
          stringr::str_replace_all("  [A-Z]  ", "  ") %>%
          stringr::str_replace_all("\\'\\'", "  ") %>%
          # remove q from next to time 10/21/2020
          stringr::str_remove_all(" q ") %>% # removes " q " sometimes used to designate a qualifying time
          stringr::str_replace_all("-{2,5}", "10000") %>% #8/26
          stringr::str_replace_all("(\\.\\d{2})\\d+", "\\1 ") %>% # added 8/21 for illinois to deal with points column merging with final times column
          stringr::str_replace_all("\\d{1,2} (\\d{1,})$", "  \\1 ") %>% # added 8/21 for illinois to deal with points column merging with final times column
          stringr::str_replace_all("\\*", "_") %>%
          stringr::str_replace_all(" \\+", "  \\+") %>%  # tf speciifc, for windspeed
          stringr::str_replace_all(" \\-", "  \\-") %>%  # tf speciifc, for windspeed
          stringr::str_replace_all("#", "  ") %>%  # tf specific, leading pound sign to spaces
          trimws()
      )

      #### splits data into variables by splitting at multiple (>= 2) spaces ####
      data_1 <-
        unlist(purrr::map(data_1, stringr::str_split, "\\s{2,}"),
               recursive = FALSE)

      #### breaks data into subsets based on how many variables it has ####
      data_length_3 <- data_1[purrr::map(data_1, length) == 3]
      data_length_4 <- data_1[purrr::map(data_1, length) == 4]
      data_length_5 <- data_1[purrr::map(data_1, length) == 5]
      data_length_6 <- data_1[purrr::map(data_1, length) == 6]
      data_length_7 <- data_1[purrr::map(data_1, length) == 7]

      # treatment of DQs new 8/19
      suppressWarnings(DQ <-
                         data_1[stringr::str_detect(data_1, Time_Score_String, negate = TRUE) == TRUE])
      DQ_length_3 <- DQ[purrr::map(DQ, length) == 3]
      DQ_length_4 <- DQ[purrr::map(DQ, length) == 4]

  }
