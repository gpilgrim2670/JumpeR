#' Reads track and field results into a list of strings in preparation for
#' parsing with \code{tf_parse}
#'
#' Outputs list of strings to be processed by \code{tf_parse}
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr case_when
#' @importFrom dplyr na_if
#' @importFrom dplyr select
#' @importFrom dplyr bind_rows
#' @importFrom dplyr arrange
#' @importFrom dplyr across
#' @importFrom dplyr left_join
#' @importFrom dplyr all_of
#' @importFrom dplyr starts_with
#' @importFrom dplyr matches
#' @importFrom stringr str_remove
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_detect
#' @importFrom stringr str_split
#' @importFrom stringr str_length
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_replace
#' @importFrom stringr str_subset
#' @importFrom stringr str_remove
#' @importFrom purrr map
#' @importFrom purrr map_lgl
#' @importFrom stats setNames
#' @importFrom SwimmeR `%!in%`
#'
#' @param file a .pdf or .html file (could be a url) where containing track and
#'   field results.  Must be formatted in a "normal" fashion - see vignette
#' @param avoid a list of strings.  Rows in \code{file} containing these strings
#'   will not be included. For example "Record:", often used to label records,
#'   could be passed to \code{avoid}.  The default is \code{avoid_default},
#'   which contains many strings similar to "Record:".  Users can supply their
#'   own lists to \code{avoid}.
#' @param typo a list of strings that are typos in the original results.
#'   \code{tf_parse} is particularly sensitive to accidental double spaces, so
#'   "Central  High School", with two spaces between "Central" and "High" is a
#'   problem, which can be fixed.  Pass "Central  High School" to \code{typo}.
#' @param replacement a list of fixes for the strings in \code{typo}.  Here one
#'   could pass "Central High School" (one space between "Central" and "High")
#'   to fix the issue described in \code{typo}
#' @param relay_athletes should \code{tf_parse} try to include the names of
#'   relay athletes for relay events?  Names will be listed in new columns
#'   "Relay-Athlete_1", "Relay_Athlete_2" etc.  Defaults to \code{FALSE}.
#' @param flights should \code{tf_parse} try to include flights for
#'   jumping/throwing events?  Please note this will add a significant number of
#'   columns to the resulting dataframe.  Defaults to \code{FALSE}.
#' @param flight_attempts should \code{tf_parse} try to include flights results
#'   (i.e. "PASS", "X", "O") for high jump and pole value events?  Please note
#'   this will add a significant number of columns to the resulting dataframe.
#'   Defaults to \code{FALSE}
#' @param split_attempts should \code{tf_parse} split attempts from each flight
#'   into separate columns?  For example "XXO" would result in three columns,
#'   one for "X', another for the second "X" and third for "O".  There will be a
#'   lot of columns.  Defaults to \code{FALSE}
#'
#' @return a data frame of track and field results
#'
#' @seealso \code{tf_parse} is meant to be preceded by
#'   \code{\link{read_results}}
#'
#' @examples \donttest{tf_parse(
#' read_results("https://www.flashresults.com/2018_Meets/Outdoor/05-05_A10/015-1.pdf"),
#' flights = TRUE,
#' flight_attempts = TRUE,
#' split_attempts = TRUE)
#' }
#'
#' @export

tf_parse <-
  function(file,
           avoid = avoid_default,
           typo = typo_default,
           replacement = replacement_default,
           relay_athletes = FALSE,
           flights = FALSE,
           flight_attempts = FALSE,
           split_attempts = FALSE) {

    # file <- read_results("http://leonetiming.com/2019/Indoor/GregPageRelays/Results.htm")
    # file <- "http://www.leonetiming.com/2020/Indoor/IvyLeague/Results.htm"
    # file <- read_results(file)
    # file <- read_results("http://tfresultsdata.deltatiming.com/2019-horizon-outdoor-championships/190503F026.htm")
    # typo <- typo_default
    # replacement <- replacement_default
    # avoid <- avoid_default
    # flights <- TRUE
    # flight_attempts <- TRUE
    # split_attempts <- TRUE
    # relay_athletes <- TRUE


    #### default typo and replacement strings ####
    typo_default <- c("typo")

    replacement_default <- c("typo")

    if (length(typo) != length(replacement)) {
      stop("typo and replacement must have the same number of elements (be the same length)")
    }

    if (all(flight_attempts == TRUE & flights == FALSE)) {
      stop("If flight_attempts is set to TRUE flights should also be set to TRUE.")
    }

    if (all(flight_attempts == FALSE & split_attempts == TRUE)) {
      stop("If split_attempts is set to TRUE flights and flight_attempts should also be set to TRUE.")
    }

    #### strings that if a line begins with one of them the line is ignored ####
    avoid_default <- c("Record\\:",
        "[:alpha:]\\: .*")

    #### testing setup ####
    # file_1 <-
    #    system.file("extdata", "Results-IVP-Track-Field-Championship-2019-20-v2.pdf", package = "JumpeR")
    #
    # file_2 <- "http://results.yentiming.com/2019/Indoor/12-21-18-west.htm"
    #
    # file_3 <- system.file("extdata", "underdistance-2020-result.pdf", package = "JumpeR")
    #
    # file_4 <- "http://results.yentiming.com/2020/Indoor/2-29-20-MOC.htm"
    # file <- read_results("http://leonetiming.com/2019/Indoor/GregPageRelays/Results.htm")
    #
    # file_1 <- read_results(file_1)
    # file_2 <- read_results(file_2)
    # file_3 <- read_results(file_3)
    # file_4 <- read_results(file_4)
    # file_5 <- read_results(file_5)
    #
    # file <- c(file_1, file_2, file_3, file_4, file_5)
    # file <-
    #   system.file("extdata", "SMTFA-2019-Full-Results.pdf", package = "JumpeR")
    # file <-
    #   system.file("extdata", "day1-combo.pdf", package = "JumpeR")
    #
    # file <- read_results(event_links[75])
    # file <- read_results(system.file("extdata", "Results-IVP-Track-Field-Championship-2019-20-v2.pdf", package = "JumpeR"))
    # file <- read_results("https://www.flashresults.com/2020_Meets/Indoor/02-21_VTChallenge/031-1.pdf")
    # avoid <- c("[:alpha:]\\: .*")
    # typo <- "typo"
    # replacement <- "typo"
    # relay_athletes <- TRUE

    if(file[2] == "try flash_parse_table"){

      data <- flash_parse_table(link = file[1], wide_format = TRUE, clean = TRUE)

      message("Column names for Flash results in table form are derived from the source data and may not match column names from other sources")

      # if(flights == FALSE){
      #   data <- data %>%
      #     dplyr::select(-dplyr::starts_with("Flight"))
      # }
      #
      # if(all(flights == TRUE, flight_attempts == FALSE))
      #   data <- data %>%
      #   dplyr::select(-dplyr::matches("Flight\\_\\d{1,}\\_"))

      # return(data)

    } else {


    #### assign row numbers ####
    as_lines_list_2 <- add_row_numbers(text = file)

    #### clean input data ####
    suppressWarnings(
      raw_results <- as_lines_list_2 %>%
        .[purrr::map_lgl(., ~ !any(stringr::str_detect(., avoid)))] %>% # remove lines contained in avoid
        stringr::str_replace_all(stats::setNames(replacement, typo)) %>%
        stringr::str_replace_all("\\*(\\d{1,})", replacement = "\\1") # removes * placed in front of place number in ties
      ) # removes * placed in front of place number in ties

    if(all((length(raw_results) < 2) &
           stringr::str_detect(raw_results, "^http"))) {
      stop("Please pass links to read_results prior to calling tf_parse")
    }

    #### Flash results or Hy-Tek ####
    # Flash
    if(any(stringr::str_detect(raw_results[1:5], "CONDITIONS|\\d\\d?\\:\\d{2}\\s?A?PM\\s+\\d\\d? [A-Z][a-z]{2} \\d{4}"), na.rm = TRUE) == TRUE){
      data <-
        flash_parse(
          flash_file = raw_results,
          flash_flights = flights,
          flash_flight_attempts = flight_attempts,
          flash_split_attempts = split_attempts
        )

      # return(data)

    } else { # Hy-Tek


      data <- hytek_parse(
        hytek_file = raw_results,
        hytek_relay_athletes = relay_athletes,
        hytek_flights = flights,
        hytek_flight_attempts = flight_attempts,
        hytek_split_attempts = split_attempts
      )

      # return(data)

    }
    }
    return(data)
  }
