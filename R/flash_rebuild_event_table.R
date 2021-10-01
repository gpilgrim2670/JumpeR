#' Rebuilds tables that \code{rvest::html_table} can't parse inside of
#' \code{\link{flash_parse_table}}
#'
#' Extracts individual \code{td} and \code{th} elements from html tables on
#' Flash Results that cannot be parsed by code{rvest::html_table} (due to
#' formatting issues in the html code)
#'
#' @importFrom dplyr na_if
#' @importFrom stringr str_remove
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_split
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
#' @importFrom purrr map
#' @importFrom utils tail
#'
#' @param event_url_rebuild a link to an event page on flashresults.com
#' @return returns a data frame of event results
#'
#' @seealso \code{rebuild_event_table} is a helper function inside
#'   \code{\link{flash_parse_table}}

flash_rebuild_event_table <- function(event_url_rebuild) {
  #### testing ####
  # event_url_rebuild <-
  #   "https://flashresults.com/2016_Meets/Indoor/02-05_CharlieThomasInvite/001-1-03.htm"
  # event_url_rebuild <-
  #   "https://flashresults.com/2017_Meets/Indoor/01-13_AggieTeam/003-1-05.htm"

  #### actual function ####
  event_table <- event_url_rebuild %>%
    xml2::read_html(options = c("DTDLOAD", "NOBLANKS")) %>% # need DTDLOAD because tables are updated from external source
    rvest::html_nodes("[id^=splitevents]")

  # column headers
  ths <- event_table %>%
    rvest::html_nodes("th") %>%
    rvest::html_text()

  # ths <- ths[ths != ""]

  # cell values
  tds <- event_table %>%
    rvest::html_nodes('td') %>%
    rvest::html_text()

  # going to build matrix from cells and columns
  num_col <- length(ths)
  num_row <-
    ceiling(length(tds) / num_col) # use ceiling to insure matrix is big enough to hold all of the data

  number_of_nas_need <-
    (num_row * num_col) - length(tds) # number of NAs need to pad tds to match size of matrix

  tds <- c(tds, rep(NA, number_of_nas_need)) # pad tds

  # build matrix, convert to dataframe
  df <- tds %>%
    matrix(nrow = num_row,
           ncol = num_col,
           byrow = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    setNames(ths) %>% # names from column names in html table
    dplyr::na_if("")

  # rebuilding non-rectangular data frames
  x <- c(t(df)) # convert to vector
  x <- paste(x, collapse = "_X_") # vector to string

  # will add "Begin" string in front of Place Lane strings
  # different cases for results with Bib in them becuase it messes with left to right order
  if ("Bib" %in% ths) {
    x <- x %>%
      stringr::str_replace_all("(^\\d{1}\\_X\\_\\d{1,}\\_X\\_\\d{1,}\\_X\\_)",
                               "Begin\\_X\\_\\1") %>% # for first place, won't be proceeded by "\\&"
      stringr::str_replace_all(
        "(?<!Begin)(\\_X\\_\\d{1,}\\_X\\_\\d{1,}\\_X\\_\\d{1,}\\_X\\_)",
        "\\_X\\_Begin\\1"
      ) %>% # for other places with legal finish
      stringr::str_replace_all("(\\_X\\_DNF\\_X\\_\\d{1,}\\_X\\_\\d{1,}\\_X\\_)",
                               "\\_X\\_Begin\\1") %>% # DNF
      stringr::str_replace_all("(\\_X\\_DNS\\_X\\_\\d{1,}\\_X\\_\\d{1,}\\_X\\_)",
                               "\\_X\\_Begin\\1") %>% # DNS
      stringr::str_replace_all("(\\_X\\_FS\\_X\\_\\d{1,}\\_X\\_\\d{1,}\\_X\\_)",
                               "\\_X\\_Begin\\1") %>% # FS
      stringr::str_replace_all("(\\_X\\_DQ\\_X\\_\\d{1,}\\_X\\_\\d{1,}\\_X\\_)",
                               "\\_X\\_Begin\\1") # DQ
  } else if (any(c("Lane", "Ln")  %in% ths)) {
    x <- x %>%
      stringr::str_replace_all("(^\\d{1}\\_X\\_\\d{1,}\\_X\\_)", "Begin\\_X\\_\\1") %>% # for first place, won't be proceeded by "\\&"
      stringr::str_replace_all("(?<!Begin)(\\_X\\_\\d{1,}\\_X\\_\\d{1,}\\_X\\_)",
                               "\\_X\\_Begin\\1") %>% # for other places with legal finish
      stringr::str_replace_all("(\\_X\\_DNF\\_X\\_\\d{1,}\\_X\\_)", "\\_X\\_Begin\\1") %>% # DNF
      stringr::str_replace_all("(\\_X\\_DNS\\_X\\_\\d{1,}\\_X\\_)", "\\_X\\_Begin\\1") %>% # DNS
      stringr::str_replace_all("(\\_X\\_FS\\_X\\_\\d{1,}\\_X\\_)", "\\_X\\_Begin\\1") %>% # FS
      stringr::str_replace_all("(\\_X\\_DQ\\_X\\_\\d{1,}\\_X\\_)", "\\_X\\_Begin\\1") # DQ
  } else {
    x <- x %>%
      stringr::str_replace_all("(^\\d{1}\\_X\\_)", "Begin\\_X\\_\\1") %>% # for first place, won't be proceeded by "\\&"
      stringr::str_replace_all("(?<!Begin)(\\_X\\_\\d{1,}\\_X\\_)", "\\_X\\_Begin\\1") %>% # for other places with legal finish
      stringr::str_replace_all("(\\_X\\_DNF\\_X\\_)", "\\_X\\_Begin\\1") %>% # DNF
      stringr::str_replace_all("(\\_X\\_DNS\\_X\\_)", "\\_X\\_Begin\\1") %>% # DNS
      stringr::str_replace_all("(\\_X\\_FS\\_X\\_)", "\\_X\\_Begin\\1") %>% # FS
      stringr::str_replace_all("(\\_X\\_DQ\\_X\\_)", "\\_X\\_Begin\\1") # DQ
  }

  x <- stringr::str_split(x, "_X_")[[1]] # string to vector

  x <-
    split(x, cumsum(x == "Begin")) # each list, which will be a row, should begin with "Begin"

  x <- purrr::map(x, utils::tail,-1) # remove all the "Begin"s

  # reassemble dataframe
  df_new <-
    setNames(transform(do.call(rbind, lapply(x, "[", 1:ncol(
      df
    )))), names(df)) %>%
    dplyr::na_if("NA")

  if(nrow(df_new) >= nrow(df)){
    df <- df_new
  } else {
    rm(df_new)
  }

  # remove column named only with new line character
  df <- df[names(df) %in% c("\n") == FALSE]

  return(df)
}
