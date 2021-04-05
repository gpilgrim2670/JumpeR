#' Rebuilds tables that html_table can't parse within flash_parse_table
#'
#' Extracts individual td and th elements from html tables on Flash Results that cannot be parsed by html_parse (due to formatting issues in the html code)
#'
#' @author Gregory A. Pilgrim
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
#' @return returns a dataframe of event results
#'
#' @seealso \code{rebuild_event_table} is a helper function inside \code{\link{flash_parse_table}}

flash_rebuild_event_table <- function(event_url_rebuild){
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
num_row <- ceiling(length(tds) / num_col) # use ceiling to insure matrix is big enough to hold all of the data

number_of_nas_need <- (num_row * num_col) - length(tds) # number of NAs need to pad tds to match size of matrix

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
x <- paste(x, collapse = "&&") # vector to string

# will add "Begin" string in front of Place Lane strings
# different cases for results with Bib in them becuase it messes with left to right order
if("Bib" %in% ths){
  x <- x %>%
    stringr::str_replace_all("(^\\d{1}&&\\d{1,}&&\\d{1,}&&)", "Begin&&\\1") %>% # for first place, won't be proceeded by "\\&"
    stringr::str_replace_all("(?<!Begin)(&&\\d{1,}&&\\d{1,}&&\\d{1,}&&)", "&&Begin\\1") %>% # for other places with legal finish
    stringr::str_replace_all("(&&DNF&&\\d{1,}&&\\d{1,}&&)", "&&Begin\\1") %>% # DNF
    stringr::str_replace_all("(&&DNS&&\\d{1,}&&\\d{1,}&&)", "&&Begin\\1") %>% # DNS
    stringr::str_replace_all("(&&FS&&\\d{1,}&&\\d{1,}&&)", "&&Begin\\1") %>% # FS
    stringr::str_replace_all("(&&DQ&&\\d{1,}&&\\d{1,}&&)", "&&Begin\\1") # DQ
} else {
  x <- x %>%
    stringr::str_replace_all("(^\\d{1}&&\\d{1,}&&)", "Begin&&\\1") %>% # for first place, won't be proceeded by "\\&"
    stringr::str_replace_all("(?<!Begin)(&&\\d{1,}&&\\d{1,}&&)", "&&Begin\\1") %>% # for other places with legal finish
    stringr::str_replace_all("(&&DNF&&\\d{1,}&&)", "&&Begin\\1") %>% # DNF
    stringr::str_replace_all("(&&DNS&&\\d{1,}&&)", "&&Begin\\1") %>% # DNS
    stringr::str_replace_all("(&&FS&&\\d{1,}&&)", "&&Begin\\1") %>% # FS
    stringr::str_replace_all("(&&DQ&&\\d{1,}&&)", "&&Begin\\1") # DQ

}
x <- stringr::str_split(x, "&&")[[1]] # string to vector

x <-
  split(x, cumsum(x == "Begin")) # each list, which will be a row, should begin with "Begin"

x <- purrr::map(x, utils::tail, -1) # remove all the "Begin"s

# reassemble dataframe
df <-
  setNames(transform(do.call(rbind, lapply(x, "[", 1:ncol(
    df
  )))), names(df)) %>%
  dplyr::na_if("NA")

return(df)
}
