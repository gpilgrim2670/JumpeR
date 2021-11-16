#' Pulls out date from text of flash results html page
#'
#' Locates an date in text of results from a flash results html page for a given
#' event.
#'
#' @importFrom stringr str_match
#' @importFrom stringr str_to_title
#' @importFrom utils tail
#'
#' @param text raw text of an event page from Flash Results
#' @return a one element list containing the date of the event
#'
#' @seealso \code{flash_date_parse} is a helper function inside
#'   \code{\link{flash_parse_table}}

flash_date_parse <- function(text) {

  # testing
  # text <- page_content %>%
  #   html_nodes("div p") %>%
  #   html_text()

  #### begin actual function ####

  # build list of regex for all dates of form "Jul 25"
  Date_String <-
    paste(paste0(month.abb, "\\s\\d{1,2}"), collapse = "|")

  # find event date in text
  event_date <- stringr::str_extract_all(text, Date_String)
  event_date <-
    event_date[!is.na(event_date)] # remove NAs from list
  event_date <-
    event_date[lapply(event_date, length) > 0] # remove empty elements from list

  if (length(event_date) == 0) {
    event_date <- NA
    # message("No event date detected, defaulting to 'Unknown'")
  }

  # keep only last element of list because "updated" field where date is is located at the bottom of the page
  # (ideally there should only be one element anyway)
  event_date <- utils::tail(unlist(event_date), 1)


  return(event_date)
}
