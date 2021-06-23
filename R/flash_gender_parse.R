#' Pulls out gender label from text of flash results html page
#'
#' Locates an gender label in text of results from a flash results html page for
#' a given event.
#'
#' @importFrom stringr str_match
#' @importFrom stringr str_to_title
#'
#' @param text raw text of an event page from Flash Results
#' @return a one element list containing the gender of the event
#'
#' @seealso \code{flash_gender_parse} is a helper function inside
#'   \code{\link{flash_parse_table}}

flash_gender_parse <- function(text) {

  #### testing ####
  # link <- "https://www.flashresults.com/2021_Meets/Indoor/03-11_NCAA/017-2-01.htm"
  # text <- xml2::read_html(link, options = c("DTDLOAD", "NOBLANKS")) %>%
  #   rvest::html_text()


  #### actual function ####

  # build list of regex for all T&F event genders
  genders <- paste0("(?i)", c("(?<=\\s)men", "women", "boys", "girls", "mixed"), collapse = "|")

  # find event gender in text
  event_gender <- stringr::str_match(text, genders)
  event_gender <-
    event_gender[!is.na(event_gender)] # remove NAs from list

  if (length(event_gender) == 0) {
    event_gender <- "Unknown"
    message("No event gender detected, defaulting to 'Unknown'")
  }

  # keep only first element of list (ideally there should only be one element anyway)
  event_gender <- event_gender[1]

  # clean event gender
  event_gender <- event_gender %>%
    stringr::str_to_title(event_gender)

  return(event_gender)
}
