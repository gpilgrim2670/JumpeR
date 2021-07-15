#' Pulls out event label from text of flash results html page
#'
#' Locates an event label in text of results from a flash results html page for
#' a given event.
#'
#' @importFrom stringr str_match
#' @importFrom stringr str_replace
#' @importFrom stringr str_to_title
#'
#' @param text raw text of an event page from Flash Results
#' @return a one element list containing the name of the event
#'
#' @seealso \code{flash_event_parse} is a helper function inside
#'   \code{\link{flash_parse_table}}

flash_event_parse <- function(text){

  #### testing ####
  # link <- "https://www.flashresults.com/2019_Meets/Outdoor/07-25_USATF_CIS/022-1-01.htm"
  # link <- "https://flashresults.com/2015_Meets/Indoor/03-13_NCAA/026-1-01.htm"
  # text <- xml2::read_html(link, options = c("DTDLOAD", "NOBLANKS")) %>%
  #   rvest::html_text()
  # link <- "https://flashresults.com/2016_Meets/Outdoor/05-10_BigSouth/012-1_compiled.htm"
  # link <- "https://flashresults.com/2021_Meets/Indoor/02-25_SEC/030-1-01.htm"
  # text <- xml2::read_html(link, options = c("DTDLOAD", "NOBLANKS")) %>%
  #   rvest::html_text()

  # build list of regex for all event names
  all_events <-
    paste0(
      "(?i)",
      c(
        "((Hep|Pen|Dec)(.*\\s))?Shot put",
        "((Hep|Pen|Dec)(.*\\s))?Discus",
        "((Hep|Pen|Dec)(.*\\s))?Javelin",
        "Hammer",
        "Weight",
        "((Hep|Pen|Dec)(.*\\s))?Long jump",
        "Triple jump",
        "((Hep|Pen|Dec)(.*\\s))?High jump",
        "((Hep|Pen|Dec)(.*\\s))?Pole vault",
        "\\d{1,5}.*walk",
        "\\d000\\s*m\\sSteeplechase",
        "distance relay",
        "sprint relay",
        "DMR",
        "\\d{3,4}?\\s?SMR\\s?(Swedish)?",
        "[:alpha:]+ medley( relay)?",
        "\\d?\\s?x?\\s?\\d{3,4} (meter)?\\srelay",
        "\\d?\\s?x?\\s?\\d{3,4} [m|M](eter)?\\sshuttle\\shurdle",
        "\\d?\\s?x?\\s?\\d [m|M](ile)?\\srelay",
        "\\dx\\d{2,}\\s*m\\srelay", # for relays
        "((Hep|Pen|Dec)(.*\\s))?\\d{2,5}\\s*[m|M](eter)?(\\shurdles)?", # also captures regular running events like 400m etc.
        "\\d? +mile(.*walk)?", # miles and racewalks as 2 Mile etc.
        "((Hep|Pen|Dec)(.*\\s))?\\d{2,3}\\s*m[:alpha:]*\\s*h[:alpha:]*"
      ),
      collapse = "|"
    )

  # find event name in text
  event_name <- stringr::str_match(text, all_events) # can produce list with NAs
  event_name <- event_name[!is.na(event_name)] # remove NAs from list

  if(length(event_name) == 0){
    event_name <- "Unknown"
    message("No event name detected, defaulting to 'Unknown'")
  }

  # keep only first element of list (ideally there should only be one element anyway)
  event_name <- event_name[1]

  # clean event name
  event_name <- event_name %>%
    stringr::str_to_title() %>%  # capitalizes every word and also m/M
    stringr::str_replace("(\\d)\\sM$", "\\1m") %>% # bring M next to digit as m
    stringr::str_replace("(\\d)\\sM ", "\\1m ") %>% # bring M next to digit as m
    stringr::str_replace("(\\d)\\s[M|m](eter)? Hurdles$", "\\1m Hurdles") %>%
    stringr::str_replace("(\\d)\\s[M|m](eter)? Relay$", "\\1m Relay") %>%
    stringr::str_replace("(\\d)\\s[M|m](eter)", "\\1m ") %>% # bring Meter next to digit as m
    stringr::str_replace("(?<![x|X])1 Mile", "Mile") %>%  # reformat mile event name, but not 4x1 Mile Relay
    stringr::str_remove("(Women )|(Men )|(Boys )|(Girls)|(Mixed )") %>%
    stringr::str_replace("Distance Medley", "Dmr") %>%
    stringr::str_replace("Sprint Medley", "Smr")
    # stringr::str_replace("(\\d)0000m$", "\\10000m Race Walk") # name race walks

  return(event_name)
}
