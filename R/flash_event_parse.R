#' Pulls out event label from text of flash results html page
#'
#' Locates an event label in text of results from a flash results html page for a given event.
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @importFrom stringr str_match
#' @importFrom stringr str_replace
#' @importFrom stringr str_to_title
#'
#' @param text raw text of an event page from Flash Results
#' @return a one element list containing the name of the event
#'
#' @seealso \code{flash_event_parse} is a helper function inside \code{\link{flash_parse_table}}

flash_event_parse <- function(text){

  # build list of regex for all event names
  all_events <-
    paste0(
      "(?i)",
      c(
        "Shot put",
        "Discus",
        "Javelin",
        "Hammer",
        "Weight",
        "Long jump",
        "Triple jump",
        "High jump",
        "Pole vault",
        "decathlon",
        "heptaathlon",
        "pentathlon",
        "\\d?\\s?x?\\s?\\d{3,4} relay",
        "distance relay",
        "distance medley relay",
        "\\dx\\d{2,}\\s*m\\srelay", # for relays
        "\\d{3}\\s*m[:alpha:]*\\s*h[:alpha:]*", # for hurdles, to differentiate between 400 m dash and 400 m hurtles
        "\\d000\\s*m\\sSteeplechase",
        # "110\\s*m[:alpha:]*\\s*h[:alpha:]*",
        # "400\\s*m[:alpha:]*\\s*h[:alpha:]*",
        # "100\\s*m(eter)?\\s*h(urdles)?",
        # "110\\s*m(eter)?\\s*h(urdles)?",
        # "400\\s*m(eter)?\\s*h(urdles)?",
        "\\d{2,5}\\s*m ",
        "\\d{1,2}?,\\d{3}\\s*m ",
        # "60\\s*m",
        # "100\\s*m",
        # "200\\s*m",
        # "300\\s*m",
        # "\\B00\\s*m\\b",
        # "400\\s*m",
        # "800\\s*m",
        # "1500\\s*m",
        # "20\\,?000\\s*m",
        "\\d mile"
        # "1 mile",
        # "5*000\\s*m",
        # "10*000\\s*m"
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

  # clean even name
  event_name <- event_name %>%
    stringr::str_to_title() %>%  # capitalizes every word and also m/M
    stringr::str_replace("(\\d)\\s\\M$", "\\1m") %>% # bring M next to digit as m
    stringr::str_replace("(\\d)\\s\\M ", "\\1m ") %>% # bring M next to digit as m
    stringr::str_replace("(\\d)\\s\\M(eter)? Hurdles$", "\\1m Hurdles") %>%
    stringr::str_replace("1 Mile", "Mile") %>% # reformat mile event name
    stringr::str_replace("(\\d)0000m$", "\\10000m Race Walk") # name race walks

  return(event_name)
}
