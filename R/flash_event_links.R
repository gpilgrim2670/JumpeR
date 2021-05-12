#' Collects all event result links from a meet landing page on flashresults.com
#'
#' Used in scraping flashresults.com.  Collects event result links from a meet
#' landing page
#'
#' @author Gregory A. Pilgrim \email{gpilgrim2670@@gmail.com} and George M.
#'   Perry
#'
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove
#' @importFrom rvest html_nodes
#' @importFrom rvest html_attr
#'
#' @param meet_home a link to a meet landing page on flashresults.com
#' @return returns a list of links to individual events from a given meet
#'
#' @examples \donttest{flash_event_links("https://flashresults.com/2019_Meets/Outdoor/07-25_USATF_CIS/")}
#'
#' @export

flash_event_links <- function(meet_home) {

  # meet_home <- "https://flashresults.com/2019_Meets/Outdoor/07-25_USATF_CIS/"

  # remove index.html if present
  meet_home <- stringr::str_remove(meet_home, "index\\.html?$")

  # read in page
  page_contents <- xml2::read_html(meet_home)

  # collect links
  # this will grab all "href" from from a table, tr, a node
  links <- page_contents %>%
    rvest::html_nodes("table") %>%
    rvest::html_nodes("tr") %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")

  # remove NA links
  links <- links[!is.na(links)]

  # Filter out start lists and scores
  compiled_links <- links[stringr::str_detect(links, "compiled") == TRUE]

  # links are missing their beginnings, which is just web_url from above
  compiled_links <- paste0(meet_home, compiled_links)
  compiled_links <- unlist(compiled_links)
  compiled_links <- unique(compiled_links) # remove duplicates

  return(compiled_links)
}

#' @rdname flash_event_links
#' @export
meet_links <- flash_event_links
