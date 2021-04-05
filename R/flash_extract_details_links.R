#' Collects links to all detailed results links from a given event link on Flash Results
#'
#' Used in scraping flashresults.com.  Collects detailed results (often called heat or flight results) from an associated event results landing page.  Detailed results often contain splits or attempts results.
#'
#' @author Gregory A. Pilgrim
#'
#' @importFrom stringr str_remove
#' @importFrom rvest html_nodes
#' @importFrom rvest html_attr
#'
#' @param link a link to an event landing page on flashresults.com
#' @return returns list of links to corresponding detailed event result pages
#'
#' @examples \donttest{flash_extract_details_links("https://flashresults.com/2015_Meets/Outdoor/06-25_USATF/008-3_compiled.htm")}
#'
#' @export

flash_extract_details_links <- function(link) {

  page_contents <- xml2::read_html(link, options = "DTDLOAD")

  links_details <- page_contents %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")

  # Throws and horizontal jumps have the 'slug': 028-1_compiledSeries.htm. Running and vertical jumps have only digits and dashes in their slug. So we need to save whichever appears: compiledSeries | digits-and-dashes.

  links_details <-  subset(links_details, grepl("compiledSeries.htm|([0-9]{1,}-[0-9]{1,}-[0-9]{1,}).htm", links_details))

  url_base <- stringr::str_remove(link, "[^\\/]*$")

  links_details <- paste0(url_base, links_details)
  links_details <- unlist(links_details)

  return(links_details)
}
