#' Collects links to all detailed results links from a given event link on Flash
#' Results
#'
#' Used in scraping flashresults.com.  Collects detailed results (often called
#' heat or flight results) from an associated event results landing page.
#' Detailed results often contain splits or attempts results.
#'
#' @importFrom purrr map
#'
#' @param link a link to an event landing page on flashresults.com
#' @return returns list of links to corresponding detailed event result pages
#'
#' @examples \donttest{flash_extract_details_links(
#' "https://flashresults.com/2015_Meets/Outdoor/06-25_USATF/008-3_compiled.htm")}
#'
#' @export

flash_extract_details_links <- function(link) {

    link <- purrr::map(link, flash_extract_details_links_helper)
    link <- unlist(link)
    link <- link[!is.na(link)]
    link <- unique(link)

    return(link)
}

#' @rdname flash_extract_details_links
#' @export
extract_details_links <- flash_extract_details_links

#'Collects links to all detailed results links from a given event link on Flash
#'Results
#'
#'Used in scraping flashresults.com.  Collects detailed results (often called
#'heat or flight results) from an associated event results landing page.
#'Detailed results often contain splits or attempts results.
#'
#'@importFrom stringr str_remove
#'@importFrom stringr str_detect
#'@importFrom rvest html_nodes
#'@importFrom rvest html_attr
#'
#'@param link_helper a link to an event landing page on flashresults.com
#'@return returns list of links to corresponding detailed event result pages
#'
#'@seealso \code{flash_extract_details_links_helper} is a helper function inside
#'  \code{\link{flash_extract_details_links}}

flash_extract_details_links_helper <- function(link_helper = link) {

  # link_helper <- links[1]

  if(stringr::str_detect(link_helper, "\\.html?$") == FALSE){
    # message(paste0("Link ", link_helper, " is invalid.  No details links extracted"))

    return(NA)

  } else {

    page_contents <- xml2::read_html(link_helper, options = "DTDLOAD")

    link_helper_details <- page_contents %>%
      rvest::html_nodes("a") %>%
      rvest::html_attr("href")

    # Throws and horizontal jumps have the 'slug': 028-1_compiledSeries.htm. Running and vertical jumps have only digits and dashes in their slug. So we need to save whichever appears: compiledSeries | digits-and-dashes.

    link_helper_details <-  subset(link_helper_details, grepl("compiledSeries.htm|([0-9]{1,}-[0-9]{1,}-[0-9]{1,}).htm", link_helper_details))

    url_base <- stringr::str_remove(link_helper, "[^\\/]*$")

    link_helper_details <- paste0(url_base, link_helper_details)
    link_helper_details <- unlist(link_helper_details)
    link_helper_details <- unique(link_helper_details)

    return(link_helper_details)
  }
}
