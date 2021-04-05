#' Collects links to all detailed results links from a given event link on Flash Results
#'
#' Used in scraping flashresults.com.  Collects detailed results (often called heat or flight results) from an associated event results landing page.  Detailed results often contain splits or attempts results.
#'
#' @author Gregory A. Pilgrim \email{gpilgrim2670@@gmail.com} and George M. Perry
#'
#' @importFrom purrr map
#'
#' @param link a link to an event landing page on flashresults.com
#' @return returns list of links to corresponding detailed event result pages
#'
#' @examples \donttest{flash_extract_details_links("https://flashresults.com/2015_Meets/Outdoor/06-25_USATF/008-3_compiled.htm")}
#'
#' @export

flash_extract_details_links <- function(link) {

    link <- purrr::map(link, flash_extract_details_links_helper)
    link <- unlist(link)
    link <- link[!is.na(links)]

    return(link)
}
