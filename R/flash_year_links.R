#' Collects all meet links from a given year on Flash Results
#'
#' Used in scraping flashresults.com.  Collects meet names, dates, and locations along with a link the the associated results landing page.
#'
#' @author Gregory A. Pilgrim \email{gpilgrim2670@@gmail.com} and George M. Perry
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr bind_cols
#' @importFrom dplyr case_when
#' @importFrom stringr str_detect
#' @importFrom stringr str_split_fixed
#' @importFrom stringr str_remove
#' @importFrom stringr str_match
#' @importFrom rvest html_nodes
#' @importFrom rvest html_table
#' @importFrom rvest html_attr
#'
#' @param flash_year a link to a year landing page on flashresults.com
#' @return returns a data frame with meet names, dates, locations, and links to flash results
#'
#' @examples \donttest{flash_year_links("https://flashresults.com/2015results.htm")}
#'
#' @export

flash_year_links <- function(flash_year) {

  # flash_year <- "https://www.flashresults.com/2019results.htm"

  main <- xml2::read_html(flash_year)

  # table of meet name, date, location
  main_table <- main %>%
    rvest::html_nodes("table") %>%
    rvest::html_table(fill = TRUE)

  # list of links
  main_links <- main %>%
    rvest::html_nodes("tr") %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")

  # main_table <- data.frame(main_table[[1]][,-2], stringsAsFactors = FALSE)
  main_table <-
    data.frame(main_table[[1]], stringsAsFactors = FALSE)

  # clean up main table
  main_table <- main_table %>%
    dplyr::select(1) %>%
    dplyr::rename("Input" = 1) %>%
    dplyr::filter(stringr::str_detect(Input, "Results"))

  main_table <- main_table %>%
    dplyr::mutate(Meet = stringr::str_split_fixed(Input, "\n", 3)[, 1]) %>%
    dplyr::mutate(Date = stringr::str_split_fixed(Input, "\n", 3)[, 2]) %>%
    dplyr::mutate(Location = stringr::str_split_fixed(Input, "\n", 3)[, 3]) %>%
    dplyr::mutate(Location = dplyr::case_when(stringr::str_detect(Location, "^$") == TRUE ~ str_split_fixed(Date, "-", 3)[,3],
                                              TRUE ~ Location)) %>%
    dplyr::select(-Input) %>%
    # tidyr::separate(Input, into = c("Meet", "Date", "Location"), sep = "\\n") %>% # JumpeR doesn't have tidyr dependency
    dplyr::mutate(Meet = stringr::str_remove(Meet, "\\s-(.*)")) %>%
    dplyr::mutate(Location = ifelse(
      is.na(Location),
      stringr::str_match(Date, "[^\\-]+$"),
      Location
    )) %>%
    dplyr::mutate(Date = stringr::str_remove(Date, "[^\\d]+$")) %>%
    dplyr::mutate(Date = stringr::str_remove(Date, "^[^(A-Z)]*")) %>%
    dplyr::mutate(Location = stringr::str_remove(Location, "^[^(A-Z)]*")) %>%
    dplyr::mutate(dplyr::across(where(is.character), stringr::str_trim)) # remove whitespaces

  # bind table and links together
  year_table <- dplyr::bind_cols(main_table, main_links)

  # add http to links that need it
  year_table <- year_table %>%
    dplyr::rename("MeetLink" = 4) %>%
    dplyr::filter(stringr::str_detect(MeetLink, "xc") == FALSE) %>%
    dplyr::mutate(MeetLink = ifelse(
      stringr::str_detect(MeetLink, "http"),
      MeetLink,
      paste0("https://flashresults.com", MeetLink)
    ))

  return(year_table)
}

#' @rdname flash_year_links
#' @export
year_links <- flash_year_links
