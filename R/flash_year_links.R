#' Collects all meet links from a given year on Flash Results
#'
#' Used in scraping flashresults.com.  Collects meet names, dates, and locations along with a link the the assoicated results landing page.
#'
#' @author George M. Perry
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr bind_cols
#' @importFrom stringr str_detect
#' @importFrom stringr str_split_fixed
#' @importFrom stringr str_remove
#' @importFrom stringr str_match
#' @importFrom rvest read_html
#' @importFrom rvest html_nodes
#' @importFrom rvest html_table
#' @importFrom rvest html_attr
#'
#' @param flash_year a link to a year landing page on flashresults.com
#' @return returns a data frame with meet names, dates, locations, and links to flash results
#'
#' @examples \donttest{flash_year_links("https://flashresults.com/2015results.htm")}

flash_year_links <- function(flash_year) {

  # flash_year <- "https://flashresults.com/2015results.htm"

  main <- rvest::read_html(flash_year)

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
    dplyr:
    select(-Input) %>%
    # tidyr::separate(Input, into = c("Meet", "Date", "Location"), sep = "\\n") %>% # JumpeR doesn't have tidyr dependancy
    dplyr::mutate(Meet = stringr::str_remove(Meet, "\\s-(.*)")) %>%
    dplyr::mutate(Location = ifelse(
      is.na(Location),
      stringr::str_match(Date, "[^\\-]+$"),
      Location
    )) %>%
    dplyr::mutate(Date = stringr::str_remove(Date, "[^\\d]+$")) %>%
    dplyr::mutate(Date = stringr::str_remove(Date, "^[^(A-Z)]*")) %>%
    dplyr::mutate(Location = stringr::str_remove(Location, "^[^(A-Z)]*"))

  # bind table and links together
  year_table <- dplyr::bind_cols(main_table, main_links)

  # add http to links that need it
  year_table <- year_table %>%
    dplyr::rename("MeetLink" = 4) %>%
    dplyr::mutate(MeetLink = ifelse(
      stringr::str_detect(MeetLink, "http"),
      MeetLink,
      paste0("https://flashresults.com", MeetLink)
    ))

  return(year_table)
}
