#' Collects all meet links from a given year on Flash Results
#'
#' Used in scraping flashresults.com.  Collects meet names, dates, and locations
#' along with a link the the associated results landing page.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr rowwise
#' @importFrom stringr str_detect
#' @importFrom stringr str_split_fixed
#' @importFrom stringr str_remove
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_extract
#' @importFrom stringr str_extract_all
#' @importFrom rvest html_nodes
#' @importFrom rvest html_table
#' @importFrom rvest html_attr
#'
#' @param flash_year a link to a year landing page on flashresults.com
#' @return returns a data frame with meet names, dates, locations, and links to
#'   flash results
#'
#' @examples \donttest{flash_year_links("https://flashresults.com/2015results.htm")}
#'
#' @export

flash_year_links <- function(flash_year) {

  # flash_year <- "https://www.flashresults.com/2020results.htm"
  # flash_year <- "https://www.flashresults.com/2019results.htm"
  # flash_year <- "https://www.flashresults.com/2018results.htm"
  # flash_year <- "https://www.flashresults.com/2017results.htm"
  # flash_year <- "https://www.flashresults.com/2016results.htm"
  # flash_year <- "https://www.flashresults.com/2015results.htm"

  main <- xml2::read_html(flash_year)

  # extract meet year
  meet_year <- stringr::str_extract(flash_year, "20\\d\\d")

  # table of meet name, date, location
  main_table <- main %>%
    rvest::html_nodes("table") %>%
    rvest::html_table(fill = TRUE)

  # list of links
  main_links <- main %>%
    rvest::html_nodes("tr") %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href") %>%
    {ifelse(stringr::str_detect(., "^/|^h"), ., paste0("/", .))}

  # main_table <- data.frame(main_table[[1]][,-2], stringsAsFactors = FALSE)
  main_table <-
    data.frame(main_table[[1]], stringsAsFactors = FALSE)

  # clean up main table
  main_table <- main_table %>%
    dplyr::select(1) %>%
    dplyr::rename("Input" = 1) %>%
    dplyr::filter(stringr::str_detect(Input, "Results"))

  main_table <- main_table %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Meet = stringr::str_split_fixed(Input, "\n", 3)[, 1],
                  Meet = stringr::str_remove(Meet, "\\s--\\sResults")) %>%
    dplyr::mutate(Input = stringr::str_replace_all(Input, "\\s{2,}", " ")) %>%
    dplyr::mutate(
      Meet_Date = stringr::str_extract_all(Input, paste0(
        "(",
        paste(month.name, collapse = "|"),
        ")",
        "\\s\\d{1,2}-?\\d?\\d?"
      )),
      Meet_Date = paste(Meet_Date, collapse = "-"), # collects together meets that span multiple months, Feb 27-March 1 etc.
      Meet_Date = paste(Meet_Date, meet_year, sep = " "),
      Meet_Date = stringr::str_replace(Meet_Date, "--", "-")
    ) %>%
    dplyr::mutate(Location = stringr::str_extract_all(Input, "(?<=\\d\\s?\\-\\s).*", simplify = TRUE)) %>%
    # View()
    dplyr::select(-Input) %>%
    dplyr::mutate(dplyr::across(where(is.character), stringr::str_trim)) # remove white spaces

  # bind table and links together
  year_table <- cbind(main_table, main_links)

  # add http to links that need it
  year_table <- year_table %>%
    dplyr::rename("Meet_Link" = main_links) %>%
    dplyr::filter(stringr::str_detect(Meet_Link, "xc") == FALSE) %>%
    dplyr::mutate(Meet_Link = ifelse(
      stringr::str_detect(Meet_Link, "http"),
      Meet_Link,
      paste0("https://flashresults.com", Meet_Link)
    )) %>%
    dplyr::mutate(Meet_Link = stringr::str_remove(Meet_Link, "index\\.htm"))

  return(year_table)
}

#' @rdname flash_year_links
#' @export
year_links <- flash_year_links
