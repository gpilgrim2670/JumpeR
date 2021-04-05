#' Collects results from a link to a Flash Results page
#'
#' Used in scraping flashresults.com.  Collects results given in html tables on a speciified page into a data frame.
#'
#' @author Gregory A. Pilgrim \email{gpilgrim2670@@gmail.com} and George M. Perry
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr na_if
#' @importFrom stringr str_remove
#' @importFrom stringr str_match
#' @importFrom stringr str_detect
#' @importFrom rvest html_nodes
#' @importFrom rvest html_attr
#' @importFrom rvest html_table
#'
#' @param link a link to an event landing page on flashresults.com
#' @param wide_format should results be presented in wide format (defaults to \code{FALSE})
#' @return returns a data frame of results scraped from \code{link}
#'
#' @examples \donttest{flash_parse_table("https://www.flashresults.com/2019_Meets/Outdoor/06-13_NBNO/067-4_compiled.htm")}
#'
#' @export


flash_parse_table <- function(link, wide_format = FALSE) {

  # link <- "https://flashresults.com/2015_Meets/Outdoor/06-25_USATF/009-2-01.htm"
  # link <- "https://flashresults.com/2015_Meets/Outdoor/05-28_NCAAEast/005-1-03.htm"
  # link <- "https://flashresults.com/2017_Meets/Outdoor/06-22_USATF/004-2-02.htm"

  page_content <- xml2::read_html(link, options = c("DTDLOAD", "NOBLANKS"))

  #This method allows us to keep the \n in the data, which is essential to cleaning.
  xml2::xml_find_all(page_content, ".//br") %>%
    xml2::xml_add_sibling("p", "\n")

  xml2::xml_find_all(page_content, ".//br") %>%
    xml2::xml_remove()

  #fill = TRUE seems to get all tables in, but reads '-' as u0097 on vertical jumps
  list_of_tables <- rvest::html_table(page_content, fill = TRUE, header = TRUE)

  # table components
  get_atts <- lapply(list_of_tables, attributes)

  # determine length of table
  get_lengths <- function(df) {
    return(length(df$names))
  }

  # desired table is the longest one
  result_table <-
    list_of_tables[which.max(lapply(get_atts, get_lengths))]

  # table as data frame
  df <- result_table[[1]]

  # if there are more than one blank headers
  # try to collect headers from raw(er) html
  if(sum(names(df) %in% "") > 1){
    event_table <- link %>%
      xml2::read_html(options = c("DTDLOAD", "NOBLANKS")) %>% # need DTDLOAD because tables are updated from external source
      rvest::html_nodes("[id^=events]")

    # column headers
    ths <- event_table %>%
      rvest::html_nodes("th") %>%
      rvest::html_text()

    if(length(names(df)) == length(ths)){
      names(df) <- ths
    }

  }


  # if the previous code comes up empty
  # this will collect rawer contents of th (headers) and td (cells)
  # and attempt to build a matrix.
  # if the data isn't rectangular (like in DNF cases for example)
  # this will result in an incorrectly formatted data frame
  if (any(nrow(df) < 1 |
          (!any(stringr::str_detect(na.omit(as.vector(t(df))), "Athlete|Name|Team"))) &
          !any(stringr::str_detect(names(df), "Athlete|Name|Team")))
  ) { # anys are needed to collapse vectors of T/F
    df <- flash_rebuild_event_table(event_url_rebuild = link)
  }


  # dplyr does not react well to nameless columns: some verbs will throw an error if there's a nameless column. Some tables populate with a nameless column. For vertical jumps, this is due to the 2-line nature of the header on Flash which puts the single-line column name in Row 1. In vertical jumps, the affected column is "Athlete," so we find the column containing "Athlete" and then name it as such. Some horizontal events do not name the "Wind" or "Qualifying" (Q/q) column, so we find the blank column and then name it "Placeholder."

  # remove unnamed columns
  df <- df[names(df) != ""]

  # remove empty columns
  df <- Filter(function(x)
    !all(is.na(x)), df)

  # remove duplicated columns
  # df[!duplicated(as.list(df))]

  athlete_col <- which(stringr::str_detect(as.vector(t(df)), "Athlete"))
  # blank_col <- which(colnames(df) == "")
  df <- df %>%
    rename(
      # "Placeholder" = all_of(blank_col),
           "Athlete" = all_of(athlete_col))

  # Add event and gender to the result table
  # convert page content to a vector
  page_content_vector <- page_content %>%
    rvest::html_text()

  # determine name of event
  event_name <- page_content_vector %>%
    flash_event_parse()

  # determine gender of event
  event_gender <- page_content_vector %>%
    flash_gender_parse()

  # include event name and gender
  df <- df %>%
    dplyr::mutate(Event = event_name,
                  Gender = event_gender) %>%
    # dplyr::select(-matches("Placeholder")) %>%
    dplyr::na_if("") # blank cells to NA

  # keep times as characters for consistency's sake
  if("Time" %in% names(df)){
    df <- df %>%
      dplyr::mutate(Time = as.character(Time))
  }

  # regularize Name and Athlete columns
  if ("Athlete" %in% names(df)) {
    df <- df %>%
      dplyr::rename("Name" = "Athlete")
  }

  # regularize Place column name
  if ("Pl" %in% names(df)) {
    df <- df %>%
      dplyr::rename("Place" = "Pl")
  }

  # regularize Lane column name
  if ("Ln" %in% names(df)) {
    df <- df %>%
      dplyr::rename("Lane" = "Ln")
  }

  # regularize Team column name
  if ("Affiliation" %in% names(df)) {
    df <- df %>%
      dplyr::rename("Team" = "Affiliation")
  }

  # clean results
  # df <- df %>%
  #   flash_clean_events(wide_format_clean = wide_format)

  return(df)
}
