#' Collects results from a link to a Flash Results page
#'
#' Used in scraping flashresults.com.  Collects results given in html tables on a specified page into a data frame.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr na_if
#' @importFrom dplyr all_of
#' @importFrom dplyr rename
#' @importFrom dplyr contains
#' @importFrom dplyr across
#' @importFrom dplyr everything
#' @importFrom stringr str_remove
#' @importFrom stringr str_match
#' @importFrom stringr str_detect
#' @importFrom stringr str_trim
#' @importFrom stringr str_replace_all
#' @importFrom rvest html_nodes
#' @importFrom rvest html_attr
#' @importFrom rvest html_table
#' @importFrom stats na.omit
#' @importFrom purrr when
#'
#' @param link a link to an event landing page on flashresults.com
#' @param wide_format should results be presented in wide format (defaults to \code{FALSE})
#' @param clean should results be cleaned by \code{flash_clean_events}?  Default is \code{FALSE}.
#' @return returns a data frame of results scraped from \code{link}
#'
#' @examples \donttest{flash_parse_table("https://www.flashresults.com/2019_Meets/Outdoor/06-13_NBNO/067-4_compiled.htm")}
#'
#' @export


flash_parse_table <- function(link, wide_format = FALSE, clean = FALSE) {

  # link <- "https://flashresults.com/2015_Meets/Outdoor/06-25_USATF/009-2-01.htm"
  # link <- "https://flashresults.com/2015_Meets/Outdoor/05-28_NCAAEast/005-1-03.htm"
  # link <- "https://flashresults.com/2017_Meets/Outdoor/06-22_USATF/004-2-02.htm"
  # link <- "https://flashresults.com/2019_Meets/Outdoor/07-25_USATF_CIS/004-1-03.htm"

  # link <- "https://flashresults.com/2015_Meets/Outdoor/05-28_NCAAEast/017-1_compiledSeries.htm"
  # link <- "https://flashresults.com/2019_Meets/Outdoor/07-25_USATF_CIS/004-1-03.htm"
  # link <- "https://flashresults.com/2016_Meets/Indoor/02-05_CharlieThomasInvite/001-1-03.htm"

  # link <- links[2]

  # link <- "https://flashresults.com/2015_Meets/Outdoor/05-28_NCAAEast/017-1_compiledSeries.htm"
  # link <- "https://flashresults.com/2016_Meets/Indoor/02-05_CharlieThomasInvite/001-1-03.htm"
  # link <- "https://flashresults.com/2015_Meets/Outdoor/05-28_NCAAEast/017-1_compiledSeries.htm"
  # link <- "https://www.flashresults.com/2018_Meets/Outdoor/04-06_UVAQuad/014-1-01.htm"
  # link <- "https://www.flashresults.com/2021_Meets/Indoor/03-11_NCAA/033-6_compiled.htm"
  # link <- "https://www.flashresults.com/2021_Meets/Indoor/03-11_NCAA/033-3_compiled.htm"
  # link <- "https://www.flashresults.com/2021_Meets/Indoor/03-11_NCAA/033-3_compiled.htm"
  # link <- "https://www.flashresults.com/2021_Meets/Indoor/03-11_NCAA/033-5_compiled.htm"
  # link <- "https://www.flashresults.com/2015_Meets/Outdoor/05-01_Dogwood/012-1_compiled.htm"
  # link <- "https://www.flashresults.com/2021_Meets/Indoor/03-11_NCAA/033-3_compiledSeries.htm"
  # link <- "https://www.flashresults.com/2021_Meets/Outdoor/04-16_VirginiaChallenge/014-1-01.htm"
  # link <- "https://flashresults.com/2019_Meets/Outdoor/07-25_USATF_CIS/004-1-03.htm"
  # link <- "https://flashresults.com/2017_Meets/Indoor/01-13_AggieTeam/003-1-05.htm"
  # link <- "https://www.flashresults.com/2017_Meets/Outdoor/04-29_VirginiaGrandPrix/025-1-01.htm"
  # link <- "https://flashresults.com/2016_Meets/Outdoor/05-10_BigSouth/012-1_compiled.htm"
  # link <- "https://flashresults.com/2016_Meets/Outdoor/07-29_SummerSeries/009-1_compiled.htm"
  # link <- "https://www.flashresults.com/2021_Meets/Outdoor/04-16_VirginiaChallenge/035-1_compiledSeries.htm"
  # link <- "https://flashresults.com/2018_Meets/Outdoor/06-15_NBHSON/045-1_compiled.htm"
  # link <- "https://www.flashresults.com/2019_Meets/Outdoor/06-05_NCAAOTF-Austin/015-1_compiled.htm"
  # link <- "https://www.flashresults.com/2018_Meets/Outdoor/06-09_NCAAEugene/025-1-01.htm"
  # link <- "https://www.flashresults.com/2019_Meets/Outdoor/06-05_NCAAOTF-Austin/015-1_compiled.htm"
  # link <- "https://flashresults.com/2021_Meets/Outdoor/03-19_49er/041-2-01.htm"
  # link <- "https://flashresults.com/2020_Meets/Indoor/01-18_ArkHSInvite/001-1-11.htm"
  # link <- "https://flashresults.com/2016_Meets/Indoor/01-16_TAMUTeam/032-1-16384.htm"
  # link <- "https://flashresults.com/2016_Meets/Outdoor/06-17_NBHSN/140-1-07.htm"


  page_content <- xml2::read_html(link, options = c("DTDLOAD", "NOBLANKS"))

  #This method allows us to keep the \n in the data, which is essential to cleaning.
  xml2::xml_find_all(page_content, ".//br") %>%
    xml2::xml_add_sibling("p", "\n")

  xml2::xml_find_all(page_content, ".//br") %>%
    xml2::xml_remove()

  #fill = TRUE seems to get all tables in, but reads '-' as u0097 on vertical jumps
  list_of_tables <- rvest::html_table(page_content, fill = TRUE, header = TRUE, convert = FALSE)

  # table components
  get_atts <- lapply(list_of_tables, attributes)

  # determine length of table
  get_lengths <- function(df) {
    return(length(df$names))
  }

  # collect wind df if it exists
  # wind_df_index <- suppressWarnings(which(stringr::str_detect(get_atts, "Wind"), TRUE))
  wind_df <- suppressWarnings(list_of_tables[stringr::str_detect(get_atts, "Wind")]) # warning about atomic vector - should fix

  if(length(wind_df) > 0){
  wind_value <- stringr::str_match(wind_df[[1]]$Wind, "(\\+|-)?\\d\\.\\d")[1]
  } else {
    wind_value <- NA # need to define as NA since we will need wind_value later
  }

  # remove record table
  list_of_tables <- suppressWarnings(list_of_tables[stringr::str_detect(get_atts, "Record", negate = TRUE)])
  list_of_tables <- suppressWarnings(list_of_tables[stringr::str_detect(get_atts, "Status", negate = TRUE)])

  # remove NULL elements
  list_of_tables <- list_of_tables[lengths(list_of_tables) != 0]

  # new table components
  get_atts_2 <- lapply(list_of_tables, attributes)

  # desired table is the longest one
  result_table <-
    list_of_tables[which.max(lapply(get_atts_2, get_lengths))]

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
          (!any(stringr::str_detect(stats::na.omit(as.vector(t(df))), "Athlete|Name|Team"))) &
          !any(stringr::str_detect(names(df), "Athlete|Name|Team")))
  ) { # anys are needed to collapse vectors of T/F
    df <- flash_rebuild_event_table(event_url_rebuild = link)
  }


  # dplyr does not react well to nameless columns: some verbs will throw an error if there's a nameless column.
  # Some tables populate with a nameless column.
  # For vertical jumps, this is due to the 2-line nature of the header on Flash which puts the single-line column name in Row 1.
  # In vertical jumps, the affected column is "Athlete," so we find the column containing "Athlete" and then name it as such.
  # Some horizontal events do not name the "Wind" or "Qualifying" (Q/q) column, so we find the blank column and then name it "Placeholder."
  # Some sprint events have an unnamed column for reaction time.  Need to match that.

  if(all(str_detect(names(df)[names(df) != ""], "\\d"))){
    colnames(df) <- paste(sep = '_', colnames(df), as.character(unlist(df[1,])))
    colnames(df) <- stringr::str_remove(colnames(df), "^_")
    colnames(df) <- stringr::str_remove(colnames(df), "_\\d.")
    colnames(df) <- stringr::str_replace(colnames(df), "NA", "")
    df <- df[-1, ]
  }

  # remove unicode characters of 1/4, 1/2, 3/4, all other unicode characters
  colnames(df) <- data.frame(lapply(colnames(df), function(x) { # remove all non ASCII characters from column names
    iconv(x, "latin1", "ASCII", sub = "")
  }))

  if (any(names(df) == "")) {
    names(df)[names(df) == ""] <-
      paste0("Placeholder_", seq(1, length(names(df)[names(df) == ""]), 1))
  }

  # athlete column
  athlete_col <-
    which(stringr::str_detect(as.vector(t(df)), "^Athlete$"))
  if (all(length(athlete_col) > 0, athlete_col > ncol(df))) {
    n_rows_to_remove <- ceiling(-athlete_col / ncol(df))

    athlete_col <- athlete_col %>%
      flash_correct_column_overshoot(df = df)

  } else {
    n_rows_to_remove <- 0
  }

  # reaction time columns
  if (any(stringr::str_detect(as.vector(t(df)), "0\\.\\d{3}"), na.rm = TRUE) == TRUE) {
    reaction_time_col <-
      min(which(stringr::str_detect(as.vector(t(
        df
      )), "0\\.\\d{3}"))) %>%
      flash_correct_column_overshoot(df = df)

  } else {
    reaction_time_col <- numeric(0)
  }

  # place column
  place_col <- which(stringr::str_detect(as.vector(t(df)), "(^Pl$)|(^Place$)")) %>%
    flash_correct_column_overshoot(df = df)

  # position column
  position_col <- which(stringr::str_detect(as.vector(t(df)), "(^Pos$)|(^Position$)")) %>%
    flash_correct_column_overshoot(df = df)

  # age column
  age_col <- which(stringr::str_detect(as.vector(t(df)), "(^SR$)|(^JR$)|(^SO$)|(^FR$)"))[1] %>%
    flash_correct_column_overshoot(df = df)

  # wind column
  wind_col <- which(stringr::str_detect(as.vector(t(df)), "^w?\\:?(\\+|-)?\\d\\.\\d$"))[1] %>%
    flash_correct_column_overshoot(df = df)

  # blank columns
  blank_col <- which(colnames(df) == "")
  # if (any(length(reaction_time_col) > 0 | length(athlete_col) > 0 | length(place_col) > 0 | length(position_col) > 0 | length(age_col) > 0 | length(wind_col) > 0)) {
  if (sum(length(reaction_time_col), length(athlete_col), length(place_col), length(position_col), length(age_col), length(wind_col), na.rm = TRUE) > 0) {
    blank_col <-
      setdiff(blank_col, ifelse(length(reaction_time_col) > 0, min(reaction_time_col), 0)) # don't want to capture reaction time column (if it exists)
    blank_col <- setdiff(blank_col, ifelse(length(athlete_col) > 0, athlete_col, 0)) # don't want to capture athlete column
    blank_col <- setdiff(blank_col, ifelse(length(place_col) > 0, place_col, 0)) # don't want to capture place column
    blank_col <- setdiff(blank_col, ifelse(length(position_col) > 0, position_col, 0)) # don't want to capture position column
    blank_col <- setdiff(blank_col, ifelse(length(age_col) > 0, age_col, 0)) # don't want to capture age column
    blank_col <- setdiff(blank_col, ifelse(length(wind_col) > 0, wind_col, 0)) # don't want to capture wind column
  }

  # if (is.na(age_col) == FALSE) {
  #   df <- df %>%
  #     dplyr::rename("Age" = dplyr::all_of(age_col),
  #                   "Placeholder" = dplyr::all_of(c(blank_col, athlete_col, reaction_time_col, place_col, position_col, wind_col)))
  # }
  #
  # if (is.na(wind_col) == FALSE) {
  #   df <- df %>%
  #     dplyr::rename("Wind" = dplyr::all_of(wind_col),
  #                   "Placeholder" = dplyr::all_of(c(blank_col, athlete_col, reaction_time_col, place_col, position_col)))
  # }

  # sometimes vertical events have the same pole height naming multiple columns
  # this will deal with that
  dup_df_names <- names(df)[duplicated(names(df))]

  if(length(dup_df_names) > 0){

  names(df)[names(df) == dup_df_names] <- names(df)[names(df) == dup_df_names] %>%
    paste0(letters[1:length(.)], .)
  }

  df <- df %>%
    purrr::when(all(length(blank_col) > 0, sum(is.na(blank_col)) < length(blank_col)) ~ rename(., "Placeholder" = all_of(blank_col)),
                ~ .) %>%
    purrr::when(all(length(age_col) > 0, sum(is.na(age_col)) < length(age_col)) ~ rename(., "Age" = all_of(age_col)), ~ .) %>%
    purrr::when(all(length(wind_col) > 0, sum(is.na(wind_col)) < length(wind_col)) ~ rename(., "Wind" = all_of(wind_col)), ~ .) %>%
    purrr::when(all(length(athlete_col) > 0, sum(is.na(athlete_col)) < length(athlete_col)) ~ rename(., "Athlete" = all_of(athlete_col)), ~ .) %>%
    purrr::when(all(
      length(reaction_time_col) > 0,
      sum(is.na(reaction_time_col)) < length(reaction_time_col)
    ) ~ rename(., "Reaction_Time" = all_of(reaction_time_col)),
    ~ .) %>%
    purrr::when(all(length(place_col) > 0, sum(is.na(place_col)) < length(place_col)) ~ rename(., "Place" = all_of(place_col)), ~ .) %>%
    purrr::when(all(length(position_col) > 0, sum(is.na(position_col)) < length(position_col)) ~ rename(., "Pos" = all_of(position_col)), ~ .) %>%
    dplyr::select(-dplyr::contains("Placeholder"))

  # remove unnamed columns
  df <- df[names(df) != ""]

  if(n_rows_to_remove < 0){
    df <- df[n_rows_to_remove,]
  }

  colnames(df) <- trimws(colnames(df)) # trim whitespaces in column names from removal of unicode characters

  df <- data.frame(lapply(df, function(x) { # unicode em dashes
    stringr::str_replace_all(x, "\u0097", "-")
  }))

  df <- data.frame(lapply(df, function(x) { # unicode em dashes
    stringr::str_replace_all(x, "\u2013", "-")
  }))

  colnames(df) <- data.frame(lapply(colnames(df), function(x) { # remove remainder of standard measurement from height columns (e.g. 3.31m-10 to 3.31m)
    stringr::str_replace_all(x, "m-\\d{1,3}", "m")
  }))

  df <- data.frame(lapply(df, function(x) {
    stringr::str_replace_all(x, "\u00BC", "\\.25")
  }))

  df <- data.frame(lapply(df, function(x) {
    stringr::str_replace_all(x, "\u00BD", "\\.5")
  }))

  df <- data.frame(lapply(df, function(x) {
    stringr::str_replace_all(x, "\u00BE", "\\.75")
  }))

  df <- data.frame(lapply(df, function(x) {
    stringr::str_replace_all(x, "\\-\\.", "\\-0\\.")
  }))

  df <- data.frame(lapply(df, function(x) { # remove all non ASCII characters
    iconv(x, "latin1", "ASCII", sub = "")
  }))

  # remove empty columns
  df <- Filter(function(x)
    !all(is.na(x)), df)

  # remove duplicated columns
  # df[!duplicated(as.list(df))]

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

  # determine date of event
  event_date <- page_content_vector %>%
    flash_date_parse()

  event_year <- stringr::str_extract(link, "20\\d\\d")

  # converted to date fromat in flash_clean_events
  if (event_date %in% c("Unknown", NA) == FALSE) {
    event_date <- paste(event_date, event_year, sep = " ")
  }

  # include event name and gender
  df <- df %>%
    dplyr::mutate(Event = event_name,
                  Gender = event_gender,
                  Event_Date = event_date) %>%
    # dplyr::select(-matches("Placeholder")) %>%
    dplyr::mutate(dplyr::across(where(is.character), stringr::str_trim)) %>%  # remove whitespaces
    dplyr::na_if("") %>%  # blank cells to NA
    dplyr::na_if("-")

  # include wind (if present in separate table)
  if(all(is.na(wind_value) == FALSE & "Wind" %in% names(df) == FALSE)){
    df <- df %>%
      dplyr::mutate(Wind = as.character(wind_value))
  }

  # keep all columns as characters for consistency's sake
  df <- df %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

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

  # regularize Finals_Result column name
  if (any(stringr::str_detect(names(df), "Best.*")) == TRUE) {
    df <- df %>%
      dplyr::rename("Finals_Result" = dplyr::contains("Best"))
  }

  # clean results
  if (clean == TRUE) {
    df <- df %>%
      flash_clean_events(wide_format_clean = wide_format)
  }

  return(df)
}

#' @rdname flash_parse_table
#' @export
get_results_table <- flash_parse_table
