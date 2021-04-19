#' Cleans event data
#'
#' Cleans event results pulled from Flash Results html tables.  Can present cleaned data in wide or long format.
#'
#' @author Gregory A. Pilgrim \email{gpilgrim2670@@gmail.com} and George M. Perry
#'
#' @importFrom dplyr select
#' @importFrom dplyr group_split
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr na_if
#' @importFrom dplyr rename
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom stringr str_trim
#' @importFrom stringr str_remove_all
#' @importFrom purrr map
#'
#' @param df a data frame or list of data frames containing event data from Flash Results
#' @param wide_format_clean should df be presented in wide format (default is \code{FALSE})?
#' @return a cleaned version of df
#'
#' @seealso \code{flash_clean_events} is a helper function inside \code{\link{flash_parse_table}}
#'
#' @export

flash_clean_events <- function(df, wide_format_clean = FALSE){

  #### testing ####
  # wide_format_clean <- FALSE
  # df <- "https://www.flashresults.com/2021_Meets/Outdoor/04-16_VirginiaChallenge/005-1-02.htm" %>%
  #   flash_parse_table()

  if(is.logical(wide_format_clean) == FALSE){
    stop("wide-format clean must be either TRUE or FALSE")
  }

  if("Event" %in% names(df) == FALSE){
    stop("df must contain a column named 'Event'")
  }

  # determine event names
  event_names <- unique(df$Event)

  # function body
  if(length(event_names) < 2){ # if there's only one event in the dataframe

    df <- df %>%
      flash_clean_events_helper(wide_format_clean_helper = wide_format_clean)

  } else { # if there are two events in the data frame split by event and map flash_clean_events_helper
    df_split <- df %>%
      dplyr::group_split(Event, Gender) %>%
      purrr::map(~ .x %>% dplyr::select(where(~ !(all(is.na(.)) | all(. == ""))))) # remove empty columns, helps make binding smoother in next step

    df <- df_split %>%
      purrr::map(flash_clean_events_helper, wide_format_clean_helper = wide_format_clean) %>%
      dplyr::bind_rows() # reassemble df_split into one df


  }

  # rename Team/Name columns for relay
  relay_strings <- paste0(c("x", "Relay", "Medley"), collapse = "|")

  if(any(stringr::str_detect(df$Event, relay_strings))){
    if("Name" %in% names(df) & "Team" %!in% names(df)){
      df <- df %>%
        dplyr::rename("Team" = "Name")
    } else if("Name" %in% names(df) & "Team" %in% names(df)){
      df <- df %>%
        dplyr::select(-Name)
    }
  }

  # Pull out ages
  Age_String <- " SR$| JR$| SO$| FR$|\\[SR\\]$|\\[JR\\]$|\\[SO\\]$|\\[FR\\]$| ^M?W?[:digit:]{1,3}$"
  df <- df %>%
    dplyr::mutate(Age = dplyr::case_when(stringr::str_detect(Team, Age_String) == TRUE ~ stringr::str_extract(Team, Age_String),
                           TRUE ~ "NA")) %>%
    dplyr::mutate(Age = stringr::str_trim(Age)) %>%
    dplyr::na_if("NA") %>%
    dplyr::mutate(Team = dplyr::case_when(is.na(Age) == FALSE ~ stringr::str_remove(Team, Age_String),
                                          TRUE ~ Team)) %>%
    dplyr::mutate(Age = stringr::str_remove_all(Age, "\\[|\\]")) %>%
    dplyr::mutate(Team = stringr::str_trim(Team))

  # Remove PB type strings
  # remove PB type strings
  df <- data.frame(lapply(df, function(x) {
    remove_string <-
      paste0(
        c(
          "#\\s*(?=\\d{1,4})[0-9]*",
          "\\$",
          "( Q )|( q )",
          "((?<=\\d)Q )|((?<=\\d) q )",
          "=",
          "(?<=\\d)PR$",
          "(?<=\\d)SB$",
          "(?<=\\d)PB$",
          " PR$",
          " SB$",
          " PB$",
          "\\[\\d{1,2}\\]"
        ),
        collapse = "|"
      )
    stringr::str_replace_all(x, remove_string, "")
  }))


  # remove empty columns again because the cleaning functions sometimes insert them
  df <- Filter(function(x)
    ! all(is.na(x)), df)

  df <- df %>%
    dplyr::na_if("") %>%
    dplyr::na_if("-")

  return(df)

}

#' @rdname flash_clean_events
#' @export
clean_results <- flash_clean_events

