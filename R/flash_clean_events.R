#' Cleans event data
#'
#' Cleans event results pulled from Flash Results html tables.  Can present
#' cleaned data in wide or long format.
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
#' @importFrom stringr str_split_fixed
#' @importFrom purrr map
#'
#' @param df a data frame or list of data frames containing event data from
#'   Flash Results
#' @param wide_format_clean should df be presented in wide format (default is
#'   \code{FALSE})?
#' @return a cleaned version of df
#'
#' @seealso \code{flash_clean_events} is a helper function inside
#'   \code{\link{flash_parse_table}}
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
  if(length(event_names) < 2){ # if there's only one event in the data frame

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
  relay_strings <- paste0(c("x", "Relay", "Medley", "DMR", "Dmr", "Shuttle", "SMR", "Smr"), collapse = "|")

  if(any(stringr::str_detect(df$Event, relay_strings))){
    if("Name" %in% names(df) & "Team" %!in% names(df)){
      df <- df %>%
        dplyr::rename("Team" = "Name")
    } else if("Name" %in% names(df) & "Team" %in% names(df) & all(is.na(df$Team)) == TRUE){
      df <- df %>%
        mutate(Team = Name) %>%
        dplyr::select(-Name)
    } else if("Name" %in% names(df) & "Team" %in% names(df) & all(is.na(df$Team)) == FALSE){
      df <- df %>%
        dplyr::select(-Name)
    }
    df <- df %>%
      dplyr::mutate(Team = stringr::str_split_fixed(Team, "\\\n", 2)[,1])
  }

  # Pull out ages
  if("Age" %in% names(df) == FALSE){
  Age_String <- " SR$| JR$| SO$| FR$|\\[SR\\]$|\\[JR\\]$|\\[SO\\]$|\\[FR\\]$| ^M?W?[:digit:]{1,3}$"
  df <- df %>%
    dplyr::mutate(Age = dplyr::case_when(stringr::str_detect(Team, Age_String) == TRUE ~ stringr::str_extract(Team, Age_String),
                           TRUE ~ "NA")) %>%
    dplyr::mutate(Age = stringr::str_trim(Age)) %>%
    dplyr::na_if("NA") %>%
    dplyr::mutate(Team = dplyr::case_when(is.na(Age) == FALSE ~ stringr::str_remove(Team, Age_String),
                                          TRUE ~ Team)) %>%
    dplyr::mutate(Team = stringr::str_remove(Team, "&nbsp$")) %>%
    dplyr::mutate(Age = stringr::str_remove_all(Age, "\\[|\\]")) %>%
    dplyr::mutate(Team = stringr::str_trim(Team))
  }

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
          "(?<=\\d)[:upper:]{1,}$",
          # "(?<=\\d)PR$",
          # "(?<=\\d)PRMR$",
          # "(?<=\\d)SB$",
          # "(?<=\\d)SBMR$",
          # "(?<=\\d)PB$",
          # "(?<=\\d)PBMR$",
          # "(?<=\\d)MR$",
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

  if(unique(df$Event_Date) %in% c("Unknown", NA) == FALSE){
  df <- df %>%
    mutate(Event_Date = as.Date(Event_Date, format = "%b %d %Y"))
  }

  return(df)

}

#' @rdname flash_clean_events
#' @export
clean_results <- flash_clean_events

#' Applies appropriate event cleaning function
#'
#' Used to apply appropriate cleaning function based on event name
#'
#' @param df_helper a data frame of vertical event data from Flash Results
#' @param wide_format_clean_helper should df be presented in wide format
#'   (default is \code{FALSE})?
#' @return a cleaned version of df
#'
#' @seealso \code{flash_clean_events_helper} is a helper function inside
#'   \code{\link{flash_clean_events}}


flash_clean_events_helper <-
  function(df_helper = df,
           wide_format_clean_helper = wide_format_clean) {

    # df_helper <- df

    event_name_helper <- unique(df_helper$Event)

    vertical_names <- c("High Jump", "Pole Vault")
    horizontal_names <- c("Long Jump",
                          "Triple Jump",
                          "Shot Put",
                          "Discus",
                          "Javelin",
                          "Hammer",
                          "Weight")
    distance_names <-
      c(
        "^\\d?[5-9|0]\\d0m",
        "^\\d\\d\\d0m",
        "^\\d?[5-9|0]\\d0 M",
        "^\\d\\d\\d0 M",
        " \\d?[5-9|0]\\d0m",
        " \\d\\d\\d\\d0m",
        " \\d?[5-9|0]\\d0 M",
        " \\d\\d\\d0 M",
        "Mile",
        "Race Walk",
        "\\d{5}",
        "Distance",
        "Dmr",
        "DMR",
        "SMR",
        "Smr"
      )
    sprint_names <-
      c(
        "^[1-4]?[0-9]\\dm",
        "^[1-4]?[0-9]\\d M",
        " [1-4]?[0-9]\\dm",
        " [1-4]?[0-9]\\d M",
        "Sprint"
      )
    relay_names <- c("Relay", "relay", "shuttle", "Shuttle")

    if (any(stringr::str_detect(event_name_helper, vertical_names)) == TRUE) {
      df_helper <- df_helper %>%
        flash_clean_vertical_events(wide_format_vertical = wide_format_clean_helper)
    } else if (any(stringr::str_detect(event_name_helper, horizontal_names)) == TRUE) {
      df_helper <- df_helper %>%
        flash_clean_horizontal_events(wide_format_horizontal = wide_format_clean_helper)
    } else if (any(stringr::str_detect(event_name_helper, distance_names)) == TRUE) {
      df_helper <- df_helper %>%
        flash_clean_distance_events(wide_format_distance = wide_format_clean_helper)
    } else if (any(stringr::str_detect(event_name_helper, sprint_names)) == TRUE) {
      df_helper <- df_helper %>%
        flash_clean_sprint_events(wide_format_sprint = wide_format_clean_helper)
    } else if (any(stringr::str_detect(event_name_helper, relay_names)) == TRUE) {
      df_helper <- df_helper %>%
        flash_clean_relay_events(wide_format_relay = wide_format_clean_helper)
    } else {
      warning(
        paste0(
          "Event name ",
          event_name_helper,
          " not recognized. No cleaning performed on ",
          event_name_helper, " data."
        )
      )
    }

    return(df_helper)
  }

