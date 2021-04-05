#' Cleans event data
#'
#' Cleans event results pulled from Flash Results html tables.  Can present cleaned data in wide or long format.
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @importFrom dplyr select
#' @importFrom dplyr group_split
#' @importFrom dplyr bind_rows
#' @importFrom purrr map
#'
#' @param df a data frame or list of data frames containing event data from Flash Results
#' @param wide_format_clean should df be presented in wide format (default is \code{FALSE})?
#' @return a cleaned version of df
#'
#' @seealso \code{flash_clean_events} is a helper function inside \code{\link{flash_parse_table}}

flash_clean_events <- function(df, wide_format_clean = wide_format){

  # tidy_table <- "table"
  #
  # df <- url_800 %>%
  #   get_result_table()

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
      dplyr::group_split(Event) %>%
      purrr::map(~ .x %>% dplyr::select(where(~ !(all(is.na(.)) | all(. == ""))))) # remove empty columns, helps make binding smoother in next step

    df <- df_split %>%
      purrr::map(flash_clean_events_helper, wide_format_clean_helper = wide_format_clean) %>%
      dplyr::bind_rows() # reassemble df_split into one df

  }

  # remove empty columns again because the cleaning functions sometimes insert them
  df <- Filter(function(x)
    ! all(is.na(x)), df)

  return(df)

}
