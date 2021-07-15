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
    relay_names <- c("Relay", "relay")

    if (any(str_detect(event_name_helper, vertical_names)) == TRUE) {
      df_helper <- df_helper %>%
        flash_clean_vertical_events(wide_format_vertical = wide_format_clean_helper)
    } else if (any(str_detect(event_name_helper, horizontal_names)) == TRUE) {
      df_helper <- df_helper %>%
        flash_clean_horizontal_events(wide_format_horizontal = wide_format_clean_helper)
    } else if (any(str_detect(event_name_helper, distance_names)) == TRUE) {
      df_helper <- df_helper %>%
        flash_clean_distance_events(wide_format_distance = wide_format_clean_helper)
    } else if (any(str_detect(event_name_helper, sprint_names)) == TRUE) {
      df_helper <- df_helper %>%
        flash_clean_sprint_events(wide_format_sprint = wide_format_clean_helper)
    } else if (any(str_detect(event_name_helper, relay_names)) == TRUE) {
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
