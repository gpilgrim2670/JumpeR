#' Creates new rows of split attempts strings (long format change)
#'
#' Given a data frame with columns "Round_1_Attempts" it will create three new
#' rows, one for each of the attempts in round 1
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom purrr map
#'
#' @param data_to_split output from \code{read_results} followed by
#'   \code{add_row_numbers}
#' @return returns a data frame with Round_X_Attempts columns split into
#'   individual attempts as rows
#'
#' @export
#'
#' @examples \donttest{
#' df <- tf_parse( read_results(
#' "https://www.flashresults.com/2018_Meets/Outdoor/04-20_DukeInvite/014-1.pdf"
#' ), rounds = TRUE, round_attempts = TRUE, )
#'
#'  df %>% attempts_split_long()
#' }

attempts_split_long <- function(data_to_split) {
  # #### testing ####
  # data_to_split <- tf_parse(
  #   read_results(
  #     "https://www.flashresults.com/2019_Meets/Outdoor/04-12_TamuInvite/014-1.pdf"
  #   ),
  #   rounds = TRUE,
  #   round_attempts = TRUE
  # )

  # data_to_split <- df

  if (any(str_detect(names(data_to_split), "^Round_\\d{1,}_Attempts")) == TRUE) {
    #### get names of all Round_X_Attempts columns ####
    cols_to_split <-
      stringr::str_subset(names(data_to_split), "^Round_\\d{1,}_Attempts")

      #### add in row numbers for joining ####
      if ("Row_Numb" %in% names(data_to_split) == FALSE) {
        data_to_split$Row_Numb <- seq(1, nrow(data_to_split))
      }

      #### map helper over length of cols_to_split ####
      df_list <-
        purrr::map(
          seq(1, length(cols_to_split), 1),
          hytek_attempts_split_long_helper,
          data = data_to_split,
          old_cols = cols_to_split
        )

      #### bind together ####
      df_split <- dplyr::bind_rows(df_list)

      #### join up by row number ####
      df_split <- data_to_split %>%
        dplyr::left_join(df_split, by  = "Row_Numb") %>%
        dplyr::select(-Row_Numb)

      return(df_split)

  } else if (any(str_detect(names(data_to_split), "X\\d\\.\\d")) == TRUE) {
    cols_to_split <-
      stringr::str_subset(names(data_to_split), "^X\\d.*")

    varying_cols <- names(df)[grepl("[0-9]", names(df))]

    # only attempt to convert to long format if there are actual round columns present
    if(length(varying_cols) > 0) {

      data_to_split <- data_to_split %>%
        reshape(
          direction = "long",
          varying = varying_cols,
          sep = "",
          timevar = "Height",
          ids = row.names(df)
        ) %>%
        dplyr::select(dplyr::everything(), "Result" = "X", -id) %>%
        dplyr::filter(is.na(Result) == FALSE) # remove rows without a result

      rownames(data_to_split) <- NULL # reshape sets row names, remove them
    }

    #### add in row numbers for joining ####
    if ("Row_Numb" %in% names(data_to_split) == FALSE) {
      data_to_split$Row_Numb <- seq(1, nrow(data_to_split))
    }

    #### use helper ####

    data_split <- data_to_split %>%
      flash_attempts_split_long_helper() %>%
      dplyr::left_join(data_to_split %>% dplyr::select(-Result), by = "Row_Numb") %>%
      dplyr::select(-Row_Numb)

    return(df_split)


  } else {
    message("No attempts columns to split in dataframe")
    return(data_to_split)
  }

}

#' Creates new columns for splitting attempts strings in long format
#'
#' Given a data frame with columns "Round_1_Attempts" it will produce three
#' rows, for each of the attempts in flight 1
#'
#' @importFrom stringr str_remove
#' @importFrom stats ave
#'
#' @param i output from \code{read_results} followed by \code{add_row_numbers}
#' @param data output from \code{tf_parse}
#' @param old_cols a list of old columns to split
#' @return returns a data frame with Round_X_Attempts columns split into
#'   individual rows
#'
#' @seealso \code{attempts_split_long_helper} is a helper function inside
#'   \code{attempts_split_long}

hytek_attempts_split_long_helper <- function(i, data, old_cols) {
   # old_cols <- cols_to_split
   # i <- 1
   # data <- data_to_split

  #### split up attempts strings ####
  string_pieces <- strsplit(data[[old_cols[i]]], split = "")

  height_cols <- stringr::str_remove(old_cols, "_Attempts")


  #### data frame with new rows by row number ####

    Row_Numb <- rep(data$Row_Numb, sapply(string_pieces, length))
    Result <- unlist(string_pieces)
    Bar_Height <- unique(data[[height_cols[i]]])

  data_split <- data.frame(
    Row_Numb,
    Result,
    Bar_Height,
    stringsAsFactors = FALSE
  )

  #### add in attempt numbers
  data_split$Attempt <- stats::ave(data_split$Result, data_split$Row_Numb, FUN = seq_along)

  # names(data_split) <- c("Row_Numb", paste0("Result_", i), "Attempt", "Bar Height")
  names(data_split) <- c("Row_Numb", "Result", "Bar_Height", "Attempt")


  return(data_split)
}

#' Creates new columns for splitting attempts strings in long format
#'
#' Given a data frame with columns "Round_1_Attempts" it will produce three
#' rows, for each of the attempts in flight 1
#'
#' @importFrom stats ave
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#'
#' @param data output from \code{tf_parse}
#' @return returns a data frame with Round_X_Attempts columns split into
#'   individual rows
#'
#' @seealso \code{attempts_split_long_helper} is a helper function inside
#'   \code{attempts_split_long}

flash_attempts_split_long_helper <- function(data) {
  # data <- data_to_split

  #### split up attempts strings ####
  string_pieces <- strsplit(data %>%
                              dplyr::filter(Result %!in% c("DNS", "DNF", "DSQ")) %>%
                              dplyr::pull(Result), split = "")

  #### data frame with new rows by row number ####
  Row_Numb <- rep(data %>%
                    dplyr::filter(Result %!in% c("DNS", "DNF", "DSQ")) %>%
                    dplyr::pull(Row_Numb), sapply(string_pieces, length))
  Result <- unlist(string_pieces)
  # Bar_Height <- rep(cols_to_split, 15)

  data_split <- data.frame(
    Row_Numb,
    Result,
    stringsAsFactors = FALSE
  )


  #### add in attempt numbers
  data_split$Attempt <- stats::ave(data_split$Result, data_split$Row_Numb, FUN = seq_along)

  names(data_split) <- c("Row_Numb", "Result", "Attempt")

  return(data_split)
}

