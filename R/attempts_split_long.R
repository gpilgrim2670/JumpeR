#' Creates new rows of split attempts strings (long format change)
#'
#' Given a dataframe with columns "Flight_1_Attempts" it will three new rows, one for each of the attempts in flight 1
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom purrr map
#'
#' @param data_to_split output from \code{read_results} followed by \code{add_row_numbers}
#' @return returns a dataframe with Flight_X_Attempts columns split into individual attempts as rows
#'
#' @export
#'
#' @examples \donttest{df <- tf_parse(
#' read_results("https://www.flashresults.com/2018_Meets/Outdoor/05-05_A10/015-1.pdf"),
#' flights = TRUE,
#' flight_attempts = TRUE,
#' split_attempts = TRUE)
#'
#' df %>%
#' attempts_split_long()
#' }

attempts_split_long <- function(data_to_split){

  #### testing ####
  # data_to_split <- tf_parse(
  #   read_results("https://www.flashresults.com/2019_Meets/Outdoor/04-12_TamuInvite/014-1.pdf"),
  #   flights = TRUE,
  #   flight_attempts = TRUE
  # )

  #### get names of all Flight_X_Attempts columns ####
  cols_to_split <- stringr::str_subset(names(data_to_split), "^Flight_\\d{1,}_Attempts")

  #### if there are no columns to replace just return original dataframe
  if(length(cols_to_split) >= 1){

    #### add in row numbers for joining ####
    if("Row_Numb" %in% names(data_to_split) == FALSE){
    data_to_split$Row_Numb <- seq(1, nrow(data_to_split))
    }

    #### map helper over length of cols_to_split ####
    df_list <- purrr::map(seq(1, length(cols_to_split), 1), attempts_split_long_helper, data = data_to_split, old_cols = cols_to_split)

    #### bind together ####
    df_split <- dplyr::bind_rows(df_list)

    #### join up by row number ####
    df_split <- data_to_split %>%
      dplyr::left_join(df_split, by  = "Row_Numb") %>%
      dplyr::select(-Row_Numb)

    return(df_split)

  } else {
    message("No attempts columns to split in dataframe")
    return(data_to_split)
  }

}

