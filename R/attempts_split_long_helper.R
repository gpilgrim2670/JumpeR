#' Creates new columns for splitting attempts strings in long format
#'
#' Given a dataframe with columns "Flight_1_Attempts" it will produce three rows, for each of the attempts in flight 1
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @param i output from \code{read_results} followed by \code{add_row_numbers}
#' @param data output from \code{tf_parse}
#' @param old_cols a list of old columns to split
#' @return returns a dataframe with Flight_X_Attempts columns split into individual rows
#'
#' @seealso \code{attempts_split_long_helper} is a helper function inside  \code{attempts_split_long}

attempts_split_long_helper <- function(i, data, old_cols) {
  # old_cols <- cols_to_split
  # i <- 2
  # data <- df

  #### split up attempts strings ####
  s <- strsplit(data[[old_cols[i]]], split = "")

  height_cols <- str_remove(old_cols, "_Attempts")


  #### dataframe with new rows by row number ####
  data_split <- data.frame(
    Row_Numb = rep(data$Row_Numb, sapply(s, length)),
    Result = unlist(s),
    Bar_Height <- unique(data[[height_cols[i]]])
  )

  #### add in attempt numbers
  data_split$Attempt <- ave(data_split$Result, data_split$Row_Numb, FUN = seq_along)

  # names(data_split) <- c("Row_Numb", paste0("Result_", i), "Attempt", "Bar Height")
  names(data_split) <- c("Row_Numb", "Result", "Bar_Height", "Attempt")


  return(data_split)
}
