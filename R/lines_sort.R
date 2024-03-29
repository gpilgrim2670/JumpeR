#' Sorts and collects lines by performance and row number
#'
#' Collects all lines, (for example containing splits or relay swimmers)
#' associated with a particular performance into a data frame with the
#' appropriate row number for that performance.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr lag
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom dplyr n
#' @importFrom dplyr arrange
#' @importFrom stats reshape
#'
#' @param x a list of character strings including performances, with row numbers
#'   added by \code{add_row_numbers}
#' @param min_row the lowest row number
#' @return a data frame with \code{Row_Numb} as the first column.  Other columns
#'   are performance elements, like splits or relay swimmers, both in order of
#'   occurrence left to right

lines_sort <- function(x, min_row = minimum_row) {
  min_row <- as.numeric(min_row)

  x <- x %>%
    dplyr::mutate(Row_Numb = as.numeric(V1)) %>%
    dplyr::arrange(Row_Numb) %>%
    dplyr::mutate(
      Row_Numb_2 = dplyr::case_when(
        Row_Numb - dplyr::lag(Row_Numb, default = min_row) == 0 ~ "Different",
        Row_Numb - dplyr::lag(Row_Numb, default = min_row) <= 1 ~ "Same",
        Row_Numb - dplyr::lag(Row_Numb, default = min_row) > 1 ~ "Different",
        TRUE ~ "Different"
      )
    ) %>%
    dplyr::mutate(
      Row_Fill = dplyr::case_when(Row_Numb_2 == "Different" ~ Row_Numb,
                                  Row_Numb_2 == "Same" ~ 0),
      Row_Fill = as.character(Row_Fill)
    ) %>%
    replace_numeric_na(0) %>%
    dplyr::mutate(Row_Fill = fill_down(Row_Fill)) %>%

    dplyr::select(-V1, -Row_Numb, -Row_Numb_2) %>%
    dplyr::mutate(row_index = 1:dplyr::n()) %>%
    stats::reshape(direction = "wide",
                   idvar = "Row_Fill",
                   timevar = "row_index") %>%
    fill_left()

  names(x)[1] <- "Row_Numb"

  return(x)
}
