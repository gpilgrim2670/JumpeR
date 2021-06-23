#' Converts Flash Results from wide to long format
#'
#' Used to convert multiple split columns to two columns, `Split_Time` and
#' `Split_Distance`.  Effectively a T&F specific version of
#' `tidyr::pivot_longer` or `base::reshape`
#'
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom stringr str_remove
#'
#' @param df a data frame or list of data frames containing event data from
#'   Flash Results
#' @param varying names of columns containing varying information (i.e.
#'   splits)
#' @return a version of df with split column values as `Split_Time` and split
#'   column names as `Split_Distance`

flash_pivot_longer <- function(df, varying){
  df <- df %>%
    reshape(
      direction = "long",
      varying = varying,
      sep = "",
      timevar = "Split_Distance",
      ids = row.names(df),
      v.names = "Split_Time"
    ) %>%
    dplyr::select(-id) %>%
    dplyr::mutate(
      Split_Distance = varying[Split_Distance],
      # reshape converts varying cols to indexes for some reason, this is a workaround
      Split_Distance = stringr::str_remove(Split_Distance, "^X"),
      Split_Distance = stringr::str_remove(Split_Distance, "[m|M]\\.?$")
    )

  rownames(df) <- NULL # reshape sets row names, remove them

  return(df)
}
