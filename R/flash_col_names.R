#' Regularizes column names from Flash Results
#'
#' Split columns have many different naming conventions within Flash Results.
#' This function attempts to enforce one convention, "Split_XXX" where XXX are
#' digits representing distance in meters
#'
#'
#' @importFrom dplyr rename_with
#' @importFrom stringr str_detect
#'
#' @param df a data frame or list of data frames containing event data from
#'   Flash Results
#'
#' @return a version of df with split column names renamed


flash_col_names <- function(df) {
  col_names <- names(df)

  old_names <-
    col_names[stringr::str_detect(col_names, "(^X\\d+m)|(^Lap.*\\d+m)|(^L.*\\d+m)")]

  df <- df %>%
    dplyr::rename_with(.fn = flash_col_names_helper, .cols = old_names)

  return(df)

}

#' Helper Function for regularizing column names from Flash Results
#'
#' @importFrom stringr str_extract
#' @importFrom stringr str_remove
#'
#' @param old_names a list of column names to be reformatted
#'
#' @return a list of strings containing corrected split column names


flash_col_names_helper <- function(old_names){

  distances <- stringr::str_extract(old_names, "\\d+m")
  distances <- stringr::str_remove(distances, "m")
  new_names <- paste0("Split_", distances)

  dup_new_names <- new_names[duplicated(new_names)]

  if(length(dup_new_names) > 0){

    new_names[new_names == dup_new_names] <- new_names[new_names == dup_new_names] %>%
      paste0(., letters[1:length(.)])
    message("Duplicate column names fixed in results.  Please check column names.")
  }

  return(new_names)

}
