#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @return no value returned, called for side effects
NULL


replace_character_na <- function(df, with) {
  dplyr::mutate(df, dplyr::across(tidyselect::where(is.character), ~ na_if(.x, with)))
}

replace_numeric_na <- function(df, with) {
  dplyr::mutate(df, dplyr::across(tidyselect::where(is.numeric), ~ na_if(.x, with)))
}
