#' Collects flight attempts within \code{tf_parse}
#'
#' Takes the output of \code{read_results} and, inside of \code{tf_parse},
#' extracts vertical jump attempts and associated row numbers
#'
#' @importFrom stringr str_detect
#'
#' @param df dataframe with jump attempt columns containing (X, O, PASS etc) and
#'   other columns
#' @return returns a dataframe with the attempt columns removed
#'
#' @seealso \code{attempts_remove} runs inside \code{\link{flash_parse}}

attempts_remove <- function(df) {


  single_strings <- c("X", "O", "PA$$ ", "PA$$ PA$$ ")
  space1 <- " "

  attempts_strings <- "PA\\$\\$|^XXX$|^XXO$|^XO$|^O$|XO "

  df_x <- Filter(function(x)
        ! all(stringr::str_detect(x, attempts_strings)), df)

  return(df_x)
}
