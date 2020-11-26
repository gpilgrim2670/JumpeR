#' Reads track and field results into a list of strings in preparation for parsing with \code{tf_parse}
#'
#' Outputs list of strings to be processed by \code{tf_parse}
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @importFrom SwimmeR read_results
#'
#' @param file a .pdf or .html file (could be a url) where containing swimming results.  Must be formatted in a "normal" fashion - see vignette
#' @param node a CSS node where html results are stored.  Required for html results.  Default is "pre", which nearly always works.
#' @return returns a list of strings containing the information from \code{file}.  Should then be parsed with \code{tf_parse}
#'
#' @seealso \code{read_results} is meant to be followed by \code{\link{tf_parse}}
#'
#' @export

read_results <- function(file, node = "pre") {
  x <- SwimmeR::read_results(file = file, node = node)
  return(x)
}
