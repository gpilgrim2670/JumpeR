#' Reads track and field results into a list of strings in preparation for parsing with \code{tf_parse}
#'
#' Outputs list of strings to be processed by \code{tf_parse}
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @importFrom stringr str_detect
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
#' @importFrom pdftools pdf_text
#'
#' @param file a .pdf or .html file (could be a url) where containing swimming track and field results.  pdfs with multiple columns will not work.
#' @param node a CSS node where html results are stored.  Required for html results.  Default is "pre", which nearly always works.
#' @return returns a list of strings containing the information from \code{file}.  Should then be parsed with \code{tf_parse}
#'
#' @examples \donttest{read_results("https://www.flashresults.com/2018_Meets/Outdoor/05-05_A10/015-1.pdf")}
#'
#' @seealso \code{read_results} is meant to be followed by \code{\link{tf_parse}}
#'
#' @export

read_results <- function(file, node = "pre") {
  '%!in%' <- function(x, y)
    ! ('%in%'(x, y))
  if (stringr::str_detect(file, "\\.pdf$|\\.pdf\\.aspx$|\\.aspx$") == TRUE) {
    ### PDF ###
    results <- pdftools::pdf_text(file)
    as_lines <- stringr::str_extract_all(results, "\n.*")
    as_lines_list_2 <- unlist(as_lines, recursive = FALSE)

    return(as_lines_list_2)

  } else {
    ### HTML ###
    if (is.character(node) == FALSE) {
      stop(" Please supply a value for node")
    } else {
      webpage <- xml2::read_html(file)
      html <- rvest::html_nodes(webpage, node)
      results <- rvest::html_text(html)
      as_lines <- stringr::str_extract_all(results, "\n.*")
      as_lines_list_2 <- unlist(as_lines, recursive = FALSE)

      return(as_lines_list_2)

    }
  }
}
