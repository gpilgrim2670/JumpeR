% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flash_parse_table.R
\name{flash_parse_table}
\alias{flash_parse_table}
\title{Collects results from a link to a Flash Results page}
\usage{
flash_parse_table(link)
}
\arguments{
\item{link}{a link to an event landing page on flashresults.com}
}
\value{
returns a data frame of results scraped from \code{link}
}
\description{
Used in scraping flashresults.com.  Collects results given in html tables on a speciified page into a data frame.
}
\examples{
\donttest{flash_parse_table("https://www.flashresults.com/2019_Meets/Outdoor/06-13_NBNO/067-4_compiled.htm")}

}
\author{
Gregory A. Pilgrim
}