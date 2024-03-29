% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remove_unneeded_rounds.R
\name{remove_unneeded_rounds}
\alias{remove_unneeded_rounds}
\title{Removes unneeded rounds columns within \code{tf_parse}}
\usage{
remove_unneeded_rounds(x)
}
\arguments{
\item{x}{data frame with columns called both "Round_X" and "Round_X_Results"
where X is a number}
}
\value{
returns a data frame where Round_X columns that do not have a
corresponding Round_X_Results have been removed
}
\description{
Inside of \code{tf_parse} & \code{tf_parse}, removes round columns that do
not have an associated round_attempts column
}
\seealso{
\code{remove_unneeded_rounds} runs inside \code{\link{flash_parse}}
& \code{\link{tf_parse}}
}
