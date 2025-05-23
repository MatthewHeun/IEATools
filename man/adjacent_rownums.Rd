% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iea_splits.R
\name{adjacent_rownums}
\alias{adjacent_rownums}
\title{Find row numbers for adjacent entries}
\usage{
adjacent_rownums(.DF, col_name, entries)
}
\arguments{
\item{.DF}{the data frame in which to search for \code{entries} in \code{col_name}.}

\item{col_name}{the column of \code{.DF} in which to search for consecutive \code{entries}.}

\item{entries}{a vector of length 2 representing the consecutive entries in  \code{col_name} for which to search.}
}
\value{
an integer vector of length 2 giving the rows for \code{entries[[1]]} and \code{entries[[2]]}, whose difference is 1.
\code{NULL} if no consecutive \code{entries} are found in \code{col_name}.
}
\description{
Given \code{.DF}, find the row numbers that contain the first instance
in \code{col_name} of adjacent \code{entries},
this function finds the row number of the adjacent \code{entries}.
}
\details{
This function is helpful for finding boundaries between rows of a data frame
in preparation for defining grouping variables.
}
\examples{
DF <- data.frame(C1 = c("A", "B", "C"))
DF \%>\% adjacent_rownums("C1", c("A", "B"))
DF \%>\% adjacent_rownums("C1", c("B", "C"))
}
