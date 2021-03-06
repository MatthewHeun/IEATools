% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{row_col_types}
\alias{row_col_types}
\title{PSUT matrix row and column types}
\format{
A string list with 5 entries.
\describe{
\item{resource}{The type of entity that provides raw resources.}
\item{industry}{The type of entity that receives inputs and makes outputs.}
\item{sector}{The type of entity that absorbs final demand.}
\item{product}{The inputs and outputs of industries.}
\item{unit}{Units of physical measurement such as ktoe or TJ.}
}
}
\usage{
row_col_types
}
\description{
PSUT matrices have row and column types.
This list provides the typical names for the row and column types
throughout the \code{IEATools} package.
}
\examples{
row_col_types
}
\keyword{datasets}
