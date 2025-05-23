% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{Fixed_OAMR_gw}
\alias{Fixed_OAMR_gw}
\title{Fixed Other non-OECD Americas Gas works 1971--1976}
\format{
A data frame with 11 columns.
}
\usage{
Fixed_OAMR_gw
}
\description{
Other Non-OECD Americas has several years (1971--1976)
in which Gas works gas is produced
but no feedstock consumed to
create the Gas works gas.
This object contains (presumably) correct data.
In particular, Gas works
now consume Natural gas in all years, and
Natural gas production is boosted accordingly.
The efficiency of World Gas works plants in 1971--1976
was used to create the filled data.
The function \code{fix_OAMR_gw()} makes use of these data.
}
\examples{
Fixed_OAMR_gw
}
\keyword{datasets}
