% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{psut_cols}
\alias{psut_cols}
\title{Physical Supply-Use Table (PSUT) data frame column names}
\format{
A string list with 14 entries.
\describe{
\item{resources,R}{The name of a column in a wide-by-matrices data frame containing resource (\code{R}) matrices.}
\item{U_feed}{The name of a column in a wide-by-matrices data frame containing use (\code{U}) matrices that exclude energy industry own use.}
\item{U_eiou}{The name of a column in a wide-by-matrices data frame containing use (\code{U}) matrices that contain exclusively energy industry own use.}
\item{U}{The name of a column in a wide data-by-matrices frame containing use (\code{U}) matrices that are the sum of \code{U_feed} and \code{U_eiou} matrices.}
\item{r_eiou}{The name of a column in a wide-by-matrices data frame containing the ratio of \code{U_eiou} and \code{U} matrices.}
\item{make,V}{The name of a column in a wide-by-matrices data frame containing make (\code{V}) matrices.}
\item{final_demand,Y}{The name of a column in a wide-by-matrices data frame containing final demand (\code{Y}) matrices.}
\item{s_units}{The name of a column in a wide-by-matrices data frame containing unit summation (\code{S_units}) matrices.}
\item{matvals}{The name of a column in a tidy data frame containing matrices.}
}
}
\usage{
psut_cols
}
\description{
A string list containing named names of columns in PSUT data frames.
Items in the list provide default values for column name function arguments
throughout the \code{IEATools} package.
}
\details{
Note that some of the values are repeated,
thereby providing synonyms.
E.g., both \code{resources} and \code{R} point to the "R" column name.
}
\examples{
psut_cols
}
\keyword{datasets}
