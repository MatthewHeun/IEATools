% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{fd_sectors}
\alias{fd_sectors}
\title{Final demand sectors}
\format{
A string list with 74, comprised of the following vectors of strings:
\describe{
\item{eiou_flows}{The string vector identifying energy industry own use flows.}
\item{industry_net_flows}{The string vector identifying non-eiou (net) Industry flows.}
\item{transport_domestic_flows}{The string vector identifying domestic transport flows.}
\item{other_flows}{The string vector identifying Other flows.}
\item{non_energy_flows}{The string vector of Non-energy flows.}
\item{memo_non_energy_flows}{The string vector \code{memo_non_energy_flows} with leading "Memo: " stripped away.}
}
}
\usage{
fd_sectors
}
\description{
A string vector containing final demand sectors used for calculating Total Final Consumption (TFC)
}
\examples{
fd_sectors
}
\keyword{datasets}
