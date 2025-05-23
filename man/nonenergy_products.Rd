% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{nonenergy_products}
\alias{nonenergy_products}
\title{Non-energy products}
\format{
A string vector with 7 entries.
\describe{
\item{additives_blending_components}{The string identifier for Additives/blending components.}
\item{bitumen}{The string identifier for Bitumen.}
\item{lubricants}{The string identifier for Lubricants.}
\item{naphtha}{The string identifier for Naphtha.}
\item{paraffin_waxes}{The string identifier for Paraffin waxes.}
\item{refinery_feedstocks}{The string identifier for Refinery feedstocks.}
\item{white_spirit_and_sbp}{The string identifier for White spirit & SBP.}
}
}
\usage{
nonenergy_products
}
\description{
A string vector containing names of products classified as "Non-energy" by
the package development team. This list include hydrocarbon-derived
materials such as "Lubricants".
This list also includes products classified as "Crude, NGL, refinery
feedstocks" in the IEA World Extended Energy Balances 2021 documentation such
as "Additives/blending components".
}
\examples{
nonenergy_products
}
\keyword{datasets}
