% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/initialize.R
\name{rename_iea_df_cols}
\alias{rename_iea_df_cols}
\title{Rename columns of an IEA data frame}
\usage{
rename_iea_df_cols(
  .iea_df,
  country = "COUNTRY",
  new_country = IEATools::iea_cols$country,
  flow = "FLOW",
  new_flow = IEATools::iea_cols$flow,
  product = "PRODUCT",
  new_product = IEATools::iea_cols$product
)
}
\arguments{
\item{.iea_df}{a data frame produced by \code{\link[=iea_df]{iea_df()}}}

\item{country}{the original name for the country column. (Default is \code{COUNTRY}.)}

\item{new_country}{the new name for the country column. (Default is \code{IEATools::iea_cols$country}.)}

\item{flow}{the original name for the flow column. (Default is \code{FLOW}.)}

\item{new_flow}{the new name for the flow column. (Default is \code{IEATools::iea_cols$flow}.)}

\item{product}{the original name for the product column. (Default is \code{PRODUCT}.)}

\item{new_product}{the new name for the product column. (Default is \code{IEATools::iea_cols$product}.)}
}
\value{
\code{.iea_df} with renamed columns
}
\description{
The IEA data has columns named \code{COUNTRY}, \code{FLOW}, and \code{PRODUCT}.
This function turns off the shouting,
renaming the columns (by default) to \code{Country}, \code{Flow}, and \code{Product}.
}
\examples{
iea_df(text = ",,TIME,1960,1961\nCOUNTRY,FLOW,PRODUCT\nWorld,Production,Hard coal,42,43") \%>\% 
  rename_iea_df_cols()
}
