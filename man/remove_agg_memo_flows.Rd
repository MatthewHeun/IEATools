% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/initialize.R
\name{remove_agg_memo_flows}
\alias{remove_agg_memo_flows}
\title{Remove aggregation and memo rows from data frames of IEA data}
\usage{
remove_agg_memo_flows(
  .iea_df,
  flow = IEATools::iea_cols$flow,
  product = IEATools::iea_cols$product,
  agg_flows = IEATools::aggregation_flows,
  memo_flow_prefixes = IEATools::memo_aggregation_flow_prefixes,
  memo_product_prefixes = IEATools::memo_aggregation_product_prefixes
)
}
\arguments{
\item{.iea_df}{a data frame of IEA data}

\item{flow}{the name of the flow column in \code{iea_df}. Default is "\code{Flow}".}

\item{product}{the name of the product columns in \code{iea_df}. Default is "\code{Product}".}

\item{agg_flows}{a vector of strings identifying \code{Flow}s that are aggregations.}

\item{memo_flow_prefixes}{a vector of string prefixes for flow memo rows in \code{.iea_df}}

\item{memo_product_prefixes}{a string prefix for product memo rows in \code{.iea_df}.}
}
\value{
\code{.iea_df} without its aggregation rows
}
\description{
Aggregation and memo rows are included with IEA data.
Sometimes, it is convenient to remove those rows.
This function does so, using default identifying strings for aggregations and memos.
}
\details{
Note that the IEA data sometimes includes a variable number of spaces
before the "Memo: " string.
There are several places where trailing spaces are found, such as "Nuclear industry ".
This function strips all leading and trailing spaces in the \code{Flow} and \code{Product} columns.
}
\examples{
sample_iea_data_path() \%>\% 
  iea_df() \%>\%
  rename_iea_df_cols() \%>\% 
  remove_agg_memo_flows()
}
