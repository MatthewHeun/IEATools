% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggregates.R
\name{primary_aggregates}
\alias{primary_aggregates}
\title{Total primary aggregate energy from IEA tables}
\usage{
primary_aggregates(
  .ieadata,
  country = IEATools::iea_cols$country,
  method = IEATools::iea_cols$method,
  energy_type = IEATools::iea_cols$energy_type,
  last_stage = IEATools::iea_cols$last_stage,
  year = IEATools::iea_cols$year,
  flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
  e_dot = IEATools::iea_cols$e_dot,
  total_primary_energy_supply = IEATools::aggregation_flows$total_primary_energy_supply,
  ex_p = IEATools::aggregate_cols$aggregate_primary
)
}
\arguments{
\item{.ieadata}{The data frame containing IEA data.}

\item{country, method, energy_type, last_stage, year, flow_aggregation_point, e_dot}{See \code{IEATools::iea_cols}.}

\item{total_primary_energy_supply}{See \code{IEATools::aggregation_flows}.}

\item{ex_p}{See \code{IEATools::aggregate_cols}.}
}
\value{
A data frame containing the \code{country}, \code{method}, \code{energy_type}, \code{last_stage}, and \code{year}
columns of  \code{.ieadata} and a column named with the value of \code{ex_p}.
}
\description{
Calculates total aggregate primary energy from a data frame of IEA data.
This function works similar to \code{dplyr::summarise()}:
it distills \code{.ieadata} to many fewer rows
according to the grouping variables
\code{country}, \code{method}, \code{energy_type}, \code{last_stage}, and \code{year},
and, possibly, additional grouping variables in the input (\code{.ieadata}).
\code{.ieadata} is grouped by those variables internally.
Any grouping variables present in \code{.ieadata} are retained
for calculating primary aggregate energy.
Grouping is removed before output.
}
\examples{
load_tidy_iea_df() \%>\% 
  primary_aggregates()
}
