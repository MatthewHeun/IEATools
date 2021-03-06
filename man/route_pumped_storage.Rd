% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/specify_tp_eiou.R
\name{route_pumped_storage}
\alias{route_pumped_storage}
\title{Route pumped storage to Main activity electricity producer plant}
\usage{
route_pumped_storage(
  .tidy_iea_df,
  flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
  flow = IEATools::iea_cols$flow,
  e_dot = IEATools::iea_cols$e_dot,
  eiou = IEATools::aggregation_flows$energy_industry_own_use,
  pumped_storage = IEATools::eiou_flows$pumped_storage_plants,
  main_act_producer_elect = IEATools::main_act_plants$main_act_prod_elect_plants,
  negzeropos = ".negzeropos"
)
}
\arguments{
\item{.tidy_iea_df}{The \code{.tidy_iea_df} which flows need to be specified.}

\item{flow_aggregation_point}{The name of the flow aggregation point column in the \code{.tidy_iea_df}.
Default is \code{IEATools::iea_cols$flow_aggregation_point}.}

\item{flow}{The name of the flow column in the \code{.tidy_iea_df}.
Default is \code{IEATools::iea_cols$flow}.}

\item{e_dot}{The name of the energy column in the \code{.tidy_iea_df}.
Default is \code{IEATools::iea_cols$flow}.}

\item{eiou}{A string identifying "Energy industry own use" in the \code{flow_aggregation_point} column of the \code{.tidy_iea_df}.
Default is \code{IEATools::aggregation_flows$energy_industry_own_use}.}

\item{pumped_storage}{A string identifying "Pumped storage plants" in the \code{flow} column of the \code{.tidy_iea_df}.
Default is \code{IEATools::eiou_flows$pumped_storage_plants}.}

\item{main_act_producer_elect}{A string identifying "Main activity producer electricity plants" in the \code{flow} column of the \code{.tidy_iea_df}.
Default is \code{IEATools::main_act_plants$main_act_prod_elect_plants}.}

\item{negzeropos}{The name of a temporary column created in \code{.tidy_iea_df}.
Default is ".negzeropos".}
}
\value{
A modified \code{.tidy_iea_df} with "Pumped storage plants" industry routed
to the "Main activity producer electricity plant" industry.
}
\description{
The function routes the Energy industry own use "Pumped storage plant" flow
to the "Main activity producer electricity plant" EIOU flow.
This function is called within the \code{specify_all()} function.
}
\examples{
library(dplyr)
load_tidy_iea_df() \%>\% 
  gather_producer_autoproducer()
}
