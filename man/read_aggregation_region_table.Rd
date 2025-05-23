% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggregates.R
\name{read_aggregation_region_table}
\alias{read_aggregation_region_table}
\title{Loads region aggregation table}
\usage{
read_aggregation_region_table(
  file_path = default_aggregation_region_table_path(2019),
  country = IEATools::iea_cols$country,
  iea_regions = "IEA_regions",
  destination_regions = "Destination_regions"
)
}
\arguments{
\item{file_path}{The path of the file (xlsx file) to be loaded. The default path leads to an aggregation table converting IEA regions
into Exiobase regions for 2019 IEA data. Using the \code{default_aggregation_region_table_path()} function, the user can
select the default IEA regions to Exiobase regions aggregation table for a different year.}

\item{country}{The name of the \verb{country`` column in the aggregation table returned by the data frame.  This column contains ISO codes for the }iea_regions\verb{column of the aggregation table. Default is}IEATools::iea_cols$country`.}

\item{iea_regions}{The name of the column containing IEA regions in the aggregation table.
Default is "IEA_regions".}

\item{destination_regions}{The name of the column containing the destination regions.
Default is "Destination_regions".}
}
\value{
A three column concordance table (as a data frame) mapping the \code{iea_regions} column to a \code{destination_regions} column,
using a \code{country} column (with ISO country IDs) as intermediate, which is added to the loaded aggregation table
within the function.
For those IEA regions that do not match to an ISO code (for instance, "World marine bunkers"),
the full IEA region name is kept in the \code{country} column.
}
\description{
This functions loads a user-defined aggregation table that re-routes each IEA region to a user-defined region.
By default, the concordance matrix used re-routes IEA regions to Exiobase regions (for 2019 version of IEA data).
See details for more information.
}
\details{
The aggregation table must have a column that identifies the IEA regions to be re-routed (default is "IEA_regions"),
and a second column that identifies the new regions to IEA regions are re-routed (default is "Destination_regions").
There is no need to include all IEA regions;
those that are not included will be removed when calling the \code{aggregate_regions()} function.
IEA regions that are rerouted to "NA" or to an empty value are also removed when calling the \code{aggregate_regions()} function.

Note that the default IEA to Exiobase mapping are only valid for the time periods relevant to Exiobase (from 1995 onward).
Using it for previous years will lead to a situation where the energy consumption of particular countries,
like Former Soviet Union or Former Yugoslavia, disappear from the data frame (because they do not correspond to an Exiobase region.
}
\examples{
# Returns the default aggregation table for the year 2019
read_aggregation_region_table()
# Returns the default aggregation table for the year 2020
read_aggregation_region_table(file_path = default_aggregation_region_table_path(2020))
}
