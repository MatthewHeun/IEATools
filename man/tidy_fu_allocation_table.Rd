% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tables.R
\name{tidy_fu_allocation_table}
\alias{tidy_fu_allocation_table}
\title{Tidy a final-to-useful allocation table}
\usage{
tidy_fu_allocation_table(
  .fu_allocation_table,
  year = IEATools::iea_cols$year,
  e_dot = IEATools::iea_cols$e_dot,
  unit = IEATools::iea_cols$unit,
  e_dot_perc = IEATools::template_cols$e_dot_perc,
  quantity = IEATools::template_cols$quantity,
  maximum_values = IEATools::template_cols$maximum_values,
  .values = IEATools::template_cols$.values
)
}
\arguments{
\item{.fu_allocation_table}{The final-to-useful allocation table to be tidied.}

\item{year, e_dot, unit}{See \code{IEATools::iea_cols}.}

\item{e_dot_perc, quantity, maximum_values, .values}{See \code{IEATools::template_cols}.}
}
\value{
A tidy version of \code{.fu_allocation_table}.
}
\description{
Analysts fill final-to-useful (FU) allocation tables in a human-readable format
provided by \code{fu_allocation_template()} and \code{write_fu_allocation_template()}.
The templates are not tidy.
However, most code uses in \code{IEATools} requires tidy data frames.
This function converts an FU allocation table with years in columns
to a tidy data frame with years in a \code{year} column and \code{C} values in a \code{.values} column.
Identifiers for the \code{C} values are in the \code{quantity} column.
}
\details{
If \code{.fu_allocation_table} is already tidy, it is returned unmodified.
}
\examples{
load_fu_allocation_data() \%>\% 
  tidy_fu_allocation_table()
}
