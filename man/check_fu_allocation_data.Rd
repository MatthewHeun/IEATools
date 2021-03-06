% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/templates.R
\name{check_fu_allocation_data}
\alias{check_fu_allocation_data}
\title{Check validity of a final-to-useful allocation table}
\usage{
check_fu_allocation_data(
  .fu_allocation_table,
  country = IEATools::iea_cols$country,
  year = IEATools::iea_cols$year,
  flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
  machine = IEATools::template_cols$machine,
  ef_product = IEATools::template_cols$ef_product,
  eu_product = IEATools::template_cols$eu_product,
  destination = IEATools::template_cols$destination,
  quantity = IEATools::template_cols$quantity,
  .values = IEATools::template_cols$.values,
  non_energy_machine = "Non-energy"
)
}
\arguments{
\item{.fu_allocation_table}{The final-to-useful allocation table you want to check.}

\item{country, year, flow_aggregation_point}{See \code{IEATools::iea_cols}.}

\item{machine, ef_product, eu_product, destination, quantity, .values}{See \code{IEATools::template_cols}.}

\item{non_energy_machine}{The string that identifies a Non-energy machine. Default is "Non-energy".}
}
\value{
\code{TRUE} if the checks are successful. An error is emitted if the checks fail.
}
\description{
When analysts fill final-to-useful allocation templates created with
\code{fu_allocation_template()}, some errors are likely.
This function checks for typical errors and
emits helpful error messages that point the analyst to the location of the problem.
}
\details{
This function checks for two typical errors.
\itemize{
\item First, this function checks that \code{ef_product} and \code{eu_product} are identical
when \code{machine} is \code{non_energy_machine}.
\item Second, this function checks that \code{machine} and \code{eu_product} are present
when \code{.values} is not \code{NA}.
This check is performed on a tidy version of \code{.fu_allocation_table}.
}
}
\examples{
load_fu_allocation_data() \%>\% 
  check_fu_allocation_data()
}
