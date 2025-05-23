% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/final_to_useful.R
\name{form_C_mats}
\alias{form_C_mats}
\title{Create allocation matrices (\code{C}) from an allocation table}
\usage{
form_C_mats(
  .fu_allocation_table,
  matrix_class = c("matrix", "Matrix"),
  ledger_side = IEATools::iea_cols$ledger_side,
  flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
  e_dot = IEATools::iea_cols$e_dot,
  unit = IEATools::iea_cols$unit,
  year = IEATools::iea_cols$year,
  supply = IEATools::ledger_sides$supply,
  consumption = IEATools::ledger_sides$consumption,
  quantity = IEATools::template_cols$quantity,
  machine = IEATools::template_cols$machine,
  ef_product = IEATools::template_cols$ef_product,
  eu_product = IEATools::template_cols$eu_product,
  destination = IEATools::template_cols$destination,
  e_dot_perc = IEATools::template_cols$e_dot_perc,
  maximum_values = IEATools::template_cols$maximum_values,
  matnames = IEATools::mat_meta_cols$matnames,
  matvals = IEATools::mat_meta_cols$matvals,
  rownames = IEATools::mat_meta_cols$rownames,
  colnames = IEATools::mat_meta_cols$colnames,
  rowtypes = IEATools::mat_meta_cols$rowtypes,
  coltypes = IEATools::mat_meta_cols$coltypes,
  product = IEATools::row_col_types$product,
  industry = IEATools::row_col_types$industry,
  notation = RCLabels::arrow_notation,
  tol = 1e-06,
  C_eiou = IEATools::template_cols$C_eiou,
  C_Y = IEATools::template_cols$C_Y,
  .should_be_1_vector = ".should_be_1_vector",
  .is_1 = ".is_1",
  .all_1 = ".all_1"
)
}
\arguments{
\item{.fu_allocation_table}{a final-to-useful allocation table read by \code{load_fu_allocation_data()}.
A template for this table should have been created by \code{fu_allocation_table()} and
\code{write_fu_allocation_table()}.
This object can also be a tidy data frame with year data gathered into a Year column.}

\item{matrix_class}{The type of matrix to be created, one of "matrix" or "Matrix".
Default is "matrix".}

\item{ledger_side, flow_aggregation_point, e_dot, unit, year}{See \code{IEATools::iea_cols}.}

\item{supply, consumption}{See \code{IEATools::ledger_sides}.}

\item{quantity, machine, ef_product, eu_product, destination, e_dot_perc, maximum_values, C_eiou, C_Y}{See \code{IEATools::template_cols}.}

\item{matnames, matvals, rownames, colnames, rowtypes, coltypes}{See \code{IEATools::mat_meta_cols}.}

\item{product, industry}{See \code{IEATools::row_col_types}.}

\item{notation}{The notation used for this template. See \code{RCLabels::notation_vec()}. Default is \code{RCLabels::arrow_notation}.}

\item{tol}{The allowable amount by which a row sum in a \code{C} matrix can be different from 1. Default is 1e-6.}

\item{.should_be_1_vector}{A temporary column created internally for error checking (and not returned unless there is an error).
This column should contain 1 vectors (i.e., vectors filled with 1's).}

\item{.is_1}{A temporary column created internally (and not returned unless there is an error)
that contains \code{TRUE} or \code{FALSE} depending on whether a rowsum was 1.}

\item{.all_1}{A temporary column created internally (and not returned unless there is an error)
that tells whether a 1-vector was created by rowsums.}
}
\value{
A wide-by-matrices data frame with metadata columns (and year) along with columns for
\strong{C_eiou} and \strong{C_Y} matrices.
If not all rows of a C matrix sum to 1,
a warning is emitted, and
a data frame is returned which shows the errors.
}
\description{
This function uses information in a filled allocation template (created by \code{write_fu_allocation_template()})
to create allocation matrices (\strong{C}).
}
\details{
rownames of the \strong{C} matrices are taken from the \code{EfProduct} and \code{Destination} columns of \code{.fu_allocation_table}
and have the form "\code{EfProduct}  ->  \code{Destination}".
colnames of the \strong{C} matrices are taken from the \code{Machine} and \code{EuProduct} columns of \code{.fu_allocation_table}
and have the form "machine  ->  useful energy form".

\strong{C} matrices are created for both energy industry own use
and final demand (\strong{C_eiou} and \strong{C_Y}, respectively).

Rows of the output \strong{C} matrices should sum to 1.
If there is a problem, a data frame that shows the errors is returned.
Such errors probably indicate the FU template was not filled correctly.
}
\examples{
load_fu_allocation_data() \%>\% 
  form_C_mats()
}
