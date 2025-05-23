% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/final_to_useful.R
\name{form_eta_fu_phi_u_vecs}
\alias{form_eta_fu_phi_u_vecs}
\title{Create \code{eta_fu} and \code{phi_u} vectors}
\usage{
form_eta_fu_phi_u_vecs(
  .eta_fu_table,
  matrix_class = c("matrix", "Matrix"),
  unit = IEATools::iea_cols$unit,
  year = IEATools::iea_cols$year,
  quantity = IEATools::template_cols$quantity,
  machine = IEATools::template_cols$machine,
  eu_product = IEATools::template_cols$eu_product,
  e_dot_machine = IEATools::template_cols$e_dot_machine,
  e_dot_machine_perc = IEATools::template_cols$e_dot_machine_perc,
  maximum_values = IEATools::template_cols$maximum_values,
  eta_fu = IEATools::template_cols$eta_fu,
  phi_u = IEATools::template_cols$phi_u,
  phi = IEATools::template_cols$phi,
  matnames = IEATools::mat_meta_cols$matnames,
  matvals = IEATools::mat_meta_cols$matvals,
  rownames = IEATools::mat_meta_cols$rownames,
  colnames = IEATools::mat_meta_cols$colnames,
  rowtypes = IEATools::mat_meta_cols$rowtypes,
  coltypes = IEATools::mat_meta_cols$coltypes,
  product = IEATools::row_col_types$product,
  industry = IEATools::row_col_types$industry,
  arrow_note = RCLabels::arrow_notation,
  from_note = RCLabels::from_notation,
  .id = ".id"
)
}
\arguments{
\item{.eta_fu_table}{a final-to-useful efficiency table read by \code{load_eta_fu_allocation_data()}.
A template for this table should have been created by \code{eta_fu_table()} and
\code{write_eta_fu_table()}.}

\item{matrix_class}{The type of matrix to be created, one of "matrix" or "Matrix".
Default is "matrix".}

\item{unit, year}{See \code{IEATools::iea_cols}.}

\item{quantity, machine, eu_product, e_dot_machine, e_dot_machine_perc, maximum_values, eta_fu, phi_u, phi}{See \code{IEATools::template_cols}.}

\item{matnames, matvals, rownames, colnames, rowtypes, coltypes}{See \code{IEATools::mat_meta_cols}.}

\item{product, industry}{See \code{IEATools::row_col_types}.}

\item{arrow_note, from_note}{Notation vectors used for creating the eta_fu and phi vectors.
See \code{matsbyname::notation_vec()}.
Defaults are \code{RCLabels::arrow_notation} and ``RCLabels::from_notation`, respectively.}

\item{.id}{The name of an identification column used internally. Default is ".id".}
}
\value{
a wide-by-matrices data frame with metadata columns (and year)
along with columns for \code{eta_fu} and \code{phi_u} vectors.
}
\description{
This function creates vectors from a filled final-to-useful efficiency table
created by \code{write_fu_allocation_template()}.
The two vectors are:
\itemize{
\item \code{eta_fu}: a vector of final-to-useful energy efficiencies, and
\item \code{phi_u}: a vector of useful exergy-to-useful energy ratios.
}
}
\details{
The vectors \code{eta_fu} and \code{phi_u} have special rownames that indicate
sources and types of useful energy flows.
Row names in the \code{eta_fu} vector have the pattern
"industry -> product" to indicate
the energy efficiency of "industry" for making "product"
or the exergy-to-energy ratio of the useful energy form created by a final-to-useful machine.
Row names in the \code{phi_u} vector are named by energy product only.

Each energy product should have the same phi, regardless of machine that produced it.
So this function checks whether each combination of metadata
produces the same phi values.
If any different phi values are found,
an error is produced.

Columns in the outgoing data frame are named by the variable in the vector:
\code{eta_fu} for final-to-useful efficiencies and
\code{phi_u} for useful exergy-to-useful energy ratios.
}
\examples{
load_eta_fu_data() \%>\% 
  form_eta_fu_phi_u_vecs()
}
