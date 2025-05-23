% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/final_to_useful.R
\name{extend_to_useful_helper}
\alias{extend_to_useful_helper}
\title{A helper function for extending to the useful energy/exergy stage}
\usage{
extend_to_useful_helper(
  .sutdata = NULL,
  dest_mat,
  C_mat,
  eta_fu_vec,
  product_type = IEATools::row_col_types$product,
  industry_type = IEATools::row_col_types$industry,
  arr_note = RCLabels::arrow_notation,
  from_note = RCLabels::from_notation,
  add_to_U = "add_to_U",
  add_to_V = "add_to_V",
  add_to_dest = "add_to_dest",
  details_fu = "details_fu"
)
}
\arguments{
\item{.sutdata}{a data frame containing \code{dest_mat}, \code{C_mat}, and \code{eta_fu_vec} columns for calculations. Default is \code{NULL}.}

\item{dest_mat}{a \code{Y} or \code{U_eiou} matrix (Product x Industry) that is a destination for final energy in an energy conversion chain
or the string name of such matrices in \code{.sutadata}.}

\item{C_mat}{an allocation matrix (either \code{Y} or \code{U_eiou}, both Product x Industry),
indicating the distribution of
final energy carriers (Products) to
final-to-useful energy conversion machines (Industries).
\code{C_mat} should have been created by \code{form_C_mats()}.
This argument could also be the string name of such matrices in \code{.sutdata}.}

\item{eta_fu_vec}{an efficiency column vector indicating the efficiency (column)
of final-to-useful energy conversion machines (rows).
\code{eta_fu_vec} should have been created by \code{form_eta_fu_phi_u_vecs()}.
This argument could also be the string name of such matrices in \code{.sutdata}.}

\item{product_type}{a string identifying product row or column types. Default is "\code{IEATools::row_col_types$product}".}

\item{industry_type}{a string identifying industry row or column types. Default is "\code{IEATools::row_col_types$industry}".}

\item{arr_note}{a row and column name notation vector that indicates a \code{source -> destination} relationship.
\code{arr_note} is used for the \code{eta_fu} matrix, among others.
See \code{matsbyname::notation_vec()}.
Default is \code{RCLabels::arrow_notation}.}

\item{from_note}{a row and column name notation vector that indicates a \verb{destination [from source]} relationship.
\code{from_note} is used for the columns of some intermediate matrices.
See \code{matsbyname::notation_vec()}.
Default is \code{RCLabels::from_notation}.}

\item{add_to_U}{a string name for the matrix to be added to a use matrix. Default is "add_to_U".}

\item{add_to_V}{a string name for the matrix to be added to a make matrix. Default is "add_to_V".}

\item{add_to_dest}{a string name for the matrix to replace some entries previous destination matrix. Default is "repl_dest".}

\item{details_fu}{A string name for the matrix containing detailed information about the final-to-useful extension,
namely, final energy product, destination sector, final-to-useful machine, and useful energy product.}
}
\value{
a named list containing four items:
\code{add_to_U_f} (a matrix to be added to a use (\code{U}) matrix),
\code{add_to_V_f} (a matrix to be added to a make (\code{V}) matrix),
\code{add_to_dest_mat} (a matrix to replace the destination matrix, typically \code{Y_f} or \code{U_eiou}), and
\code{details_fu} (a matrix that retails all details for
final energy product,
destination sector,
final-to-useful machine, and
useful energy product).
}
\description{
The helper function is needed, because moving from final to useful energy
occurs for both the final demand matrix (\code{Y}) and the energy industry own use matrix (\code{U_eiou}).
The calculations are identical, so we factor the calculations into this function.
}
