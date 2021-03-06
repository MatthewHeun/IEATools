% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/final_to_useful.R
\name{extend_to_useful}
\alias{extend_to_useful}
\title{Move an ECC from final to useful as its last stage}
\usage{
extend_to_useful(
  .sutdata,
  last_stage = IEATools::iea_cols$last_stage,
  final = IEATools::last_stages$final,
  useful = IEATools::last_stages$useful,
  industry_type = IEATools::row_col_types$industry,
  product_type = IEATools::row_col_types$product,
  R = IEATools::psut_cols$R,
  U_eiou = IEATools::psut_cols$U_eiou,
  U_feed = IEATools::psut_cols$U_feed,
  U = IEATools::psut_cols$U,
  r_eiou = IEATools::psut_cols$r_eiou,
  V = IEATools::psut_cols$V,
  Y = IEATools::psut_cols$Y,
  C_eiou = IEATools::template_cols$C_eiou,
  C_Y = IEATools::template_cols$C_Y,
  eta_fu = IEATools::template_cols$eta_fu,
  phi_u = IEATools::template_cols$phi_u,
  interface_ind = IEATools::interface_industries,
  losses = IEATools::tfc_compare_flows$losses,
  stat_diffs = IEATools::tfc_compare_flows$statistical_differences,
  notation = IEATools::arrow_notation,
  tol = 0.001,
  .eta_fu_hat = ".eta_fu_hat",
  .add_to_U_f = ".add_to_U_f",
  .add_to_U_eiou = ".add_to_U_eiou",
  .add_to_V_f = ".add_to_V_f",
  .add_to_dest = ".repl_dest",
  .err = ".err",
  .e_bal_ok = ".e_bal_ok",
  .useful = "_useful",
  .keep_in_Y = "_keep_in_Y"
)
}
\arguments{
\item{.sutdata}{A wide-by-matrices data frame of PSUT matrices that represent an energy conversion chain.
Each row of \code{.sutdata} should contain the matrices that represent the energy conversion chain.
Matrices should be in columns identified by their names.
The last stage of these ECCs should be final (not useful).
\code{.sutdata} is likely the result of calling (in sequence)
\code{load_tidy_iea_df()}, \code{specify_all()}, and \code{prep_psut()}.
\code{.sutdata} should also include columns of matrices \code{C_Y}, \code{C_eiou}, and \code{eta_fu},
probably created by functions \code{form_C_mats()} and \code{form_eta_fu_phi_u_vecs()}.}

\item{last_stage}{See \code{IEATools::iea_cols$last_stage}.
Each of these should be a column in \code{.sutdata}, \code{C_data}, and \code{eta_fu_data}.}

\item{final, useful}{See \code{IEATools::last_stages}.}

\item{industry_type, product_type}{See \code{IEATools::row_col_types}}

\item{R, U_eiou, U_feed, U, r_eiou, V, Y}{See \code{IEATools::psut_cols}.
These columns should be present in the \code{.sutdata} data frame.}

\item{C_eiou, C_Y, eta_fu, phi_u}{See \code{IEATools::template_cols}.
\code{C_eiou} and \code{C_Y} matrices should be found in columns of the \code{wide_C_data} data frame.
\code{eta_fu} and \code{phi_u} should be found in columns of the \code{wide_eta_fu_data} data frame.}

\item{interface_ind}{See \code{IEATools::interface_industries}. Interface industries are kept same from \code{Y_final} to \code{Y_useful}.}

\item{losses}{See \code{IEATools::tfc_compare_flows}. Losses are kept same from \code{Y_final} to \code{Y_useful}.}

\item{stat_diffs}{See \code{IEATools::tfc_compare_flows}. Statistical differences are kept same from \code{Y_final} to \code{Y_useful}.}

\item{notation}{The row and column notation for this template.
See \code{matsbyname::notation_vec()}. Default is \code{arrow_notation}.}

\item{tol}{the allowable error in energy balances for both the incoming matrices (last stage final)
and the outgoing matrices (last stage useful). Default is \code{1e-3}.}

\item{.eta_fu_hat}{an internal matrix name. Default is ".eta_fu_hat".}

\item{.add_to_U_f}{an internal matrix name for the a matrix to be added to the U_feed_f matrix
to form the useful form of the U_feed matrix. Default is ".add_to_U_f".}

\item{.add_to_U_eiou}{an internal matrix name for the a matrix to be added to the U_eiou_f matrix
to form the useful form of the U_eiou matrix. Default is ".add_to_U_eiou".}

\item{.add_to_V_f}{an internal matrix name for a matrix to add to the Y_f matrix. Default is ".add_to_V_f".}

\item{.add_to_dest}{an internal matrix name for a matrix that replaces a previous energy destination. Default is ".repl_dest".}

\item{.err}{an internal matrix name for calculating energy balance errors. Default is ".err".}

\item{.e_bal_ok}{an internal column name for assessing whether energy balance is within acceptable tolerances set by the \code{tol} argument. Default is ".e_bal_OK".}

\item{.useful}{A suffix applied to versions of PSUT matrices where useful is the last stage. Default is "_useful".}

\item{.keep_in_Y}{A suffix applied to versions of the \code{Y} matrix that contain only industries retained in the move from final to useful last stage,
including strings from the arguments \code{interface_ind}, \code{non_energy_ind}, \code{losses}, and \code{stat_diffs}.
Default is "_keep_in_Y".}
}
\value{
A version of \code{.sutdata} that contains additional rows with useful final stage ECC matrices.
If the energy balance check fails, a warning is emitted and
additional diagnostic matrices will appear in the output: \code{.err} and \code{.e_bal_ok}.
}
\description{
This function uses a matrix method to move
from final energy/exergy
to useful energy/exergy as the last stage of an energy conversion chain.
}
\examples{
C_data <- load_fu_allocation_data() \%>\% 
  form_C_mats()
eta_fu_data <- load_eta_fu_data() \%>\% 
  form_eta_fu_phi_u_vecs()
  m_cols <- eta_fu_data \%>\% 
    IEATools::meta_cols(return_names = TRUE,
                        years_to_keep = IEATools::iea_cols$year,
                        not_meta = c(IEATools::template_cols$eta_fu,
                                     IEATools::template_cols$phi_u))
psut_mats <- load_tidy_iea_df() \%>\% 
  specify_all() \%>\% 
  prep_psut() \%>\% 
  dplyr::full_join(C_data, by = m_cols) \%>\% 
  dplyr::full_join(eta_fu_data, by = m_cols)
psut_mats \%>\% 
  extend_to_useful()
}
