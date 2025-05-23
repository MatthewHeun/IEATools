% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{sample_eta_fu_table_path}
\alias{sample_eta_fu_table_path}
\title{The path to a filled final-to-useful efficiency table}
\usage{
sample_eta_fu_table_path(version = 2022)
}
\arguments{
\item{version}{the desired version (expressed as the year of IEA data release) of
the sample final-to-useful efficiencies table.
Options are 2021 (default), 2020, 2019, and 2018.}
}
\value{
The path to a final-to-useful efficiencies table.
}
\description{
The path to a filled final-to-useful efficiency table
}
\examples{
sample_eta_fu_table_path()     # Assumes 2022
sample_eta_fu_table_path(2022) # Same
}
