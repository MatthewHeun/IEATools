% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fixes.R
\name{fix_GHA_psb}
\alias{fix_GHA_psb}
\title{Improve Ghana Primary solid biofuels data from 1991 through 1999}
\usage{
fix_GHA_psb(
  .tidy_iea_df,
  country = IEATools::iea_cols$country,
  year = IEATools::iea_cols$year,
  e_dot = IEATools::iea_cols$e_dot
)
}
\arguments{
\item{.tidy_iea_df}{a tidy IEA data frame produced by \code{load_tidy_iea_df()}}

\item{country, year, e_dot}{See \code{IEATools::iea_cols}.}
}
\value{
\code{.tidy_iea_df} with smoothed Ghana Primary solid biofuels data
}
\description{
Ghana's Primary solid biofuels data show a very large and dramatic decline from 1999 to 2000.
This decline is due to new survey data being used for the 2000 data.
When we look at the PSB data on a per-capita basis, it is clear that
a near-constant PSB/capita value was used to extrapolate per-capita usage in the late 1990s.
When new survey data became available for the 2000 reporting year,
the per-capita consumption of PSB obviously changed.
Our approach to this problem is to smooth out the really big peak in PSB consumption
by reducing the per-capita consumption of PSB, starting in 1991.
}
\details{
If \code{.tidy_iea_df} does not contain data from Ghana for the years in question,
no fixing is performed, and \code{.tidy_iea_df} is returned unmodified.

See the Supplemental information to
M. K. Heun and P. E. Brockway.
Meeting 2030 primary energy and economic growth goals: Mission impossible?
Applied Energy, 251(112697):1–24, May 2019
for additional details.

Also see the file named "GHA-PSB.xlsx" for the actual calculations.
}
\examples{
library(dplyr)
# Build an example tidy IEA data frame in which Ghana's Primary solid biofuels can be fixed.
example_tidy_iea_df <- load_tidy_iea_df() \%>\% 
  filter(Country == "GHA") \%>\% 
  filter(Product == "Primary solid biofuels") \%>\% 
  # The example data frame has PSB for Ghana for 1971 and 2000.  
  # Let's pretend that 1971 is 1991 and 2000 is 1992.
  mutate(
    Year := dplyr::case_when(
      Year == 1971 ~ 1991,
      Year == 2000 ~ 1992
    )
  ) 
example_tidy_iea_df
fixed <- example_tidy_iea_df \%>\% 
  fix_GHA_psb()
# Compare production of Primary solid biofuels in 1991
example_tidy_iea_df \%>\% 
  filter(Year == 1991, Flow == "Production") \%>\% 
  select("Edot", "Unit")
fixed \%>\% 
  filter(Year == 1991, Flow == "Production") \%>\% 
  select("Edot", "Unit")
}
