# 
# This file contains functions to "fix" specifics about IEA extended energy balance data for some countries.
# These functions should use data obtained from specific and detailed country studies.
#

#' Improve Ghana Primary solid biofuels data from 1991 through 1999
#' 
#' Ghana's Primary solid biofuels data show a very large and dramatic decline from 1999 to 2000.
#' This decline is due to new survey data being used for the 2000 data.  
#' When we look at the PSB data on a per-capita basis, it is clear that 
#' a near-constant PSB/capita value was used to extrapolate per-capita usage in the late 1990s.
#' When new survey data became available for the 2000 reporting year, 
#' the per-capita consumption of PSB obviously changed.  
#' Our approach to this problem is to smooth out the really big peak in PSB consumption 
#' by reducing the per-capita consumption of PSB, starting in 1991.
#' 
#' If `.tidy_iea_df` does not contain data from Ghana for the years in question,
#' no fixing is performed, and `.tidy_iea_df` is returned unmodified.
#' 
#' See the Supplemental information to 
#' M. K. Heun and P. E. Brockway. 
#' Meeting 2030 primary energy and economic growth goals: Mission impossible? 
#' Applied Energy, 251(112697):1–24, May 2019
#' for additional details.
#' 
#' Also see the file named "GHA-PSB.xlsx" for the actual calculations.
#'
#' @param .tidy_iea_df a tidy IEA data frame produced by `load_tidy_iea_df()`
#' @param country,year,e_dot See `IEATools::iea_cols`.
#'
#' @return `.tidy_iea_df` with smoothed Ghana Primary solid biofuels data
#' 
#' @export
#'
#' @examples
#' library(dplyr)
#' # Build an example tidy IEA data frame in which Ghana's Primary solid biofuels can be fixed.
#' example_tidy_iea_df <- load_tidy_iea_df() %>% 
#'   filter(Country == "GHA") %>% 
#'   filter(Product == "Primary solid biofuels") %>% 
#'   # The example data frame has PSB for Ghana for 1971 and 2000.  
#'   # Let's pretend that 1971 is 1991 and 2000 is 1992.
#'   mutate(
#'     Year := dplyr::case_when(
#'       Year == 1971 ~ 1991,
#'       Year == 2000 ~ 1992
#'     )
#'   ) 
#' example_tidy_iea_df
#' fixed <- example_tidy_iea_df %>% 
#'   fix_GHA_psb()
#' # Compare production of Primary solid biofuels in 1991
#' example_tidy_iea_df %>% 
#'   filter(Year == 1991, Flow == "Production") %>% 
#'   select("E.dot", "Unit")
#' fixed %>% 
#'   filter(Year == 1991, Flow == "Production") %>% 
#'   select("E.dot", "Unit")
fix_GHA_psb <- function(.tidy_iea_df,
                        country = IEATools::iea_cols$country, 
                        year = IEATools::iea_cols$year, 
                        e_dot = IEATools::iea_cols$e_dot){
  do_fix(.tidy_iea_df, replacement = IEATools::Fixed_GHA_PSB, 
         country = country, year = year, e_dot = e_dot)
}


#' Improve COL electricity generation in the IEA's WEEB.
#' 
#' The 2022 release of the IEA's WEEB data
#' is different from the 2021 release of the IEA's WEEB data
#' in terms of Colombia's Electricity generation.
#' For example:
#' - 2021 release: 
#'     - Main activity producer electricity plants 34196.4008 TJ
#'     - Autoproducer electricity plants: 2671.1993 TJ
#' - 2022 release: 
#'     - Main activity producer electricity plants 31467.5994 TJ
#'     - Autoproducer electricity plants: 899.9987 TJ
#' Similar differences appear in all years 1971 - 1977. 
#' From 1978 onward, the 2021 and 2022 releases agree.
#' Note that this change leads to overall energy imbalance 
#' for Colombia 1971-1977 and World 1971-1977.
#' This function reverts to the values from the 2021 release of the IEA WEEB.
#' 
#' Similarly, World Electricity is different 2021 release to 2022 release.
#' The 2022 data are unbalanced for 1971 --> 1977.
#' This function reverts to the value from the 2021 release of the IEA WEEB 
#' for World Electricity in 1971--1977.
#'
#' @param .tidy_iea_df a tidy IEA data frame produced by [load_tidy_iea_df()]
#' @param country,year,e_dot See `IEATools::iea_cols`.
#'
#' @return `.tidy_iea_df` with improved Ghana Industry Electricity, if warranted
#' 
#' @export
#'
#' @examples
#' library(dplyr)
#' example_tidy_iea_df <- load_tidy_iea_df() %>% 
#'   filter(Country == "GHA") |> 
#'   dplyr::mutate(
#'     # Pretend the GHA is COL.
#'     Country = "COL"
#'   )
#' example_tidy_iea_df
#' fixed <- example_tidy_iea_df %>% 
#'   fix_COL_WRLD_electricity()
#' # Compare changed values
#' example_tidy_iea_df %>% 
#'   filter(Flow %in% c("Main activity producer electricity plants",
#'                       "Autoproducer electricity plants"), 
#'          Product == "Electricity") %>% 
#'   select("Year", "Flow", "E.dot", "Unit")
#' fixed %>% 
#'   filter(Flow %in% c("Main activity producer electricity plants",
#'                       "Autoproducer electricity plants"), 
#'          Product == "Electricity") %>% 
#'   select("Year", "Flow", "E.dot", "Unit")
fix_COL_WRLD_electricity <- function(.tidy_iea_df,
                                     country = IEATools::iea_cols$country,
                                     year = IEATools::iea_cols$year,
                                     e_dot = IEATools::iea_cols$e_dot) {
  do_fix(.tidy_iea_df, replacement = IEATools::Fixed_COL_WRLD_Electricity,
         country = country, year = year, e_dot = e_dot)
}


fix_HND_fuels <- function(.iea_df) {
  
}


fix_JPN_psp <- function(.iea_df) {
  
}


#' Performs fixes to IEA data
#' 
#' This is an internal convenience function that performs fixes on IEA data
#' given a replacement data frame. 
#' It makes use of the `replace_join()` function internally.
#'
#' @param .tidy_iea_df The tidy IEA data frame to be fixed.
#' @param replacement A data frame containing the data that fixes the IEA data.
#' @param country The name of the country column in `.tidy_iea_df` and `replacement`.
#' @param year The name of the year column in `.tidy_iea_df` and `replacement`.
#' @param e_dot The name of the energy flow rate column in `.tidy_iea_df` and `replacement`.
#'
#' @return A modified version of `.tidy_iea_df` with `replacement` included, if warranted.
do_fix <- function(.tidy_iea_df, 
                   replacement, 
                   country,
                   year,
                   e_dot) {
  # Check to see if we are wide by years.
  wide_by_years <- !(year %in% colnames(.tidy_iea_df))
  if (wide_by_years) {
    # Pivot to pull all years into a column
    yr_cols <- year_cols(.tidy_iea_df)
    .tidy_iea_df <- .tidy_iea_df |> 
      tidyr::pivot_longer(cols = dplyr::all_of(yr_cols), names_to = year, values_to = e_dot) |> 
      dplyr::mutate(
        "{year}" := as.numeric(.data[[year]])
      )
  }
  # Figure out the years present in the .tidy_iea_df
  years_present <- .tidy_iea_df[[year]] %>% unique()
  countries_present <- .tidy_iea_df[[country]] %>% unique()
  # Filter the replacement data to restrict to countries and years
  # present in .tidy_iea_df.
  data_to_join <- replacement %>% 
    dplyr::filter(.data[[year]] %in% years_present, 
                  .data[[country]] %in% countries_present)
  out <- replace_join(.tidy_iea_df, data_to_join, replace_col = e_dot)
  if (wide_by_years) {
    # Pivot wider to deliver data in same format as received
    out <- out |> 
      tidyr::pivot_wider(names_from = dplyr::all_of(year), values_from = dplyr::all_of(e_dot))
  }
  return(out)
}



