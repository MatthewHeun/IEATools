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
  do_fix(.tidy_iea_df, replacement = Fixed_GHA_PSB, 
         country = country, year = year, e_dot = e_dot)
}


#' Improve IEA Ghana Industry Electricity data from 1974 through 2017
#' 
#' Ghana's Industry Electricity data are specified for 
#' Mining and quarrying, 
#' Non-ferrous metals, and 
#' Textiles and leather
#' for the year 1971--1973 only. 
#' However, data are available from 
#' the Ghana Grid Company (GridCo) and
#' the Volta River Authority 
#' that fill most remaining years to 2017.
#' The details to fill the extra years are in the object `Fixed_GHA_Industry_Electricity`.
#' This function applies the fixes to years that are available in `.tidy_iea_data`.
#' 
#' See the Supplemental information to 
#' M. K. Heun and P. E. Brockway. 
#' Meeting 2030 primary energy and economic growth goals: Mission impossible? 
#' Applied Energy, 251(112697):1–24, May 2019
#' for additional details.
#' 
#' If `.tidy_iea_df` does not contain data from Ghana for the years in question,
#' no fixing is performed, and `.tidy_iea_df` is returned unmodified.
#' 
#' Also see the file named "GHA-IndustryElectricity.xlsx" for the actual calculations.
#' 
#' @param .tidy_iea_df a tidy IEA data frame produced by [load_tidy_iea_df()]
#' @param country,year,e_dot See `IEATools::iea_cols`.
#'
#' @return `.tidy_iea_df` with improved Ghana Industry Electricity, if warranted.
#' 
#' @export
#' 
#' @examples
#' library(dplyr)
#' example_tidy_iea_df <- load_tidy_iea_df() %>% 
#'   filter(Country == "GHA")
#' example_tidy_iea_df
#' fixed <- example_tidy_iea_df %>% 
#'   fix_GHA_industry_electricity()
#' # Compare changed values
#' example_tidy_iea_df %>% 
#'   filter(Flow %in% c("Mining and quarrying",
#'                      "Non-ferrous metals",
#'                      "Textile and leather", 
#'                      "Industry not elsewhere specified"), 
#'          Product == "Electricity") %>% 
#'   select("Year", "Flow", "E.dot", "Unit")
#' fixed %>% 
#'   filter(Flow %in% c("Mining and quarrying",
#'                      "Non-ferrous metals",
#'                      "Textile and leather", 
#'                      "Industry not elsewhere specified"), 
#'          Product == "Electricity") %>% 
#'   select("Year", "Flow", "E.dot", "Unit")
#' # Show that new data are still in balance
#' example_tidy_iea_df %>% 
#'   filter(Year == 2000, 
#'          Product == "Electricity", 
#'          Flow == "Industry not elsewhere specified") %>% 
#'   select(E.dot) %>% 
#'   as.numeric()
#' fixed %>% 
#'   filter(Year == 2000, 
#'          Product == "Electricity", 
#'          Flow %in% c("Mining and quarrying",
#'                      "Non-ferrous metals",
#'                      "Textile and leather", 
#'                      "Industry not elsewhere specified")) %>% 
#'   select(E.dot) %>% 
#'   sum()
fix_GHA_industry_electricity <- function(.tidy_iea_df, 
                                         country = IEATools::iea_cols$country,
                                         year = IEATools::iea_cols$year,
                                         e_dot = IEATools::iea_cols$e_dot) {
  do_fix(.tidy_iea_df, replacement = Fixed_GHA_Industry_Electricity,
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
#'   fix_COL_electricity_generation()
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
fix_COL_electricity_generation <- function(.tidy_iea_df,
                                           country = IEATools::iea_cols$country,
                                           year = IEATools::iea_cols$year,
                                           e_dot = IEATools::iea_cols$e_dot) {
  do_fix(.tidy_iea_df, replacement = Fixed_COL_Electricity_Generation,
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
#' @param .tidy_iea_df the tidy IEA data frame to be fixed
#' @param replacement a data frame containing the data that fixes the IEA data
#' @param country the name of the country column in `.tidy_iea_df` and `replacement`
#' @param year the name of the year column in `.tidy_iea_df` and `replacement`
#' @param e_dot the name of the energy flow rate column in `.tidy_iea_df` and `replacement`
#'
#' @return a modified version of `.tidy_iea_df` with `replacement` included, if warranted
do_fix <- function(.tidy_iea_df, 
                   replacement, 
                   country,
                   year,
                   e_dot) {
  # Figure out the years present in the .tidy_iea_df
  years_present <- .tidy_iea_df[[year]] %>% unique()
  countries_present <- .tidy_iea_df[[country]] %>% unique()
  # The internal data that contains the updated Ghana Industry Electricity data
  # can be accessed with Fixed_GHA_Industry_Electricity.
  data_to_join <- replacement %>% 
    dplyr::filter(.data[[year]] %in% years_present, 
                  .data[[country]] %in% countries_present)
  replace_join(.tidy_iea_df, data_to_join, replace_col = e_dot)
}



