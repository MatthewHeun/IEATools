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
#' See the Supplemental information to 
#' M. K. Heun and P. E. Brockway. 
#' Meeting 2030 primary energy and economic growth goals: Mission impossible? 
#' Applied Energy, 251(112697):1–24, May 2019
#' for additional details.
#' 
#' Also see the file named "GHA-PSB.xlsx" for the actual calculations.
#'
#' @param .tidy_iea_df a tidy IEA data frame produced by [load_tidy_iea_df()]
#' @param col_names a list of column names in IEA data frames. Default is `IEATools::iea_cols`.
#' @param country the name of the country column in `.tidy_iea_df`. Default is `col_names$country`.
#' @param method the name of the method column in `.tidy_iea_df`. Default is `col_names$method`.
#' @param energy_type the name of the energy type column in `.tidy_iea_df`. Default is `col_names$energy_type`.
#' @param last_stage the name of the last stage column in `.tidy_iea_df`. Default is `col_names$last_stage`.
#' @param ledger_side the name of the ledger side column in `.tidy_iea_df`. Default is `col_names$ledger_side`.
#' @param flow_aggregation_point the name of the flow aggregation point column in `.tidy_iea_df`. Default is `col_names$flow_aggregation_point`.
#' @param flow the name of the flow column in `.tidy_iea_df`. Default is `col_names$flow`.
#' @param product the name of the product column in `.tidy_iea_df`. Default is `col_names$product`.
#' @param unit the name of the unit column in `.tidy_iea_df`. Default is `col_names$unit`.
#' @param year the name of the year column in `.tidy_iea_df`. Default is `col_names$year`.
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
                        col_names = IEATools::iea_cols,
                        country = col_names$country, 
                        method = col_names$method, 
                        energy_type = col_names$energy_type,
                        last_stage = col_names$last_stage,
                        ledger_side = col_names$ledger_side, 
                        flow_aggregation_point = col_names$flow_aggregation_point,
                        flow = col_names$flow,
                        product = col_names$product,
                        unit = col_names$unit,
                        year = col_names$year, 
                        e_dot = col_names$e_dot){
  # # Figure out the years present in the .tidy_iea_df
  # years_present <- .tidy_iea_df[[year]] %>% 
  #   unique()
  # # The internal data that contains the updated Ghana Primary solid biofuels data
  # # can be accessed with Fixed_GHA_PSB.
  # data_to_bind <- Fixed_GHA_PSB %>% 
  #   dplyr::filter(!!as.name(year) %in% years_present)
  # .tidy_iea_df %>% 
  #   # anti_join eliminates all rows in .tidy_iea_df that will be replaced
  #   dplyr::anti_join(Fixed_GHA_PSB, by = c(country, method, energy_type, last_stage, ledger_side,
  #                                          flow_aggregation_point, flow, product, unit, year)) %>% 
  #   # Now add the new data at the bottom.
  #   dplyr::bind_rows(data_to_bind)
  do_fix(.tidy_iea_df, replacement = Fixed_GHA_PSB, 
         year = year, e_dot = e_dot)
  
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
#' Also see the file named "GHA-IndustryElectricity.xlsx" for the actual calculations.
#' 
#' @param .tidy_iea_df a tidy IEA data frame produced by [load_tidy_iea_df()]
#' @param col_names a list of column names in IEA data frames. Default is `IEATools::iea_cols`.
#' @param country the name of the country column in `.tidy_iea_df`. Default is `col_names$country`.
#' @param method the name of the method column in `.tidy_iea_df`. Default is `col_names$method`.
#' @param energy_type the name of the energy type column in `.tidy_iea_df`. Default is `col_names$energy_type`.
#' @param last_stage the name of the last stage column in `.tidy_iea_df`. Default is `col_names$last_stage`.
#' @param ledger_side the name of the ledger side column in `.tidy_iea_df`. Default is `col_names$ledger_side`.
#' @param flow_aggregation_point the name of the flow aggregation point column in `.tidy_iea_df`. Default is `col_names$flow_aggregation_point`.
#' @param flow the name of the flow column in `.tidy_iea_df`. Default is `col_names$flow`.
#' @param product the name of the product column in `.tidy_iea_df`. Default is `col_names$product`.
#' @param unit the name of the unit column in `.tidy_iea_df`. Default is `col_names$unit`.
#' @param year the name of the year column in `.tidy_iea_df`. Default is `col_names$year`.
#'
#' @return `.tidy_iea_df` with smoothed Ghana Primary solid biofuels data
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
                                         col_names = IEATools::iea_cols,
                                         year = col_names$year,
                                         e_dot = col_names$e_dot) {
  # # Figure out the years present in the .tidy_iea_df
  # years_present <- .tidy_iea_df[[year]] %>%
  #   unique()
  # # The internal data that contains the updated Ghana Industry Electricity data
  # # can be accessed with Fixed_GHA_Industry_Electricity.
  # data_to_join <- Fixed_GHA_Industry_Electricity %>%
  #   dplyr::filter(!!as.name(year) %in% years_present)
  # replace_join(.tidy_iea_df, data_to_join, replace_col = e_dot)
  do_fix(.tidy_iea_df, replacement = Fixed_GHA_Industry_Electricity, 
         year = year, e_dot = e_dot)
}


fix_HND_fuels <- function(.iea_df) {
  
}



do_fix <- function(.tidy_iea_df, 
                   replacement, 
                   year,
                   e_dot) {
  # Figure out the years present in the .tidy_iea_df
  years_present <- .tidy_iea_df[[year]] %>% unique()
  # The internal data that contains the updated Ghana Industry Electricity data
  # can be accessed with Fixed_GHA_Industry_Electricity.
  data_to_join <- replacement %>% dplyr::filter(!!as.name(year) %in% years_present)
  replace_join(.tidy_iea_df, data_to_join, replace_col = e_dot)
}
