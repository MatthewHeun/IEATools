# 
# This file contains functions to "fix" specifics about IEA extended energy balance data for some countries.
# These functions should use data obtained from specific and detailed country studies.
#

#' Fix Ghana Primary solid biofuels data from 1991 through 1999
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
#'   ) %>% 
#'   # Filter rows from years beyond our interest for this test.
#'   filter(Year %in% c(1991, 1992))
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
                        year = col_names$year){
  # Figure out the years present in the .tidy_iea_df
  years_present <- .tidy_iea_df[[year]] %>% 
    unique()
  # The internal data that contains the updated Ghana Primary solid biofuels data
  # can be accessed with Fixed_GHA_PSB.
  data_to_bind <- Fixed_GHA_PSB %>% 
    dplyr::filter(!!as.name(year) %in% years_present)
  .tidy_iea_df %>% 
    # anti_join eliminates all rows in .tidy_iea_df that will be replaced
    dplyr::anti_join(Fixed_GHA_PSB, by = c(country, method, energy_type, last_stage, ledger_side,
                                           flow_aggregation_point, flow, product, unit, year)) %>% 
    # Now add the new data at the bottom.
    dplyr::bind_rows(data_to_bind)
}


fix_GHA_industry_electricity <- function(.tidy_iea_df,
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
                                         year = col_names$year) {
  
}



fix_HND_fuels <- function(.iea_df) {
  
}



