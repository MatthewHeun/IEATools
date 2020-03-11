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
fix_GHA_psb <- function(.tidy_iea_df) {
  # The internal data that contains the updated Ghana Primary Solid Biofuel data
  # can be accessed with Fixed_GHA_PSB.
  # We'll do a left-join with .tidy_iea_df to pull the updated data into the tidy data frame.
  .tidy_iea_df %>% 
    # anti_join eliminates all rows in .tidy_iea_df that will be replaced
    dplyr::anti_join(Fixed_GHA_PSB, by = c("Country", "Method", "Energy.type", "Last.stage", "Ledger.side",
                                           "Flow.aggregation.point", "Flow", "Product", "Unit", "Year")) %>% 
    # Now add the new data at the bottom.
    dplyr::bind_rows(Fixed_GHA_PSB)
}


fix_GHA_industry_electricity <- function(.iea_df) {
  
}



fix_HND_fuels <- function(.iea_df) {
  
}



