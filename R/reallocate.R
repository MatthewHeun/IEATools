#' Reallocate Industry not elsewhere specified
#' 
#' The IEA data include a final demand sector called
#' "Industry not elsewhere specified". 
#' Sometimes, such as when performing decomposition analysis,
#' it is desirable to reallocate "Industry not elsewhere specified"
#' to other industries in proportion to the consumption of each 
#' energy carrier in specified industries. 
#' This function performs that reallocation.
#' 
#' This function calls [matsbyname::reallocate_byname()]
#' internally to perform the reallocation.
#'
#' @param .sutmats A data frame of PSUT matrices, 
#'                 most likely the reult of [IEATools::prep_psut()].
#' @param industry_nes A vector of industry names (columns in the **Y** matrix)
#'                     that indicate unspecified industries.
#'                     Default is `c(IEATools::industry_flows$non_specified_industry, IEATools::industry_flows$industry_not_elsewhere_specified)`
#'                     to account for the IEA's nomenclature change in 2019.
#' @param industry_sectors A vector of industry names (columns in the **Y** matrix)
#'                         that consume energy,
#'                         including the "Industry not elsewhere specified" 
#'                         columns included in `industry_nes`.
#'                         Default is `IEATools::industry_flows`.
#' @param Y The name of the `Y` column in `.sutmats`. 
#'          Default is [IEATools::psut_cols]`$Y`.
#' @param Y_prime The name of the modified `Y` column in which 
#'                "Industry not elsewhere specified" has been reallocated.
#'                Default is "Y_prime".
#'
#' @return A version of `.sutmats` in which energy consumption by "Industry not elsewhere specified"
#'         is reallocated to other Industries in proportion to their energy consumption.
#' 
#' @export
#'
#' @examples
reallocate_industry_nes <- function(.sutmats, 
                                    industry_nes = c(IEATools::industry_flows$non_specified_industry, 
                                                     IEATools::industry_flows$industry_not_elsewhere_specified), 
                                    industry_sectors = IEATools::industry_flows, 
                                    Y = IEATools::psut_cols$Y, 
                                    Y_prime = "Y_prime") {
  
  reallocate_func <- function(Y_mat) {
    # Extract only the industry sector columns
    Y_industries <- Y_mat |> 
      matsbyname::select_cols_byname(retain_pattern = RCLabels::make_or_pattern(industry_sectors))
    
    Y_reallocated <- Y_industries |> 
      # The industries are in columns of the Y matrix, so use margin = 2.
      matsbyname::reallocate_byname(rowcolnames = industry_nes, margin = 2)
    
    out <- Y_mat |> 
      # Subtract the original Y industries 
      matsbyname::difference_byname(Y_industries) |> 
      # And add the reallocated Y industries
      matsbyname::sum_byname(Y_reallocated)
    
    list(out) |> 
      magrittr::set_names(Y_prime)
  }
  matsindf::matsindf_apply(.sutmats, FUN = reallocate_func, Y_mat = Y)
}