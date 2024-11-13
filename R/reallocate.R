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
#'                 most likely the result of [IEATools::prep_psut()].
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


#' Reallocate Statistical differences
#' 
#' The IEA data include "Statistical differences".
#' In some cases, it is desirable to reallocate Statistical differences
#' in proportion to the non-zero consumption of each 
#' energy carrier in other industries.
#' This function performs that reallocation
#' and should be called after the PSUT matrices have been formed, 
#' most likely by calling [IEATools::prep_psut()].
#' 
#' Statistical differences can be found in either the **R** or **Y** matrix.
#' Both are reallocated to the **U_EIOU** matrix.
#' The steps are:
#' 
#' * Move Statistical differences found in the **R** matrix to the **Y** matrix by subtraction.
#' * Reallocate Statistical differences in the **Y** matrix to the **U_EIOU** matrix using [matsbyname::reallocate_byname()].
#' 
#' This function calls [matsbyname::reallocate_byname()]
#' internally to perform the reallocation.
#'
#' @param .sutmats A data frame of PSUT matrices, 
#'                 most likely the result of [IEATools::prep_psut()].
#' @param stat_diffs 
#'
#' @return A version of `.sutmats` in which energy consumption by "Statistical differences"
#'         is reallocated to other Industries in proportion to their energy consumption.
#'         
#' @export
#'
#' @examples
reallocate_statistical_differences <- function(.sutmats, 
                                               stat_diffs = IEATools::tfc_compare_flows$statistical_differences, 
                                               R = IEATools::psut_cols$R, 
                                               U_feed = IEATools::psut_cols$U_feed,
                                               U_eiou = IEATools::psut_cols$U_eiou,
                                               V = IEATools::psut_cols$V,
                                               Y = IEATools::psut_cols$Y, 
                                               prime_suffix = "_prime",
                                               R_prime = paste0(R, prime_suffix),
                                               U_feed_prime = paste0(U_feed, prime_suffix),
                                               U_eiou_prime = paste0(U_eiou, prime_suffix),
                                               Y_prime = paste0(Y, prime_suffix)) {
  reallocate_func <- function(R_mat, U_feed_mat, U_eiou_mat, V_mat, Y_mat) {
    # Reallocate Stat diffs from the R matrix to the V matrix.
    R_stat_diffs <- R_mat |> 
      matsbyname::select_rows_byname(retain_pattern = stat_diffs, fixed = TRUE)
    
    # If there is no Stat diffs row in R_mat, 
    # R_stat_diffs will be NULL.
    if (!is.null(R_stat_diffs)) {
      V_mat_prime <- V_mat |> 
        # Add to the V matrix
        matsbyname::sum_byname(R_stat_diffs) |> 
        # Reallocate in proportion to other producers
        matsbyname::reallocate_byname(rowcolnames = stat_diffs, margin = 1)
      
      # Remove from the R matrix
      R_mat_prime <- R_mat |> 
        matsbyname::select_rows_byname(remove_pattern = stat_diffs, fixed = TRUE)
    } else {
      R_mat_prime <- R_mat
      V_mat_prime <- V_mat
    }
    
    
    
    
    
    # Reallocate Stat diffs from the Y matrix to the U_feed matrix.
    Y_stat_diffs <- Y_mat |> 
      matsbyname::select_cols_byname(retain_pattern = stat_diffs, fixed = TRUE)  
    
    # If there is no Stat diffs column in Y_mat, 
    # Y_stat_diffs will be NULL.
    if (!is.null(Y_stat_diffs)) {
      U_feed_mat_prime <- U_feed_mat |> 
        # Add to the U_feed matrix
        matsbyname::sum_byname(Y_stat_diffs) |> 
        # Reallocate in proportion to other consumers
        matsbyname::reallocate_byname(rowcolnames = stat_diffs, margin = 2)
      # Remove from Y matrix
      Y_mat_prime <- Y_mat |> 
        matsbyname::select_cols_byname(remove_pattern = stat_diffs, fixed = TRUE)
    } else {
      U_feed_mat_prime <- U_feed_mat
      Y_mat_prime <- Y_mat
    }
    list(R_mat_prime, U_mat_prime, U_feed_mat_prime, V_mat_prime, Y_mat_prime) |> 
      magrittr::set_names(c(R_prime, U_prime, U_feed_prime, V_prime, Y_prime))
  }
  matsindf::matsindf_apply(.sutmats, FUN = reallocate_func, 
                           R_mat = R, U_feed_mat = U_feed, U_eiou_mat = U_eiou, 
                           V_mat = V, Y_mat = Y)
}

















