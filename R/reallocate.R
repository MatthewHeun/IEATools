
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
#' Both are reallocated to the **Y** and **U** matrices in proportion.
#' The steps are:
#' 
#' * Move Statistical differences found in the **R** matrix to the **Y** matrix by subtraction.
#' * Reallocate Statistical differences in the **Y** matrix to the **Y** and **U** 
#'   matrices using [matsbyname::reallocate_byname()].
#' 
#' Internally, the **Y** and **U_feed** matrices are added before calling 
#' [matsbyname::reallocate_byname()].
#' The matrices are split again prior to returning.
#'
#' @param .sutmats A data frame of PSUT matrices, 
#'                 most likely the result of [IEATools::prep_psut()].
#' @param stat_diffs The name of the row in **R** and columns in **U_feed** and **Y**
#'                   for Statistical differences.
#'                   Default is `IEATools::tfc_compare_flows$statistical_differences`.
#' @param R,U_feed,U,V,Y Names of columns of matrices in `.sutmats`.
#'                            See [IEATools::psut_cols].
#' @param prime_suffix The string suffix for new versions of matrices with reallocated
#'                     statistical differences.
#'                     Default is "_prime".
#' @param R_prime,U_feed_prime,U_prime
#'
#' @return A version of `.sutmats` in which energy consumption by "Statistical differences"
#'         is reallocated to other Industries in proportion to their energy consumption.
#'         
#' @export
#'
#' @examples
reallocate_statistical_differences <- function(.sutmats = NULL, 
                                               stat_diffs = IEATools::tfc_compare_flows$statistical_differences, 
                                               R = IEATools::psut_cols$R, 
                                               U = IEATools::psut_cols$U,
                                               U_feed = IEATools::psut_cols$U_feed,
                                               U_eiou = IEATools::psut_cols$U_eiou,
                                               r_eiou = IEATools::psut_cols$r_eiou,
                                               V = IEATools::psut_cols$V,
                                               Y = IEATools::psut_cols$Y, 
                                               R_colname = IEATools::psut_cols$R, 
                                               U_colname = IEATools::psut_cols$U,
                                               U_feed_colname = IEATools::psut_cols$U_feed,
                                               U_eiou_colname = IEATools::psut_cols$U_eiou,
                                               r_eiou_colname = IEATools::psut_cols$r_eiou,
                                               V_colname = IEATools::psut_cols$V,
                                               Y_colname = IEATools::psut_cols$Y, 
                                               prime_suffix = "_prime") {
  
  R_prime = paste0(R_colname, prime_suffix)
  U_prime = paste0(U_colname, prime_suffix)
  U_feed_prime = paste0(U_feed_colname, prime_suffix)
  U_eiou_prime = paste0(U_eiou_colname, prime_suffix)
  r_eiou_prime = paste0(r_eiou_colname, prime_suffix)
  V_prime = paste0(V_colname, prime_suffix)
  Y_prime = paste0(Y_colname, prime_suffix)
  
  reallocate_func <- function(R_mat, U_mat, U_feed_mat, U_eiou_mat, r_eiou_mat, V_mat, Y_mat) {

    # Store rownames of R and V
    rownames_R_mat <- rownames(R_mat)
    rownames_V_mat <- rownames(V_mat)
    # Store colnames of U and Y
    colnames_U_mat <- colnames(U_mat)
    colnames_Y_mat <- colnames(Y_mat)

    # Form matrix sums
    UY_mat <- matsbyname::sum_byname(U_mat, Y_mat) |> 
      matsbyname::clean_byname()
    RV_mat <- matsbyname::sum_byname(R_mat, V_mat) |> 
      matsbyname::clean_byname()

    # Find (Y statdiffs rows with no other consumption in U or Y).
    UY_mat_no_stat_diffs <- UY_mat |> 
      matsbyname::select_cols_byname(remove_pattern = stat_diffs, fixed = TRUE) |> 
      # Eliminate zero rows
      matsbyname::clean_byname(margin = 1)
    # If the number of rows of UY_mat_no_stat_diffs is less than 
    # the number of rows of UY_mat, 
    # we have a situation where at least one statdiffs entry cannot be 
    # reallocated within the U+Y matrices.  
    # Find out which ones.
    rows_to_move_to_R <- setdiff(rownames(UY_mat), rownames(UY_mat_no_stat_diffs))
    if (length(rows_to_move_to_R) > 0) {
      # Move these rows to R and reallocate
      UY_statdiffs_subtract <- UY_mat |> 
        matsbyname::select_cols_byname(retain_pattern = stat_diffs, fixed = TRUE) |> 
        matsbyname::select_rows_byname(retain_pattern = RCLabels::make_or_pattern(rows_to_move_to_R, 
                                                                                  pattern_type = "exact"))
      UY_mat <- matsbyname::difference_byname(UY_mat, UY_statdiffs_subtract) |> 
        matsbyname::clean_byname()
      RV_mat <- matsbyname::difference_byname(RV_mat, 
                                              matsbyname::transpose_byname(UY_statdiffs_subtract)) |> 
        matsbyname::clean_byname()
    }
    
    
    
    # Move (Y statdiffs rows with no other consumption in U or Y) to R by subtraction.
    
    
    
    # Sum R and V.
    # Reallocate negative statdiffs only into R+V.
    # Split R and V using stored rownames.
    
    
    # Move remaining R statdiffs to Y by subtraction.

        
    # Sum U and Y.
    # Reallocate stadiffs columns into U+Y.
    # Split U and Y using stored colnames.
    
    
    # Calculate U_eiou = U * r_eiou (Hadamard product)
    # Calculate U_feed = U - U_eiou
    
    
    
    
    
    
    
    # # Reallocate Stat diffs from the R matrix to the V matrix.
    # R_stat_diffs <- R_mat |> 
    #   matsbyname::select_rows_byname(retain_pattern = stat_diffs, fixed = TRUE)
    # 
    # # If there is no Stat diffs row in R_mat, 
    # # R_stat_diffs will be NULL.
    # if (!is.null(R_stat_diffs)) {
    #   V_mat_prime <- V_mat |> 
    #     # Add to the V matrix
    #     matsbyname::sum_byname(R_stat_diffs) |> 
    #     # Reallocate in proportion to other producers
    #     matsbyname::reallocate_byname(rowcolnames = stat_diffs, margin = 1)
    #   
    #   # Remove from the R matrix
    #   R_mat_prime <- R_mat |> 
    #     matsbyname::select_rows_byname(remove_pattern = stat_diffs, fixed = TRUE)
    # } else {
    #   R_mat_prime <- R_mat
    #   V_mat_prime <- V_mat
    # }
    # 
    # 
    # 
    # 
    # 
    # # Reallocate Stat diffs from the Y matrix to the U_feed matrix.
    # Y_stat_diffs <- Y_mat |> 
    #   matsbyname::select_cols_byname(retain_pattern = stat_diffs, fixed = TRUE)  
    # 
    # # If there is no Stat diffs column in Y_mat, 
    # # Y_stat_diffs will be NULL.
    # if (!is.null(Y_stat_diffs)) {
    #   U_feed_mat_prime <- U_feed_mat |> 
    #     # Add to the U_feed matrix
    #     matsbyname::sum_byname(Y_stat_diffs) |> 
    #     # Reallocate in proportion to other consumers
    #     matsbyname::reallocate_byname(rowcolnames = stat_diffs, margin = 2)
    #   # Remove from Y matrix
    #   Y_mat_prime <- Y_mat |> 
    #     matsbyname::select_cols_byname(remove_pattern = stat_diffs, fixed = TRUE)
    # } else {
    #   U_feed_mat_prime <- U_feed_mat
    #   Y_mat_prime <- Y_mat
    # }
    list(R_mat_prime, U_mat_prime, U_feed_mat_prime, U_eiou_mat_prime, 
         V_mat_prime, Y_mat_prime) |> 
      magrittr::set_names(c(R_prime, U_prime, U_feed_prime, V_prime, Y_prime))
  }
  matsindf::matsindf_apply(.sutmats, FUN = reallocate_func, 
                           R_mat = R, U_mat = U, U_feed_mat = U_feed, U_eiou_mat = U_eiou, 
                           r_eiou_mat = r_eiou,
                           V_mat = V, Y_mat = Y)
}

















