
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
#' 1. For those rows with no other consumption in **U** or **Y**,
#'    move **Y** Statistical differences to **R**  by subtraction.
#' 2. Reallocate negative Statistical differences in **R** to 
#'    **R** and **V** using [matsbyname::reallocate_byname()].
#' 3. Move remaining (positive) Statistical differences found in 
#'    **R** to the **Y** matrix by subtraction.
#' 4. Reallocate Statistical differences in the **Y** matrix to the **Y** and **U** 
#'    matrices using [matsbyname::reallocate_byname()].
#' 
#' Internally, the **R** and **V** matrices are added before calling 
#' [matsbyname::reallocate_byname()].
#' **R** and **V** are split again prior to returning.
#' Similarly, the **Y** and **U** matrices are added before calling 
#' [matsbyname::reallocate_byname()].
#' **Y** and **U** are split again prior to returning.
#' 
#' Energy balance is checked both 
#' prior to reallocating statistical differences
#' and after reallocating statistical differences.
#' Imbalances greater than `tol` cause an error.
#' 
#' Note that most functions in `IEATools` operate on 
#' tabular IEA data. 
#' However, this function assumes IEA data have already been 
#' converted to matrix (PSUT) format, 
#' most likely with [IEATools::prep_psut()].
#'
#' @param .sutmats A data frame of PSUT matrices, 
#'                 most likely the result of [IEATools::prep_psut()].
#' @param stat_diffs The name of the row in **R** and columns in **U_feed** and **Y**
#'                   for Statistical differences.
#'                   Default is `IEATools::tfc_compare_flows$statistical_differences`.
#' @param R,U,U_feed,U_eiou,r_eiou,V,Y Matrices or names of columns of matrices in `.sutmats`.
#'                                     See [IEATools::psut_cols].
#' @param R_colname,U_colname,U_feed_colname,U_eiou_colname,r_eiou_colname,V_colname,Y_colname
#'                                     Names of columns of matrices in `.sutmats`.
#'                                     See [IEATools::psut_cols].
#' @param prime_suffix The string suffix for new versions of matrices with reallocated
#'                     statistical differences.
#'                     Default is "_prime".
#' @param tol The tolerance for energy imbalance.
#'            Default is `1e-6`.
#'
#' @return A version of `.sutmats` in which energy consumption by "Statistical differences"
#'         is reallocated to other Industries in proportion to their energy consumption.
#'         
#' @export
#'
#' @examples
#' R <- matrix(c(98, 0, 
#'                0, 50, 
#'                2, 0), 
#'             byrow = TRUE, nrow = 3, ncol = 2, 
#'             dimnames = list(c("Resources [of Coal]", 
#'                               "Resources [of Prod C]",
#'                               "Statistical differences"), 
#'                             c("Coal [from Resources]", "Prod C"))) |> 
#'   matsbyname::setrowtype("Industry") |> matsbyname::setcoltype("Product")
#' R
#' U <- matrix(c(100, 
#'               2), 
#'             byrow = TRUE, nrow = 2, ncol = 1, 
#'             dimnames = list(c("Coal [from Resources]", "Electricity"), 
#'                             c("Mapep"))) |> 
#'   matsbyname::setrowtype("Product") |> matsbyname::setcoltype("Industry")
#' U
#' V <- matrix(40, 
#'             byrow = TRUE, nrow = 1, ncol = 1, 
#'             dimnames = list(c("Mapep"), c("Electricity"))) |> 
#'   matsbyname::setrowtype("Industry") |> matsbyname::setcoltype("Product")
#' V
#' Y <- matrix(c(20, 10, 8, 
#'                0, 0, 50), 
#'             byrow = TRUE, nrow = 2, ncol = 3, 
#'             dimnames = list(c("Electricity", "Prod C"), 
#'                             c("Industry 1", "Industry 2", 
#'                             "Statistical differences"))) |> 
#'   matsbyname::setrowtype("Product") |> matsbyname::setcoltype("Industry")
#' Y
#' r_eiou <- matrix(1, 
#'                  byrow = TRUE, nrow = 1, ncol = 1, 
#'                  dimnames = list("Electricity", "Mapep")) |> 
#'   matsbyname::setrowtype("Product") |> matsbyname::setcoltype("Industry")
#' r_eiou
#' U_EIOU <- matsbyname::hadamardproduct_byname(U, r_eiou)
#' U_EIOU
#' U_feed <- matsbyname::difference_byname(U, U_EIOU)
#' U_feed
#' reallocate_statistical_differences(R = R, 
#'                                    U = U, 
#'                                    U_feed = U_feed, 
#'                                    U_eiou = U_EIOU, 
#'                                    r_eiou = r_eiou, 
#'                                    V = V,
#'                                    Y = Y)
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
                                               prime_suffix = "_prime", 
                                               tol = 1e-6) {
  
  R_prime = paste0(R_colname, prime_suffix)
  U_prime = paste0(U_colname, prime_suffix)
  U_feed_prime = paste0(U_feed_colname, prime_suffix)
  U_eiou_prime = paste0(U_eiou_colname, prime_suffix)
  r_eiou_prime = paste0(r_eiou_colname, prime_suffix)
  V_prime = paste0(V_colname, prime_suffix)
  Y_prime = paste0(Y_colname, prime_suffix)
  
  reallocate_func <- function(R_mat, U_mat, U_feed_mat, U_eiou_mat, r_eiou_mat, V_mat, Y_mat) {

    # Verify that energy is conserved initially
    matsbyname::sum_byname(R_mat, V_mat) |> 
      matsbyname::transpose_byname() |> 
      matsbyname::rowsums_byname(colname = "rowsums") |> 
      matsbyname::difference_byname(matsbyname::sum_byname(U_mat, Y_mat) |> 
                                      matsbyname::rowsums_byname(colname = "rowsums")) |> 
      matsbyname::iszero_byname(tol = tol) |> 
      assertthat::assert_that(msg = "Energy not conserved at the top of reallocate_statistical_differences()")
    
    # Store rownames of R and V (industries)
    # But be sure we eliminate all zero rows first
    # so that row names are unique between R_mat and V_mat.
    rownames_R_mat <- R_mat |> 
      matsbyname::clean_byname(margin = 1) |> 
      rownames()
    rownames_V_mat <- V_mat |> 
      matsbyname::clean_byname(margin = 1) |> 
      rownames()
    # Store colnames of U and Y (industries)
    # But be sure we eliminate all zero cols first
    # so that row names are unique between U_mat and Y_mat.
    colnames_U_mat <- U_mat |> 
      matsbyname::clean_byname(margin = 2) |> 
      colnames()
    colnames_Y_mat <- Y_mat |> 
      matsbyname::clean_byname(margin = 2) |> 
      colnames()

    # Form matrix sums
    UY_mat <- matsbyname::sum_byname(U_mat, Y_mat) |> 
      matsbyname::clean_byname()
    RV_mat <- matsbyname::sum_byname(R_mat, V_mat) |> 
      matsbyname::clean_byname()

    # Move (Y statdiffs rows with no other consumption in U or Y) to R by subtraction.
    
    ## Find (Y statdiffs rows with no other consumption in U or Y).
    UY_mat_no_stat_diffs <- UY_mat |> 
      matsbyname::select_cols_byname(remove_pattern = stat_diffs, fixed = TRUE) |> 
      # Eliminate zero rows
      matsbyname::clean_byname(margin = 1)
    ## If the number of rows of UY_mat_no_stat_diffs is less than 
    ## the number of rows of UY_mat, 
    ## we have a situation where at least one statdiffs entry cannot be 
    ## reallocated within the U+Y matrices.  
    ## Find out which ones.
    statdiffs_rows_to_move_to_R <- setdiff(rownames(UY_mat), rownames(UY_mat_no_stat_diffs))
    if (length(statdiffs_rows_to_move_to_R) > 0) {
      ## Move these rows to R and reallocate across R and V
      UY_statdiffs_subtract <- UY_mat |> 
        matsbyname::select_cols_byname(retain_pattern = stat_diffs, fixed = TRUE) |> 
        matsbyname::select_rows_byname(retain_pattern = RCLabels::make_or_pattern(statdiffs_rows_to_move_to_R, 
                                                                                  pattern_type = "exact"))
      UY_mat <- matsbyname::difference_byname(UY_mat, UY_statdiffs_subtract) |> 
        matsbyname::clean_byname()
      RV_mat <- matsbyname::difference_byname(RV_mat, 
                                              matsbyname::transpose_byname(UY_statdiffs_subtract)) |> 
        matsbyname::clean_byname()
      ## Now reallocate only the negative statdiffs that we just moved.
      RV_mat <- RV_mat |> 
        matsbyname::reallocate_byname(rownames = stat_diffs, 
                                      colnames = statdiffs_rows_to_move_to_R, 
                                      margin = 1)
      ## Split R and V again
      R_mat_prime <- RV_mat |> 
        matsbyname::select_rows_byname(retain_pattern = RCLabels::make_or_pattern(rownames_R_mat, 
                                                                                  pattern_type = "exact")) |> 
        matsbyname::clean_byname()
      V_mat_prime <- RV_mat |> 
        matsbyname::select_rows_byname(retain_pattern = RCLabels::make_or_pattern(rownames_V_mat, pattern_type = "exact")) |> 
        matsbyname::clean_byname()
      ## No longer need RV_mat, so NULL it
      RV_mat <- NULL
    }

    # Move remaining R statdiffs to Y by subtraction.

    ## Find R statdiffs rows.
    R_stat_diffs <- R_mat_prime |> 
      matsbyname::select_rows_byname(retain_pattern = stat_diffs, fixed = TRUE) |> 
      matsbyname::clean_byname(margin = 2)

    if (ncol(R_stat_diffs) > 0) {
      ## Subtract the R_stat_diffs from both R_mat and UY_mat
      R_mat_prime <- matsbyname::difference_byname(R_mat_prime, R_stat_diffs) |> 
        matsbyname::clean_byname()
      UY_mat <- matsbyname::difference_byname(UY_mat, 
                                              matsbyname::transpose_byname(R_stat_diffs))
      ## Reallocate Stat diffs in UY_mat
      UY_mat <- UY_mat |> 
        matsbyname::reallocate_byname(colnames = stat_diffs, margin = 2)
      
      ## Split U and Y again
      U_mat_prime <- UY_mat |> 
        matsbyname::select_cols_byname(retain_pattern = RCLabels::make_or_pattern(colnames_U_mat, 
                                                                                  pattern_type = "exact")) |> 
        matsbyname::clean_byname()
      Y_mat_prime <- UY_mat |> 
        matsbyname::select_cols_byname(retain_pattern = RCLabels::make_or_pattern(colnames_Y_mat, 
                                                                                  pattern_type = "exact")) |> 
        matsbyname::clean_byname()
      ## No longer need UY_mat, so NULL it
      UY_mat <- NULL
      
      ## Calculate U_eiou = U * r_eiou (Hadamard product)
      U_eiou_mat_prime <- matsbyname::hadamardproduct_byname(U_mat_prime, r_eiou_mat) |> 
        matsbyname::clean_byname()
      ## Calculate U_feed = U - U_eiou
      U_feed_mat_prime <- matsbyname::difference_byname(U_mat_prime, U_eiou_mat_prime) |> 
        matsbyname::clean_byname()
    }
    
    # Verify that energy is conserved after reallocation
    matsbyname::sum_byname(R_mat_prime, V_mat_prime) |> 
      matsbyname::transpose_byname() |> 
      matsbyname::rowsums_byname(colname = "rowsums") |> 
      matsbyname::difference_byname(matsbyname::sum_byname(U_mat_prime, Y_mat_prime) |> 
                                      matsbyname::rowsums_byname(colname = "rowsums")) |> 
      matsbyname::iszero_byname(tol = tol) |> 
      assertthat::assert_that(msg = "Energy not conserved after reallocating statistical differences.")

    list(R_mat_prime, U_mat_prime, U_feed_mat_prime, U_eiou_mat_prime, 
         V_mat_prime, Y_mat_prime) |> 
      magrittr::set_names(c(R_prime, U_prime, U_feed_prime, U_eiou_prime, 
                            V_prime, Y_prime))
  }
  matsindf::matsindf_apply(.sutmats, FUN = reallocate_func, 
                           R_mat = R, U_mat = U, U_feed_mat = U_feed, U_eiou_mat = U_eiou, 
                           r_eiou_mat = r_eiou,
                           V_mat = V, Y_mat = Y)
}

















