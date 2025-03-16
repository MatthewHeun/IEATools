#' Extract a unit summation matrix from a tidy data frame
#'
#' Unit summation matrices have products in rows and units in columns, with
#' `1`s where a product is expressed in the unit and `0`s otherwise.
#' 
#' `.tidy_iea_df` should be grouped as needed, typically on 
#' `Country`, `Year`, `EnergyType`, `Last.stage`, etc., but
#' _not_ on `Unit`, `Flow` or `Product`.
#' `.tidy_iea_df` is typically obtained from `tidy_iea_df()`.
#'
#' @param .tidy_iea_df the tidy data frame from which a unit summation `S_units` matrix is to be formed.
#' @param matrix_class The type of matrix to be created, one of "matrix" or "Matrix".
#'                     Default is "matrix".
#' @param ledger_side,flow_aggregation_point,flow,product,e_dot,unit,matnames See `IEATools::iea_cols`.
#' @param s_units See `IEATools::psut_cols`.
#' @param product_type,unit_type See `IEATools::row_col_types`.
#' @param .val the name of a temporary value column to be created in `.tidy_iea_df`. Default is ".val".
#' @param .rowtype the name of a temporary rowtype column created in `.tidy_iea_df`. Default is ".rowtype".
#' @param .coltype the name of a temporary coltype column created in `.tidy_iea_df`. Default is ".coltype".
#'
#' @return a data frame containing grouping variables and a new column of unit summation matrices called `s_unit`.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' load_tidy_iea_df() %>% 
#'   extract_S_units_from_tidy()
extract_S_units_from_tidy <- function(.tidy_iea_df, 
                                      matrix_class = c("matrix", "Matrix"),
                                      # Column names in .tidy_iea_df
                                      ledger_side = IEATools::iea_cols$ledger_side, 
                                      flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point, 
                                      flow = IEATools::iea_cols$flow, 
                                      product = IEATools::iea_cols$product, 
                                      e_dot = IEATools::iea_cols$e_dot,
                                      unit = IEATools::iea_cols$unit,
                                      matnames = IEATools::mat_meta_cols$matnames,
                                      # Row and product types
                                      product_type = IEATools::row_col_types$product,
                                      unit_type = IEATools::row_col_types$unit, 
                                      # Output column name
                                      s_units = IEATools::psut_cols$s_units, 
                                      # Intermediate column names
                                      .val = ".val", 
                                      .rowtype = ".rowtype", 
                                      .coltype = ".coltype"){
  
  matrix_class <- match.arg(matrix_class)
  
  # grouping_vars <- matsindf::everything_except(.tidy_iea_df, ledger_side, flow_aggregation_point, flow, product, e_dot, unit, matnames)
  grouping_symbols <- matsindf::everything_except(.tidy_iea_df, ledger_side, flow_aggregation_point, flow, product, e_dot, unit, matnames)
  grouping_vars <- matsindf::everything_except(.tidy_iea_df, ledger_side, flow_aggregation_point, 
                                               flow, product, e_dot, unit, matnames, .symbols = FALSE)
  matsindf::verify_cols_missing(.tidy_iea_df, c(s_units, .val, .rowtype, .coltype))
  
  .tidy_iea_df %>% 
    # dplyr::group_by(!!!grouping_vars) %>% 
    dplyr::group_by(!!!grouping_symbols) %>%
    # dplyr::select(!!!grouping_vars, .data[[product]], .data[[unit]]) %>%
    dplyr::select(dplyr::all_of(c(grouping_vars, product, unit))) %>%
    dplyr::do(unique(.data)) %>%
    dplyr::mutate(
      "{.val}" := 1,
      "{s_units}" := s_units,
      "{.rowtype}" := product_type,
      "{.coltype}" := unit_type
    ) %>%
    matsindf::collapse_to_matrices(matnames = s_units, matvals = .val,
                                   rownames = product, colnames = unit,
                                   rowtypes = .rowtype, coltypes = .coltype, 
                                   matrix_class = matrix_class) %>%
    dplyr::rename(
      # "{s_units}" := .data[[.val]]
      "{s_units}" := dplyr::all_of(.val)
    ) %>% 
    dplyr::ungroup()
}


#' Add a column of matrix names to tidy data frame
#'
#' This function adds a column of matrix names to a tidy data frame
#' wherein each row of `.tidy_iea_df` is a single value in an energy conversion chain.
#' The default argument values assume that `.tidy_iea_df` uses IEA-style nomenclature
#' and terminology, although `.tidy_iea_df` does not necessarily need to contain IEA data.
#' In a typical workflow, this function would be followed by a call to
#' `add_row_col_meta()` and `matsindf::collapse_to_matrices()`.
#'
#' This function respects groups when identifying entries in the resource matrix (`R`).
#' So be sure to group `.tidy_iea_df` before calling this function.
#'
#' If `.tidy_iea_df` already has a `matnames` column, 
#' this function returns the `.tidy_iea_df` without modification, 
#' assuming that the caller has already supplied a destination
#' matrix name for each row of `.tidy_iea_df`.
#' 
#' The argument `R_includes_all_exogenous_flows` controls how the **R** matrix is formed.
#' When `TRUE`, all exogenous flows 
#' (including Resources, Production, Bunkers,
#' Imports, Statistical differences, and Stock changes)
#' are placed in the **R** matrix.
#' When `FALSE`, only Resources and Production are placed in the **R** matrix.
#' Default is `TRUE`.
#' `FALSE` retains previous behavior.
#' 
#' @param .tidy_iea_df a data frame with `ledger_side`, `flow_aggregation_point`, `flow`, and `e_dot` columns.
#' @param R_includes_all_exogenous_flows Tells how to construct the **R** matrix.
#'                                       Default is `TRUE`. 
#'                                       See details.
#' @param ledger_side,flow_aggregation_point,flow,product,e_dot See `IEATools::iea_cols`.
#' @param supply,consumption See `IEATools::ledger_sides`.
#' @param production,resources See `IEATools::tpes_flows`.
#' @param eiou See `IEATools::tfc_compare_flows`.
#' @param neg_supply_in_fd For "Exports", "International aviation bunkers", "International marine bunkers", and "Stock changes", see `IEATools::tpes_flows`.
#'                         For "Losses" and "Statistical differences", see `IEATools::tfc_compare_flows`.
#' @param pos_supply_in_R For "Resources", "Imports", "Statistical differences", "X Bunkers", and "Stock changes", positive flows 
#'                        should be placed in the **R** matrix. See `IEATools::tfc_compare_flows`.
#' @param matnames See `IEATools::mat_meta_cols`.
#' @param R,U_feed,U_EIOU,V,Y See `IEATools::psut_matnames`.
#'
#' @return `.tidy_iea_df` with an added column `matnames`.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' load_tidy_iea_df() %>% 
#'   add_psut_matnames() %>%
#'   glimpse()
add_psut_matnames <- function(.tidy_iea_df,
                              # Controls how the R matrix is constructed.
                              R_includes_all_exogenous_flows = TRUE,
                              # Input columns
                              ledger_side = IEATools::iea_cols$ledger_side,
                              flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                              flow = IEATools::iea_cols$flow, 
                              product = IEATools::iea_cols$product, 
                              e_dot = IEATools::iea_cols$e_dot,
                              supply = IEATools::ledger_sides$supply,
                              consumption = IEATools::ledger_sides$consumption,
                              production = IEATools::tpes_flows$production,
                              resources = IEATools::tpes_flows$resources,
                              # Input identifiers for supply, consumption, and EIOU
                              eiou = IEATools::tfc_compare_flows$energy_industry_own_use,
                              pos_supply_in_R = c(IEATools::tpes_flows$resources, 
                                                  IEATools::tpes_flows$imports, 
                                                  IEATools::tpes_flows$international_aviation_bunkers,
                                                  IEATools::tpes_flows$international_marine_bunkers,
                                                  IEATools::tfc_compare_flows$statistical_differences,
                                                  IEATools::tpes_flows$stock_changes),
                              neg_supply_in_fd = c(IEATools::tpes_flows$exports,
                                                   IEATools::tpes_flows$international_aviation_bunkers,
                                                   IEATools::tpes_flows$international_marine_bunkers,
                                                   IEATools::tpes_flows$stock_changes,
                                                   IEATools::tfc_compare_flows$losses,
                                                   IEATools::tfc_compare_flows$statistical_differences),
                              # Output column
                              matnames = IEATools::mat_meta_cols$matnames,
                              # Output identifiers for
                              # use matrix excluding EIOU (U_feed),
                              # use matrix energy industry own use items (U_EIOU),
                              # make (V), and
                              # final demand (Y)
                              # matrices.
                              R = IEATools::psut_cols$R, 
                              U_feed = IEATools::psut_cols$U_feed, 
                              U_EIOU = IEATools::psut_cols$U_eiou,
                              V = IEATools::psut_cols$V, 
                              Y = IEATools::psut_cols$Y){
  
  # If the matrix names column already exist in the .tidy_iea_df, 
  # then the function should not perform any operation.
  if (matnames %in% colnames(.tidy_iea_df)){
    return(.tidy_iea_df)
  }
  
  .tidy_iea_df %>%
    dplyr::mutate(
      "{matnames}" := dplyr::case_when(
        # Positive resources items only belong in the resources (R) matrix.
        (! R_includes_all_exogenous_flows) & starts_with_any_of(.data[[flow]], resources) & .data[[e_dot]] > 0 ~ R,
        # All positive exogenous flows belong in the resources (R) matrix.
        R_includes_all_exogenous_flows & starts_with_any_of(.data[[flow]], pos_supply_in_R) & .data[[e_dot]] > 0 ~ R, 
        # All other positive values on the Supply side of the ledger belong in the make (V) matrix.
        .data[[ledger_side]] == supply & .data[[e_dot]] > 0 ~ V,
        # All Consumption items belong in the final demand (Y) matrix.
        .data[[ledger_side]] == consumption ~ Y,
        # Negative values on the supply side of the ledger with Flow == "Energy industry own use"
        # are placed in the U_EIOU matrix
        .data[[ledger_side]] == supply & .data[[e_dot]] <= 0 & .data[[flow_aggregation_point]] == eiou ~ U_EIOU,
        # Negative values on the supply side that have Flow %in% neg_supply_in_fd go in the final demand matrix
        .data[[ledger_side]] == supply & .data[[e_dot]] <= 0 & starts_with_any_of(.data[[flow]], neg_supply_in_fd) ~ Y,
        # All other negative values on the Supply side of the ledger belong in the use matrix
        # that excludes EIOU (U_feed).
        .data[[ledger_side]] == supply & .data[[e_dot]] <= 0 ~ U_feed,
        # Identify any places where our logic is faulty.
        TRUE ~ NA_character_
      )
    )
}


#' Add row, column, row type, and column type metadata
#' 
#' After calling `add_psut_matnames()`, call this function
#' to add `rownames`, `colnames`, `rowtypes`, and `coltypes` columns to `.tidy_iea_df`.
#' 
#' If `.tidy_iea_df` already contains all of `rownames`, `colnames`, `rowtypes`, and `coltypes`, 
#' `.tidy_iea_df` is returned without modification.
#' If `.tidy_iea_df` contains some but not all of `rownames`, `colnames`, `rowtypes`, or `coltypes`, 
#' an error is returned.
#'
#' @param .tidy_iea_df a data frame containing column `matnames`
#' @param flow,product See `IEATools::iea_cols`.
#' @param matnames the name of the column in `.tidy_iea_df` that contains names of matrices
#'        (a string).  Default is "matnames".
#' @param R,U,U_EIOU,V,Y,B See `IEATools::psut_cols`.
#' @param industry_type,product_type,sector_type,resource_type See `IEATools::row_col_types`.
#' @param rownames,colnames,rowtypes,coltypes See `IEATools::mat_meta_cols`.
#'
#' @return `.tidy_iea_df` with additional columns named
#'         `rowname`, `colname`,
#'         `rowtype`, and `coltype`.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' load_tidy_iea_df() %>% 
#'   add_psut_matnames() %>%
#'   add_row_col_meta()
add_row_col_meta <- function(.tidy_iea_df,
                             # Column names for Product and Flow
                             product = IEATools::iea_cols$product, 
                             flow = IEATools::iea_cols$flow,
                             # Name of the input column containing matrix names
                             matnames = IEATools::mat_meta_cols$matnames,
                             # Expected matrix names in the matnames column
                             U = IEATools::psut_cols$U,
                             U_EIOU = IEATools::psut_cols$U_eiou,
                             R = IEATools::psut_cols$R, 
                             V = IEATools::psut_cols$V,
                             Y = IEATools::psut_cols$Y,
                             B = IEATools::psut_cols$B,
                             # Row and column Type identifiers
                             industry_type = IEATools::row_col_types$industry,
                             product_type = IEATools::row_col_types$product, 
                             sector_type = IEATools::row_col_types$sector, 
                             resource_type = IEATools::row_col_types$resource,
                             # Output columns
                             rownames = IEATools::mat_meta_cols$rownames, 
                             colnames = IEATools::mat_meta_cols$colnames,
                             rowtypes = IEATools::mat_meta_cols$rowtypes, 
                             coltypes = IEATools::mat_meta_cols$coltypes){
  
  # If all of rownames, colnames, rowtypes, and coltypes are in the column names, then don't do anything.
  if (all(c(rownames, colnames, rowtypes, coltypes) %in% colnames(.tidy_iea_df))){
    return(.tidy_iea_df)
  }
  
  # Else, do everything as planned before.
  matsindf::verify_cols_missing(.tidy_iea_df, c(rownames, colnames, rowtypes, coltypes))
  
  .tidy_iea_df %>%
    dplyr::mutate(
      "{rownames}" := dplyr::case_when(
        startsWith(.data[[matnames]], U) ~ .data[[product]],
        .data[[matnames]] == R ~ .data[[flow]],
        .data[[matnames]] == V ~ .data[[flow]],
        .data[[matnames]] == Y ~ .data[[product]],
        .data[[matnames]] == B ~ .data[[product]],
        TRUE ~ NA_character_
      ),
      "{colnames}" := dplyr::case_when(
        startsWith(.data[[matnames]], U) ~ .data[[flow]],
        .data[[matnames]] == V ~ .data[[product]],
        .data[[matnames]] == R ~ .data[[product]],
        .data[[matnames]] == Y ~ .data[[flow]],
        .data[[matnames]] == B ~ .data[[flow]],
        TRUE ~ NA_character_
      ),
      "{rowtypes}" := dplyr::case_when(
        startsWith(.data[[matnames]], U) ~ product_type,
        .data[[matnames]] == R ~ resource_type,
        .data[[matnames]] == V ~ industry_type,
        .data[[matnames]] == Y ~ product_type,
        .data[[matnames]] == B ~ product_type,
        TRUE ~ NA_character_
      ),
      "{coltypes}" := dplyr::case_when(
        startsWith(.data[[matnames]], U) ~ industry_type,
        .data[[matnames]] == R ~ product_type,
        .data[[matnames]] == V ~ product_type,
        .data[[matnames]] == Y ~ sector_type,
        .data[[matnames]] == B ~ sector_type,
        TRUE ~ NA_character_
      )
    )
}


#' Collapse a tidy data frame of IEA data to a tidy PSUT data frame
#' 
#' Call this function after calling `add_row_col_meta()``
#' to collapse `.tidy_iea_df` into a tidy PSUT data frame. 
#' 
#' This function ensures that all energy flow numbers are positive
#' before creating the matrices.
#' 
#' Note that the `.tidy_iea_df` is ungrouped using the function 
#' `dplyr::ungroup()` prior to undergoing any modification.
#'
#' @param .tidy_iea_df a data frame containing `matnames` and several other columns
#' @param matrix_class The type of matrix to be created, one of "matrix" or "Matrix".
#'                     Default is "matrix".
#' @param ledger_side,flow_aggregation_point,flow,product,e_dot,unit See `IEATools::iea_cols`.
#' @param matnames,rownames,colnames,rowtypes,coltypes See `IEATools::mat_meta_cols`.
#' @param matvals See `IEATools::psut_cols`.
#' @param B Name of the Balancing matrix. See `IEATools::psut_cols`.
#'
#' @return `.tidy_iea_df` with all values converted to matrices in the `matvals` column
#' 
#' @export
#'
#' @examples
#' load_tidy_iea_df() %>% 
#'   specify_all() %>% 
#'   add_psut_matnames() %>% 
#'   add_row_col_meta() %>% 
#'   collapse_to_tidy_psut()
collapse_to_tidy_psut <- function(.tidy_iea_df,
                                  matrix_class = c("matrix", "Matrix"),
                                  # Names of input columns
                                  ledger_side = IEATools::iea_cols$ledger_side,
                                  flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point, 
                                  flow = IEATools::iea_cols$flow,
                                  product = IEATools::iea_cols$product,
                                  e_dot = IEATools::iea_cols$e_dot,
                                  unit = IEATools::iea_cols$unit, 
                                  B = IEATools::psut_cols$B,
                                  matnames = IEATools::mat_meta_cols$matnames,
                                  rownames = IEATools::mat_meta_cols$rownames,
                                  colnames = IEATools::mat_meta_cols$colnames,
                                  rowtypes = IEATools::mat_meta_cols$rowtypes, 
                                  coltypes = IEATools::mat_meta_cols$coltypes, 
                                  # Name of output column of matrices
                                  matvals = IEATools::psut_cols$matvals){
  
  matrix_class <- match.arg(matrix_class)
  
  matsindf::verify_cols_missing(.tidy_iea_df, matvals)
  
  .tidy_iea_df %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      # All values in the matrices must be positive, but for Balancing matrix terms.
      "{e_dot}" := dplyr::case_when(
        .data[[matnames]] == B ~ .data[[e_dot]],
        TRUE ~ abs(.data[[e_dot]])
      )
    ) %>%
    dplyr::mutate(
      # Eliminate columns that we no longer need.
      # Set to NULL in mutate, because if the columns are missing, 
      # perhaps because the caller already deleted them,
      # no errors are given.
      "{ledger_side}" := NULL,
      "{flow_aggregation_point}" := NULL,
      "{unit}" := NULL,
      "{flow}" := NULL,
      "{product}" := NULL
    ) %>% 
    # We assume that everything remaining is a metadata column.
    matsindf::group_by_everything_except(e_dot, rownames, colnames, rowtypes, coltypes) %>% 
    # Now we can collapse!
    matsindf::collapse_to_matrices(matnames = matnames, matvals = e_dot,
                                   rownames = rownames, colnames = colnames,
                                   rowtypes = rowtypes, coltypes = coltypes, 
                                   matrix_class = matrix_class) %>%
    dplyr::rename(
      # "{matvals}" := .data[[e_dot]]
      "{matvals}" := dplyr::all_of(e_dot)
    ) %>% 
    dplyr::ungroup()
}


#' Fill missing **U** and **V** matrices
#' 
#' In some cases (e.g., bunkers where `Last.stage` is "final"),
#' **U**, **U_feed**, **U_EIOU**, and **V** matrices can be missing, because
#' Imports (which appear in the **R** matrix) are consumed directly 
#' in the final demand (**Y**) matrix, 
#' without any intermediate processing.
#' When a data frame is pivoted wider by matrices, 
#' the **U_feed**, **U_EIOU**, **r_EIOU**, and **V** columns 
#' will contain `NULL` entries.
#' This function fills those `NULL` entries with reasonable defaults.
#' 
#' Reasonable defaults are obtained by introducing one new
#' industry for each product in the **R** and **Y** matrices.
#' The new industry is called 
#' "Manufacture \[of XYZ\]", where "XYZ" is the name of the product.
#' The new "Manufacture" industries have 100% efficiency.
#'
#' @param .sutmats A data frame of metadata columns and named matrix columns
#' @param R,U_feed,U_eiou,U,r_eiou,V,Y See `IEATools::psutcols`. 
#'                                     Default values are names for variables 
#'                                     incoming with `.sutmats`. 
#'                                     Can be overridden with actual matrices.
#' @param .R_temp_name,.U_temp_name,.U_feed_temp_name,.U_eiou_temp_name,.r_eiou_temp_name,.V_temp_name Names of temporary variables unused internally to the function.
#' @param R_name,U_name,U_feed_name,U_eiou_name,r_eiou_name,V_name See `IEATools::psutcols`. 
#'                                                                 The final names 
#'                                                                 for matrices in the output.
#'
#' @return A version of `.sutmats` 
#'         with **R**, **U**, **U_feed**, **U_EIOU**, or **V**
#'         filled with **0** matrices if they were missing.
#' 
#' @export
#'
#' @examples
#' # Set up a PSUT data frame with NULL for
#' # U_feed, U_EIOU, and V in 1971 for GHA.
#'   R <- matrix(c(10, 0, 
#'                 0, 200), nrow = 2, ncol = 2, byrow = TRUE, 
#'               dimnames = list(c("Imports [of Fuel oil]", 
#'                                 "Imports [of Aviation gasoline]"), 
#'                               c("Fuel oil", "Aviation gasoline")))
#'   Y <- matrix(c(10, 0, 
#'                 0, 200), nrow = 2, ncol = 2, byrow = TRUE, 
#'               dimnames = list(c( "Fuel oil", "Aviation gasoline"), 
#'                               c("International marine bunkers", 
#'                                 "International aviation bunkers")))
#' # Replace the missing **U** and **V** matrices.
#' replace_null_UV(R = R, Y = Y)
replace_null_UV <- function(.sutmats = NULL,
                            R = IEATools::psut_cols$R,
                            U_feed = IEATools::psut_cols$U_feed, 
                            U_eiou = IEATools::psut_cols$U_eiou,
                            U = IEATools::psut_cols$U,
                            r_eiou = IEATools::psut_cols$r_eiou,
                            V = IEATools::psut_cols$V, 
                            Y = IEATools::psut_cols$Y,
                            manufacture = IEATools::transformation_processes$manufacture, 
                            manufacture_notation = RCLabels::of_notation,
                            resources_ind_notation = RCLabels::of_notation,
                            raw_product_notation = RCLabels::from_notation, 
                            .R_temp_name = ".R_temp", 
                            .U_temp_name = ".U_temp", 
                            .U_feed_temp_name = ".U_feed_temp", 
                            .U_eiou_temp_name = ".U_EIOU_temp", 
                            .r_eiou_temp_name = ".r_EIOU_temp",
                            .V_temp_name = ".V_temp", 
                            R_name = IEATools::psut_cols$R, 
                            U_name = IEATools::psut_cols$U, 
                            U_feed_name = IEATools::psut_cols$U_feed, 
                            U_eiou_name = IEATools::psut_cols$U_eiou, 
                            r_eiou_name = IEATools::psut_cols$r_eiou, 
                            V_name = IEATools::psut_cols$V, 
                            tol = 1e-6) {
  
  # Set default argument values to NULL so that missing and NULL look the same.
  fix_RUV_func <- function(R_mat = NULL,
                           U_mat = NULL, U_feed_mat = NULL, U_eiou_mat = NULL, r_eiou_mat = NULL, 
                           V_mat = NULL, Y_mat = NULL) {
    # Strategy is to assign the matrices to a temporary name. 
    # After using matsindf_apply, swap to the actual name.
    # This step is necessary, because matsindf_apply() does not allow renaming columns 
    # (for good reason!).
    
    if (is.null(U_mat) & is.null(V_mat)) {
      # No intermediate processing.
      
      # Test that energy is conserved for each product originally
      R_mat |> 
        matsbyname::transpose_byname() |> 
        matsbyname::rowsums_byname(colname = "rowsums") |> 
        matsbyname::difference_byname(Y_mat |> 
                                        matsbyname::rowsums_byname(colname = "rowsums")) |> 
        matsbyname::iszero_byname(tol = tol) |> 
        assertthat::assert_that(msg = "Energy is not balanced originally in replace_null_UV()")
      
      # Get original product and industry names
      prod_names_orig <- colnames(R_mat)
      ind_names_orig <- rownames(R_mat)

      # Define new product names
      new_product_names <- ind_names_orig |> 
        RCLabels::switch_notation(from = resources_ind_notation, 
                                  to = raw_product_notation, 
                                  flip = TRUE)
      # Define manufacture industries
      new_manufacture_industries <- RCLabels::paste_pref_suff(pref = manufacture, 
                                                              suff = prod_names_orig, 
                                                              notation = manufacture_notation)
      
      # Make new R matrix
      R_new <- R_mat |> 
        matsbyname::setcolnames_byname(new_product_names)
      
      # Make new V matrix
      V_new <- R_mat |> 
        matsbyname::setrownames_byname(new_manufacture_industries)
      
      # Make new U matrices
      U_new <- V_new |> 
        matsbyname::transpose_byname() |> 
        matsbyname::setrownames_byname(new_product_names)
      U_feed_new <- U_new
      U_eiou_new <- matsbyname::hadamardproduct_byname(U_new, 0)
      r_eiou_new <- U_eiou_new
      
      # Verify that energy is still conserved
      matsbyname::sum_byname(R_new, V_new) |> 
        matsbyname::transpose_byname() |> 
        matsbyname::rowsums_byname(colname = "rowsums") |> 
        matsbyname::difference_byname(matsbyname::sum_byname(U_new, Y_mat) |> 
                                        matsbyname::rowsums_byname(colname = "rowsums")) |> 
        matsbyname::iszero_byname(tol = tol) |> 
        assertthat::assert_that()
    } else if (is.null(U_eiou_mat)) {
      # In this case, there is no EIOU. 
      # Set a reasonable default for U_EIOU, 
      # namely a zeroed-out U matrix.
      R_new <- R_mat
      U_new <- U_mat
      U_feed_new <- U_feed_mat
      U_eiou_new <- matsbyname::hadamardproduct_byname(U_mat, 0)
      r_eiou_new <- U_eiou_new
      V_new <- V_mat
    } else {
      # Nothing to be done
      R_new <- R_mat
      U_new <- U_mat
      U_feed_new <- U_feed_mat
      U_eiou_new <- U_eiou_mat
      r_eiou_new <- r_eiou_mat
      V_new <- V_mat
    }

    list(R_new, U_new, U_feed_new, U_eiou_new, r_eiou_new, V_new) %>% 
      magrittr::set_names(c(.R_temp_name, 
                            .U_temp_name, .U_feed_temp_name, 
                            .U_eiou_temp_name, .r_eiou_temp_name,
                            .V_temp_name))
  }
  
  out <- matsindf::matsindf_apply(.sutmats, FUN = fix_RUV_func, 
                                  R_mat = R,
                                  U_mat = U, U_feed_mat = U_feed,
                                  U_eiou_mat = U_eiou, r_eiou_mat = r_eiou,
                                  V_mat = V, Y_mat = Y)
  
  # Delete the previous items in a way that will work for both lists and data frames
  out[[R_name]]      <- NULL
  out[[U_name]]      <- NULL
  out[[U_feed_name]] <- NULL
  out[[U_eiou_name]] <- NULL
  out[[r_eiou_name]] <- NULL
  out[[V_name]]      <- NULL
  
  # Rename the temporary item to the actual name
  names(out)[names(out) == .R_temp_name]      <- R_name
  names(out)[names(out) == .U_temp_name]      <- U_name
  names(out)[names(out) == .U_feed_temp_name] <- U_feed_name
  names(out)[names(out) == .U_eiou_temp_name] <- U_eiou_name
  names(out)[names(out) == .r_eiou_temp_name] <- r_eiou_name
  names(out)[names(out) == .V_temp_name]      <- V_name
  
  return(out)
  
}


#' Prepare for PSUT analysis
#' 
#' Converts a tidy IEA data frame into a PSUT data frame
#' by collapsing the IEA data into PSUT matrices (`R`, `U`, `V`, `Y`, and `S_units`).
#' 
#' This function bundles several others:
#' 1. `add_psut_matnames()`
#' 2. `add_row_col_meta()`
#' 3. `collapse_to_tidy_psut()`
#' 4. `replace_null_UV()`
#' 
#' Furthermore, it extracts `S_units` matrices using `extract_S_units_from_tidy()`
#' and adds those matrices to the data frame.
#' 
#' If `.tidy_iea_df` is a zero-row data frame, 
#' the return value is a zero-row data frame with expected columns.
#'
#' @param .tidy_iea_df a tidy data frame that has been specified with `specify_all()`.
#' @param matrix_class The type of matrix to be created, one of "matrix" or "Matrix".
#'                     Default is "matrix".
#' @param year,ledger_side,flow_aggregation_point,flow,product,e_dot,unit See `IEATools::iea_cols`.
#' @param supply,consumption See `IEATools::ledger_sides`.
#' @param matnames,rownames,colnames,rowtypes,coltypes See `IEATools::mat_meta_cols`.
#' @param matvals,R,U_eiou,U_feed,U,r_eiou,V,Y,s_units,B See `IEATools::psut_cols`.
#'
#' @return A wide-by-matrix data frame with metadata columns and columns named for each type of matrix.
#' 
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#' Simple <- load_tidy_iea_df() %>% 
#'   specify_all() %>% 
#'   prep_psut() %>% 
#'   pivot_longer(cols = c(R, U_EIOU, U_feed, V, Y, S_units), 
#'                names_to = "matnames",
#'                values_to = "matval_simple")
#' S_units <- load_tidy_iea_df() %>% 
#'   specify_all() %>% 
#'   extract_S_units_from_tidy()
#' Complicated <- load_tidy_iea_df() %>% 
#'   specify_all() %>% 
#'   add_psut_matnames() %>% 
#'   add_row_col_meta() %>% 
#'   collapse_to_tidy_psut() %>% 
#'   spread(key = matnames, value = matvals) %>% 
#'   replace_null_UV() %>% 
#'   full_join(S_units, by = c("Method", "EnergyType", "LastStage", 
#'                             "Country", "Year")) %>% 
#'   gather(key = matnames, value = matvals, R, U_EIOU, U_feed, 
#'                                         V, Y, S_units) %>% 
#'   rename(matval_complicated = matvals)
#' # Simple and Complicated are same.
#' full_join(Simple, Complicated, by = c("Method", "EnergyType", 
#'                                       "LastStage", "Country", 
#'                                       "Year", "matnames")) %>% 
#'   dplyr::mutate(
#'     same = matsbyname::equal_byname(matval_simple, matval_complicated)
#'   ) %>% 
#'   magrittr::extract2("same") %>% 
#'   as.logical() %>% 
#'   all()
prep_psut <- function(.tidy_iea_df, 
                      matrix_class = c("matrix", "Matrix"),
                      year = IEATools::iea_cols$year,
                      ledger_side = IEATools::iea_cols$ledger_side, 
                      flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                      flow = IEATools::iea_cols$flow, 
                      product = IEATools::iea_cols$product, 
                      e_dot = IEATools::iea_cols$e_dot,
                      unit = IEATools::iea_cols$unit, 
                      supply = IEATools::ledger_sides$supply, 
                      consumption = IEATools::ledger_sides$consumption, 
                      matnames = IEATools::mat_meta_cols$matnames,
                      rownames = IEATools::mat_meta_cols$rownames, 
                      colnames = IEATools::mat_meta_cols$colnames, 
                      rowtypes = IEATools::mat_meta_cols$rowtypes, 
                      coltypes = IEATools::mat_meta_cols$coltypes,
                      matvals = IEATools::psut_cols$matvals, 
                      R = IEATools::psut_cols$R,
                      U_eiou = IEATools::psut_cols$U_eiou,
                      U_feed = IEATools::psut_cols$U_feed,
                      r_eiou = IEATools::psut_cols$r_eiou,
                      U = IEATools::psut_cols$U,
                      V = IEATools::psut_cols$V,
                      Y = IEATools::psut_cols$Y,
                      B = IEATools::psut_cols$B,
                      s_units = IEATools::psut_cols$s_units){
  
  matrix_class <- match.arg(matrix_class)
  
  if (nrow(.tidy_iea_df) == 0) {
    # We can get a no-row data frame for .tidy_iea_df. 
    # If so, we should return a no-row data frame with empty columns added.
    meta_columns <- meta_cols(.tidy_iea_df, 
                              return_names = TRUE, 
                              not_meta = c(ledger_side, flow_aggregation_point, flow, product, e_dot, unit))
    out <- .tidy_iea_df %>% 
      dplyr::select(dplyr::all_of(c(meta_columns, year)))
    # Make a tibble with no rows for the remainder of the columns, 
    # R, U_eiou, U_feed, V, Y, S_units (6 in total)
    # Use 1.1 for the value so that columns are created as double type columns.
    mats_cols <- data.frame(rep(list(double()), 8)) |> 
      magrittr::set_names(c(R, U, U_eiou, U_feed, r_eiou, V, Y, s_units)) |> 
      tibble::as_tibble() |> 
      dplyr::mutate(dplyr::across(dplyr::any_of(c(R, U, U_eiou, U_feed, r_eiou,V, Y, s_units)), as.list))
    # Join to out
    return(dplyr::bind_cols(out, mats_cols))
  } 
  
  # We actually have some rows in .tidy_iea_df, so work with them
  S_units <- extract_S_units_from_tidy(.tidy_iea_df, 
                                       matrix_class = matrix_class,
                                       product = product, 
                                       unit = unit)
  # Bundle functions together
  Collapsed <- .tidy_iea_df %>% 
    # Add matrix names
    add_psut_matnames(ledger_side = ledger_side, supply = supply, consumption = consumption) %>% 
    # Add additional metadata
    add_row_col_meta(flow = flow, product = product, matnames = matnames) %>% 
    # Now collapse to matrices
    collapse_to_tidy_psut(matrix_class = matrix_class, e_dot = e_dot, matnames = matnames, matvals = matvals, 
                          rownames = rownames, colnames = colnames,
                          rowtypes = rowtypes, coltypes = coltypes) 
  # Get a list of matrix names for future use
  matrix_names <- Collapsed[[matnames]] %>%
    unique() %>%
    # We add U and r_eiou later, so append them here.
    append(U) %>%
    append(r_eiou)
  # Spread to put each matrix into its own column
  CollapsedSpread <- Collapsed %>% 
    tidyr::spread(key = matnames, value = matvals)
  # There may be cases where U_feed or U_eiou matrices are absent.
  # For example, World marine bunkers where final energy is the last stage 
  # will have imports of fuel (V matrix) and final demand (Y matrix),
  # but no resources creating the fuel (R matrix) or use of fuel (U matrix).
  # In other cases (BEN, GIB, MUS, NAM), the U_feed is present, but
  # U_EIOU matrices are missing.
  # So we check for the presence of U_feed or U_EIOU, as appropriate,
  # before creating the U or r_eiou matrices.
  # ---Matthew Kuperus Heun, 9 Nov 2021
  if (U_feed %in% names(CollapsedSpread) & !(U_eiou %in% names(CollapsedSpread))) {
    # With no U_eiou matrix, we simply set U equal to U_feed
    CollapsedSpread <- CollapsedSpread %>% 
      dplyr::mutate(
        # Add the U matrices.
        "{U}" := .data[[U_feed]], 
        # Add r_EIOU matrices
        # Create r_EIOU, a matrix that identifies the ratio of EIOU to total energy used.
        "{r_eiou}" := matsbyname::quotient_byname(.data[[U]], .data[[U]]) %>% 
          matsbyname::replaceNaN_byname(val = 0)
      )
  } else if (U_feed %in% names(CollapsedSpread) & U_eiou %in% names(CollapsedSpread)) {
    # Add the U matrix to the data frame
    CollapsedSpread <- CollapsedSpread %>% 
      dplyr::mutate(
        # Add the U matrix.
        "{U}" := matsbyname::sum_byname(.data[[U_feed]], .data[[U_eiou]]), 
        # Add r_EIOU matrices
        # Create r_EIOU, a matrix that identifies the ratio of EIOU to total energy used.
        "{r_eiou}" := matsbyname::quotient_byname(.data[[U_eiou]], .data[[U]]) %>% 
          matsbyname::replaceNaN_byname(val = 0)
      ) %>% 
      # Rearrange columns to get more-natural locations for the U and r_EIOU matrices.
      # dplyr::relocate(.data[[U]], .after = .data[[U_feed]]) %>% 
      # dplyr::relocate(.data[[r_eiou]], .after = .data[[U]])
      dplyr::relocate(dplyr::all_of(U), .after = dplyr::all_of(U_feed)) %>% 
      dplyr::relocate(dplyr::all_of(r_eiou), .after = dplyr::all_of(U))
  }
  
  CollapsedSpread %>% 
    # Add the S_units matrix and return
    dplyr::full_join(S_units, by = matsindf::everything_except(CollapsedSpread, matrix_names, .symbols = FALSE)) %>% 
    # Add R and U matrices (0 matrices) if R or any of the U matrices are missing
    # in a row of the data frame.
    replace_null_UV(R = R, U_feed = U_feed, U_eiou = U_eiou, r_eiou = r_eiou, U = U, V = V, Y = Y)
}
