


#' Create allocation matrices (`C`) from an allocation table
#' 
#' This function uses information in a filled allocation template (created by `write_fu_allocation_template()`)
#' to create allocation matrices (`C`). 
#' 
#' rownames of the `C` matrices are taken from the `Ef.product` and `Destination` columns of `.fu_allocation_table`
#' and have the form "`Ef.product` `r RCLabels::arrow_notation[["pref_end"]]` `Destination`".
#' colnames of the `C` matrices are taken from the `Machine` and `Eu.product` columns of `.fu_allocation_table`
#' and have the form "machine `r RCLabels::arrow_notation[["pref_end"]]` useful energy form".
#' 
#' `C` matrices are created for both energy industry own use
#' and final demand (`C_eiou` and `C_Y`, respectively).
#' 
#' Rows of the output `C` matrices should sum to 1.  
#' If there is a problem, a data frame that shows the errors is returned.
#' Such errors probably indicate the FU template was not filled correctly.
#'
#' @param .fu_allocation_table a final-to-useful allocation table read by `load_fu_allocation_data()`.
#'                             A template for this table should have been created by `fu_allocation_table()` and 
#'                             `write_fu_allocation_table()`.
#'                             This object can also be a tidy data frame with year data gathered into a Year column.
#' @param ledger_side,flow_aggregation_point,e_dot,unit,year See `IEATools::iea_cols`.
#' @param supply,consumption See `IEATools::ledger_sides`.
#' @param quantity,machine,ef_product,eu_product,destination,e_dot_perc,maximum_values,C_eiou,C_Y See `IEATools::template_cols`.
#' @param matnames,matvals,rownames,colnames,rowtypes,coltypes See `IEATools::mat_meta_cols`.
#' @param product,industry See `IEATools::row_col_types`.
#' @param notation the notation used for this template. See `RCLabels::notation_vec()`. Default is `RCLabels::arrow_notation`.
#' @param tol the allowable amount by which a row sum in a `C` matrix can be different from 1. Default is 1e-6.
#' @param .should_be_1_vector a temporary column created internally for error checking (and not returned unless there is an error). 
#'                            This column should contain 1 vectors (i.e., vectors filled with 1's).
#' @param .is_1 a temporary column created internally (and not returned unless there is an error)
#'              that contains `TRUE` or `FALSE` depending on whether a rowsum was 1.
#' @param .all_1 a temporary column created internally (and not returned unless there is an error)
#'               that tells whether a 1-vector was created by rowsums.
#'
#' @return a wide-by-matrices data frame with metadata columns (and year) along with columns for 
#'         `C_eiou` and `C_Y` matrices.
#'         If not all rows of a C matrix sum to 1, 
#'         a warning is emitted, and
#'         a data frame is returned which shows the errors.
#'         
#' @export
#'
#' @examples
#' load_fu_allocation_data() %>% 
#'   form_C_mats()
form_C_mats <- function(.fu_allocation_table, 
                        
                        ledger_side = IEATools::iea_cols$ledger_side,
                        flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                        e_dot = IEATools::iea_cols$e_dot,
                        unit = IEATools::iea_cols$unit,
                        year = IEATools::iea_cols$year,
                        
                        supply = IEATools::ledger_sides$supply,
                        consumption = IEATools::ledger_sides$consumption,
                        
                        quantity = IEATools::template_cols$quantity,
                        machine = IEATools::template_cols$machine,
                        ef_product = IEATools::template_cols$ef_product,
                        eu_product = IEATools::template_cols$eu_product,
                        destination = IEATools::template_cols$destination,
                        e_dot_perc = IEATools::template_cols$e_dot_perc,
                        maximum_values = IEATools::template_cols$maximum_values,
                        
                        matnames = IEATools::mat_meta_cols$matnames,
                        matvals  = IEATools::mat_meta_cols$matvals,
                        rownames = IEATools::mat_meta_cols$rownames,
                        colnames = IEATools::mat_meta_cols$colnames,
                        rowtypes = IEATools::mat_meta_cols$rowtypes,
                        coltypes = IEATools::mat_meta_cols$coltypes,
                        
                        product = IEATools::row_col_types$product,
                        industry = IEATools::row_col_types$industry,
                        
                        notation = RCLabels::arrow_notation,

                        tol = 1e-6,
                        
                        # Names of output matrices
                        C_eiou = IEATools::template_cols$C_eiou,
                        C_Y = IEATools::template_cols$C_Y,
                        
                        # Temporary column names
                        .should_be_1_vector = ".should_be_1_vector", 
                        .is_1 = ".is_1", 
                        .all_1 = ".all_1") {
  
  cleaned <- .fu_allocation_table %>% 
    # Eliminate rows titled e_dot or e_dot_perc. These are just helper rows for the analyst.
    dplyr::filter(! (.data[[quantity]] %in% c(e_dot, e_dot_perc)) ) %>% 
    dplyr::rename(
      # We will eventually put matrix names in the ledger_side column.
      "{matnames}" := ledger_side
    ) %>% 
    dplyr::mutate(
      # Change the values in the matnames column to reflect which C matrix will be constructed.
      # C_Y is for moving final demand to the useful stage.
      # C_EIOU is for moving energy industry own use to the final stage.
      "{matnames}" := dplyr::case_when(
        # When we have ledger_side of EIOU, we want to make the C_EIOU matrix.
        .data[[matnames]] == supply ~ C_eiou,
        # When we have ledger_side of any of the tfc_flows, we want to make the C_Y matrix.
        .data[[matnames]] == consumption ~ C_Y,
        # Catch any logic errors here.
        TRUE ~ NA_character_
      ),
      # Eliminate the unit column (if it exists), as it has no meaning for allocation fractions.
      "{unit}" := NULL,
      # Eliminate the maximum_values column. It was only a helper for the analyst.
      "{maximum_values}" := NULL,
      # Eliminate the quantity column. Everything is a C at this point.
      "{quantity}" := NULL, 
      # Eliminate the flow aggregation point column. We don't need it.
      "{flow_aggregation_point}" := NULL
    ) %>% 
    # Get rid of rows where machine and eu_product are NA. No data have been provided in these locations..
    dplyr::filter(! (is.na(.data[[machine]]) & is.na(.data[[eu_product]])) )
  # Gather years into a tidy data frame.
  year_names <- year_cols(cleaned, return_names = TRUE)
  # It could be that the incoming .fu_allocation_table is already gathered (pivoted longer).
  # If so, don't need to pivot longer here.
  # Detect if it is already pivoted longer by whether or not a year column is present.
  # If not, need to gather.
  if (!(year %in% colnames(cleaned))) {
    cleaned <- cleaned %>% 
      # Gather to put years in a column
      tidyr::pivot_longer(cols = year_names, names_to = year, values_to = matvals)  
  }
  gathered <- cleaned %>% 
    # Eliminate rows where C is NA. They came from places where data are not available.
    dplyr::filter(!is.na(.data[[matvals]])) %>% 
    # Make sure the year column is numeric
    dplyr::mutate(
      "{year}" := as.numeric(.data[[year]])
    )
    
  # Prepare for collapsing to matrices by adding row and column names and types.
  prepped <- gathered %>% 
    # Create row and column names.
    dplyr::mutate(
      # Row names come from Ef.product -> Destination for both C_Y and C_EIOU.
      "{rownames}" := RCLabels::paste_pref_suff(pref = .data[[ef_product]], suff = .data[[destination]], notation = notation),
      # Column names come from Machine -> Eu.product for both C_Y and C_EIOU.
      "{colnames}" := RCLabels::paste_pref_suff(pref = .data[[machine]], suff = .data[[eu_product]], notation = notation),
      # Row types are Product -> Industry
      # "{rowtypes}" := product,
      "{rowtypes}" := RCLabels::paste_pref_suff(pref = product, suff = industry, notation = notation),
      # Column types are Industry -> Product
      # "{coltypes}" := industry,
      "{coltypes}" := RCLabels::paste_pref_suff(pref = industry, suff = product, notation = notation),
      # Eliminate columns we no longer need
      "{ef_product}" := NULL,
      "{machine}" := NULL,
      "{eu_product}" := NULL,
      "{destination}" := NULL
    )

  # Group and collapse to C_EIOU and C_Y matrices.
  # In particular, group by matnames so that we create one set of C matrices for EIOU flows and another for consumption flows.
  group_cols <- matsindf::everything_except(prepped, matvals, rownames, colnames, rowtypes, coltypes)
  out <- prepped %>% 
    dplyr::group_by(!!!group_cols) %>% 
    matsindf::collapse_to_matrices(matnames = matnames, matvals  = matvals, 
                                   rownames = rownames, colnames = colnames, 
                                   rowtypes = rowtypes, coltypes = coltypes)
  
  # Check that all rows sum to 1, but only if we had some rows to begin with!
  if (nrow(.fu_allocation_table) > 0) {
    # Verify that all rows sum to 1. If not, there has been a problem somewhere.
    verify <- out %>% 
      dplyr::mutate(
        "{.should_be_1_vector}" := matsbyname::rowsums_byname(.data[[matvals]]),
        "{.is_1}" := matsbyname::difference_byname(.data[[.should_be_1_vector]], 1) %>% 
          matsbyname::abs_byname() %>% 
          matsbyname::compare_byname("<=", tol),
        "{.all_1}" := .data[[.is_1]] %>% matsbyname::all_byname()
      )
    if (!all(verify[[.all_1]] %>% as.logical())) {
      # Not all rows summed to 1. Emit a warning and return debugging information.
      warning("Not all rows in the C matrices sum to 1. Returning a diagnostic data frame from form_C_mats().")
      # Create a problems data frame that we will return instead of out.
      probs <- verify %>% 
        dplyr::mutate(
          # Get rid of some columns
          "{matvals}" := NULL,
          "{.is_1}" := NULL,
          "{.all_1}" := NULL
        ) %>% 
        matsindf::expand_to_tidy(matvals = .should_be_1_vector) %>% 
        # Eliminate some unneeded columns
        dplyr::mutate(
          "{colnames}" := NULL,
          "{rowtypes}" := NULL,
          "{coltypes}" := NULL
        ) %>%
        dplyr::filter(
          abs(.data[[.should_be_1_vector]] - 1) > 1e-6
        )
      return(probs)
    }
  }

  # If we passed the test, we can return the out data frame without the verification columns, 
  # after we pivot wider.
  out %>% 
    tidyr::pivot_wider(names_from = matnames, values_from = matvals)
}


#' Create `eta_fu` and `phi_u` vectors
#' 
#' This function creates vectors from a filled final-to-useful efficiency table
#' created by `write_fu_allocation_template()`.
#' The two vectors are:
#'   * `eta_fu`: a vector of final-to-useful energy efficiencies, and 
#'   * `phi_u`: a vector of useful exergy-to-useful energy ratios.
#' 
#' The vectors `eta_fu` and `phi_u` have special rownames that indicate 
#' sources and types of useful energy flows.
#' Row names in the `eta_fu` vector have the pattern 
#' "industry`r RCLabels::arrow_notation[["pref_end"]]`product" to indicate 
#' the energy efficiency of "industry" for making "product"
#' or the exergy-to-energy ratio of the useful energy form created by a final-to-useful machine.
#' Row names in the `phi_u` vector are named by energy product only.
#' 
#' Each energy product should have the same phi, regardless of machine that produced it.
#' So this function checks whether each combination of metadata
#' produces the same phi values. 
#' If any different phi values are found, 
#' an error is produced. 
#' 
#' Columns in the outgoing data frame are named by the variable in the vector: 
#' `eta_fu` for final-to-useful efficiencies and
#' `phi_u` for useful exergy-to-useful energy ratios.
#' 
#' @param .eta_fu_table a final-to-useful efficiency table read by `load_eta_fu_allocation_data()`.
#'                      A template for this table should have been created by `eta_fu_table()` and 
#'                      `write_eta_fu_table()`.
#' @param unit,year See `IEATools::iea_cols`.
#' @param quantity,machine,eu_product,e_dot_machine,e_dot_machine_perc,maximum_values,eta_fu,phi_u,phi See `IEATools::template_cols`.
#' @param matnames,matvals,rownames,colnames,rowtypes,coltypes See `IEATools::mat_meta_cols`.
#' @param product,industry See `IEATools::row_col_types`.
#' @param arrow_note,from_note Notation vectors used for creating the eta_fu and phi vectors. 
#'                             See `matsbyname::notation_vec()`. 
#'                             Defaults are `RCLabels::arrow_notation` and ``RCLabels::from_notation`, respectively.
#' @param .id The name of an identification column used internally. Default is ".id".
#'
#' @return a wide-by-matrices data frame with metadata columns (and year) 
#'         along with columns for `eta_fu` and `phi_u` vectors.
#' 
#' @export
#'
#' @examples
#' load_eta_fu_data() %>% 
#'   form_eta_fu_phi_u_vecs()
form_eta_fu_phi_u_vecs <- function(.eta_fu_table, 
                                   
                                   unit = IEATools::iea_cols$unit,
                                   year = IEATools::iea_cols$year,
                                   
                                   quantity = IEATools::template_cols$quantity,
                                   machine = IEATools::template_cols$machine,
                                   eu_product = IEATools::template_cols$eu_product,
                                   e_dot_machine = IEATools::template_cols$e_dot_machine,
                                   e_dot_machine_perc = IEATools::template_cols$e_dot_machine_perc,
                                   maximum_values = IEATools::template_cols$maximum_values,
                                   eta_fu = IEATools::template_cols$eta_fu,
                                   phi_u = IEATools::template_cols$phi_u,
                                   phi = IEATools::template_cols$phi,
                                   
                                   matnames = IEATools::mat_meta_cols$matnames,
                                   matvals  = IEATools::mat_meta_cols$matvals,
                                   rownames = IEATools::mat_meta_cols$rownames,
                                   colnames = IEATools::mat_meta_cols$colnames,
                                   rowtypes = IEATools::mat_meta_cols$rowtypes,
                                   coltypes = IEATools::mat_meta_cols$coltypes, 
                                   
                                   product = IEATools::row_col_types$product,
                                   industry = IEATools::row_col_types$industry,
                                   
                                   arrow_note = RCLabels::arrow_notation, 
                                   from_note = RCLabels::from_notation, 
                                   .id = ".id") {
  
  cleaned <- .eta_fu_table %>% 
    # Eliminate rows titled e_dot_machine or e_dot_machine_perc. These are just helper rows for the analyst.
    dplyr::filter(! (.data[[quantity]] %in% c(e_dot_machine, e_dot_machine_perc)) ) %>% 
    dplyr::mutate(
      # Eliminate the unit column (if it exists), as it has no meaning for efficiencies and exergy-to-energy ratios.
      "{unit}" := NULL,
      # Eliminate the maximum_values column. It was only a helper for the analyst.
      "{maximum_values}" := NULL
    ) %>% 
    dplyr::rename(
      # The quantity column gives us the matrix names
      "{matnames}" := quantity
    )
    
  # Gather years into a tidy data frame.
  year_names <- year_cols(cleaned, return_names = TRUE)
  # It could be that the incoming .eta_fu_table is already gathered (pivoted longer).
  # If so, don't need to pivot longer here.
  # Detect if it is already pivoted longer by whether or not a year column is present.
  # If not, need to gather.
  if (!(year %in% colnames(cleaned))) {
    cleaned <- cleaned %>% 
      # Gather to put years in a column
      tidyr::pivot_longer(cols = year_names, names_to = year, values_to = matvals)  
  }
  gathered <- cleaned %>% 
    # Eliminate rows where C is NA. They came from places where data are not available.
    dplyr::filter(!is.na(.data[[matvals]])) %>% 
    # Make sure the year column is numeric
    dplyr::mutate(
      "{year}" := as.numeric(.data[[year]])
    )
  
  prepped <- gathered %>% 
    dplyr::mutate(
      # Create rownames from the machine and eu_product rows.
      "{rownames}" := dplyr::case_when(
        .data[[matnames]] == eta_fu ~ RCLabels::paste_pref_suff(pref = .data[[machine]], suff = .data[[eu_product]], notation = arrow_note),
        .data[[matnames]] == phi_u ~ RCLabels::paste_pref_suff(pref = .data[[eu_product]], suff = .data[[machine]], notation = from_note),
        TRUE ~ NA_character_
      ), 
      # Eliminate machine and eu_product columns, because we no longer need them.
      "{machine}" := NULL,
      "{eu_product}" := NULL,
      # Create colnames according to the name of the matrix to be created
      "{colnames}" := .data[[matnames]],
      "{rowtypes}" := dplyr::case_when(
        .data[[matnames]] == eta_fu ~ RCLabels::paste_pref_suff(pref = industry, suff = product, notation = arrow_note),
        .data[[matnames]] == phi_u ~ RCLabels::paste_pref_suff(pref = product, suff = industry, notation = from_note), 
        TRUE ~ NA_character_
      ), 
      "{coltypes}" := dplyr::case_when(
        .data[[matnames]] == eta_fu ~ eta_fu,
        .data[[matnames]] == phi_u ~ phi,
        TRUE ~ NA_character_
      )
    )
  
  # Check for inconsistencies in the phi values. 
  # For every combination of metadata (country, year, etc.),
  # we expect only one phi value per product, 
  # regardless of the machine that produced it. 
  # Do that check here.
  meta_cols <- prepped %>% 
    IEATools::meta_cols(return_names = TRUE, 
                        not_meta = c(matnames, rownames, colnames, rowtypes, coltypes))
  
  # Find the number of unique row of metadata, product, year, and phi values (matvals)
  unique_with_phi_vals <- prepped %>% 
    dplyr::filter(.data[[colnames]] == phi_u) %>% 
    dplyr::mutate(
      "{product}" := .data[[rownames]] %>% 
        RCLabels::get_pref_suff(which = "pref", notation = from_note)
    ) %>% 
    dplyr::select(meta_cols, product, year) %>% 
    unique()
  
  # Find the number of unique row of metadata, product, and year
  unique_without_phi_vals <- prepped %>% 
    dplyr::filter(.data[[colnames]] == phi_u) %>% 
    dplyr::mutate(
      "{product}" := .data[[rownames]] %>% 
        RCLabels::get_pref_suff(which = "pref", notation = from_note)
    ) %>% 
    dplyr::select(meta_cols, product, year, -matvals) %>% 
    unique()
  
  # We should have the exact same number of rows in each data frame.
  # If not, there is a problem.
  nrow_diff <- nrow(unique_with_phi_vals) - nrow(unique_without_phi_vals)
  
  if (nrow_diff != 0) {
    # There are different phi values for some combinations of 
    # metadata plus year.
    # Craft an error message and throw an error.
    # Add an identifier row number to unique_without_phi_values
    with_id <- unique_without_phi_vals %>% 
      tibble::rowid_to_column(.id)
    err_rows <- dplyr::left_join(unique_with_phi_vals, with_id,
                                 # join by everything but matvals
                                 by = names(unique_with_phi_vals)[-which(names(unique_with_phi_vals) == "matvals")]) %>% 
      dplyr::group_by(.data[[.id]]) %>%
      dplyr:: filter(dplyr::n() > 1)
    err_msg <- paste("Found useful products with different phi values in form_eta_fu_phi_u_vecs(). All phi values should be same for all combinations of metadata. Error(s) in:",
                     matsindf::df_to_msg(err_rows))
    stop(err_msg)
  }
  
  # If we get here, everything is OK.
  # Set the grouping columns for later.
  group_cols <- matsindf::everything_except(prepped, matvals, rownames, colnames, rowtypes, coltypes)
  
  prepped %>% 
    dplyr::mutate(
      "{rownames}" := dplyr::case_when(
        # For the phi_u rows, we want to keep only the product in the name.
        .data[[matnames]] == phi_u ~ .data[[rownames]] %>%
          RCLabels::get_pref_suff(which = "pref", notation = from_note), 
        TRUE ~ .data[[rownames]], 
      ), 
      "{colnames}" := dplyr::case_when(
        .data[[matnames]] == phi_u ~ phi, 
        TRUE ~ .data[[colnames]]
      ),
      "{rowtypes}" := dplyr::case_when(
        .data[[matnames]] == phi_u ~ product, 
        TRUE ~ .data[[rowtypes]]
      )
      
    ) %>% 
    # Eliminate-duplicate phi.u rows
    unique() %>% 
    # Collapse to matrices (actually, column vectors) and return  
    dplyr::group_by(!!!group_cols) %>% 
    matsindf::collapse_to_matrices(matnames = matnames, matvals  = matvals, 
                                   rownames = rownames, colnames = colnames, 
                                   rowtypes = rowtypes, coltypes = coltypes) %>% 
    # pivot wider to the sutmats format
    tidyr::pivot_wider(names_from = matnames, values_from = matvals)
}


#' Move an ECC from final to useful as its last stage
#' 
#' This function uses a matrix method to move 
#' from final energy/exergy 
#' to useful energy/exergy as the last stage of an energy conversion chain.
#' 
#' `.sutdata` or individual matrices are always assumed to have final energy as its last stage.
#' 
#' Internally, this function uses `matsindf::matsindf_apply()` to perform its calculations.
#' If `.sutdata` is `NULL`, and `R`, `U_eiou`, `U_feed`, `U`, `r_eiou`, `V`, and `Y` are individual matrices,
#' the output is a named list of matrices containing new values for 
#' **U_eiou**, **U_feed**, **U**, **r_eiou**, **V**, and **Y** matrices, 
#' named with `.sep` plus `useful` appended to the variable names.
#' 
#' If `.sutdata` is a named list of matrices, 
#' output is a list of matrices with names appended to include `.sep` and `useful`, 
#' where appropriate. 
#' Note that output matrices are appended to the original list supplied to `.sutdata`.
#' 
#' If `.sutdata` is a data frame, arguments
#' `R`, `U_feed`, `U_eiou`, `U`, `r_eiou`, `V`, `Y`, `C_eiou`, `C_Y`, `eta_fu`, and `phi_u` 
#' should all be strings (as the default)
#' identifying which columns in `.sutdata` should be used for each matrix.
#' Output is determined by argument `clean_up_df`. 
#' When `clean_up_df = TRUE` (the default), output will contain a `last_stage` column with either `final` or `useful` indicated.
#' When `clean_up_df = FALSE`, output will contain an unmodified `last_stage` column, 
#' probably containing "Final" in all rows.
#' Columns containing versions of matrices where last stage is "Useful" will have `sep` and `useful` 
#' appended to the column name.
#' `clean_up_df = FALSE` is probably not what is desired and can lead to confusion.
#' `clean_up_df = TRUE` (the default) is recommended.
#' 
#' An energy balance check is performed on the useful matrices. 
#' If the energy balance check fails, a warning is emitted and 
#' additional diagnostic information will appear in the output: `.err` and `.e_bal_ok`.
#' 
#' @param .sutdata A wide-by-matrices data frame of PSUT matrices that represent an energy conversion chain.
#'                 Each row of `.sutdata` should contain the matrices that represent one energy conversion chain.
#'                 Matrices should be in columns identified by their names.
#'                 The last stage of these ECCs should be final (not useful).
#'                 `.sutdata` is likely the result of calling (in sequence)
#'                 `load_tidy_iea_df()`, `specify_all()`, and `prep_psut()`.
#'                 `.sutdata` should also include columns of matrices `C_Y`, `C_eiou`, and `eta_fu`,
#'                 probably created by functions `form_C_mats()` and `form_eta_fu_phi_u_vecs()`.
#'                 `.sutdata` can also be a named list of matrices that forms a store of variables.
#'                 Default is `NULL` to enable use of single matrices, too.
#' @param clean_up_df When `.sutdata` is a data frame, tells whether to `tidyr::pivot_longer()` the result
#'                    and remove no-longer-needed input columns `C_eiou`, `C_Y`, `eta_fu`, and `phi_u`.
#'                    Default is `TRUE`.
#' @param tol The allowable error in energy balances for the outgoing matrices (last stage useful). 
#'            Default is `1e-3`.
#' @param last_stage See `IEATools::iea_cols$last_stage`. 
#' @param final,useful See `IEATools::last_stages`.
#' @param industry_type,product_type See `IEATools::row_col_types`
#' @param R,U_eiou,U_feed,U,r_eiou,V,Y See `IEATools::psut_cols`. 
#'        These should be strings (if `.sutdata` is a data frame or a list)
#'        or individual matrices (if `.sutdata` is `NULL`).
#' @param C_eiou,C_Y,eta_fu,phi_u See `IEATools::template_cols`. 
#'        These should be strings (if `.sutdata` is a data frame or a list)
#'        or individual matrices (if `.sutdata` is `NULL`).
#' @param interface_ind See `IEATools::interface_industries`. Interface industries are kept same from `Y_final` to `Y_useful`.
#' @param losses See `IEATools::tfc_compare_flows`. Losses are kept same from `Y_final` to `Y_useful`.
#' @param stat_diffs See `IEATools::tfc_compare_flows`. Statistical differences are kept same from `Y_final` to `Y_useful`.
#' @param arrow_note,from_note The row and column notation in the `eta_fu` vectors.
#'                             See `RCLabels::notation_vec()`. Defaults is `RCLabels::arrow_notation` and `RCLabels::from_notation`.
#' @param .add_to_U_f An internal matrix name for the a matrix to be added to the U_feed_f matrix 
#'                    to form the useful form of the U_feed matrix. Default is ".add_to_U_f".
#' @param .add_to_U_eiou An internal matrix name for the a matrix to be added to the U_eiou_f matrix 
#'                       to form the useful form of the U_eiou matrix. Default is ".add_to_U_eiou".
#' @param .add_to_V_f An internal matrix name for a matrix to add to the Y_f matrix. Default is ".add_to_V_f".
#' @param .add_to_dest An internal matrix name for a matrix that replaces a previous energy destination. Default is ".repl_dest".
#' @param .err An internal matrix name for calculating energy balance errors. Default is ".err".
#' @param .e_bal_ok An internal column name for assessing whether energy balance is within acceptable tolerances set by the `tol` argument. Default is ".e_bal_OK".
#' @param .sep A separator between matrix names and `final` or `useful` indicators. Default is "_".
#' @param U_eiou_name,U_feed_name,U_name,r_eiou_name,V_name,Y_name See `IEATools::psut_cols`. 
#'        Distinct from `U_feed`,`U_eiou`, `U`, `r_eiou`, `V`, and `Y` (which can be matrices or strings), 
#'        these variables determine the names of these matrices on output.
#'        Default values are taken from `IEATools::psut_cols`. 
#'        Note that `.sep` and `useful` are appended to the strings in `U_eiou_name` ... `Y_name` 
#'        to form the output names. 
#'
#' @return Output depends on input, roughly according to 
#'         conventions in `matsindf::apply()`. 
#'         If `.sutdata` is `NULL` and individual matrices are supplied in 
#'         `U_eiou`, `U_feed`, `U`, `r_eiou`, `V`, and `Y` arguments, 
#'         output is a named list of individual matrices with `.sep` and `useful` appended.
#'         If `.sutdata` is a named list of individual matrices, 
#'         the output is a list of matrices with `.sep` and `final` or `.sep` and `useful` appended
#'         to the names.
#'         If `.sutdata` is a data frame, output will be a data frame of matrices
#'         with column names with with `.sep` and `final` or `.sep` and `useful` appended
#'         to the names (when `gather = FALSE`).
#'         If `.sutdata` is a data frame, output will be a data frame of matrices
#'         with a `last.stage` column containing `final` or `useful` and 
#'         columns named for matrices (when `gather = TRUE`, the default).
#' 
#' @export
#'
#' @examples
#' C_data <- load_fu_allocation_data() %>% 
#'   form_C_mats()
#' eta_fu_data <- load_eta_fu_data() %>% 
#'   form_eta_fu_phi_u_vecs()
#'   m_cols <- eta_fu_data %>% 
#'     IEATools::meta_cols(return_names = TRUE,
#'                         years_to_keep = IEATools::iea_cols$year,
#'                         not_meta = c(IEATools::template_cols$eta_fu,
#'                                      IEATools::template_cols$phi_u))
#' psut_mats <- load_tidy_iea_df() %>% 
#'   specify_all() %>% 
#'   prep_psut() %>% 
#'   dplyr::full_join(C_data, by = m_cols) %>% 
#'   dplyr::full_join(eta_fu_data, by = m_cols)
#' psut_mats %>% 
#'   extend_to_useful() %>% 
#'   head()
extend_to_useful <- function(.sutdata = NULL, 
                             clean_up_df = TRUE, 
                             tol = 1e-3,
                             
                             R = IEATools::psut_cols$R, 
                             U_feed = IEATools::psut_cols$U_feed,
                             U_eiou = IEATools::psut_cols$U_eiou,
                             U = IEATools::psut_cols$U,
                             r_eiou = IEATools::psut_cols$r_eiou,
                             V = IEATools::psut_cols$V, 
                             Y = IEATools::psut_cols$Y, 
                             
                             C_eiou = IEATools::template_cols$C_eiou,
                             C_Y = IEATools::template_cols$C_Y, 
                             eta_fu = IEATools::template_cols$eta_fu,
                             phi_u = IEATools::template_cols$phi_u,
                             
                             last_stage = IEATools::iea_cols$last_stage,
                             final = IEATools::last_stages$final,
                             useful = IEATools::last_stages$useful,
                             
                             industry_type = IEATools::row_col_types$industry, 
                             product_type = IEATools::row_col_types$product,
                             
                             interface_ind = IEATools::interface_industries,
                             losses = IEATools::tfc_compare_flows$losses,
                             stat_diffs = IEATools::tfc_compare_flows$statistical_differences,
                             
                             arrow_note = RCLabels::arrow_notation,
                             from_note = RCLabels::from_notation,

                             .add_to_U_f = ".add_to_U_f",
                             .add_to_U_eiou = ".add_to_U_eiou",
                             .add_to_V_f = ".add_to_V_f",
                             .add_to_dest = ".repl_dest",
                             .err = ".err", 
                             .e_bal_ok = ".e_bal_ok",
                             .sep = "_", 

                             U_feed_name = IEATools::psut_cols$U_feed,
                             U_eiou_name = IEATools::psut_cols$U_eiou,
                             U_name = IEATools::psut_cols$U,
                             r_eiou_name = IEATools::psut_cols$r_eiou,
                             V_name = IEATools::psut_cols$V, 
                             Y_name = IEATools::psut_cols$Y) {
  
  # New names
  U_feed_useful_name <- paste0(U_feed_name, .sep, useful)
  U_eiou_useful_name <- paste0(U_eiou_name, .sep, useful)
  U_useful_name <- paste0(U_name, .sep, useful)
  r_eiou_useful_name <- paste0(r_eiou_name, .sep, useful)
  V_useful_name <- paste0(V_name, .sep, useful)
  Y_useful_name <- paste0(Y_name, .sep, useful)
  
  extend_func <- function(eta_fu_vector, Y_mat, C_Y_mat, U_feed_mat, V_mat, C_eiou_mat, U_eiou_mat, R_mat) {
    
    # Industries to retain from Y_f to Y_u. 
    # These industries are not allocated to f-u machines, nor are they tracked for useful energy.
    Y_keep_inds <- c(interface_ind, losses, stat_diffs)
    
    # Calculate .eta_fu_hat, which is needed twice below.
    # Doing the calculation here makes it available for other downstream calculations.
    .eta_fu_hat_mat <- eta_fu_vector %>% 
      matsbyname::hatinv_byname(keep = "rownames") %>% 
      # Swap column names from notation (default is arrow notation) to "from" notation.
      # Internally, we use the "from" notation.
      matsbyname::switch_notation_byname(margin = 2, 
                                         from = list(arrow_note), 
                                         to = list(from_note), 
                                         flip = list(TRUE))

    # There are two destinations for final energy: final demand (the Y matrix) and EIOU (the U_EIOU matrix)
    # We take each of these in turn, adjusting the energy conversion chain to account for the fact that 
    # useful energy is now the last stage.
    res_Y <- extend_to_useful_helper(dest_mat = Y_mat, C_mat = C_Y_mat, eta_fu_vec = eta_fu_vector, 
                                     add_to_U = .add_to_U_f, add_to_V = .add_to_V_f, add_to_dest = .add_to_dest)
    U_feed_useful_mat <- matsbyname::sum_byname(U_feed_mat, res_Y[[.add_to_U_f]])
    # At this point, we have no EIOU, so just set U_useful equal to U_feed_useful.
    U_useful_mat <- U_feed_useful_mat
    # Set EIOU-related matrices to the 0 matrix, keeping the correct row and column names.
    U_eiou_useful_mat <- matsbyname::hadamardproduct_byname(U_useful_mat, 0)
    r_eiou_useful_mat <- U_eiou_useful_mat
    V_useful_mat <- matsbyname::sum_byname(V_mat, res_Y[[.add_to_V_f]])
    # We need to keep industries in Y that are interface industries 
    # (exports, stock changes, international marine and aviation bunkers, and 
    # imports, though there won't be any imports in the Y matrix, because imports are in the V matrix).
    # Also keep non-energy flows.
    # None of the interface industries nor the non-energy flows are in the allocation matrix (C), 
    # so we must retain them in the Y matrix.
    Y_keep_mat <- matsbyname::select_cols_byname(Y_mat, 
                                                 retain_pattern = RCLabels::make_or_pattern(Y_keep_inds, 
                                                                                         pattern_type = "leading"))
    Y_useful_mat <- matsbyname::sum_byname(Y_keep_mat, res_Y[[.add_to_dest]])
    
    # Now check to see if we have any EIOU. 
    # If so, make further adjustments to the matrices.
    # If not, no big deal. 
    # We can live with the matrices calculated above.
    if (missing(C_eiou_mat)) {
      C_eiou_mat <- NULL
    }
    if (!is.null(C_eiou_mat)) {
      # We have some EIOU. Calculate modifications to matrices accounting for the EIOU portion of the ECC.
      res_eiou <- extend_to_useful_helper(dest_mat = U_eiou_mat, C_mat = C_eiou_mat, eta_fu_vec = eta_fu_vector, 
                                          add_to_U = .add_to_U_eiou, add_to_V = .add_to_V_f, add_to_dest = .add_to_dest)
      # Add the modifications to the U_feed, U_eiou, U, 
      U_feed_useful_mat <- matsbyname::sum_byname(U_feed_useful_mat, res_eiou[[.add_to_U_eiou]]) 
      U_eiou_useful_mat <- res_eiou[[.add_to_dest]]
      U_useful_mat <- matsbyname::sum_byname(U_feed_useful_mat, U_eiou_useful_mat)
      r_eiou_useful_mat <- matsbyname::quotient_byname(U_eiou_useful_mat, U_useful_mat) %>% 
        matsbyname::replaceNaN_byname(val = 0)
      V_useful_mat <- matsbyname::sum_byname(V_useful_mat, res_eiou[[.add_to_V_f]])
    }
    
    # Check Product energy balances.
    # It would be nice to use Recca::verify_SUT_energy_balance() for this purpose.
    # However, IEATools is (by design) independent of Recca.
    # So we need to do our own energy balance here.
    # Fortunately, energy balance calculations for products are relatively simple.
    # The energy balance for products is given by row sums of (R + V)^T - U - Y, which should all equal 0
    # within acceptable error.
    .err_vec <- matsbyname::sum_byname(R_mat, V_useful_mat) %>%   # R + V
      matsbyname::transpose_byname() %>%                          # (R + V)^T
      matsbyname::difference_byname(U_useful_mat) %>%             # (R + V)^T - U
      matsbyname::difference_byname(Y_useful_mat) %>%             # (R + V)^T - U - Y
      matsbyname::rowsums_byname()
      
    .ebal_ok <- .err_vec %>%
      matsbyname::iszero_byname(tol = tol) %>% 
      as.logical()

    # Return a named list, as required by matsindf_apply()
    out <- list(U_feed_useful_mat, 
                U_eiou_useful_mat, 
                U_useful_mat,
                r_eiou_useful_mat, 
                V_useful_mat, 
                Y_useful_mat, 
                .err_vec, 
                .ebal_ok) %>% 
      magrittr::set_names(c(U_feed_useful_name, 
                            U_eiou_useful_name, 
                            U_useful_name, 
                            r_eiou_useful_name, 
                            V_useful_name, 
                            Y_useful_name, 
                            .err, 
                            .e_bal_ok))
    if (.ebal_ok) {
      # Remove the error and .e_bal_ok items
      out[[.err]] <- NULL
      out[[.e_bal_ok]] <- NULL
    } else {
      # Emit a warning if there is a problem and return the wrong thing.
      warning(paste0("Energy is not balanced to within ", tol, " in IEATools::extend_to_useful(). See columns ", 
                     .err, " and ", .e_bal_ok, " for problems."))
    }
    return(out)
  }

  out <- matsindf::matsindf_apply(.sutdata, FUN = extend_func,
                                  eta_fu_vector = eta_fu, 
                                  Y_mat = Y, 
                                  C_Y_mat = C_Y, 
                                  U_feed_mat = U_feed, 
                                  V_mat = V, 
                                  C_eiou_mat = C_eiou, 
                                  U_eiou_mat = U_eiou, 
                                  R_mat = R)
  
  # Gather (tidyr::pivot_longer) the outgoing data frame, if requested.
  if (is.data.frame(out) & clean_up_df) {
    # Build a data frame with metadata columns and columns that end in sep+useful.
    # That data frame should be able to be added to the bottom of the incoming data frame.
    cols_to_keep <- out %>% 
      matsindf::everything_except(U_feed_name, U_eiou_name, U_name,
                                  r_eiou_name, V_name, Y_name, .symbols = FALSE)
    # We'll need to strip suffixes off column names.
    suff_to_remove <- paste0(.sep, useful)
    useful_df <- out %>% 
      dplyr::select(cols_to_keep) %>% 
      # Change the Last.stage column to Useful
      dplyr::mutate(
        "{last_stage}" := useful
      ) %>% 
      # Strip sep_useful from end of any column names. 
      # Hint obtained from https://stackoverflow.com/questions/45960269/removing-suffix-from-column-names-using-rename-all
      dplyr::rename_with(~ gsub(paste0(suff_to_remove, "$"), "", .x))
    # Bind the final and useful data frames together.
    out <- dplyr::bind_rows(.sutdata, useful_df) %>% 
      # Trim away unneeded columns
      dplyr::mutate(
        "{C_eiou}" := NULL, 
        "{C_Y}" := NULL, 
        "{eta_fu}" := NULL, 
        "{phi_u}" := NULL
      )
  }
  
  return(out)
}


#' A helper function for extending to the useful energy/exergy stage
#' 
#' The helper function is needed, because moving from final to useful energy 
#' occurs for both the final demand matrix (`Y`) and the energy industry own use matrix (`U_eiou`).
#' The calculations are identical, so we factor the calculations into this function.
#'
#' @param .sutdata a data frame containing `dest_mat`, `C_mat`, and `eta_fu_vec` columns for calculations. Default is `NULL`.
#' @param dest_mat a `Y` or `U_eiou` matrix (Product x Industry) that is a destination for final energy in an energy conversion chain
#'                 or the string name of such matrices in `.sutadata`.
#' @param C_mat an allocation matrix (either `Y` or `U_eiou`, both Product x Industry),
#'              indicating the distribution of 
#'              final energy carriers (Products) to 
#'              final-to-useful energy conversion machines (Industries).
#'              `C_mat` should have been created by `form_C_mats()`.
#'              This argument could also be the string name of such matrices in `.sutdata`.
#' @param eta_fu_vec an efficiency column vector indicating the efficiency (column) 
#'                   of final-to-useful energy conversion machines (rows).
#'                   `eta_fu_vec` should have been created by `form_eta_fu_phi_u_vecs()`.
#'                   This argument could also be the string name of such matrices in `.sutdata`.
#' @param product_type a string identifying product row or column types. Default is "`IEATools::row_col_types$product`".
#' @param industry_type a string identifying industry row or column types. Default is "`IEATools::row_col_types$industry`".
#' @param arr_note a row and column name notation vector that indicates a `source -> destination` relationship. 
#'                 `arr_note` is used for the `eta_fu` matrix, among others.
#'                 See `matsbyname::notation_vec()`.
#'                 Default is `RCLabels::arrow_notation`.
#' @param from_note a row and column name notation vector that indicates a `destination [from source]` relationship. 
#'                  `from_note` is used for the columns of some intermediate matrices.
#'                  See `matsbyname::notation_vec()`.
#'                  Default is `RCLabels::from_notation`.
#' @param add_to_U a string name for the matrix to be added to a use matrix. Default is "add_to_U".
#' @param add_to_V a string name for the matrix to be added to a make matrix. Default is "add_to_V".
#' @param add_to_dest a string name for the matrix to replace some entries previous destination matrix. Default is "repl_dest".
#'
#' @return a named list containing three items: 
#'         `add_to_U_f` (a matrix to be added to a use (`U`) matrix),
#'         `add_to_V_f` (a matrix to be added to a make (`V`) matrix), and 
#'         `add_to_dest_mat` (a matrix to replace the destination matrix, typically `Y_f` or `U_eiou`).
extend_to_useful_helper <- function(.sutdata = NULL, 
                                    # Input matrix names
                                    dest_mat, C_mat, eta_fu_vec, 
                                    # Input parameters
                                    product_type = IEATools::row_col_types$product, 
                                    industry_type = IEATools::row_col_types$industry,
                                    arr_note = RCLabels::arrow_notation, 
                                    from_note = RCLabels::from_notation, 
                                    # Output names
                                    add_to_U = "add_to_U", 
                                    add_to_V = "add_to_V", 
                                    add_to_dest = "add_to_dest") {
  
  helper_func <- function(dest_m, C_m, eta_fu_v) {
    #### Step 1 on the "Pushing Y to useful" tab in file "Matrix f->u example calcs.xlsx"
    
    dest_mat_vec <- matsbyname::vectorize_byname(dest_m, notation = arr_note)
    
    # Calculate dest_mat_vec_hat_C, the matrix product of dest_mat_vec_hat and C
    # This matrix is useful in several calculations below. We calculate it once here.
    dest_mat_vec_hat_C <- dest_mat_vec %>%
      # No longer cleaning here, because we may have 0 matrices for bunkers.
      # ---MKH 13 Aug 2021
      # matsbyname::clean_byname() %>%
      matsbyname::hatize_byname(keep = "rownames") %>%
      matsbyname::matrixproduct_byname(C_m)
    
    eta_fu_hat <- matsbyname::hatize_byname(eta_fu_v, keep = "rownames") %>% 
      # Swap column names from arrow notation to paren notation
      matsbyname::switch_notation_byname(margin = 2, from = arr_note, to = from_note, flip = TRUE)
    
    #### Step 2 on the "Pushing Y to useful" tab in file "Matrix f->u example calcs.xlsx"
    
    # Calculate the matrix that should be added to the U_f matrix.
    add_to_U_f_mat <- dest_mat_vec_hat_C %>% 
      # aggregate_to_pref_suff_byname() is superseded.
      # matsbyname::aggregate_to_pref_suff_byname(keep = "pref", margin = 1, notation = arr_note) %>%
      matsbyname::aggregate_pieces_byname(piece = "pref", margin = 1, notation = arr_note) %>%
      matsbyname::clean_byname(margin = 1) %>% 
      # Set column type to industry to match other use matrices.
      matsbyname::setcoltype(industry_type)

    #### Step 3 on the "Pushing Y to useful" tab in file "Matrix f->U example calcs.xlsx"
    
    # Calculate the matrix that should be added to the V_f matrix.
    add_to_V_f_mat <- dest_mat_vec_hat_C %>% 
      matsbyname::colsums_byname(rowname = NULL) %>%
      matsbyname::hatize_byname(keep = "colnames") %>%
      matsbyname::matrixproduct_byname(eta_fu_hat) %>% 
      # Set row and column type to match other make matrices.
      matsbyname::setrowtype(industry_type) %>% 
      matsbyname::setcoltype(product_type)
    
    #### Step 4 on the "Pushing Y to useful" tab in file "Matrix f->U example calcs.xlsx"
    
    # Calculate replacement for the destination matrix (Y_useful instead of Y_f or U_eiou_useful instead of U_eiou)
    add_to_dest_mat <- matsbyname::matrixproduct_byname(dest_mat_vec_hat_C, eta_fu_hat) %>%
      matsbyname::transpose_byname() %>%
      # aggregate_to_pref_suff_byname() is superseded.
      # matsbyname::aggregate_to_pref_suff_byname(keep = "suff", margin = 2, notation = arr_note) %>%
      matsbyname::aggregate_pieces_byname(piece = "suff", margin = 2, notation = arr_note) %>%
      matsbyname::clean_byname() %>% 
      # Set row and column types to match other destination matrices.
      matsbyname::setrowtype(product_type) %>% 
      matsbyname::setcoltype(industry_type)
    
    # Create the outgoing list and set names according to arguments.
    list(add_to_U_f_mat, add_to_V_f_mat, add_to_dest_mat) %>% 
      magrittr::set_names(c(add_to_U, add_to_V, add_to_dest))
  }
  
  matsindf::matsindf_apply(.sutdata, FUN = helper_func, dest_m = dest_mat, C_m = C_mat, eta_fu_v = eta_fu_vec)
}

