


#' Create allocation matrices (`C`) from an allocation table
#' 
#' This function uses information in a filled allocation template (created by `write_fu_allocation_template()`)
#' to create allocation matrices (`C`). 
#' rownames of the `C` matricds are taken from the `Ef.product` and `Destination` columns, and
#' colnames are taken from the `Machine` and `Eu.product` columns.
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
#' @param ledger_side,flow_aggregation_point,e_dot,year See `IEATools::iea_cols`.
#' @param supply,consumption See `IEATools::ledger_sides`.
#' @param quantity,machine,ef_product,eu_product,destination,e_dot_perc,maximum_values,C_eiou,C_Y See `IEATools::template_cols`.
#' @param matnames,matvals,rownames,colnames,rowtypes,coltypes See `IEATools::mat_meta_cols`.
#' @param product,industry See `IEATools::row_col_types`.
#' @param sep The string separator between prefix and suffix of compound row and column names. Default is " -> ".
#'            The default value matches the default value for the `sep` argument of `matsbyname::vectorize_byname()`, because
#'            `matsbyname::vectorize_byname()` will be used for further manipulations.
#' @param tol The allowable amount by which a row sum in a `C` matrix can be different from 1. Default is 1e-6.
#' @param .should_be_1_vector a temporary column created internally for error checking (and not returned unless there is an error). 
#'                            This column should contain 1 vectors (i.e., vectors filled with 1's).
#' @param .is_1 a temporary column created internally (and not returned unless there is an error)
#'              that contains `TRUE` or `FALSE` depending on whether a rowsum was 1.
#' @param .all_1 a temporary column created internally (and not returned unless there is an error)
#'               that tells whether a 1-vector was created by rowsums.
#'
#' @return a tidy data frame with metadata columns (and year) along with `matnames` and `matvals` columns
#'         indicating and containing `C_eiou` and `C_Y` matrices, respectively.
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
                        
                        sep = " -> ",
                        
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
  gathered <- cleaned %>% 
    # Gather to put years in a column
    tidyr::pivot_longer(year_names, names_to = year, values_to = matvals) %>% 
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
      "{rownames}" := paste0(.data[[ef_product]], sep, .data[[destination]]),
      # Column names come from Machine -> Eu.product for both C_Y and C_EIOU.
      "{colnames}" := paste0(.data[[machine]], sep, .data[[eu_product]]), 
      # Row types are Products
      "{rowtypes}" := product,
      # Column types are industries
      "{coltypes}" := industry,
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
  
  # Verify that all rows sum to 1. If not, there has been a problem somewhere.
  verify <- out %>% 
    dplyr::mutate(
      "{.should_be_1_vector}" := matsbyname::rowsums_byname(.data[[matvals]]),
      "{.is_1}" := matsbyname::difference_byname(.data[[.should_be_1_vector]], 1) %>% 
        matsbyname::abs_byname() %>% 
        matsbyname::compare_byname("<=", tol),
      "{.all_1}" := .data[[.is_1]] %>% matsbyname::all_byname()
    )
  # Check that all rows sum to 1.
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

  # If we passed the test, we can return the out data frame without the verification columns.
  return(out)
}


#' Create final-to-useful efficiency vectors (`eta_fu`) and exergy-to-energy ratio vectors (`phi_u`) from a final-to-useful efficiency table
#'
#' @param .eta_fu_table a final-to-useful efficiency table read by `load_eta_fu_allocation_data()`.
#'                      A template for this table should have been created by `eta_fu_table()` and 
#'                      `write_eta_fu_table()`.
#' @param year See `IEATools::iea_cols`.
#' @param quantity,machine,eu_product,e_dot_machine,e_dot_machine_perc,maximum_values,eta_fu,phi_u See `IEATools::template_cols`.
#' @param matnames,matvals,rownames,colnames,rowtypes,coltypes See `IEATools::mat_meta_cols`.
#' @param sep The string separator between prefix and suffix of compound row and column names. Default is " -> ".
#'            The default value matches the default value for the `sep` argument of `matsbyname::vectorize_byname()`, because
#'            `matsbyname::vectorize_byname()` will be used for further manipulations.
#'
#' @return a tidy data frame with metadata columns (and year) along with `matnames` and `matvals` columns
#'         indicating and containing `eta_fu` and `phi_u` vectors, respectively.
#' 
#' @export
#'
#' @examples
#' load_eta_fu_data() %>% 
#'   form_eta_fu_phi_u_vecs()
form_eta_fu_phi_u_vecs <- function(.eta_fu_table, 
                                   
                                   year = IEATools::iea_cols$year,
                                   
                                   quantity = IEATools::template_cols$quantity,
                                   machine = IEATools::template_cols$machine,
                                   eu_product = IEATools::template_cols$eu_product,
                                   e_dot_machine = IEATools::template_cols$e_dot_machine,
                                   e_dot_machine_perc = IEATools::template_cols$e_dot_machine_perc,
                                   maximum_values = IEATools::template_cols$maximum_values,
                                   eta_fu = IEATools::template_cols$eta_fu,
                                   phi_u = IEATools::template_cols$phi_u,
                                   
                                   matnames = IEATools::mat_meta_cols$matnames,
                                   matvals  = IEATools::mat_meta_cols$matvals,
                                   rownames = IEATools::mat_meta_cols$rownames,
                                   colnames = IEATools::mat_meta_cols$colnames,
                                   rowtypes = IEATools::mat_meta_cols$rowtypes,
                                   coltypes = IEATools::mat_meta_cols$coltypes, 
                                   
                                   sep = " -> ") {
  
  cleaned <- .eta_fu_table %>% 
    # Eliminate rows titled e_dot_machine or e_dot_machine_perc. These are just helper rows for the analyst.
    dplyr::filter(! (.data[[quantity]] %in% c(e_dot_machine, e_dot_machine_perc)) ) %>% 
    dplyr::mutate(
      # Eliminate the maximum_values column. It was only a helper for the analyst.
      "{maximum_values}" := NULL
    ) %>% 
    dplyr::rename(
      # The quantity column gives us the matrix names
      "{matnames}" := quantity
    )
  # Gather years into a tidy data frame.
  year_names <- year_cols(cleaned, return_names = TRUE)
  gathered <- cleaned %>% 
    # Gather to put years in a column
    tidyr::pivot_longer(year_names, names_to = year, values_to = matvals) %>% 
    # Eliminate rows where C is NA. They came from places where data are not available.
    dplyr::filter(!is.na(.data[[matvals]])) %>% 
    # Make sure the year column is numeric
    dplyr::mutate(
      "{year}" := as.numeric(.data[[year]])
    )
  
  prepped <- gathered %>% 
    dplyr::mutate(
      # Create rownames from the machine and eu_product rows.
      "{rownames}" := paste0(.data[[machine]], sep, .data[[eu_product]]),
      # Eliminate machine and eu_product columns, becasue we no longer need them.
      "{machine}" := NULL,
      "{eu_product}" := NULL,
      # Create colnames according to the name of the matrix to be created
      "{colnames}" := .data[[matnames]],
      "{rowtypes}" := IEATools::row_col_types$industry,
      "{coltypes}" := IEATools::row_col_types$product
    )

  # Collapse to matrices (actually, column vectors) and return  
  group_cols <- matsindf::everything_except(prepped, matvals, rownames, colnames, rowtypes, coltypes)
  out <- prepped %>% 
    dplyr::group_by(!!!group_cols) %>% 
    matsindf::collapse_to_matrices(matnames = matnames, matvals  = matvals, 
                                   rownames = rownames, colnames = colnames, 
                                   rowtypes = rowtypes, coltypes = coltypes)
  
  return(out)
}


#' Move an ECC from final to useful as its last stage
#'
#' @param .tidy_psut_data A tidy data frame of PSUT matrices that represent an energy conversion chain.
#'                        Matrix names should be in the `matnames` column, and
#'                        matrices themselves should be in the `matvals` column.
#'                        The last stage of these ECCs should be final (not useful).
#'                        `.tidy_psut_data` is likely the result of calling (in sequence)
#'                        `load_tidy_iea_df()` `%>%` `specify_all()` `%>%` `prep_psut()`
#' @param tidy_C_data a tidy data frame of final-to-useful allocation matrices, probably the result of calling `form_C_mats()`.
#' @param tidy_eta_fu_data a tidy data frame of final-to-useful machine efficiency matrices, probably the result of calling `form_eta_fu_phi_u_vecs`.
#' @param last_stage See `IEATools::iea_cols$last_stage`. 
#'                   `last_stage` should be a column in all of `.tidy_psut_data`, `C_data`, and `eta_fu_data`.
#' @param final,useful See `IEATools::last_stages`.
#' @param R,U_eiou,U_excl_eiou,V,Y See `IEATools::psut_cols`. 
#'                                 These matrices should be found in the `matvals` column of the `.tidy_psut_data` data frame.
#' @param matnames,matvals See `IEATools::mat_meta_cols`. 
#' @param C_eiou,C_Y,eta_fu See `IEATools::template_cols`. 
#'                          `C_eiou` and `C_Y` matrices should be found in the `matvals` column of the `C_Y_data` data frame.
#'                          `eta_fu` should be found in the `matvals` column of the `eta_fu_data` data frame.
#' @param .useful A suffix applied to versions of PSUT matrices where useful is the last stage. Default is "_useful".
#'
#' @return a version of `.tidy_sut_data` that contains additional rows with useful final stage ECC matrices 
#' 
#' @export
#'
#' @examples
move_to_useful_last_stage <- function(.tidy_psut_data, 
                                      tidy_C_data,
                                      tidy_eta_fu_data,
                                      
                                      last_stage = IEATools::iea_cols$last_stage,
                                      
                                      final = IEATools::last_stages$final,
                                      useful = IEATools::last_stages$useful,
                                      
                                      R = IEATools::psut_cols$R, 
                                      U_eiou = IEATools::psut_cols$U_eiou,
                                      U_excl_eiou = IEATools::psut_cols$U_excl_eiou,
                                      V = IEATools::psut_cols$V, 
                                      Y = IEATools::psut_cols$Y, 
                                      
                                      matnames = IEATools::mat_meta_cols$matnames,
                                      matvals = IEATools::mat_meta_cols$matvals,
                                      
                                      C_eiou = IEATools::template_cols$C_eiou,
                                      C_Y = IEATools::template_cols$C_Y, 
                                      eta_fu = IEATools::template_cols$eta_fu,
                                      
                                      .useful = "_useful") {
  
  .tidy_psut_data %>% 
    # Bind the C and eta_fu vectors to the bottom of the .tidy_sut_data frame
    dplyr::bind_rows(C_data, eta_fu_data) %>% 
    tidyr::pivot_wider(names_from = matnames, values_from = matvals)
  
  print()
  
  # Spread the matrices for calculations
  
  # Calculate matrices with _useful suffixes for the useful version
  
  # Eliminate the final stage matrices
  
  # Eliminate suffixes on the useful stage columns and change last_stage to "Useful".
  
  # Bind the useful stage mats to the bottom of the final stage mats
}
