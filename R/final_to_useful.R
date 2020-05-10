


#' Create allocation matrices (`C`) from an allocation table
#' 
#' This function uses information in a filled allocation template (created by `write_fu_allocation_template()`)
#' to create allocation matrices (`C`). 
#' 
#' rownames of the `C` matrices are taken from the `Ef.product` and `Destination` columns of `.fu_allocation_table`
#' and have the form "`Ef.product` `r arrow_notation[["pref_end"]]` `Destination`".
#' colnames of the `C` matrices are taken from the `Machine` and `Eu.product` columns of `.fu_allocation_table`
#' and have the form "machine `r arrow_notation[["pref_end"]]` useful energy form".
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
#' @param ledger_side,flow_aggregation_point,e_dot,unit,year See `IEATools::iea_cols`.
#' @param supply,consumption See `IEATools::ledger_sides`.
#' @param quantity,machine,ef_product,eu_product,destination,e_dot_perc,maximum_values,C_eiou,C_Y See `IEATools::template_cols`.
#' @param matnames,matvals,rownames,colnames,rowtypes,coltypes See `IEATools::mat_meta_cols`.
#' @param product,industry See `IEATools::row_col_types`.
#' @param notation the notation used for this template. See `matsbyname::notation_vec()`. Default is `IEATools::arrow_notation`.
#' @param tol the allowable amount by which a row sum in a `C` matrix can be different from 1. Default is 1e-6.
#' @param .should_be_1_vector a temporary column created internally for error checking (and not returned unless there is an error). 
#'                            This column should contain 1 vectors (i.e., vectors filled with 1's).
#' @param .is_1 a temporary column created internally (and not returned unless there is an error)
#'              that contains `TRUE` or `FALSE` depending on whether a rowsum was 1.
#' @param .all_1 a temporary column created internally (and not returned unless there is an error)
#'               that tells whether a 1-vector was created by rowsums.
#'
#' @return a wide data frame with metadata columns (and year) along with columns for 
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
                        
                        notation = IEATools::arrow_notation,

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
      "{rownames}" := matsbyname::paste_pref_suff(pref = .data[[ef_product]], suff = .data[[destination]], notation = notation),
      # Column names come from Machine -> Eu.product for both C_Y and C_EIOU.
      "{colnames}" := matsbyname::paste_pref_suff(pref = .data[[machine]], suff = .data[[eu_product]], notation = notation),
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
#' Row names have the pattern 
#' "machine`r arrow_notation[["pref_end"]]`useful energy form" to indicate 
#' the energy efficiency of "machine" for making "useful energy form"
#' or the exergy-to-energy ratio of the useful energy form created by machine.
#' 
#' Columns are named by the variable in the vector: 
#' "eta_fu" for final-to-useful efficiencies and
#' "phi_u" for useful exergy-to-useful energy ratios.
#' 
#' @param .eta_fu_table a final-to-useful efficiency table read by `load_eta_fu_allocation_data()`.
#'                      A template for this table should have been created by `eta_fu_table()` and 
#'                      `write_eta_fu_table()`.
#' @param unit,year See `IEATools::iea_cols`.
#' @param quantity,machine,eu_product,e_dot_machine,e_dot_machine_perc,maximum_values,eta_fu,phi_u See `IEATools::template_cols`.
#' @param matnames,matvals,rownames,colnames,rowtypes,coltypes See `IEATools::mat_meta_cols`.
#' @param notation The notation used for creating the eta_fu and phi vectors. 
#'                 See `matsbyname::notation_vec()`. Default is `arrow_notation`.
#'
#' @return a wide data frame with metadata columns (and year) along with columns for `eta_fu` and `phi_u` vectors.
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
                                   
                                   matnames = IEATools::mat_meta_cols$matnames,
                                   matvals  = IEATools::mat_meta_cols$matvals,
                                   rownames = IEATools::mat_meta_cols$rownames,
                                   colnames = IEATools::mat_meta_cols$colnames,
                                   rowtypes = IEATools::mat_meta_cols$rowtypes,
                                   coltypes = IEATools::mat_meta_cols$coltypes, 
                                   
                                   notation = IEATools::arrow_notation) {
  
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
      "{rownames}" := matsbyname::paste_pref_suff(pref = .data[[machine]], suff = .data[[eu_product]], notation = notation),
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
  # pivot wider to the sutmats format
  out %>% 
    tidyr::pivot_wider(names_from = matnames, values_from = matvals)
}


#' Move an ECC from final to useful as its last stage
#' 
#' This function uses a matrix method to move 
#' from final energy/exergy 
#' to useful energy/exergy as the last stage of an energy conversion chain.
#'
#' @param .sutdata A wide data frame of PSUT matrices that represent an energy conversion chain.
#'                 Matrices should be in column identified by their names.
#'                 The last stage of these ECCs should be final (not useful).
#'                 `.sutdata` is likely the result of calling (in sequence)
#'                 `load_tidy_iea_df()` `%>%` `specify_all()` `%>%` `prep_psut()`
#' @param wide_C_data a wide data frame of final-to-useful allocation matrices, probably the result of calling `form_C_mats()`.
#' @param wide_eta_fu_data a wide data frame of final-to-useful machine efficiency matrices, probably the result of calling `form_eta_fu_phi_u_vecs`.
#' @param last_stage,unit See `IEATools::iea_cols$last_stage`. 
#'                        Each of these should be a column in all of `.tidy_psut_data`, `C_data`, and `eta_fu_data`.
#' @param final,useful See `IEATools::last_stages`.
#' @param industry_type,product_type See `IEATools::row_col_types`
#' @param R,U_eiou,U_excl_eiou,V,Y,s_units See `IEATools::psut_cols`. 
#'                                 These matrices should be found in the `matvals` column of the `.tidy_psut_data` data frame.
#' @param sut_meta_cols See `IEATools::sut_meta_cols`.
#' @param matnames,matvals See `IEATools::mat_meta_cols`. 
#' @param C_eiou,C_Y,eta_fu,phi_u See `IEATools::template_cols`. 
#'                          `C_eiou` and `C_Y` matrices should be found in the `matvals` column of the `C_Y_data` data frame.
#'                          `eta_fu` and `phi_u` should be found in the `matvals` column of the `eta_fu_data` data frame.
#' @param notation The row and column notation for this template.  
#'                 See `matsbyname::notation_vec()`. Default is `arrow_notation`.
#' @param tol the allowable error in energy balances. Default is `1e-3`.
#' @param .Y_f_vec_hat_C_Y an internal matrix name for the product of the Y_f_vec_hat and C_Y matrices. Default is ".Y_f_vec_hat_C_Y".
#' @param .U_eiou_f_vec_hat_C_eiou an internal matrix name for the product of the U_eiou_f_vec_hat and C_eiou matrices. Default is ".U_eiou_f_vec_hat_C_eiou".
#' @param .eta_fu_hat an internal matrix name. Default is ".eta_fu_hat".
#' @param .add_to_U_f an internal matrix name for the a matrix to be added to the U_excl_eiou_f matrix 
#'                    to form the useful form of the U_excl_eiou matrix. Default is ".add_to_U_f".
#' @param .add_to_U_eiou an internal matrix name for the a matrix to be added to the U_eiou_f matrix 
#'                       to form the useful form of the U_eiou matrix. Default is ".add_to_U_eiou".
#' @param .add_to_V_f an internal matrix name for a matrix to add to the Y_f matrix. Default is ".add_to_V_f".
#' @param .useful A suffix applied to versions of PSUT matrices where useful is the last stage. Default is "_useful".
#'
#' @return a version of `.tidy_sut_data` that contains additional rows with useful final stage ECC matrices 
#' 
#' @export
#'
#' @examples
#' psut_mats <- load_tidy_iea_df() %>% 
#'   specify_all() %>% 
#'   prep_psut()
#' C_data <- load_fu_allocation_data() %>% 
#'   form_C_mats()
#' eta_fu_data <- load_eta_fu_data() %>% 
#'   form_eta_fu_phi_u_vecs()
#' psut_mats %>% 
#'   extend_to_useful(wide_C_data = C_data, 
#'                             wide_eta_fu_data = eta_fu_data)
extend_to_useful <- function(.sutdata, 
                             wide_C_data,
                             wide_eta_fu_data,
                             
                             last_stage = IEATools::iea_cols$last_stage,
                             unit = IEATools::iea_cols$unit,
                             
                             final = IEATools::last_stages$final,
                             useful = IEATools::last_stages$useful,
                             
                             industry_type = IEATools::row_col_types$industry, 
                             product_type = IEATools::row_col_types$product,
                             
                             R = IEATools::psut_cols$R, 
                             U_eiou = IEATools::psut_cols$U_eiou,
                             U_excl_eiou = IEATools::psut_cols$U_excl_eiou,
                             V = IEATools::psut_cols$V, 
                             Y = IEATools::psut_cols$Y, 
                             s_units = IEATools::psut_cols$s_units,
                             
                             matnames = IEATools::mat_meta_cols$matnames,
                             matvals = IEATools::mat_meta_cols$matvals,
                             
                             sut_meta_cols = IEATools::sut_meta_cols,
                             
                             C_eiou = IEATools::template_cols$C_eiou,
                             C_Y = IEATools::template_cols$C_Y, 
                             eta_fu = IEATools::template_cols$eta_fu,
                             phi_u = IEATools::template_cols$phi_u,
                             
                             notation = IEATools::arrow_notation,
                             
                             tol = 1e-3,
                             
                             .Y_f_vec_hat_C_Y = ".Y_f_vec_hat_C_Y",
                             .U_eiou_f_vec_hat_C_eiou = ".U_eiou_f_vec_hat_C_eiou",
                             .eta_fu_hat = ".eta_fu_hat",
                             .add_to_U_f = ".add_to_U_f",
                             .add_to_U_eiou = ".add_to_U_eiou",
                             .add_to_V_f = ".add_to_V_f",
                             .useful = "_useful") {
  
  wide_psut_data <- .sutdata %>% 
    # Join the C and eta_fu vectors to the right of the .sutdata frame
    dplyr::full_join(wide_C_data, by = sut_meta_cols %>% unlist() %>% unname()) %>% 
    dplyr::full_join(wide_eta_fu_data, by = sut_meta_cols %>% unlist() %>% unname()) %>% 
    dplyr::mutate(
      # Calculate .eta_fu_hat, which is needed twice below.
      # Doing the calculation here makes it available for other downstream calculations.
      "{.eta_fu_hat}" := matsbyname::hatize_byname(.data[[eta_fu]]) %>% 
        # Swap column names from arrow notation to from notation
        arrow_to_from_byname(margin = 2)
    )
  
  # There are two destinations for final energy: final demand (the Y matrix) and EIOU (the U_EIOU matrix)
  # We take each of these in turn, adjusting the energy conversion chain to account for the fact that 
  # useful energy is now the final stage.

  
  ########################
  ########################
  # 
  # Use matsindf_apply to do this transformation, using the extend_to_useful_helper function
  #  
  ########################
  ########################
  
    
  wide_useful_Y <- wide_psut_data %>%
    dplyr::mutate(

      #### Step 1 on the "Pushing Y to useful" tab in file "Matrix f->u example calcs.xlsx"

      # Calculate Y_f_vec_hat_C_Y, the matrix product of Y_f_vec_hat and C_Y
      "{.Y_f_vec_hat_C_Y}" := matsbyname::vectorize_byname(.data[[Y]], notation = list(notation)) %>%
        matsbyname::clean_byname() %>%
        matsbyname::hatize_byname() %>%
        matsbyname::matrixproduct_byname(.data[[C_Y]]),

      # Calculate eta_fu_hat
      # Already calculated above.

      #### Step 2 on the "Pushing Y to useful" tab in file "Matrix f->U example calcs.xlsx"

      # Calculate the matrix that should be added to the U_f matrix.
      "{.add_to_U_f}" := .data[[.Y_f_vec_hat_C_Y]] %>%
        matsbyname::aggregate_to_pref_suff_byname(sep = sep, keep = "prefix", margin = 1) %>%
        matsbyname::clean_byname(margin = 1),

      #### Step 3 on the "Pushing Y to useful" tab in file "Matrix f->U example calcs.xlsx"

      # Calculate the matrix that should be added to the V_f matrix.
      "{.add_to_V_f}" := .data[[.Y_f_vec_hat_C_Y]] %>%
        matsbyname::setcoltype(industry_type) %>% setrowtype(industry_type) %>%
        matsbyname::colsums_byname() %>%
        matsbyname::hatize_byname() %>%
        matsbyname::matrixproduct_byname(.data[[.eta_fu_hat]]),

      #### Step 4 on the "Pushing Y to useful" tab in file "Matrix f->U example calcs.xlsx"

      # Calculate replacement for Y matrix (Y_useful instead of Y_f)
      "{paste0(Y, .useful)}" := matsbyname::matrixproduct_byname(.data[[.Y_f_vec_hat_C_Y]], .data[[.eta_fu_hat]]) %>%
        matsbyname::transpose_byname() %>%
        matsbyname::setrowtype(product_type) %>% matsbyname::setcoltype(industry_type) %>%
        matsbyname::aggregate_to_pref_suff_byname(sep = sep, keep = "suffix", margin = 2) %>%
        matsbyname::clean_byname(),

      #### Step 5

      # Create U_useful matrix
      "{paste0(U_excl_eiou, .useful)}" := matsbyname::sum_byname(.data[[U_excl_eiou]], .data[[.add_to_U_f]]),

      # Create V_useful matrix
      "{paste0(V, .useful)}" := matsbyname::sum_byname(.data[[V]], .data[[.add_to_V_f]]),

      # Eliminate columns we no longer need from the data frame
      "{.Y_f_vec_hat_C_Y}" := NULL,
      "{.add_to_U_f}" := NULL,
      "{.add_to_V_f}" := NULL,
    )
  
  wide_useful_EIOU <- wide_useful_Y %>% 
    dplyr::mutate(
      
      #### Step 1 on the "Pushing EIOU to useful" tab in file "Matrix f->U example calcs.xlsx"
      
      # Calculate U_eiou_f_vec_hat_C_eiou
      "{.U_eiou_f_vec_hat_C_eiou}" := matsbyname::vectorize_byname(.data[[U_eiou]]) %>% 
        matsbyname::clean_byname() %>%
        matsbyname::hatize_byname() %>% 
        matsbyname::matrixproduct_byname(.data[[C_eiou]]),
      
      #### Step 2 on the "Pushing EIOU to useful" tab in file "Matrix f->U example calcs.xlsx"
      
      # Calculate the matrix that should be added to the U_f matrix.
      "{.add_to_U_f}" := .data[[.U_eiou_f_vec_hat_C_eiou]] %>% 
        matsbyname::aggregate_to_pref_suff_byname(sep = sep, keep = "prefix", margin = 1) %>% 
        matsbyname::clean_byname(margin = 1), 
      
      #### Step 3 on the "Pushing EIOU to useful" tab in file "Matrix f->U example calcs.xlsx"
      
      # Calculate the matrix that should be added to the V_f matrix.
      "{.add_to_V_f}" := .data[[.U_eiou_f_vec_hat_C_eiou]] %>% 
        matsbyname::colsums_byname() %>% 
        matsbyname::hatize_byname() %>% 
        matsbyname::matrixproduct_byname(.data[[.eta_fu_hat]]), 
      
      #### Step 4 on the "Pushing EIOU to useful" tab in file "Matrix f->U example calcs.xlsx"
      
      # Calculate replacement for U_eiou_f matrix (U_eiou_useful instead of U_eiou_f)
      "{paste0(U_eiou, .useful)}" := matsbyname::matrixproduct_byname(.data[[.U_eiou_f_vec_hat_C_eiou]], .data[[.eta_fu_hat]]) %>% 
        matsbyname::transpose_byname() %>% 
        matsbyname::aggregate_to_pref_suff_byname(sep = sep, keep = "suffix", margin = 2) %>% 
        matsbyname::clean_byname(), 
      
      #### Step 5
      
      # Add more to the U_useful matrix
      "{paste0(U_excl_eiou, .useful)}" := matsbyname::sum_byname(.data[[paste0(U_excl_eiou, .useful)]], .data[[.add_to_U_f]]),
      
      # Add more to the V_useful matrix
      "{paste0(V, .useful)}" := matsbyname::sum_byname(.data[[paste0(V, .useful)]], .data[[.add_to_V_f]]), 
      
      # Eliminate columns we no longer need from the data frame
      "{.U_eiou_f_vec_hat_C_eiou}" := NULL,
      "{.add_to_U_f}" := NULL,
      "{.add_to_V_f}" := NULL
    )
  
  # Prepare the outgoing data frame
  out <- wide_useful_EIOU %>% 
    dplyr::mutate(
      # Show that these all now have useful last stage
      "{last_stage}" := useful,
      # Eliminate unneeded columns associated with the final stage being last stage
      "{U_eiou}" := NULL,
      "{U_excl_eiou}" := NULL,
      "{V}" := NULL,
      "{Y}" := NULL,
      # Eliminate other temporary columns
      "{C_eiou}" := NULL,
      "{C_Y}" := NULL,
      "{eta_fu}" := NULL,
      "{phi_u}" := NULL,
      "{.eta_fu_hat}" := NULL
    ) %>% 
    # Rename columns that are the matrices for useful last stage
    dplyr::rename(
      "{U_excl_eiou}" := .data[[paste0(U_excl_eiou, .useful)]], 
      "{U_eiou}" := .data[[paste0(U_eiou, .useful)]], 
      "{V}" := .data[[paste0(V, .useful)]], 
      "{Y}" := .data[[paste0(Y, .useful)]]
    )

  out <- dplyr::bind_rows(.sutdata, out)
  
  # Check energy balance
  verify_ebal <- out %>% 
    dplyr::mutate(
      R_plus_V_mat = matsbyname::sum_byname(.data[[R]], .data[[V]]),
      RV_sums = matsbyname::transpose_byname(R_plus_V_mat) %>% matsbyname::rowsums_byname(),
      U_sums = matsbyname::sum_byname(.data[[U_eiou]], .data[[U_excl_eiou]]) %>% matsbyname::rowsums_byname(),
      Y_sums = matsbyname::rowsums_byname(.data[[Y]]), 
      # (R + V) - U - Y
      err = matsbyname::difference_byname(RV_sums, U_sums) %>% matsbyname::difference_byname(Y_sums), 
      OK = err %>% matsbyname::iszero_byname(tol = tol) %>% as.logical()
    )
  # Error if there is a problem.
  
  return(out)
}


#' A helper function for extending to the useful energy/exergy stage
#' 
#' The helper function is needed, because moving from final to useful energy 
#' occurs for both the final demand matrix (`Y`) and the energy industry own use matrix (`U_eiou`).
#' The calculations are identical, so we factor the calculations into this function.
#'
#' @param dest_mat a `Y` or `U_eiou` matrix (Product x Industry) that is a destination for final energy in an energy conversion chain
#' @param C_mat an allocation matrix (either `Y` or `U_eiou`, both Product x Industry),
#'          indicating the distribution of 
#'          final energy carriers (Products) to 
#'          final-to-useful energy conversion machines (Industries).
#'          `C_mat` should have been created by `form_C_mats()`.
#' @param eta_fu_vec an efficiency column vector indicating the efficiency (column) 
#'               of final-to-useful energy conversion machines (rows).
#'               `eta_fu_vec` should have been created by `form_eta_fu_phi_u_vecs()`.
#' @param product_type a string identifying product row or column types. Default is "`IEATools::row_col_types$product`".
#' @param industry_type a string identifying industry row or column types. Default is "`IEATools::row_col_types$industry`".
#' @param arr_note a row and column name notation vector that indicates a `source -> destination` relationship. 
#'                 `arr_note` is used for the `eta_fu` matrix, among others.
#'                 See `matsbyname::notation_vec()`.
#'                 Default is `IEATools::arrow_notation`.
#' @param from_note a row and column name notation vector that indicates a `destination [from source]` relationship. 
#'                  `from_note` is used for the columns of some intermediate matrices.
#'                  See `matsbyname::notation_vec()`.
#'                  Default is `IEATools::from_notation`.
#'
#' @return a named list containing three items: 
#'         `add_to_U_f` (a matrix to be added to the `U_excl_eiou` matrix),
#'         `add_to_V_f` (a matrix to be added to the `V` matrix), and 
#'         `repl_dest_mat` (a matrix to replace either `Y_f` or `U_eiou`).
extend_to_useful_helper <- function(dest_mat, C_mat, eta_fu_vec, 
                                    product_type = IEATools::row_col_types$product, 
                                    industry_type = IEATools::row_col_types$industry,
                                    arr_note = arrow_notation, 
                                    from_note = from_notation) {
  
  #### Step 1 on the "Pushing Y to useful" tab in file "Matrix f->u example calcs.xlsx"

  dest_mat_vec <- matsbyname::vectorize_byname(dest_mat, notation = arr_note)

  # Calculate dest_mat_vec_hat_C, the matrix product of dest_mat_vec_hat and C
  # This matrix is useful in several calculations below. We calculate it once here.
  dest_mat_vec_hat_C <- dest_mat_vec %>%
    matsbyname::clean_byname() %>%
    matsbyname::hatize_byname() %>%
    matsbyname::matrixproduct_byname(C_mat)
  
  eta_fu_hat <- matsbyname::hatize_byname(eta_fu) %>% 
    # Swap column names from arrow notation to paren notation
    matsbyname::switch_notation_byname(margin = 2, from = arr_note, to = from_note, flip = TRUE)

  #### Step 2 on the "Pushing Y to useful" tab in file "Matrix f->U example calcs.xlsx"
  
  # Calculate the matrix that should be added to the U_f matrix.
  add_to_U_f <- dest_mat_vec_hat_C %>% 
    matsbyname::aggregate_to_pref_suff_byname(keep = "prefix", margin = 1, notation = arrow_notation) %>%
    matsbyname::clean_byname(margin = 1)
  
  #### Step 3 on the "Pushing Y to useful" tab in file "Matrix f->U example calcs.xlsx"
  
  # Calculate the matrix that should be added to the V_f matrix.
  add_to_V_f <- dest_mat_vec_hat_C %>% 
    matsbyname::colsums_byname() %>%
    matsbyname::hatize_byname() %>%
    matsbyname::matrixproduct_byname(eta_fu_hat)
  
  #### Step 4 on the "Pushing Y to useful" tab in file "Matrix f->U example calcs.xlsx"
  
  # Calculate replacement for the destination matrix (Y_useful instead of Y_f or U_eiou_useful instead of U_eiou)
  repl_dest_mat <- matsbyname::matrixproduct_byname(dest_mat_vec_hat_C, eta_fu_hat) %>%
    matsbyname::transpose_byname() %>%
    matsbyname::aggregate_to_pref_suff_byname(keep = "suffix", margin = 2, notation = arrow_notation) %>%
    matsbyname::clean_byname()
  
  list(add_to_U_f = add_to_U_f, add_to_V_f = add_to_V_f, repl_dest_mat = repl_dest_mat)
}

