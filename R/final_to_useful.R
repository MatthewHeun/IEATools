


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
      # Row types are Product -> Industry
      # "{rowtypes}" := product,
      "{rowtypes}" := matsbyname::paste_pref_suff(pref = product, suff = industry, notation = notation),
      # Column types are Industry -> Product
      # "{coltypes}" := industry,
      "{coltypes}" := matsbyname::paste_pref_suff(pref = industry, suff = product, notation = notation),
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
#' @param product,industry See `IEATools::row_col_types`.
#' @param arrow_note,from_note Notation vectors used for creating the eta_fu and phi vectors. 
#'                             See `matsbyname::notation_vec()`. 
#'                             Defaults are `arrow_notation` and ``from_notation`, respectively.
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
                                   
                                   product = IEATools::row_col_types$product,
                                   industry = IEATools::row_col_types$industry,
                                   
                                   arrow_note = IEATools::arrow_notation, 
                                   from_note = IEATools::from_notation) {
  
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
      "{rownames}" := dplyr::case_when(
        .data[[matnames]] == eta_fu ~ matsbyname::paste_pref_suff(pref = .data[[machine]], suff = .data[[eu_product]], notation = arrow_note),
        .data[[matnames]] == phi_u ~ matsbyname::paste_pref_suff(pref = .data[[eu_product]], suff = .data[[machine]], notation = from_note),
        TRUE ~ NA_character_
      ), 
      # Eliminate machine and eu_product columns, because we no longer need them.
      "{machine}" := NULL,
      "{eu_product}" := NULL,
      # Create colnames according to the name of the matrix to be created
      "{colnames}" := .data[[matnames]],
      "{rowtypes}" := dplyr::case_when(
        .data[[matnames]] == eta_fu ~ matsbyname::paste_pref_suff(pref = industry, suff = product, notation = arrow_note),
        .data[[matnames]] == phi_u ~ matsbyname::paste_pref_suff(pref = product, suff = industry, notation = from_note), 
        TRUE ~ NA_character_
      ), 
      "{coltypes}" := NA_character_ # Will change to NULL later.
    )

  # Collapse to matrices (actually, column vectors) and return  
  group_cols <- matsindf::everything_except(prepped, matvals, rownames, colnames, rowtypes, coltypes)
  out <- prepped %>% 
    dplyr::group_by(!!!group_cols) %>% 
    matsindf::collapse_to_matrices(matnames = matnames, matvals  = matvals, 
                                   rownames = rownames, colnames = colnames, 
                                   rowtypes = rowtypes, coltypes = coltypes) %>% 
    dplyr::mutate(
      matvals = matvals %>% matsbyname::setcoltype(NULL)
    )
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
#' @param R,U_eiou,U_feed,V,Y,s_units See `IEATools::psut_cols`. 
#'                                 These matrices should be found in the `matvals` column of the `.tidy_psut_data` data frame.
#' @param sut_meta_cols See `IEATools::sut_meta_cols`.
#' @param matnames,matvals See `IEATools::mat_meta_cols`. 
#' @param C_eiou,C_Y,eta_fu,phi_u See `IEATools::template_cols`. 
#'                          `C_eiou` and `C_Y` matrices should be found in the `matvals` column of the `C_Y_data` data frame.
#'                          `eta_fu` and `phi_u` should be found in the `matvals` column of the `eta_fu_data` data frame.
#' @param interface_ind See `IEATools::interface_industries`. Interface industries are kept same from `Y_final` to `Y_useful`.
#' @param non_energy_ind See `IEATools::non_energy_flows`. Non-energy industries are kept same from `Y_final` to `Y_useful`.
#' @param losses See `IEATools::tfc_compare_flows`. Losses are kept same from `Y_final` to `Y_useful`.
#' @param stat_diffs See `IEATools::tfc_compare_flows`. Statistical differences are kept same from `Y_final` to `Y_useful`.
#' @param notation The row and column notation for this template.
#'                 See `matsbyname::notation_vec()`. Default is `arrow_notation`.
#' @param tol the allowable error in energy balances for both the incoming matrices (last stage final) 
#'            and the outgoing matrices (last stage useful). Default is `1e-3`.
#' @param .Y_f_vec_hat_C_Y an internal matrix name for the product of the Y_f_vec_hat and C_Y matrices. Default is ".Y_f_vec_hat_C_Y".
#' @param .U_eiou_f_vec_hat_C_eiou an internal matrix name for the product of the U_eiou_f_vec_hat and C_eiou matrices. Default is ".U_eiou_f_vec_hat_C_eiou".
#' @param .eta_fu_hat an internal matrix name. Default is ".eta_fu_hat".
#' @param .add_to_U_f an internal matrix name for the a matrix to be added to the U_feed_f matrix 
#'                    to form the useful form of the U_feed matrix. Default is ".add_to_U_f".
#' @param .add_to_U_eiou an internal matrix name for the a matrix to be added to the U_eiou_f matrix 
#'                       to form the useful form of the U_eiou matrix. Default is ".add_to_U_eiou".
#' @param .add_to_V_f an internal matrix name for a matrix to add to the Y_f matrix. Default is ".add_to_V_f".
#' @param .add_to_dest an internal matrix name for a matrix that replaces a previous energy destination. Default is ".repl_dest".
#' @param .err an internal matrix name for calculating energy balance errors. Default is ".err".
#' @param .e_bal_ok an internal column name for assessing whether energy balance is within acceptable tolerances set by the `tol` argument. Default is ".e_bal_OK".
#' @param .useful A suffix applied to versions of PSUT matrices where useful is the last stage. Default is "_useful".
#' @param .keep_in_Y A suffix applied to versions of the `Y` matrix that contain only industries retained in the move from final to useful last stage, 
#'                   including strings from the arguments `interface_ind`, `non_energy_ind`, `losses`, and `stat_diffs`. 
#'                   Default is "_keep_in_Y".
#'
#' @return A version of `.tidy_sut_data` that contains additional rows with useful final stage ECC matrices.
#'         If the energy balance check fails, a warning is emitted and 
#'         additional diagnostic matrices will appear in the output: `.err` and `.e_bal_ok`.
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
                             
                             # Outgoing names
                             
                             
                             last_stage = IEATools::iea_cols$last_stage,
                             unit = IEATools::iea_cols$unit,
                             
                             final = IEATools::last_stages$final,
                             useful = IEATools::last_stages$useful,
                             
                             industry_type = IEATools::row_col_types$industry, 
                             product_type = IEATools::row_col_types$product,
                             
                             R = IEATools::psut_cols$R, 
                             U_eiou = IEATools::psut_cols$U_eiou,
                             U_feed = IEATools::psut_cols$U_feed,
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
                             
                             interface_ind = IEATools::interface_industries,
                             non_energy_ind = IEATools::non_energy_flows,
                             losses = IEATools::tfc_compare_flows$losses,
                             stat_diffs = IEATools::tfc_compare_flows$statistical_differences,
                             
                             notation = IEATools::arrow_notation,
                             
                             tol = 1e-3,
                             
                             .Y_f_vec_hat_C_Y = ".Y_f_vec_hat_C_Y",
                             .U_eiou_f_vec_hat_C_eiou = ".U_eiou_f_vec_hat_C_eiou",
                             .eta_fu_hat = ".eta_fu_hat",
                             .add_to_U_f = ".add_to_U_f",
                             .add_to_U_eiou = ".add_to_U_eiou",
                             .add_to_V_f = ".add_to_V_f",
                             .add_to_dest = ".repl_dest",
                             .err = ".err", 
                             .e_bal_ok = ".e_bal_ok",
                             .useful = "_useful", 
                             .keep_in_Y = "_keep_in_Y") {
  
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
  
  # New column names
  U_feed_useful <- paste0(U_feed, .useful)
  U_eiou_useful <- paste0(U_eiou, .useful)
  V_useful <- paste0(V, .useful)
  Y_useful <- paste0(Y, .useful)
  Y_keep <- paste0(Y, .keep_in_Y)
  
  # Industries to retain from Y_f to Y_u. 
  # These industries are not allocated to f-u machines, nor are they tracked for useful energy.
  # As of 19 June 2020, we are including Non-energy uses in the F-U calculations.
  # Y_keep_inds <- c(interface_ind, non_energy_ind, losses, stat_diffs)
  Y_keep_inds <- c(interface_ind, losses, stat_diffs)
  
  # There are two destinations for final energy: final demand (the Y matrix) and EIOU (the U_EIOU matrix)
  # We take each of these in turn, adjusting the energy conversion chain to account for the fact that 
  # useful energy is now the final stage.

  wide_useful_Y <- wide_psut_data %>% 
    extend_to_useful_helper(dest_mat = Y, C_mat = C_Y, eta_fu_vec = eta_fu, 
                            add_to_U = .add_to_U_f, add_to_V = .add_to_V_f, add_to_dest = .add_to_dest) %>% 
    dplyr::mutate(
      "{U_feed_useful}" := matsbyname::sum_byname(.data[[U_feed]], .data[[.add_to_U_f]]), 
      "{V_useful}" := matsbyname::sum_byname(.data[[V]], .data[[.add_to_V_f]]),
      # We need to keep industries in Y that are interface industries 
      # (exports, stock changes, international marine and aviation bunkers, and 
      # imports, though there won't be any imports in the Y matrix, because imports are in the V matrix).
      # Also keep non-energy flows.
      # None of the interface industries nor the non-energy flows are in the allocation matrix (C), 
      # so we must retain them in the Y matrix.
      "{Y_keep}" := matsbyname::select_cols_byname(.data[[Y]], 
                                                   retain_pattern = matsbyname::make_pattern(Y_keep_inds, 
                                                                                             pattern_type = "leading")), 
      "{Y_useful}" := matsbyname::sum_byname(.data[[Y_keep]], .data[[.add_to_dest]]), 
      # Eliminate columns that are no longer needed
      "{.add_to_U_f}" := NULL,
      "{.add_to_V_f}" := NULL,
      "{.add_to_dest}" := NULL,
      "{Y_keep}" := NULL
    )
  
  wide_useful_EIOU <- wide_useful_Y %>% 
    extend_to_useful_helper(dest_mat = U_eiou, C_mat = C_eiou, eta_fu_vec = eta_fu, 
                            add_to_U = .add_to_U_eiou, add_to_V = .add_to_V_f, add_to_dest = .add_to_dest) %>% 
    dplyr::mutate(
      "{U_feed_useful}" := matsbyname::sum_byname(.data[[U_feed_useful]], .data[[.add_to_U_eiou]]), 
      "{V_useful}" := matsbyname::sum_byname(.data[[V_useful]], .data[[.add_to_V_f]]), 
      "{U_eiou_useful}" := .data[[.add_to_dest]], 
      # Eliminate columns that are no longer needed
      "{.add_to_U_eiou}" := NULL,
      "{.add_to_V_f}" := NULL,
      "{.add_to_dest}" := NULL
    )
  
  # Prepare the outgoing data frame
  out <- wide_useful_EIOU %>%
    dplyr::mutate(
      # Show that these all now have useful last stage
      "{last_stage}" := useful,
      # Eliminate unneeded columns associated with the final stage being last stage
      "{U_eiou}" := NULL,
      "{U_feed}" := NULL,
      "{V}" := NULL,
      "{Y}" := NULL,
      # Eliminate other temporary columns
      "{C_eiou}" := NULL,
      "{C_Y}" := NULL,
      "{eta_fu}" := NULL,
      "{phi_u}" := NULL,
      "{.eta_fu_hat}" := NULL
    ) %>%
    # Rename columns to align with columns in final stage data in .sutdata
    dplyr::rename(
      "{U_feed}" := .data[[paste0(U_feed, .useful)]],
      "{U_eiou}" := .data[[paste0(U_eiou, .useful)]],
      "{V}" := .data[[paste0(V, .useful)]],
      "{Y}" := .data[[paste0(Y, .useful)]]
    )

  out <- dplyr::bind_rows(.sutdata, out)
  
  # Check Product energy balances.
  # It would be nice to use the Recca function verify_SUT_energy_balance() for this purpose.
  # However, IEATools is designed to be independent of Recca.
  # So we need to do our own energy balance here.
  # Fortunately, energy balance calculations for products are relatively simple.
  # The energy balance for products is given by rowsums of (R + V)^T - U - Y, which should all equal 0
  # within acceptable error.
  verify_ebal <- out %>%
    dplyr::mutate(
      # R_plus_V_mat = matsbyname::sum_byname(.data[[R]], .data[[V]]),
      # RV_sums = matsbyname::transpose_byname(R_plus_V_mat) %>% matsbyname::rowsums_byname(),
      # U_sums = matsbyname::sum_byname(.data[[U_eiou]], .data[[U_feed]]) %>% matsbyname::rowsums_byname(),
      # Y_sums = matsbyname::rowsums_byname(.data[[Y]]),
      # # (R + V) - U - Y  
      # "{.err}" := RV_sums %>% matsbyname::difference_byname(U_sums) %>% matsbyname::difference_byname(Y_sums),
      # "{.e_bal_ok}" := err %>% matsbyname::iszero_byname(tol = tol) %>% as.logical()
      
      
      # (R + V)^T - U - Y  
      "{.err}" := matsbyname::sum_byname(.data[[R]], .data[[V]]) %>% # R + V
        matsbyname::transpose_byname() %>% 
        matsbyname::difference_byname(.data[[U_feed]]) %>%           # - U_feed
        matsbyname::difference_byname(.data[[U_eiou]]) %>%           # - U_eiou
        matsbyname::difference_byname(.data[[Y]]) %>%                # - Y
        matsbyname::rowsums_byname(),
      "{.e_bal_ok}" := .data[[.err]] %>%
        matsbyname::iszero_byname(tol = tol) %>% 
        as.logical()
    )
  all_OK <- all(verify_ebal[[.e_bal_ok]])
  if (!all_OK) {
    # Emit a warning if there is a problem and return the wrong thing.
    warning(paste0("Energy is not balanced to within ", tol, " in IEATools::extend_to_useful(). See columns ", 
                   .err, " and ", .e_bal_ok, " for problems."))
    return(verify_ebal)
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
#'                 Default is `IEATools::arrow_notation`.
#' @param from_note a row and column name notation vector that indicates a `destination [from source]` relationship. 
#'                  `from_note` is used for the columns of some intermediate matrices.
#'                  See `matsbyname::notation_vec()`.
#'                  Default is `IEATools::from_notation`.
#' @param add_to_U a string name for the matrix to be added to a use matrix. Default is "add_to_U".
#' @param add_to_V a string name for the matrix to be added to a make matrix. Default is "add_to_V".
#' @param add_to_dest a string name for the matrix to replace some entries previous destination matrix. Default is "repl_dest".
#'
#' @return a named list containing three items: 
#'         `add_to_U_f` (a matrix to be added to a use (`U`) matrix),
#'         `add_to_V_f` (a matrix to be added to a make (`V`) matrix), and 
#'         `add_to_dest_mat` (a matrix to replace the destination matrix, typically `Y_f` or `U_eiou`.
extend_to_useful_helper <- function(.sutdata = NULL, 
                                    # Input matrix names
                                    dest_mat, C_mat, eta_fu_vec, 
                                    # Input parameters
                                    product_type = IEATools::row_col_types$product, 
                                    industry_type = IEATools::row_col_types$industry,
                                    arr_note = IEATools::arrow_notation, 
                                    from_note = IEATools::from_notation, 
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
      matsbyname::clean_byname() %>%
      matsbyname::hatize_byname() %>%
      matsbyname::matrixproduct_byname(C_m)
    
    eta_fu_hat <- matsbyname::hatize_byname(eta_fu_v) %>% 
      # Swap column names from arrow notation to paren notation
      matsbyname::switch_notation_byname(margin = 2, from = arr_note, to = from_note, flip = TRUE)
    
    #### Step 2 on the "Pushing Y to useful" tab in file "Matrix f->U example calcs.xlsx"
    
    # Calculate the matrix that should be added to the U_f matrix.
    add_to_U_f_mat <- dest_mat_vec_hat_C %>% 
      matsbyname::aggregate_to_pref_suff_byname(keep = "prefix", margin = 1, notation = arr_note) %>%
      matsbyname::clean_byname(margin = 1) %>% 
      # Set column type to industry to match other use matrices.
      matsbyname::setcoltype(industry_type)

    #### Step 3 on the "Pushing Y to useful" tab in file "Matrix f->U example calcs.xlsx"
    
    # Calculate the matrix that should be added to the V_f matrix.
    add_to_V_f_mat <- dest_mat_vec_hat_C %>% 
      matsbyname::colsums_byname() %>%
      matsbyname::hatize_byname() %>%
      matsbyname::matrixproduct_byname(eta_fu_hat) %>% 
      # Set row and column type to match other make matrices.
      matsbyname::setrowtype(industry_type) %>% 
      matsbyname::setcoltype(product_type)
    
    #### Step 4 on the "Pushing Y to useful" tab in file "Matrix f->U example calcs.xlsx"
    
    # Calculate replacement for the destination matrix (Y_useful instead of Y_f or U_eiou_useful instead of U_eiou)
    add_to_dest_mat <- matsbyname::matrixproduct_byname(dest_mat_vec_hat_C, eta_fu_hat) %>%
      matsbyname::transpose_byname() %>%
      matsbyname::aggregate_to_pref_suff_byname(keep = "suffix", margin = 2, notation = arr_note) %>%
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

