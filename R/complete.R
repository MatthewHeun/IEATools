#' Complete an FU Allocation table
#' 
#' An FU (final-to-useful) Allocation table 
#' tells how final energy carriers are allocated to final-to-useful machines
#' in each final demand sector.
#' A template for an FU Allocation table can be created with 
#' `fu_allocation_template()`.
#' If the analyst does not know some FU allocations for a given country, 
#' this function can be used to build a complete FU allocation table
#' by supplying allocations from any number of exemplar countries.
#' 
#' `fu_allocation_table` is the FU Allocation table to be completed.
#' Any missing information is obtained from the FU Allocation tables of the exemplar countries,
#' provided in the `exemplar_fu_allocation_tables` argument.
#' Each exemplar table is interrogated in order, 
#' with data taken from the first exemplar that contains the needed information.
#' 
#' `tidy_specified_iea_data` supplies information about which data are needed.
#' The `tidy_specified_iea_data` data frame should be obtained from a call to `specify_all()`. 
#' 
#' If `fu_allocation_table` can't be completed (because not enough information is available in 
#' `exemplar_fu_allocation_tables`), an warning is emitted
#' and a data frame is returned containing rows from `tidy_specified_iea_data` that were not allocated.
#' 
#' @param fu_allocation_table The FU allocation table to be completed. 
#'                            This data frame is probably read by `load_fu_allocation_data()`.
#'                            If `NULL`, the table will be constructed exclusively from 
#'                            information available in the exemplar country tables.
#'                            Only one country is allowed in this data frame.
#' @param exemplar_fu_allocation_tables A list of FU Allocation tables, each probably created by `load_fu_allocation_data()`. 
#'                                      Note that each exemplar table must contain data for a single country only. 
#'                                      If more than one country is found, an error occurs.
#' @param tidy_specified_iea_data A data frame of specified IEA data in tidy format.
#' @param country,ledger_side,flow,product,e_dot,year,flow_aggregation_point See `IEATools::ieacols`.
#' @param supply,consumption See `IEATools::ledger_sides`.
#' @param eiou See `IEATools::tfc_compar_flows`.
#' @param e_dot_perc,destination,machine,eu_product,ef_product,maximum_values,quantity See `IEATools::template_cols`.
#' @param c_source The name of a column added to output that describes the source of the allocation values (the C values). 
#'               Default is "C_source".
#' @param .values The name of a values column created internally. Default is "values".
#'
#' @return A completed tidy data frame containing an FU Allocation table to replace argument `fu_allocation_table`.
#'         Note that the `max_vals` column is absent on output.
#'         Also, the `e_dot` and `e_dot_perc` rows are absent on output.
#' 
#' @export
#'
#' @examples
#' fu_table <- load_fu_allocation_data()
#' # Make an FU Allocation table for Ghana that is missing Residential consumption of PSBs.
#' # Allocations for Residential consumption of PSBs will be picked up from the exemplar, South Africa.
#' fu_table_GHA <- fu_table %>% 
#'   dplyr::filter(Country == "GHA") %>% 
#'   dplyr::filter(!(Flow.aggregation.point == IEATools::tfc_flows$other & 
#'                     Ef.product == IEATools::biofuels_and_waste_products$primary_solid_biofuels & 
#'                     Destination == IEATools::other_flows$residential))
#' # Make the exemplar, South Africa.
#' fu_table_ZAF <- fu_table %>% 
#'   dplyr::filter(Country == "ZAF")
#' # The South African data have Residential PSB consumption, 
#' # which will be used to complete the Ghanaian FU Allocation table.
#' fu_table_ZAF %>% 
#'   dplyr::filter(Flow.aggregation.point == IEATools::tfc_flows$other & 
#'                   Ef.product == IEATools::biofuels_and_waste_products$primary_solid_biofuels & 
#'                   Destination == IEATools::other_flows$residential) %>% 
#'   dplyr::select(!c(Method, Energy.type, Last.stage, Flow.aggregation.point))
#' # Get the IEA data for GHA and ZAF and specify it.
#' tidy_specified_iea_data <- load_tidy_iea_df() %>% 
#'   specify_all()
#' # Now complete the Ghanaian FU Allocation table using information from South Africa.
#' completed <- complete_fu_allocation_table(fu_allocation_table = fu_table_GHA, 
#'                                           exemplar_fu_allocation_tables = list(fu_table_ZAF), 
#'                                           tidy_specified_iea_data = tidy_specified_iea_data)
#' # Note that the C_source column shows that these data have been b from South Africa.
#' completed %>% 
#'   dplyr::filter(Flow.aggregation.point == IEATools::tfc_flows$other & 
#'                   Ef.product == IEATools::biofuels_and_waste_products$primary_solid_biofuels & 
#'                   Destination == IEATools::other_flows$residential) %>% 
#'   dplyr::select(!c(Method, Energy.type, Last.stage, Flow.aggregation.point))
complete_fu_allocation_table <- function(fu_allocation_table, 
                                         exemplar_fu_allocation_tables, 
                                         tidy_specified_iea_data, 
                                         country = IEATools::iea_cols$country, 
                                         ledger_side = IEATools::iea_cols$ledger_side,
                                         flow = IEATools::iea_cols$flow,
                                         product = IEATools::iea_cols$product,
                                         e_dot = IEATools::iea_cols$e_dot, 
                                         year = IEATools::iea_cols$year, 
                                         flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point, 
                                         supply = IEATools::ledger_sides$supply,
                                         consumption = IEATools::ledger_sides$consumption, 
                                         eiou = IEATools::tfc_compare_flows$energy_industry_own_use, 
                                         e_dot_perc = IEATools::template_cols$e_dot_perc,
                                         destination = IEATools::template_cols$destination, 
                                         machine = IEATools::template_cols$machine,
                                         eu_product = IEATools::template_cols$eu_product,
                                         ef_product = IEATools::template_cols$ef_product,
                                         maximum_values = IEATools::template_cols$maximum_values, 
                                         quantity = IEATools::template_cols$quantity,
                                         c_source = IEATools::template_cols$c_source,
                                         .values = IEATools::template_cols$.values) {
  # Find all countries in fu_allocation_table
  country_to_complete <- fu_allocation_table %>% 
    magrittr::extract2(country) %>% 
    unique()
  assertthat::assert_that(length(country_to_complete) == 1, 
                          msg = glue::glue("Found more than one country to complete in complete_fu_allocation_table(): {glue::glue_collapse(country_to_complete, sep = ', ', last = ' and ')}"))

  # Figure out the metadata columns in the fu_allocation_table
  year_columns <- year_cols(fu_allocation_table, return_names = TRUE) %>% as.character()
  meta_cols <- colnames(fu_allocation_table) %>% 
    setdiff(c(quantity, maximum_values, year_columns))
  
  # Figure out which IEA rows need to be allocated.
  # Each time we find data in an exemplar to allocate, we will subtract rows from this data frame.
  # When we get to zero rows in this data frame, we know we are done.
  iea_rows_yet_to_be_allocated <- tidy_specified_iea_data %>% 
    # Figure out the rows for which allocations are needed, based on the IEA data.
    dplyr::filter(.data[[country]] == country_to_complete) %>% 
    # Only need rows where we actually have energy to be allocated.
    dplyr::filter(e_dot != 0 & !is.na(e_dot)) %>% 
    # Only need rows that indicate final consumption (on the consumption side of the ledger) or
    # EIOU (on the supply side of the ledger).
    dplyr::filter(.data[[ledger_side]] == consumption | (.data[[ledger_side]] == supply & .data[[flow_aggregation_point]] == eiou)) %>% 
    # Keep only the columns of interest to the FU Allocation process
    dplyr::select(dplyr::any_of(c(meta_cols, year, product, flow))) %>% 
    # Rename the flow column to be "destination" to match the corresponding column in the FU Analysis tables.
    dplyr::rename(
      "{destination}" := .data[[flow]], 
      "{ef_product}" := .data[[product]]
    )
  
  # We expect exemplar_fu_allocation_tables to be a list. 
  # If it is not a list but rather something that looks like a data frame, 
  # wrap it in a list.
  if (!inherits(exemplar_fu_allocation_tables, "list") & inherits(exemplar_fu_allocation_tables, "data.frame")) {
    exemplar_fu_allocation_tables <- list(exemplar_fu_allocation_tables)
  }
  
  # Use a trick here.
  # Add the fu_allocation_table to the front of the exemplar list
  # so that it acts as the first exemplar.
  exemplar_fu_allocation_tables <- c(list(fu_allocation_table), exemplar_fu_allocation_tables)
  
  # Then eliminate all rows in the data frame to be filled from each exemplar.
  fu_allocation_table <- fu_allocation_table %>% 
    dplyr::select(!maximum_values) %>% 
    tidyr::pivot_longer(cols = year_columns, names_to = year, values_to = .values) %>% 
    dplyr::mutate(
      "{c_source}" := country_to_complete, 
      "{year}" := as.numeric(.data[[year]])
    ) %>% 
    # Eliminate all rows
    magrittr::extract(c(), )

  n_exemplars <- length(exemplar_fu_allocation_tables)
  
  for (i in 1:n_exemplars) {
    # Trim to the exemplar to essential columns and rows and make tidy.
    exemplar_info_available <- exemplar_fu_allocation_tables[[i]] %>% 
      # Eliminate e_dot and e_dot_perc rows
      dplyr::filter(!.data[[quantity]] %in% c(e_dot, e_dot_perc)) %>% 
      # Eliminate Maximum.values column
      dplyr::select(!maximum_values) %>% 
      # Make tidy.
      tidyr::pivot_longer(cols = year_columns, names_to = year, values_to = .values) %>% 
      dplyr::filter(!is.na(.data[[.values]])) %>% 
      dplyr::mutate(
        "{year}" := as.numeric(.data[[year]]), 
        "{c_source}" := .data[[country]], 
        "{country}" := country_to_complete # Pretend that the exemplar is the country we're analyzing.
      )
    
    # iea_rows_already_allocated contains the rows of final energy consumption (from the IEA data) that have already been allocated.
    # We don't need to pull data from an exemplar for these rows.
    iea_rows_already_allocated <- fu_allocation_table %>% 
      # Now keep only the columns of interest to us.
      dplyr::select(!c(quantity, machine, eu_product, .values)) %>% 
      unique()
    
    exemplar_rows_to_use <- dplyr::semi_join(exemplar_info_available, 
                                             iea_rows_yet_to_be_allocated, 
                                             # We can't join by source, because the exemplar source is different.
                                             by = colnames(iea_rows_yet_to_be_allocated) %>% setdiff(c_source)) 
    # Join the exemplar_rows_to_use to fu_allocation_table
    fu_allocation_table <- fu_allocation_table %>% 
      dplyr::bind_rows(exemplar_rows_to_use)
    
    # Figure out the rows of allocations that are still missing and, therefore, 
    # must be obtained from a forthcoming exemplar.
    iea_rows_yet_to_be_allocated <- dplyr::anti_join(iea_rows_yet_to_be_allocated, exemplar_rows_to_use, 
                                                     by = colnames(iea_rows_yet_to_be_allocated))
    
    if (nrow(iea_rows_yet_to_be_allocated) == 0) {
      break
    }
  } # End of for loop.
  
  # Figure out if we completed everything.
  # Emit a warning if all final energy was NOT allocated to FU machines.
  if (nrow(iea_rows_yet_to_be_allocated) != 0) {
    warning("Didn't complete FU Allocation table for ", country_to_complete,
            ". Returning a data frame of final energy that wasn't allocated.")
    return(iea_rows_yet_to_be_allocated)
  }

  return(fu_allocation_table)
}


#' Complete an FU Efficiency table
#' 
#' An FU (final-to-useful) Efficiency table 
#' tells the efficiency with which final energy carriers are converted to useful energy carriers by final-to-useful machines.
#' It also provides the exergy-to-energy ratio for the useful product (phi.u).
#' A template for an FU Efficiency table can be created with 
#' `eta_fu_template()`.
#' If the analyst does not know some FU efficiencies or exergy-to-energy efficiencies for a given country, 
#' this function can be used to build a complete FU Efficiency table
#' by supplying efficiencies and exergy-to-energy ratios from any number of exemplar countries.
#' 
#' `eta_fu_table` is the FU Efficiency table to be completed.
#' Any missing information is obtained from the FU Efficiency tables of the exemplar countries,
#' provided in the `exemplar_eta_fu_tables` argument.
#' Each exemplar table is interrogated in order, 
#' with data taken from the first exemplar that contains the needed information.
#' 
#' `fu_allocation_table` supplies information about which data are needed for a complete FU Efficiency table.
#' The `fu_allocation_table` data frame should be obtained from a call to `load_fu_allocation_data()`. 
#' 
#' If `eta_fu_table` can't be completed (because not enough information is available in 
#' `exemplar_eta_fu_tables`), an warning is emitted
#' and a data frame is returned containing rows from `fu_allocation_table` that were not found.
#'
#' @param eta_fu_table The efficiency table to be completed, possibly having missing incomplete rows.
#' @param exemplar_eta_fu_tables A list of efficiency tables, each queried in turn for information needed by `eta_fu_table`.
#' @param tidy_fu_allocation_table An FU (final-to-useful) allocation table from which the needed combinations of final-to-useful machines and useful products is determined.
#'                                 This data frame should be "tidy," i.e., years are pulled into a column.
#' @param country,method,energy_type,last_stage,e_dot,unit,year See `IEATools::iea_cols`.
#' @param machine,eu_product,e_dot_perc,e_dot_machine,e_dot_machine_perc,eta_fu,phi_u,quantity,maximum_values,eta_fu_phi_u_source,.values See `IEATools::template_cols`.
#'
#' @return A tidy version of `eta_fu_table` with missing values filled from `exemplar_eta_fu_tables`.
#' 
#' @export
#'
#' @examples
complete_eta_fu_table <- function(eta_fu_table, 
                                  exemplar_eta_fu_tables, 
                                  tidy_fu_allocation_table, 
                                  which_quantity = c(IEATools::template_cols$eta_fu, IEATools::template_cols$phi_u),
                                  country = IEATools::iea_cols$country,
                                  method = IEATools::iea_cols$method, 
                                  energy_type = IEATools::iea_cols$energy_type,
                                  last_stage = IEATools::iea_cols$last_stage,
                                  e_dot = IEATools::iea_cols$e_dot,
                                  unit = IEATools::iea_cols$unit,
                                  year = IEATools::iea_cols$year,
                                  machine = IEATools::template_cols$machine, 
                                  eu_product = IEATools::template_cols$eu_product, 
                                  e_dot_perc = IEATools::template_cols$e_dot_perc,
                                  e_dot_machine = IEATools::template_cols$e_dot_machine,
                                  e_dot_machine_perc = IEATools::template_cols$e_dot_machine_perc,
                                  eta_fu = IEATools::template_cols$eta_fu,
                                  phi_u = IEATools::template_cols$phi_u,
                                  quantity = IEATools::template_cols$quantity,
                                  maximum_values = IEATools::template_cols$maximum_values,
                                  eta_fu_phi_u_source = IEATools::template_cols$eta_fu_phi_u_source,
                                  .values = IEATools::template_cols$.values) {
  
  # eta_fu_table should have only 1 country in it
  country_to_complete <- eta_fu_table %>% 
    magrittr::extract2(country) %>% 
    unique()
  assertthat::assert_that(length(country_to_complete) == 1, 
                          msg = glue::glue("Found more than one country to complete in complete_eta_fu_table(): {glue::glue_collapse(country_to_complete, sep = ', ', last = ' and ')}"))
  
  fu_allocation_country <- tidy_fu_allocation_table %>% 
    magrittr::extract2(country) %>% 
    unique()
  assertthat::assert_that(length(fu_allocation_country) == 1, 
                          msg = glue::glue("Found more than one country in argument tidy_fu_allocation_table in complete_eta_fu_table(): {glue::glue_collapse(fu_allocation_country, sep = ', ', last = ' and ')}"))
  
  # Make sure the country of the eta_fu_table matches the tidy_fu_allocation_table
  assertthat::assert_that(fu_allocation_country == country_to_complete, 
                          msg = paste0("The country of eta_fu_table (", country_to_complete, 
                                       ") is not the same as the country in the tidy_fu_allocation_table (", fu_allocation_country, 
                                       "). They must match."))
  
  # Figure out the metadata columns in the eta_fu_table
  year_columns <- year_cols(eta_fu_table, return_names = TRUE) %>% as.character()
  meta_cols <- colnames(eta_fu_table) %>% 
    setdiff(c(quantity, maximum_values, year_columns))
  
  # Extract machines and products for this country from the tidy_fu_allocation_table
  # fu_allocation_years <- year_cols(tidy_fu_allocation_table, return_names = TRUE)
  machines_that_need_etas <- tidy_fu_allocation_table %>% 
    dplyr::filter(.data[[quantity]] != e_dot & .data[[quantity]] != e_dot_perc) %>% 
    dplyr::select(!dplyr::any_of(maximum_values)) %>% 
    dplyr::filter(!is.na(.values)) %>% 
    dplyr::select(dplyr::any_of(c(meta_cols, year))) %>% 
    unique() %>% 
    dplyr::mutate(
      "{year}" := as.numeric(.data[[year]])
    )
  # Add the quantities that are needed.
  machines_that_need_etas <- lapply(which_quantity, FUN = function(q) {
    machines_that_need_etas %>% 
      dplyr::mutate(
        "{quantity}" := q
      )
  }) %>% 
    dplyr::bind_rows()
  
  # We expect exemplar_eta_fu_tables to be a list. 
  # If it is not a list but rather something that looks like a data frame, 
  # wrap it in a list.
  if (!inherits(exemplar_eta_fu_tables, "list") & inherits(exemplar_eta_fu_tables, "data.frame")) {
    exemplar_eta_fu_tables <- list(exemplar_eta_fu_tables)
  }
  
  # Use a trick here.
  # Add the eta_fu_table to the front of the exemplar list
  # so that it acts as the first exemplar.
  exemplar_eta_fu_tables <- c(list(eta_fu_table), exemplar_eta_fu_tables)
  
  # Then eliminate all rows in the data frame to be filled from each exemplar.
  eta_fu_table <- eta_fu_table %>% 
    dplyr::select(!maximum_values) %>% 
    tidyr::pivot_longer(cols = year_columns, names_to = year, values_to = .values) %>% 
    dplyr::mutate(
      "{eta_fu_phi_u_source}" := country_to_complete, 
      "{year}" := as.numeric(.data[[year]])
    ) %>% 
    # Eliminate all rows
    magrittr::extract(c(), )
  
  n_exemplars <- length(exemplar_eta_fu_tables)
  
  for (i in 1:n_exemplars) {
    
    # Trim to the exemplar to essential columns and rows and make tidy.
    exemplar_info_available <- exemplar_eta_fu_tables[[i]] %>% 
      # Eliminate e_dot_machine and e_dot_machine_perc rows
      dplyr::filter(!.data[[quantity]] %in% c(e_dot_machine, e_dot_machine_perc)) %>% 
      # Eliminate Maximum.values column
      dplyr::select(!maximum_values) %>% 
      # Make tidy.
      tidyr::pivot_longer(cols = year_columns, names_to = year, values_to = .values) %>% 
      dplyr::filter(!is.na(.data[[.values]]), .data[[quantity]] %in% which_quantity) %>% 
      dplyr::mutate(
        "{year}" := as.numeric(.data[[year]]), 
        "{eta_fu_phi_u_source}" := .data[[country]], 
        "{country}" := country_to_complete # Pretend that the exemplar is the country we're analyzing.
      )
    
    # machines_already_specified contains the rows of machines (from the FU allocation data) that have already 
    # had efficiencies specified.
    # We don't need to pull data from an exemplar for these rows.
    machines_already_specified <- eta_fu_table %>% 
      # Now keep only the columns of interest to us.
      dplyr::select(dplyr::any_of(c(meta_cols, quantity, year, eta_fu_phi_u_source))) %>% 
      unique()
    
    exemplar_rows_to_use <- dplyr::semi_join(exemplar_info_available, 
                                             machines_that_need_etas, 
                                             # We can't join by source, because the exemplar source is different.
                                             by = colnames(machines_that_need_etas) %>% setdiff(eta_fu_phi_u_source)) 
    # Join the exemplar_rows_to_use to eta_fu_table
    eta_fu_table <- eta_fu_table %>% 
      dplyr::bind_rows(exemplar_rows_to_use)
    
    # Figure out the rows of efficiencies that are still missing and, therefore, 
    # must be obtained from a forthcoming exemplar.
    machines_that_need_etas <- dplyr::anti_join(machines_that_need_etas, exemplar_rows_to_use, 
                                                by = colnames(machines_that_need_etas))
    
    if (nrow(machines_that_need_etas) == 0) {
      break
    }
    
  } # End of for loop.
  
  # Figure out if we completed everything.
  # Emit a warning if all final energy was NOT allocated to FU machines.
  if (nrow(machines_that_need_etas) != 0) {
    warning("Didn't complete eta FU table for ", country_to_complete,
            ". Returning a data frame of machines for which an efficiency wasn't available.")
    return(machines_that_need_etas)
  }
  
  return(eta_fu_table)
}



