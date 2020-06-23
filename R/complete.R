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
#' @param country,method,energy_type,last_stage,ledger_side,flow,product,unit,e_dot,year,flow_aggregation_point See `IEATools::ieacols`.
#' @param supply,consumption See `IEATools::ledger_sides`.
#' @param eiou See `IEATools::tfc_compar_flows`.
#' @param e_dot_perc,destination,machine,eu_product,ef_product,max_vals,quantity See `IEATools::template_cols`.
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
                                         method = IEATools::iea_cols$method,
                                         energy_type = IEATools::iea_cols$energy_type,
                                         last_stage = IEATools::iea_cols$last_stage,
                                         ledger_side = IEATools::iea_cols$ledger_side,
                                         flow = IEATools::iea_cols$flow,
                                         product = IEATools::iea_cols$product,
                                         unit = IEATools::iea_cols$unit,
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
                                         max_vals = IEATools::template_cols$maximum_values, 
                                         quantity = IEATools::template_cols$quantity,
                                         c_source = IEATools::template_cols$c_source,
                                         .values = IEATools::template_cols$.values) {
  # Find all countries in fu_allocation_table
  country_to_complete <- fu_allocation_table %>% 
    magrittr::extract2(country) %>% 
    unique()
  assertthat::assert_that(length(country_to_complete) == 1, 
                          msg = glue::glue("Found more than one country to complete in complete_fu_allocation_table(): {glue::glue_collapse(country_to_complete, sep = ', ', last = ' and ')}"))
  
  # Figure out the year columns in the fu_allocation_table
  year_cols <- c(max_vals, year_cols(fu_allocation_table, return_names = TRUE) %>% as.character())
  
  # Get all the IEA data for these countries
  country_iea_data <- tidy_specified_iea_data %>% 
    dplyr::filter(.data[[country]] == country_to_complete)
  # Figure out the rows for which allocations are needed, based on the IEA data.
  allocation_rows_needed <- country_iea_data %>% 
    # Only need rows where we actually have energy to be allocated.
    dplyr::filter(e_dot != 0 & !is.na(e_dot)) %>% 
    # Only need rows that indicate final consumption (on the consumption side of the ledger) or
    # EIOU (on the supply side of the ledger).
    dplyr::filter(.data[[ledger_side]] == consumption | (.data[[ledger_side]] == supply & .data[[flow_aggregation_point]] == eiou)) %>% 
    # Keep only the columns of interest to the FU Allocation process
    dplyr::select(country, method, energy_type, last_stage, ledger_side, flow_aggregation_point, unit, product, flow, year) %>% 
    # Rename the flow column to be "destination" to match the corresponding column in the FU Analysis tables.
    dplyr::rename(
      "{destination}" := .data[[flow]], 
      "{ef_product}" := .data[[product]]
    )
  
  # Figure out the allocations that are available from fu_allocation_table.
  fu_allocation_data_available <- fu_allocations_available(fu_allocation_table, year = year, 
                                                           .values = .values, max_vals = max_vals, quantity = quantity, 
                                                           e_dot = e_dot, e_dot_perc = e_dot_perc, country = country, c_source = c_source)
  
  # allocated_rows contains the rows of final energy consumption (from the IEA data) that have already been allocated.
  # We don't need to pull data from an exemplar for these rows.
  allocated_rows <- find_allocated_rows(fu_allocation_data_available, quantity, machine, eu_product, .values)
  
  # Figure out the rows of allocations that are missing and, therefore, 
  # must be obtained from the exemplar country FU Allocations.
  rows_to_get_elsewhere <- dplyr::anti_join(allocation_rows_needed, allocated_rows, by = colnames(allocation_rows_needed))
  
  # We expect exemplar_fu_allocation_tables to be a list. 
  # If it is not a list but rather something that looks like a data frame, 
  # wrap it in a list.
  if (!inherits(exemplar_fu_allocation_tables, "list") & inherits(exemplar_fu_allocation_tables, "data.frame")) {
    exemplar_fu_allocation_tables <- list(exemplar_fu_allocation_tables)
  }
  n_exemplars <- length(exemplar_fu_allocation_tables)
  
  # Look in exemplar_fu_allocation_tables for missing rows.
  # Search until we have information for each of the rows_to_get_elsewhere.
  for (i in 1:n_exemplars) {
    if (nrow(rows_to_get_elsewhere) == 0) {
      # If we don't need to get any rows, we can stop looping now.
      break
    }
    
    exemplar <- exemplar_fu_allocation_tables[[i]]
    # Make sure there is only 1 country in this exemplar.
    exemplar_country <- exemplar %>% magrittr::extract2(country) %>% unique()
    assertthat::assert_that(length(exemplar_country) == 1, 
                            msg = paste0("Found more than one country in exemplar: ", exemplar_country))
    
    # Figure out which missing rows this exemplar can contribute 
    exemplar_info_available <- fu_allocations_available(exemplar, year = year, 
                                                        .values = .values, max_vals = max_vals, quantity = quantity, 
                                                        e_dot = e_dot, e_dot_perc = e_dot_perc, country = country, c_source = c_source) %>% 
      # Get rid of the country column, because we don't want two country columns in the rows_to_use data frame.
      dplyr::select(!country)
    
    # We can't join by country or source, because the exemplar data frame doesn't have those columns.
    exemplar_rows_to_use <- dplyr::semi_join(exemplar_info_available, 
                                             rows_to_get_elsewhere, 
                                             by = colnames(rows_to_get_elsewhere) %>% 
                                               setdiff(country) %>% 
                                               setdiff(c_source)) %>% 
      dplyr::mutate(
        # Add the country column
        "{country}" := country_to_complete, 
      )
    # Join the exemplar_rows_to_use to fu_allocation_data_available
    fu_allocation_data_available <- fu_allocation_data_available %>% 
      dplyr::bind_rows(exemplar_rows_to_use)
    allocated_rows <- find_allocated_rows(fu_allocation_data_available, quantity, machine, eu_product, .values)
    rows_to_get_elsewhere <- dplyr::anti_join(allocation_rows_needed, allocated_rows, by = colnames(allocation_rows_needed))
  } # End of big for loop.
  
  # Figure out if we completed everything.
  # Emit a warning if all final energy was NOT allocated to FU machines.
  if (nrow(rows_to_get_elsewhere) != 0) {
    warning("Didn't complete FU Allocation table for ", country_to_complete,
            ". Returning a data frame of final energy that wasn't allocated.")
    return(rows_to_get_elsewhere)
  }
  
  # Spread (pivot_wider) to put years in columns before returning.
  fu_allocation_data_available %>% 
    tidyr::pivot_wider(names_from = year, values_from = .values)
}


#' Determine which final energy uses are covered in an FU Allocation table
#' 
#' This is convenience function that reduces duplicated code in `complete_fu_allocation_table()`.
#' As such, it is not exported.
#'
#' @param .fu_allocation_table The FU Allocation table from which allocations are to be determined
#' @param max_vals The name of maximum value rows in the `fu_allocation_table`.
#' @param year The Year column in a tidy version of the `fu_allocation_table`.
#' @param .values The name of the values column in a tidy versino of the `fu_allocation_table`.
#' @param quantity The name of the quantity column in the `fu_allocation_table`.
#' @param e_dot The name of the E.dot rows in the `fu_allocation_table`.
#' @param e_dot_perc The name of the E.dot percentage rows in the `fu_allocation_table`.
#' @param country The name of the Country column in the `fu_allocation_table`.
#' @param c_source The name of the C_source column in the tidy `fu_allocation_table`.
#'
#' @return A data frame containing final energy consumption that has been allocated in the `fu_allocation_table`
fu_allocations_available <- function(.fu_allocation_table, max_vals, year, .values, 
                                     quantity, e_dot, e_dot_perc, country, c_source) {
  year_columns <- c(max_vals, year_cols(.fu_allocation_table, return_names = TRUE) %>% 
                      as.character())
  .fu_allocation_table %>% 
    # Pivot the FU allocation table to a tidy data frame.
    # tidy_fu_allocation_table() %>% 
    tidyr::pivot_longer(cols = year_columns, names_to = year, values_to = .values) %>% 
    # Get rid of rows we don't want.
    dplyr::filter(
      # Rows where the year column has "Maximum.values" in it.
      .data[[year]] != max_vals, 
      # Rows where the value column has NA are rows where we don't have allocation data.
      !is.na(.data[[.values]]), 
      # Rows where quantity is E.dot or E.dot [%] aren't allocation rows
      !.data[[quantity]] %in% c(e_dot, e_dot_perc)
    ) %>% 
    dplyr::mutate(
      "{year}" := as.numeric(.data[[year]]), 
      "{c_source}" := .data[[country]]
    )
}

#' Find rows of final energy consumption already allocated to final-to-useful machines.
#' 
#' This is convenience function that reduces duplicated code in `complete_fu_allocation_table()`.
#' As such, it is not exported.
#'
#' @param fu_allocation_table A tidy final-to-useful allocation table.
#' @param quantity The name of the Quantity column.
#' @param machine The name of the Machine column.
#' @param eu_product The name of the Eu.product column.
#' @param .values The name of the .valuse column.
#'
#' @return A data frame containing unique rows of final energy that are allocated in `fu_allocation_table`.
find_allocated_rows <- function(fu_allocation_table, quantity, machine, eu_product, .values) {
  fu_allocation_table %>% 
    # Now keep only the columns of interest to us.
    dplyr::select(!c(quantity, machine, eu_product, .values)) %>% 
    unique()
}


#' Title
#'
#' @param eta_fu_table 
#' @param exemplar_eta_fu_tables 
#' @param fu_allocation_table 
#' @param country 
#' @param method 
#' @param energy_type 
#' @param last_stage 
#' @param unit 
#' @param machine 
#' @param eu_product 
#' @param e_dot 
#' @param e_dot_perc 
#' @param year 
#' @param .values 
#'
#' @return
#' @export
#'
#' @examples
complete_eta_fu_table <- function(eta_fu_table, exemplar_eta_fu_tables, fu_allocation_table, 
                                  country = IEATools::iea_cols$country,
                                  method = IEATools::iea_cols$method, 
                                  energy_type = IEATools::iea_cols$energy_type,
                                  last_stage = IEATools::iea_cols$last_stage,
                                  unit = IEATools::iea_cols$unit,
                                  machine = IEATools::template_cols$machine, 
                                  eu_product = IEATools::template_cols$eu_product, 
                                  e_dot = IEATools::iea_cols$e_dot,
                                  e_dot_perc = IEATools::template_cols$e_dot_perc,
                                  e_dot_machine = IEATools::template_cols$e_dot_machine,
                                  e_dot_machine_perc = IEATools::template_cols$e_dot_machine_perc,
                                  eta_fu = IEATools::template_cols$eta_fu, 
                                  year = IEATools::iea_cols$year,
                                  quantity = IEATools::template_cols$quantity,
                                  maximum_values = IEATools::template_cols$maximum_values,
                                  .values = IEATools::template_cols$.values) {
  
  # eta_fu_table should have only 1 country in it
  country_to_complete <- eta_fu_table %>% 
    magrittr::extract2(country) %>% 
    unique()
  assertthat::assert_that(length(country_to_complete) == 1, 
                          msg = glue::glue("Found more than one country to complete in complete_fu_allocation_table(): {glue::glue_collapse(country_to_complete, sep = ', ', last = ' and ')}"))
  
  # Make sure the country of the eta_ful_table matches the fu_allocation_table
  assertthat::assert_that(fu_allocation_table %>% magrittr::extract2(country) %>% unique() == country_to_complete, 
                          msg = paste0("The country of eta_fu_table (", country_to_complete, 
                                       ") is not the same as the country in the fu_allcoation_table (", fu_allocation_table, 
                                       "). They must match."))

  # Extract machines and products for this country from the fu_allocation_table
  # NEED TO MAKE IT TIDY!
  fu_allocation_years <- year_cols(fu_allocation_table, return_names = TRUE)
  eta_fu_data_needed <- fu_allocation_table %>% 
    dplyr::filter(.data[[country]] == country_to_complete) %>% 
    dplyr::filter(.data[[quantity]] != e_dot & .data[[quantity]] != e_dot_perc) %>% 
    dplyr::select(!maximum_values) %>% 
    tidyr::pivot_longer(names_to = year, values_to = .values, fu_allocation_years) %>% 
    dplyr::filter(!is.na(.values)) %>% 
    dplyr::select(country, method, energy_type, last_stage, unit, machine, eu_product, year) %>% 
    unique()
  
  eta_fu_years <- year_cols(eta_fu_table, return_names = TRUE)
  eta_fu_data_provided <- eta_fu_table %>% 
    dplyr::filter(.data[[quantity]] != e_dot_machine, .data[[quantity]] != e_dot_machine_perc, .data[[quantity]] == eta_fu) %>% 
    dplyr::select(!maximum_values) %>% 
    tidyr::pivot_longer(names_to = year, values_to = .values, eta_fu_years) %>% 
    dplyr::filter(!is.na(.data[[.values]])) %>% 
    dplyr::select(!c(quantity, .values)) %>% 
    unique()
  
  # Find out which eta_fu data are missing
  eta_fu_data_missing <- dplyr::anti_join(eta_fu_data_needed, eta_fu_data_provided, by = colnames(eta_fu_data_needed))
  
  # phi_u_data_available <- 
    
}