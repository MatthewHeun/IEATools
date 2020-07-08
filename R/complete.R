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
  iea_rows_that_must_be_allocated <- tidy_specified_iea_data %>% 
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
  
  # iea_rows_yet_to_be_allocated is our running accounting of remaining rows.
  iea_rows_yet_to_be_allocated <- iea_rows_that_must_be_allocated
  
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

    # Check to see if we have allocated everything
    done <- fu_allocation_table_completed(fu_allocation_table, iea_rows_that_must_be_allocated)
    if (done) {
      break
    }
    
  } # End of for loop.
  
  if (!done) {
    # Emit a warning if all final energy was NOT allocated to FU machines by any of the exemplars..
    warning("Didn't complete FU Allocation table for ", country_to_complete,
            ". Returning a data frame of final energy that wasn't allocated.")
    return(attr(done, "unallocated_rows"))
  }
  
  # If we get here, everything was allocated, so return the fu_allocation_table.
  return(fu_allocation_table)
}


#' Tell whether a final-to-useful allocation table has been completed
#' 
#' A final-to-useful allocation table is complete iff all of the final energy flows for a country 
#' are routed to a final-to-useful machine for each year in which those final energy flows exist.
#' Also, all routes need to add to 100%.
#'
#' @param fu_allocation_table The final-to-useful allocation table whose completeness is to be determined.
#' @param specified_iea_data An IEA data frame from which final energy flows are gleaned.
#'                           This data frame should be generated by `specify_all()`.
#' @param year,ledger_side,e_dot,flow,product,flow_aggregation_point See `IEATools::iea_cols`.
#' @param consumption See `IEATools::ledger_sides`.
#' @param eiou See `IEATools::tfc_compare_flows`.
#' @param maximum_values,quantity,.values,ef_product,destination,machine,e_u_product,e_dot_perc,C_source
#'        See `IEATools::template_cols`.
#'
#' @return A boolean telling whether `fu_allocation_table` is complete. 
#'         If `FALSE`, a data frame of IEA final energy rows that have not been allocated
#'         is stored in the "unallocated_rows" attribute of the return value. 
#'         Retrieve with `attr(done, "unallocated_rows")`
#'         if the result of this function is assigned to the variable `done`.
#' 
#' @export
#'
#' @examples 
#' iea_data <- load_tidy_iea_df() %>% 
#'   specify_all()
#' fu_allocations <- load_fu_allocation_data()
#' fu_allocation_table_completed(fu_allocations, iea_data)
fu_allocation_table_completed <- function(fu_allocation_table, 
                                          specified_iea_data, 
                                          year = IEATools::iea_cols$year, 
                                          ledger_side = IEATools::iea_cols$ledger_side,
                                          e_dot = IEATools::iea_cols$e_dot, 
                                          flow = IEATools::iea_cols$flow,
                                          product = IEATools::iea_cols$product,
                                          flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                                          consumption = IEATools::ledger_sides$consumption,
                                          eiou = IEATools::tfc_compare_flows$energy_industry_own_use,
                                          maximum_values = IEATools::template_cols$maximum_values, 
                                          quantity = IEATools::template_cols$quantity, 
                                          .values = IEATools::template_cols$.values,
                                          C_source = IEATools::template_cols$c_source,
                                          ef_product = IEATools::template_cols$ef_product,
                                          destination = IEATools::template_cols$destination,
                                          machine = IEATools::template_cols$machine,
                                          e_u_product = IEATools::template_cols$eu_product,
                                          e_dot_perc = IEATools::template_cols$e_dot_perc) {
  # Accept a non-tidy specified_iea_data frame.
  iea_year_columns <- specified_iea_data %>% 
    year_cols(return_names = TRUE)
  if (length(iea_year_columns) > 0) {
    specified_iea_data <- specified_iea_data %>% 
      tidyr::pivot_longer(cols = iea_year_columns, names_to = year, values_to = e_dot) %>% 
      dplyr::filter(!is.na(.data[[e_dot]])) %>% 
      dplyr::mutate(
        "{year}" := as.numeric(.data[[year]])
      )
  }
  # Figure out which rows have been allocated in each year
  # Accept a non-tidy fu_allocation_table if it arrives.
  fu_year_columns <- fu_allocation_table %>% 
    year_cols(return_names = TRUE)
  if (length(fu_year_columns) > 0) {
    fu_allocation_table <- fu_allocation_table %>% 
      dplyr::select(!maximum_values) %>% 
      dplyr::filter(!.data[[quantity]] %in% c(e_dot, e_dot_perc)) %>% 
      tidyr::pivot_longer(cols = fu_year_columns, names_to = year, values_to = .values) %>% 
      dplyr::filter(!is.na(.data[[.values]])) %>% 
      dplyr::mutate(
        "{year}" := as.numeric(.data[[year]])
      )
  }
  # Eliminate the quantity, Machine, and Eu.product columns and summarize.
  # We should get all 1's.
  allocation_sums <- fu_allocation_table %>% 
    dplyr::select(!c(quantity, machine, e_u_product)) %>%
    matsindf::group_by_everything_except(.values) %>% 
    dplyr::summarise(
      "{.values}" := sum(.data[[.values]])
    )
  if (!all(allocation_sums[[.values]] == 1)) {
    return(FALSE)
  }
  # Figure out the rows of final energy that need to be allocated in each year.
  rows_to_be_allocated <- specified_iea_data %>% 
    dplyr::filter(.data[[ledger_side]] == consumption | .data[[flow_aggregation_point]] == eiou) %>% 
    dplyr::mutate(
      "{e_dot}" := NULL
    )
  # Rename some columns if necessary
  if (product %in% colnames(rows_to_be_allocated)) {
    rows_to_be_allocated <- rows_to_be_allocated %>% 
      dplyr::rename(
        "{ef_product}" := .data[[product]]
      )
  }
  if (flow %in% colnames(rows_to_be_allocated)) {
    rows_to_be_allocated <- rows_to_be_allocated %>% 
      dplyr::rename(
        "{destination}" := .data[[flow]]
      )
  }
  # Now check that all rows that need to be allocated have been allocated.
  # Get rid of some columns that might conflict.
  allocated_rows <- allocation_sums %>% 
    dplyr::mutate(
      "{.values}" := NULL,
      "{C_source}" := NULL
    )
  unallocated_rows <- dplyr::anti_join(rows_to_be_allocated, allocated_rows, by = colnames(allocated_rows))
  if (nrow(unallocated_rows) > 0) {
    out <- FALSE
    # Store the unallocated rows as an attribute on out so others can figure out what went wrong.
    attr(out, "unallocated_rows") <- unallocated_rows
    return(out)
  }
  return(TRUE)
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
#' @param which_quantity A vector of quantities to be completed in the eta_FU table.
#'                       Default is `c(IEATools::template_cols$eta_fu, IEATools::template_cols$phi_u)`.
#' @param country,method,energy_type,last_stage,e_dot,unit,year See `IEATools::iea_cols`.
#' @param machine,eu_product,e_dot_perc,e_dot_machine,e_dot_machine_perc,eta_fu,phi_u,quantity,maximum_values,eta_fu_phi_u_source,.values See `IEATools::template_cols`.
#'
#' @return A tidy version of `eta_fu_table` with missing values filled from `exemplar_eta_fu_tables`.
#' 
#' @export
#'
#' @examples
#' # Load efficiency tables
#' eta_fu_table <- load_eta_fu_data()
#' eta_fu_table_GHA <- eta_fu_table %>% 
#'   dplyr::filter(.data[[IEATools::iea_cols$country]] == "GHA")
#' eta_fu_table_ZAF <- eta_fu_table %>% 
#'   dplyr::filter(.data[[IEATools::iea_cols$country]] == "ZAF")
#' # Load an FU Allocation table
#' fu_allocation_table <- load_fu_allocation_data() %>% 
#'   dplyr::select(!IEATools::template_cols$maximum_values) %>% 
#'   dplyr::filter(!.data[[IEATools::template_cols$quantity]] 
#'     %in% c(IEATools::iea_cols$e_dot, 
#'            IEATools::template_cols$e_dot_perc)) %>% 
#'   # Make it tidy
#'   tidyr::pivot_longer(cols = year_cols(., return_names = TRUE), 
#'                       names_to = IEATools::iea_cols$year,
#'                       values_to = IEATools::template_cols$.values) %>% 
#'   dplyr::filter(!is.na(.data[[IEATools::template_cols$.values]]))
#' fu_allocation_table_GHA <- fu_allocation_table %>% 
#'   dplyr::filter(.data[[IEATools::iea_cols$country]] == "GHA") 
#' # Eliminate 2 machines (Automobiles and Irons) from GHA and 
#' # see if their efficiencies get picked up from ZAF and World.
#' eta_fu_table_GHA_incomplete <- eta_fu_table_GHA %>% 
#'   dplyr::filter(.data[[IEATools::template_cols$machine]] != "Automobiles", 
#'                 .data[[IEATools::template_cols$machine]] != "Irons")
#' # Make exemplar tables from ZAF.
#' # The first exemplar (ZAF) will have Automobiles but not Irons.
#' exemplar_ZAF <- eta_fu_table_ZAF %>% 
#'   dplyr::filter(.data[[IEATools::template_cols$machine]] != "Irons")
#' # The second exemplar (World) will have Irons, but not Automobiles.
#' exemplar_World <- eta_fu_table_ZAF %>% 
#'   dplyr::filter(.data[[IEATools::template_cols$machine]] != "Automobiles") %>% 
#'   dplyr::mutate(
#'     "{IEATools::iea_cols$country}" := "World"
#'   )
#' # Now call the completion function to pick up Automobiles from ZAF and Irons from World
#' completed <- complete_eta_fu_table(
#'                eta_fu_table = eta_fu_table_GHA_incomplete,
#'                exemplar_eta_fu_tables = list(exemplar_ZAF, exemplar_World), 
#'                tidy_fu_allocation_table = fu_allocation_table_GHA)
#' # Check that we got Automobiles from ZAF
#' completed %>% 
#'   dplyr::filter(.data[[IEATools::template_cols$machine]] == "Automobiles", 
#'                 .data[[IEATools::template_cols$eta_fu_phi_u_source]] == "ZAF")
#' # Check that we got Irons from World
#' completed %>% 
#'   dplyr::filter(.data[[IEATools::template_cols$machine]] == "Irons", 
#'                 .data[[IEATools::template_cols$eta_fu_phi_u_source]] == "World")
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



