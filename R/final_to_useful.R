


#' Create an allocation matrix (`C`) from an allocation table
#' 
#' To create an allocation matrix (`C`), 
#' rownames are taken from the `Ef.product` and `Destination` columns, and
#' colnames are taken from the `Machine` and `Eu.product` columns.
#' 
#' The `.fu_alocations` data frame is first made tidy by gathering all years.
#'
#' @param .fu_allocations 
#'
#' @return a tidy data frame with metadata columns (and year) along with a `C` column.
#' @export
#'
#' @examples
make_C_mats <- function(.fu_allocations, 
                   country = IEATools::iea_cols$country,
                   method = IEATools::iea_cols$method,
                   energy_type = IEATools::iea_cols$energy_type,
                   last_stage = IEATools::iea_cols$last_stage,
                   ledger_side = IEATools::iea_cols$ledger_side,
                   flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                   e_dot = IEATools::iea_cols$e_dot,
                   year = IEATools::iea_cols$year,
                   supply = IEATools::ledger_sides$supply,
                   consumption = IEATools::ledger_sides$consumption,
                   quantity = IEATools::template_cols$quantity,
                   machine = IEATools::template_cols$machine,
                   eu_product = IEATools::template_cols$eu_product,
                   e_dot_perc = IEATools::template_cols$e_dot_perc,
                   maximum_values = IEATools::template_cols$maximum_values,
                   matnames = IEATools::mat_meta_cols$matnames,
                   # Names of output matrices
                   C_eiou = "C_EIOU",
                   C_Y = "C_Y",
                   # Temporary column name
                   .C = ".C") {

  cleaned <- .fu_allocations %>% 
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
    # Get rid of rows where machine and eu_product are NA. No data has been provided here.
    dplyr::filter(! (is.na(.data[[machine]]) & is.na(.data[[eu_product]])) )
  year_names <- year_cols(cleaned, return_names = TRUE)
  gathered <- cleaned %>% 
    # Gather to put years in a column
    tidyr::pivot_longer(year_names, names_to = year, values_to = .C) %>% 
    # Eliminate rows where C is NA. They came from places where data are not available.
    dplyr::filter(!is.na(.data[[.C]])) %>% 
    # Group by ledger_side so that we create one set of C matrices for EIOU flows and another for consumption flows.
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[matnames]])
    
  
    
  # Next step is to make matrices in this data frame.
  
    
  
}