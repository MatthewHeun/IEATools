#' Extract a data frame of electricity and heat output information
#'
#' @param .iea_file The IEA data file to read
#' @param electricity_output_prefix The prefix for electricity output rows.
#'                                  Default is `IEATools::elec_heat_output$electricity_output_prefix`.
#' @param heat_output_prefix The prefix for heat output rows.
#'                           Default is `IEATools::elec_heat_output$heat_output_prefix`.
#' @param country,year,flow,product,e_dot,unit_colname See `IEATools::iea_cols`.
#' @param input_colname,output_colname,output_machine_delimiter See `IEATools::elec_heat_output`.
#' @param machine_colname See `IEATools::template_cols`.
#' @param unit The desired output unit. Default is "TJ". Best not to change this.
#' @param electricity A string that defines electricity output. 
#'                    Default is "Electricity".
#' @param total See `IEATools::memo_aggregation_product_prefixes`.
#' @param memo See `IEATools::memo_aggregation_flow_prefixes`.
#'
#' @return A data frame of electricity and heat output data.
#'
#' @export
#'
#' @examples
#' sample_iea_data_path() |> 
#'   load_electricity_heat_output()
load_electricity_heat_output <- function(.iea_file = NULL, 
                                         electricity_output_prefix = IEATools::elec_heat_output$electricity_output_prefix, 
                                         heat_output_prefix = IEATools::elec_heat_output$heat_output_prefix,
                                         country = IEATools::iea_cols$country,
                                         year = IEATools::iea_cols$year,
                                         flow = IEATools::iea_cols$flow, 
                                         product = IEATools::iea_cols$product,
                                         e_dot = IEATools::iea_cols$e_dot,
                                         input_colname = IEATools::elec_heat_output$input_product,
                                         output_colname = IEATools::elec_heat_output$output_product, 
                                         machine_colname = IEATools::template_cols$machine, 
                                         unit_colname = IEATools::iea_cols$unit,
                                         unit = "TJ",
                                         electricity = "Electricity", 
                                         output_machine_delimiter = IEATools::elec_heat_output$output_machine_delimiter, 
                                         total = IEATools::memo_aggregation_product_prefixes$total, 
                                         memo = IEATools::memo_aggregation_flow_prefixes$memo) {
  
  iea_data <- .iea_file |>
    iea_df() |> 
    rename_iea_df_cols() |> 
    clean_iea_whitespace() |> 
    use_iso_countries()
  elec_heat_data <- iea_data |> 
    dplyr::filter((startsWith(.data[[flow]], electricity_output_prefix) | 
                     startsWith(.data[[flow]], heat_output_prefix)), 
                  .data[[product]] != total, 
                  !startsWith(.data[[product]], memo)) |> 
    tidyr::separate_wider_delim(dplyr::all_of(flow), delim = "-", names = c(output_colname, machine_colname)) |> 
    dplyr::mutate(
      # Capitalize first letter of machine name.
      "{machine_colname}" := stringr::str_to_sentence(.data[[machine_colname]]), 
      # Select only the first word in the output column, either "Electricity" or "Heat"
      "{output_colname}" := stringr::word(.data[[output_colname]], 1),
      # Fix chp
      "{machine_colname}" := stringr::str_replace(.data[[machine_colname]], "chp", "CHP")
    ) |> 
    dplyr::rename(
      "{input_colname}" := dplyr::all_of(product)
    ) |> 
    tidyr::pivot_longer(cols = !dplyr::all_of(c(country, output_colname, machine_colname, input_colname)),
                        names_to = year,
                        values_to = e_dot) |> 
    dplyr::filter(.data[[e_dot]] != 0) |> 
    dplyr::mutate(
      # Convert GWh to TJ for electricity only.
      "{e_dot}" := dplyr::case_when(
        # Convert for electricity only.
        .data[[output_colname]] == electricity ~ .data[[e_dot]] * 3.6, 
        # Heat is already in TJ.
        TRUE ~ .data[[e_dot]]
      ),
      # Set the unit string.
      "{unit_colname}" := unit, 
      # Year should be numeric
      "{year}" := as.numeric(.data[[year]])
    ) |> 
    # Update the order of columns.
    dplyr::select(dplyr::all_of(c(country, year, input_colname, machine_colname, output_colname, e_dot, unit_colname)))
  
  return(elec_heat_data)
}