#' Extract a data frame of electricity and heat output information
#'
#' @param .iea_file The IEA data file to read
#' @param electricity_output_prefix The prefix for electricity output rows.
#'                                  Default is `IEATools::elec_heat_output$electricity_output_prefix`.
#' @param heat_output_prefix The prefix for heat output rows.
#'                           Default is `IEATools::elec_heat_output$heat_output_prefix`.
#'
#' @return A data frame of electricity and heat output data.
#'
#' @export
#'
#' @examples
electricity_heat_output <- function(.iea_file = NULL, 
                                    electricity_output_prefix = IEATools::elec_heat_output$electricity_output_prefix, 
                                    heat_output_prefix = IEATools::elec_heat_output$heat_output_prefix,
                                    country = IEATools::iea_cols$country,
                                    year = IEATools::iea_cols$year,
                                    flow = IEATools::iea_cols$flow, 
                                    product = IEATools::iea_cols$product,
                                    e_dot = IEATools::iea_cols$e_dot,
                                    output_colname = template_cols$ef_product, 
                                    machine_colname = IEATools::template_cols$machine, 
                                    output_machine_delimiter = IEATools::elec_heat_output$output_machine_delimiter) {

  iea_data <- .iea_file |>
    iea_df() |> 
    rename_iea_df_cols() |> 
    clean_iea_whitespace() |> 
    use_iso_countries()
  elec_heat_data <- dplyr::filter(iea_data, 
                                  grepl(electricity_output_prefix, iea_data[[flow]], fixed = TRUE) | 
                                    grepl(heat_output_prefix, iea_data[[flow]], fixed = TRUE)) |> 
    tidyr::separate_wider_delim(dplyr::all_of(flow), delim = "-", names = c(output_colname, machine_colname)) |> 
    dplyr::mutate(
      "{machine_colname}" := stringr::str_to_sentence(.data[[machine_colname]])
    ) |> 
    tidyr::pivot_longer(cols = !dplyr::any_of(c(country, output_colname, machine_colname, product)),
                        names_to = year,
                        values_to = e_dot) |> 
    dplyr::filter(.data[[e_dot]] != 0)
  
    
  return(elec_heat_data)
}