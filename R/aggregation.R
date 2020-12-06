#' Loads region aggregation table
#' 
#' This functions loads a user-defined aggregation table that re-routes each IEA region to a user-defined region.
#' By default, the concordance matrix used re-routes IEA regions to Exiobase regions (for 2019 version of IEA data).
#' See details for more information.
#' 
#' The aggregation table must have a column that identifies the IEA regions to be re-routed (default is "IEA_regions"),
#' and a second column that identifies the new regions to IEA regions are re-routed (default is "Destination_regions"). 
#' There is no need to include all IEA regions; 
#' those that are not included will be removed when calling the `aggregate_regions()` function.
#' IEA regions that are rerouted to "NA" or to an empty value are also removed when calling the `aggregate_regions()` function.
#' 
#' Note that the default IEA to Exiobase mapping are only valid for the time periods relevant to Exiobase (from 1995 onward). 
#' Using it for previous years will lead to a situation where the energy consumption of particular countries, 
#' like Former Soviet Union or Former Yugoslavia, disappear from the data frame (because they do not correspond to an Exiobase region.
#' 
#' @param file_path The path of the file (xlsx file) to be loaded. The default path leads to an aggregation table converting IEA regions 
#' into Exiobase regions for 2019 IEA data. Using the `default_aggregation_region_table_path()` function, the user can
#' select the default IEA regions to Exiobase regions aggregation table for a different year. 
#' @param country The name of the `country`` column in the aggregation table returned by the data frame. 
#' This column contains ISO codes for the `iea_regions` column of the aggregation table.
#' Default is `IEATools::iea_cols$country`.
#' @param iea_regions The name of the column containing IEA regions in the aggregation table.
#' Default is "IEA_regions".
#' @param destination_regions The name of the column containing the destination regions.
#' Default is "Destination_regions".
#' 
#' @return A three column concordance table (as a data frame) mapping the `iea_regions` column to a `destination_regions` column,
#' using a `country` column (with ISO country IDs) as intermediate, which is added to the loaded aggregation table 
#' within the function. 
#' For those IEA regions that do not match to an ISO code (for instance, "World marine bunkers"), 
#' the full IEA region name is kept in the `country` column.
#' 
#' @export
#' 
#' @examples
#' # Returns the default aggregation table for the year 2019
#' read_aggregation_region_table()
#' # Returns the default aggregation table for the year 2020
#' read_aggregation_region_table(file_path = default_aggregation_region_table_path(2020))
read_aggregation_region_table <- function(file_path = default_aggregation_region_table_path(2019),
                                          country = IEATools::iea_cols$country,
                                          iea_regions = "IEA_regions",
                                          destination_regions = "Destination_regions"){
  concordance_table <- openxlsx::read.xlsx(file_path) %>%
    dplyr::mutate(
      "{country}" := .data[[iea_regions]]
    ) %>%
    use_iso_countries(country = country) %>%
    dplyr::filter(! (is.na(.data[[destination_regions]]) | .data[[destination_regions]] == "" | is.null(.data[[destination_regions]])))
  return(concordance_table)
}
# --- EAR, 01/10/2020

#' Aggregates IEA regions based on a user-defined aggregation table.
#' 
#' Takes as input a tidy dataframe, an aggregation table routing IEA regions to destination regions (as a data frame), 
#' and aggregates flows per regions following the user-defined aggregation table. 
#' The boolean argument `net_trade`` enables to perform the aggregation by keeping only net imports
#' and/or net exports or by keeping gross imports and exports.
#' 
#' @param .tidy_iea_df The `.tidy_iea_df` data frame that needs to be aggregated by regions. The `.tidy_iea_df` is likely
#' to have been obtained with the `load_tidy_iea_df()` function.
#' @param aggregation_table An aggregation table that routes the IEA regions (`iea_regions` column) to destination regions
#' (`destination_regions` column). The aggregation table can be built manually 
#' or loaded from an Excel file with the `read_aggregation_region_table()` function.
#' Default is the 2019 IEA to Exiobase aggregation table, as provided by the `read_aggregation_region_table()` function.
#' @param net_trade The boolean that defines whether imports and exports by aggregation region should be converted 
#' into net imports / exports or not. Default is `FALSE`.
#' @param destination_regions The name of the `destination_regions` in the `aggregation_table` data frame.
#' Default is "Destination_regions".
#' @param iea_regions The name of the `iea_regions` in the `aggregation_table` data frame.
#' Default is "IEA_regions".
#' @param imports The name of the `imports` flow in the `.tidy_iea_df`. 
#' Default is `IEATools::interface_industries$imports`.
#' @param exports The name of the `exports` flow in the `.tidy_iea_df`. 
#' Default is `IEATools::interface_industries$exports`.
#' @param country The name of the `country` column in the `.tidy_iea_df`.
#' Default is `IEATools::iea_cols$country`.
#' @param e_dot The name of the `e_dot` column in the `.tidy_iea_df`.
#' Default is `IEATools::iea_cols$e_dot`.
#' @param flow The name of the `flow` column in the `.tidy_iea_df`.
#' Default is `IEATools::iea_cols$flow`.
#' @param year The name of the `year` column in the `.tidy_iea_df`.
#' Default is `IEATools::iea_cols$year`.
#' @param ledger_side The name of the `ledger_side` column in the `.tidy_iea_df`.
#' Default is `IEATools::iea_cols$ledger_side`.
#' @param flow_aggregation_point The name of the `flow_aggregation_point` column in the `.tidy_iea_df`.
#' Default is `IEATools::iea_cols$flow_aggregation_point`.
#' @param .net_imports The name of the `.net_import` variable, that is only used internally to the function. Not returned.
#' Default is "Net_Imports". It is suggested that this parameter is only used in the particular case that there is a column
#' or a flow named "Net_Imports" in the `.tidy_iea_df` input data frame.
#' 
#' @return A `.tidy_iea_df` that contains the data of the input `.tidy_iea_df` aggregated by regions as specified in the user-defined
#' country aggregation table provided.
#' 
#' @export
#' 
#' @examples
#' # Performs the regional aggregation using the default IEA to Exiobase mapping 
#' # for IEA data 2019, using the example `.tidy_iea_df` 
#' # returned by the `load_tidy_iea_df()` function when run without argument.
#' aggregate_regions(.tidy_iea_df = load_tidy_iea_df())
#' # Performs the regional aggregation using the default IEA to Exiobase mapping 
#' # for IEA data 2020, using the example `.tidy_iea_df` 
#' # returned by the `load_tidy_iea_df()` function when run without argument.
#' aggregate_regions(.tidy_iea_df = load_tidy_iea_df(), 
#'                   aggregation_table = read_aggregation_region_table(
#'                     default_aggregation_region_table_path(2020)))
aggregate_regions <- function(.tidy_iea_df,
                              aggregation_table = read_aggregation_region_table(),
                              net_trade = FALSE, 
                              destination_regions = "Destination_regions",
                              iea_regions = "IEA_regions",
                              imports = IEATools::interface_industries$imports,
                              exports = IEATools::interface_industries$exports,
                              country = IEATools::iea_cols$country,
                              e_dot = IEATools::iea_cols$e_dot,
                              flow = IEATools::iea_cols$flow,
                              year = IEATools::iea_cols$year,
                              ledger_side = IEATools::iea_cols$ledger_side,
                              flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                              .net_imports = "Net_Imports"){
  
  iea_code_regions <- aggregation_table[[country]]
  dest_regions <- as.character(aggregation_table[[destination_regions]])
  
  aggregated_tidy_iea_df <-.tidy_iea_df %>%
    dplyr::filter(
      .data[[country]] %in% iea_code_regions
    ) %>%
    dplyr::inner_join(
      aggregation_table, by = country
    ) %>%
    dplyr::mutate(
      # Country = Destination_regions
      "{country}" := .data[[destination_regions]],
      "{iea_regions}" := NULL # Or else, we would also group_by IEA regions - and thus not perform any aggregation!
    ) %>%
    matsindf::group_by_everything_except(e_dot) %>%
    dplyr::summarise(
      "{e_dot}" := sum(.data[[e_dot]])
    )
  
  if (net_trade == TRUE){
    aggregated_net_trade <- aggregated_tidy_iea_df %>% 
      dplyr::filter(.data[[flow]] == imports | .data[[flow]] == exports) %>% 
      tidyr::pivot_wider(names_from = .data[[flow]], values_from = .data[[e_dot]]) %>% 
      dplyr::mutate(
        "{imports}" := tidyr::replace_na(.data[[imports]], 0),
        "{exports}" := tidyr::replace_na(.data[[exports]], 0),
        "{.net_imports}" := .data[[imports]] + .data[[exports]]
      ) %>% 
      tidyr::pivot_longer(cols = c({imports}, {exports}, {.net_imports}), names_to = flow, values_to = e_dot) %>%
      dplyr::filter(.data[[flow]] == {.net_imports}) %>% 
      dplyr::mutate(
        "{flow}" = dplyr::case_when(
          .data[[e_dot]] >= 0 ~ {imports},
          .data[[e_dot]] < 0 ~ {exports}#,
          #.data[[e_dot]] == 0 ~ {net_imports}
        )
      ) %>% 
      dplyr::filter(.data[[e_dot]] != 0) %>%
      dplyr::arrange({year}, {country}, dplyr::desc({ledger_side}), {flow_aggregation_point}, {flow})
    
    aggregated_tidy_iea_df <- aggregated_tidy_iea_df %>% 
      dplyr::filter(! .data[[flow]] %in% c({imports}, {exports})) %>%
      dplyr::bind_rows(aggregated_net_trade) %>%
      dplyr::arrange({year}, {country}, dplyr::desc({ledger_side}), {flow_aggregation_point}, {flow})
  }
  
  return(aggregated_tidy_iea_df)
}
# --- EAR, 01/10/2020