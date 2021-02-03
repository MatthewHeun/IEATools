
#' Gather main activity producer and autoproducer industries
#' 
#' The IEA extended energy balances include both main activity producer 
#' and autoproducer industries for electricity, heat, and CHP plants. 
#' See details for an explication of each.
#' This function gathers main activity producer and autoproducer, for each
#' of the three types of plants: electricity, heat, and CHP plants.
#' This function is called within the `specify_all()` function.
#' 
#' Autoproducer plants are those that consume in-situ the energy they produce. 
#' For instance, an iron and steel plant that produces electricity 
#' and directly consumes it would be classified as an autoproducer electricity plant.
#' Conversely, main activity producer plants are those that produce
#' a product, be it electricity, heat, or both (CHP plants) and sell it
#' to the market.
#'
#' @param .tidy_iea_df The `.tidy_iea_df` which flows need to be specified.
#' @param flow_aggregation_point The name of the flow aggregation point column in the `.tidy_iea_df`.
#'                               Default is `IEATools::iea_cols$flow_aggregation_point`.
#' @param flow The name of the flow column in the `.tidy_iea_df`.
#'             Default is `IEATools::iea_cols$flow`.
#' @param e_dot The name of the energy column in the `.tidy_iea_df`.
#'              Default is `IEATools::iea_cols$flow`.
#' @param transformation_processes A string identifying transformation processes in the `flow_aggregation_point` column of the `.tidy_iea_df`
#'                                 Default is `IEATools::aggregation_flows$flow_aggregation_point`.
#' @param negzeropos The name of a temporary column created in `.tidy_iea_df`. 
#'                   Default is ".negzeropos".
#' @param autoproducer_elect A string identifying "Autoproducer electricity plants" in the `flow` column of the `.tidy_iea_df`.
#'                           Default is `IEATools::transformation_processes$autoproducer_electricity_plants`.
#' @param autoproducer_chp A string identifying "Autoproducer CHP plants" in the `flow` column of the `.tidy_iea_df`.
#'                         Default is `IEATools::transformation_processes$autoproducer_CHP_plants`.
#' @param autoproducer_heat A string identifying "Autoproducer heat plants" in the `flow` column of the `.tidy_iea_df`.
#'                          Default is `IEATools::transformation_processes$autoproducer_heat_plants`.
#' @param main_act_producer_elect A string identifying "Main activity producer electricity plants" in the `flow` column of the `.tidy_iea_df`.
#'                                Default is `IEATools::transformation_processes$main_activity_producer_electricity_plants`.
#' @param main_act_producer_heat A string identifying "Main activity producer heat plants" in the `flow` column of the `.tidy_iea_df`.
#'                               Default is `IEATools::transformation_processes$main_activity_producer_heat_plants`.
#' @param main_act_producer_chp A string identifying "Main activity producer CHP plants" in the `flow` column of the `.tidy_iea_df`.
#'                              Default is `IEATools::transformation_processes$main_activity_producer_CHP_plants`.
#'
#' @return The `tidy_iea_df` with autoproducer plants merged with main activity producer plants.
#' @export
#'
#' @examples
#' library(dplyr)
#' # The following should return something
#' load_tidy_iea_df() %>% 
#'   gather_producer_autoproducer() %>% 
#'   dplyr::filter(Flow == IEATools::transformation_processes$main_activity_producer_electricity_plants)
#' # The following should return an empty data frame
#' load_tidy_iea_df() %>% 
#'   gather_producer_autoproducer() %>% 
#'   dplyr::filter(Flow == IEATools::transformation_processes$autoproducer_electricity_plants)
gather_producer_autoproducer <- function(.tidy_iea_df,
                                         # Column names
                                         flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                                         flow = IEATools::iea_cols$flow,
                                         e_dot = IEATools::iea_cols$e_dot,
                                         # Other parameters
                                         transformation_processes = IEATools::aggregation_flows$transformation_processes,
                                         negzeropos = ".negzeropos",
                                         # Autoproducer industries names
                                         autoproducer_elect = IEATools::transformation_processes$autoproducer_electricity_plants,
                                         autoproducer_chp = IEATools::transformation_processes$autoproducer_CHP_plants,
                                         autoproducer_heat = IEATools::transformation_processes$autoproducer_heat_plants,
                                         # Main activity industries names
                                         main_act_producer_elect = IEATools::transformation_processes$main_activity_producer_electricity_plants,
                                         main_act_producer_heat = IEATools::transformation_processes$main_activity_producer_heat_plants,
                                         main_act_producer_chp = IEATools::transformation_processes$main_activity_producer_CHP_plants){
  
  .tidy_iea_df %>%
    dplyr::mutate(
      "{flow}" := dplyr::case_when(
        (.data[[flow]] == autoproducer_elect & .data[[flow_aggregation_point]] == transformation_processes) ~ main_act_producer_elect,
        (.data[[flow]] == autoproducer_chp & .data[[flow_aggregation_point]] == transformation_processes) ~ main_act_producer_chp,
        (.data[[flow]] == autoproducer_heat & .data[[flow_aggregation_point]] == transformation_processes) ~ main_act_producer_heat,
        TRUE ~ .data[[flow]]
      )
    ) %>%
    # Aggregating. We need to add a pos/neg/null column to add up differently positive and negative values, otherwise we'd only get NET flows.
    dplyr::mutate(
      "{negzeropos}" := dplyr::case_when(
        .data[[e_dot]] < 0 ~ "neg",
        .data[[e_dot]] == 0 ~ "zero",
        .data[[e_dot]] > 0 ~ "pos"
      )
    ) %>%
    # Now sum similar rows using summarise.
    # Group by everything except the energy flow rate column, "E.dot".
    matsindf::group_by_everything_except(e_dot) %>%
    dplyr::summarise(
      "{e_dot}" := sum(.data[[e_dot]])
    ) %>%
    dplyr::mutate(
      # Eliminate the column we added.
      "{negzeropos}" := NULL
    ) %>%
    dplyr::ungroup()
}




route_pumped_storage <- function(.tidy_iea_df,
                                 flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                                 eiou = "Energy industry own use",
                                 flow = IEATools::iea_cols$flow,
                                 # Industries that receive EIOU but are not in Transformation processes
                                 pumped_storage = "Pumped storage plants",
                                 e_dot = IEATools::iea_cols$e_dot,
                                 negzeropos = ".negzeropos",
                                 # Places where the EIOU will e reassigned
                                 main_act_producer_elect = "Main activity producer electricity plants"){
  
  .tidy_iea_df %>%
    dplyr::mutate(
      "{flow}" := dplyr::case_when(
        (.data[[flow]] == pumped_storage & .data[[flow_aggregation_point]] == eiou) ~ main_act_producer_elect,
        TRUE ~ .data[[flow]]
      )
    ) %>%
    # Aggregating. We need to add a pos/neg/null column to add up differently positive and negative values, otherwise we'd only get NET flows.
    dplyr::mutate(
      "{negzeropos}" := dplyr::case_when(
        .data[[e_dot]] < 0 ~ "neg",
        .data[[e_dot]] == 0 ~ "zero",
        .data[[e_dot]] > 0 ~ "pos"
      )
    ) %>%
    # Now sum similar rows using summarise.
    # Group by everything except the energy flow rate column, "E.dot".
    matsindf::group_by_everything_except(e_dot) %>%
    dplyr::summarise(
      "{e_dot}" := sum(.data[[e_dot]])
    ) %>%
    dplyr::mutate(
      # Eliminate the column we added.
      "{negzeropos}" := NULL
    ) %>%
    dplyr::ungroup()
}


# This function re-routes the "Own use in electricity, CHP and heat plants" EIOU flow to the different main activity activities
# Using the shares of each main activity supply.

route_own_use_elect_chp_heat <- function(.tidy_iea_df,
                                         country = IEATools::iea_cols$country,
                                         flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                                         flow = IEATools::iea_cols$flow,
                                         ledger_side = IEATools::iea_cols$ledger_side,
                                         method = IEATools::iea_cols$method,
                                         energy_type = IEATools::iea_cols$energy_type,
                                         last_stage = IEATools::iea_cols$last_stage,
                                         year = IEATools::iea_cols$year,
                                         product = IEATools::iea_cols$product,
                                         unit = IEATools::iea_cols$unit,
                                         e_dot = IEATools::iea_cols$e_dot,
                                         supply = "Supply",
                                         eiou = "Energy industry own use",
                                         transformation_processes = "Transformation processes",
                                         own_use_elect_chp_heat = "Own use in electricity, CHP and heat plants",
                                         negzeropos = ".negzeropos",
                                         main_act_producer_elect = "Main activity producer electricity plants",
                                         main_act_producer_chp = "Main activity producer CHP plants",
                                         main_act_producer_heat = "Main activity producer heat plants"){
  
  
  # Check whether one of the three main activity elect, heat, and/or chp exists in the TP - supply,
  # for each (Country, Method, Energy.type, Last.stage, Year)
  # If not, then route to main activity elect as the code originally did
  # Store results in a tidy_iea_df_routed_own_use_missing_activities data frame.
  # tidy_iea_df_routed_own_use_missing_activities <- .tidy_iea_df %>%
  #   dplyr::group_by({country}, {method}, {energy_type}, {last_stage}, {year}) %>%
  #   dplyr::filter(!(main_act_producer_elect %in% .data[[flow]] |
  #                     main_act_producer_chp %in% .data[[flow]] |
  #                     main_act_producer_heat %in% .data[[flow]])) %>%
  # dplyr::mutate(
  #   "{flow}" := dplyr::case_when(
  #     (.data[[flow]] == own_use_elect_chp_heat & .data[[flow_aggregation_point]] == eiou) ~ main_act_producer_elect,
  #     TRUE ~ .data[[flow]]
  #   )
  # )
  
  # Now, we move to the other case - one of the three main activities elect, heat, and/or CHP EXISTS in the TP - supply.
  # So each time we need to start by doing the inverse filter.
  
  # HEre's an alternative to expand(nesting())
  df_observations_included_tidy_iea_df <- .tidy_iea_df %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]]) %>%
    dplyr::summarise(
      n_From_Func = dplyr::n()
    ) %>%
    dplyr::select(-n_From_Func)
  #tidyr::expand(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]])
  
  
  total_main_activity_output <- .tidy_iea_df %>%
    dplyr::filter(
      .data[[flow]] %in% c(main_act_producer_elect, main_act_producer_chp, main_act_producer_heat)
      & .data[[flow_aggregation_point]] == transformation_processes
      & .data[[ledger_side]] == supply
      & .data[[e_dot]] > 0
    ) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]], .data[[ledger_side]], .data[[flow_aggregation_point]]) %>%
    dplyr::summarise(
      Total_supply_main_activity_From_Func = sum(.data[[e_dot]])
    )
  
  list_not_included_total_main_activity_output <- df_observations_included_tidy_iea_df %>%
    dplyr::anti_join(total_main_activity_output, by = c({country}, {method}, {energy_type}, {last_stage}, {year})) %>%
    tidyr::unite(col = "ID", .data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]]) %>%
    dplyr::pull()
  
  
  output_per_main_activity <- .tidy_iea_df %>%
    dplyr::filter(
      .data[[flow]] %in% c(main_act_producer_elect, main_act_producer_chp, main_act_producer_heat)
      & .data[[flow_aggregation_point]] == transformation_processes
      & .data[[ledger_side]] == supply
      & .data[[e_dot]] > 0
    ) %>%
    dplyr::group_by(
      .data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[flow]], .data[[unit]], .data[[ledger_side]], .data[[flow_aggregation_point]]
    ) %>%
    dplyr::summarise(
      Supply_per_main_activity_From_Func = sum(.data[[e_dot]])
    )
  
  
  share_output_per_main_activity <- output_per_main_activity %>%
    dplyr::left_join(
      total_main_activity_output, by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {unit}, {ledger_side}, {flow_aggregation_point})
    ) %>%
    dplyr::mutate(
      Share_supply_per_main_activity_From_Func = Supply_per_main_activity_From_Func / Total_supply_main_activity_From_Func
    ) %>%
    dplyr::select(-.data[[flow_aggregation_point]])
  
  
  routed_own_use_with_main_activity <- .tidy_iea_df %>%
    dplyr::filter(.data[[flow]] == own_use_elect_chp_heat) %>%
    dplyr::filter(!(stringr::str_c(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], sep = "_")
                    %in% list_not_included_total_main_activity_output)) %>%
    dplyr::group_by(
      .data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]], .data[[flow_aggregation_point]], .data[[ledger_side]]
    ) %>%
    tidyr::crossing(
      destination_flow := c(main_act_producer_elect, main_act_producer_chp, main_act_producer_heat)
    ) %>%
    dplyr::mutate(
      "{flow}" := .data[["destination_flow"]]
    ) %>%
    dplyr::select(-destination_flow) %>%
    dplyr::inner_join(
      share_output_per_main_activity, by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {flow}, {unit}, {ledger_side})
    ) %>%
    dplyr::mutate(
      "{e_dot}" := .data[[e_dot]] * Share_supply_per_main_activity_From_Func
    ) %>%
    dplyr::select(-Share_supply_per_main_activity_From_Func, -Supply_per_main_activity_From_Func, -Total_supply_main_activity_From_Func)
  
  
  
  routed_own_use_without_main_activity <- .tidy_iea_df %>%
    dplyr::filter(.data[[flow]] == own_use_elect_chp_heat) %>%
    dplyr::filter(stringr::str_c(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], sep = "_")
                  %in% list_not_included_total_main_activity_output) %>%
    dplyr::mutate(
      "{flow}" := dplyr::case_when(
        (.data[[flow]] == own_use_elect_chp_heat & .data[[flow_aggregation_point]] == eiou) ~ main_act_producer_elect,
        TRUE ~ .data[[flow]]
      )
    )
  
  
  routed_own_use <- dplyr::bind_rows(routed_own_use_with_main_activity, routed_own_use_without_main_activity)
  
  
  tidy_iea_df_routed_own_use <- .tidy_iea_df %>%
    dplyr::filter(.data[[flow]] != own_use_elect_chp_heat) %>%
    dplyr::bind_rows(routed_own_use) %>%
    #Aggregating. We need to add a pos/neg/null column to add up differently positive and negative values, otherwise we'd only get NET flows.
    dplyr::mutate(
      "{negzeropos}" := dplyr::case_when(
        .data[[e_dot]] < 0 ~ "neg",
        .data[[e_dot]] == 0 ~ "zero",
        .data[[e_dot]] > 0 ~ "pos"
      )
    ) %>%
    # Now sum similar rows using summarise.
    # Group by everything except the energy flow rate column, "E.dot".
    matsindf::group_by_everything_except(e_dot) %>%
    dplyr::summarise(
      "{e_dot}" := sum(.data[[e_dot]])
    ) %>%
    dplyr::mutate(
      #Eliminate the column we added.
      "{negzeropos}" := NULL
    ) %>%
    dplyr::ungroup()
  
  return(tidy_iea_df_routed_own_use)
}




# This function adds a nuclear industry to the PSUT.
add_nuclear_industry <- function(.tidy_iea_df,
                                 flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                                 flow = IEATools::iea_cols$flow,
                                 e_dot = IEATools::iea_cols$e_dot,
                                 product = IEATools::iea_cols$product,
                                 method = IEATools::iea_cols$method,
                                 ledger_side = IEATools::iea_cols$ledger_side,
                                 last_stage = IEATools::iea_cols$last_stage,
                                 energy_type = IEATools::iea_cols$energy_type,
                                 country = IEATools::iea_cols$country,
                                 year = IEATools::iea_cols$year,
                                 unit = IEATools::iea_cols$unit,
                                 eiou = "Energy industry own use",
                                 transformation_processes = "Transformation processes",
                                 own_use_elect_chp_heat = "Own use in electricity, CHP and heat plants",
                                 nuclear_industry = "Nuclear industry",
                                 negzeropos = ".negzeropos",
                                 main_act_producer_elect = "Main activity producer electricity plants",
                                 main_act_producer_chp = "Main activity producer CHP plants",
                                 autoproducer_elect = "Autoproducer electricity plants",
                                 autoproducer_chp = "Autoproducer CHP plants",
                                 nuclear = "Nuclear",#perhaps to change if it becomes Nuclear [from Resources]
                                 electricity = "Electricity",
                                 heat = "Heat"){
  
  
  products_tibble = tibble::tibble(!!nuclear := NA,
                                   !!electricity := NA,
                                   !!heat := NA)
  
  # Here we keep only the flows that we are going to modify:
  modified_flows <- .tidy_iea_df %>%
    dplyr::filter(
      .data[[flow_aggregation_point]] == transformation_processes &
        ((.data[[flow]] %in% c(main_act_producer_elect, autoproducer_elect) & .data[[product]] %in% c(nuclear, electricity)) |
           (.data[[flow]] %in% c(main_act_producer_chp, autoproducer_chp) & .data[[product]] %in% c(nuclear, electricity, heat)))
    ) %>%
    tidyr::pivot_wider(names_from = .data[[product]], values_from = .data[[e_dot]]) %>%
    dplyr::select(-tidyselect::any_of({e_dot})) %>% 
    tibble::add_column(!!products_tibble[! names(products_tibble) %in% names(.)]) %>%
    dplyr::mutate(
      "{nuclear}" := tidyr::replace_na(.data[[nuclear]], 0),
      "{electricity}" := tidyr::replace_na(.data[[electricity]], 0),
      "{heat}" := tidyr::replace_na(.data[[heat]], 0)
    ) %>%
    dplyr::mutate(
      share_elect_output_From_Func = .data[[electricity]] / (.data[[electricity]] + .data[[heat]]),
      "{electricity}" := .data[[electricity]] + (.data[[nuclear]] * 0.33) * .data[["share_elect_output_From_Func"]],
      "{heat}" := .data[[heat]] + (.data[[nuclear]] * 0.33) * (1 - .data[["share_elect_output_From_Func"]]),
      Electricity_Nuclear = - .data[[nuclear]] * 0.33 * share_elect_output_From_Func,
      Heat_Nuclear = - .data[[nuclear]] * 0.33 * (1 - share_elect_output_From_Func)
    ) %>%
    dplyr::select(-.data[["share_elect_output_From_Func"]]) %>%
    tidyr::pivot_longer(cols = c({electricity}, {heat}, {nuclear}, "Electricity_Nuclear", "Heat_Nuclear"), values_to = {e_dot}, names_to = {product}) %>%
    dplyr::filter(.data[[e_dot]] != 0) %>%
    dplyr::mutate(
      "{flow}" := dplyr::case_when(
        stringr::str_detect(.data[[product]], nuclear) ~ nuclear_industry,
        TRUE ~ .data[[flow]]
      ),
      "{product}" := stringr::str_remove(.data[[product]], "_Nuclear")
    )
  
  
  to_return <- .tidy_iea_df %>%
    dplyr::filter(
      ! (.data[[flow_aggregation_point]] == transformation_processes &
           ((.data[[flow]] %in% c(main_act_producer_elect, autoproducer_elect) & .data[[product]] %in% c(nuclear, electricity)) |
              (.data[[flow]] %in% c(main_act_producer_chp, autoproducer_chp) & .data[[product]] %in% c(nuclear, electricity, heat))))
    ) %>%
    dplyr::bind_rows(
      modified_flows
    ) %>%
    dplyr::mutate(
      "{negzeropos}" := dplyr::case_when(
        .data[[e_dot]] < 0 ~ "neg",
        .data[[e_dot]] == 0 ~ "zero",
        .data[[e_dot]] > 0 ~ "pos"
      )
    ) %>%
    # Now sum similar rows using summarise.
    # Group by everything except the energy flow rate column, "E.dot".
    matsindf::group_by_everything_except(e_dot) %>%
    dplyr::summarise(
      "{e_dot}" := sum(.data[[e_dot]])
    ) %>%
    dplyr::mutate(
      #Eliminate the column we added.
      "{negzeropos}" := NULL
    ) %>%
    dplyr::ungroup()
  
  
  return(to_return)
}



route_non_specified_flows <- function(.tidy_iea_df,
                                      flow_aggregation_point = "Flow.aggregation.point",
                                      eiou = "Energy industry own use",
                                      transformation_processes = "Transformation processes",
                                      flow = "Flow",
                                      # Industries that receive EIOU but are not in Transformation processes
                                      own_use_elect_chp_heat = "Own use in electricity, CHP and heat plants",
                                      pumped_storage = "Pumped storage plants",
                                      nuclear_industry = "Nuclear industry",
                                      e_dot = "E.dot",
                                      negzeropos = ".negzeropos",
                                      # Places where the EIOU will e reassigned
                                      main_act_producer_elect = "Main activity producer electricity plants"){
  
  .tidy_iea_df %>%
    route_non_specified_eiou() %>%
    route_non_specified_tp()
}


route_non_specified_eiou <- function(.tidy_iea_df,
                                     country = IEATools::iea_cols$country,
                                     flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                                     flow = IEATools::iea_cols$flow,
                                     ledger_side = IEATools::iea_cols$ledger_side,
                                     method = IEATools::iea_cols$method,
                                     energy_type = IEATools::iea_cols$energy_type,
                                     last_stage = IEATools::iea_cols$last_stage,
                                     year = IEATools::iea_cols$year,
                                     product = IEATools::iea_cols$product,
                                     unit = IEATools::iea_cols$unit,
                                     e_dot = IEATools::iea_cols$e_dot,
                                     non_spec = "Non-specified",
                                     eiou = "Energy industry own use",
                                     negzeropos = ".negzeropos"){
  
  
  # Here's an alternative to expand
  df_observations_included_tidy_iea_df <- .tidy_iea_df %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]]) %>%
    dplyr::summarise(
      n_From_Func = dplyr::n()
    ) %>%
    dplyr::select(-n_From_Func)
  #tidyr::expand(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]])
  
  
  total_eiou_excl_nonspec <- .tidy_iea_df %>%
    dplyr::filter(
      .data[[flow_aggregation_point]] == eiou & .data[[flow]] != non_spec
    ) %>%
    dplyr::group_by(
      .data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]], .data[[ledger_side]], .data[[flow_aggregation_point]]
    ) %>%
    dplyr::summarise(
      Total_eiou_excl_nonspec_From_Func = sum(.data[[e_dot]])
    )
  
  
  list_not_included_total_eiou <- df_observations_included_tidy_iea_df %>%
    dplyr::anti_join(total_eiou_excl_nonspec, by = c({country}, {method}, {energy_type}, {last_stage}, {year})) %>%
    tidyr::unite(col = "ID", .data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]]) %>%
    dplyr::pull()
  
  
  eiou_per_industry <- .tidy_iea_df %>%
    dplyr::filter(
      .data[[flow_aggregation_point]] == eiou & .data[[flow]] != non_spec
    ) %>%
    dplyr::group_by(
      .data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[flow]], .data[[unit]], .data[[ledger_side]], .data[[flow_aggregation_point]]
    ) %>%
    dplyr::summarise(
      EIOU_per_industry_From_Func = sum(.data[[e_dot]])
    )
  
  
  share_eiou_per_industry <- eiou_per_industry %>%
    dplyr::left_join(
      total_eiou_excl_nonspec, by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {unit}, {ledger_side}, {flow_aggregation_point})
    ) %>%
    dplyr::mutate(
      Share_eiou_per_industry_From_Func = EIOU_per_industry_From_Func / Total_eiou_excl_nonspec_From_Func
    ) %>%
    dplyr::select(-.data[[flow_aggregation_point]])
  
  
  list_eiou_flows_excl_nonspec <- .tidy_iea_df %>%
    dplyr::filter(.data[[flow_aggregation_point]] == eiou & .data[[flow]] != non_spec) %>%
    tidyr::expand(.data[[flow]]) %>%
    dplyr::pull()
  
  
  
  # First, when eiou flows are available.
  routed_nonspec_energy <- .tidy_iea_df %>%
    dplyr::filter(
      .data[[flow_aggregation_point]] == eiou & .data[[flow]] == non_spec
    ) %>%
    dplyr::filter(!(stringr::str_c(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], sep = "_")
                    %in% list_not_included_total_eiou)) %>%
    dplyr::group_by(
      .data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]], .data[[flow_aggregation_point]], .data[[ledger_side]]
    ) %>%
    tidyr::crossing(
      destination_flow := list_eiou_flows_excl_nonspec
    ) %>%
    dplyr::mutate(
      "{flow}" := .data[["destination_flow"]]
    ) %>%
    dplyr::select(-destination_flow) %>%
    dplyr::inner_join(
      share_eiou_per_industry, by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {flow}, {unit}, {ledger_side})
    ) %>%
    dplyr::mutate(
      "{e_dot}" := .data[[e_dot]] * Share_eiou_per_industry_From_Func
    ) %>%
    dplyr::select(-Share_eiou_per_industry_From_Func, -EIOU_per_industry_From_Func, -Total_eiou_excl_nonspec_From_Func)
  
  
  # Second, when EIOU flows are not available
  # First, when eiou flows are available.
  # routed_nonspec_energy_without_eiou <- .tidy_iea_df %>%
  #   dplyr::filter(
  #     .data[[flow_aggregation_point]] == eiou & .data[[flow]] == non_spec
  #   ) %>%
  #   dplyr::filter((str_c(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], sep = "_")
  #                   %in% list_not_included_total_eiou))
  #
  #
  # routed_nonspec_energy <- bind_rows(routed_nonspec_energy_with_eiou, routed_nonspec_energy_without_eiou)
  
  # squeezing all conditions in first filter I think
  tidy_iea_df_routed_nonspec_energy <- .tidy_iea_df %>%
    dplyr::filter(
      ! (.data[[flow_aggregation_point]] == eiou &
           .data[[flow]] == non_spec &
           (! stringr::str_c(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], sep = "_")
            %in% list_not_included_total_eiou))
    ) %>%
    dplyr::bind_rows(routed_nonspec_energy) %>%
    #Aggregating. We need to add a pos/neg/null column to add up differently positive and negative values, otherwise we'd only get NET flows.
    dplyr::mutate(
      "{negzeropos}" := dplyr::case_when(
        .data[[e_dot]] < 0 ~ "neg",
        .data[[e_dot]] == 0 ~ "zero",
        .data[[e_dot]] > 0 ~ "pos"
      )
    ) %>%
    # Now sum similar rows using summarise.
    # Group by everything except the energy flow rate column, "E.dot".
    matsindf::group_by_everything_except(e_dot) %>%
    dplyr::summarise(
      "{e_dot}" := sum(.data[[e_dot]])
    ) %>%
    dplyr::mutate(
      #Eliminate the column we added.
      "{negzeropos}" := NULL
    ) %>%
    dplyr::ungroup()
  
  return(tidy_iea_df_routed_nonspec_energy)
  
}



route_non_specified_tp <- function(.tidy_iea_df,
                                   country = IEATools::iea_cols$country,
                                   flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                                   flow = IEATools::iea_cols$flow,
                                   ledger_side = IEATools::iea_cols$ledger_side,
                                   method = IEATools::iea_cols$method,
                                   energy_type = IEATools::iea_cols$energy_type,
                                   last_stage = IEATools::iea_cols$last_stage,
                                   year = IEATools::iea_cols$year,
                                   product = IEATools::iea_cols$product,
                                   unit = IEATools::iea_cols$unit,
                                   e_dot = IEATools::iea_cols$e_dot,
                                   transformation_processes = "Transformation processes",
                                   non_spec = "Non-specified",
                                   negzeropos = ".negzeropos"){
  
  
  df_observations_included_tidy_iea_df <- .tidy_iea_df %>%
    dplyr::filter(.data[[flow_aggregation_point]] == transformation_processes) %>%
    dplyr::mutate(
      "{negzeropos}" := dplyr::case_when(
        .data[[e_dot]] < 0 ~ "neg",
        .data[[e_dot]] == 0 ~ "zero",
        .data[[e_dot]] > 0 ~ "pos"
      )
    ) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product]], .data[[negzeropos]]) %>%
    dplyr::summarise(
      n_From_Func = dplyr::n()
    ) %>%
    dplyr::select(-n_From_Func)
  #tidyr::expand(tidyr::nesting(.env[[country]], .env[[method]], .env[[energy_type]], .env[[last_stage]], .env[[year]], .env[[product]], .env[[negzeropos]]))
  
  
  total_input_output_by_prod_tps <- .tidy_iea_df %>%
    dplyr::filter(
      .data[[flow_aggregation_point]] == transformation_processes & .data[[flow]] != non_spec
    ) %>%
    dplyr::mutate(
      "{negzeropos}" := dplyr::case_when(
        .data[[e_dot]] < 0 ~ "neg",
        .data[[e_dot]] == 0 ~ "zero",
        .data[[e_dot]] > 0 ~ "pos"
      )
    ) %>%
    dplyr::group_by(
      .data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]], .data[[ledger_side]],
      .data[[flow_aggregation_point]], .data[[product]], .data[[negzeropos]]
    ) %>%
    dplyr::summarise(
      Total_input_output_by_prod_excl_nonspec_From_Func = sum(.data[[e_dot]])
    )
  
  list_not_included_total_input_output_by_prod_tps <- df_observations_included_tidy_iea_df %>%
    dplyr::anti_join(total_input_output_by_prod_tps, by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {product}, {negzeropos})) %>%
    tidyr::unite(col = "ID", .data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product]], .data[[negzeropos]]) %>%
    dplyr::pull()
  
  
  input_output_by_prod_per_tp <- .tidy_iea_df %>%
    dplyr::filter(
      .data[[flow_aggregation_point]] == transformation_processes & .data[[flow]] != non_spec
    ) %>%
    dplyr::mutate(
      "{negzeropos}" := dplyr::case_when(
        .data[[e_dot]] < 0 ~ "neg",
        .data[[e_dot]] == 0 ~ "zero",
        .data[[e_dot]] > 0 ~ "pos"
      )
    ) %>%
    dplyr::group_by(
      .data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]], .data[[ledger_side]],
      .data[[flow_aggregation_point]], .data[[flow]], .data[[product]], .data[[negzeropos]]
    ) %>%
    dplyr::summarise(
      Input_output_by_prod_per_tp_From_Func = sum(.data[[e_dot]])
    )
  
  
  share_input_output_by_prod_per_tp <- input_output_by_prod_per_tp %>%
    dplyr::left_join(
      total_input_output_by_prod_tps,
      by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {unit}, {ledger_side}, {flow_aggregation_point}, {product}, {negzeropos})
    ) %>%
    dplyr::mutate(
      Share_input_output_by_prod_per_tp_From_Func = Input_output_by_prod_per_tp_From_Func / Total_input_output_by_prod_excl_nonspec_From_Func
    ) %>%
    dplyr::select(-.data[[flow_aggregation_point]])
  
  
  
  list_tp_flows_excl_nonspec <- .tidy_iea_df %>%
    dplyr::filter(.data[[flow_aggregation_point]] == transformation_processes & .data[[flow]] != non_spec) %>%
    tidyr::expand(.data[[flow]]) %>%
    dplyr::pull()
  
  
  # When tps with the given product and sign are available in the data frame
  routed_nonspec_tp_with_io_by_prod <- .tidy_iea_df %>%
    dplyr::filter(
      .data[[flow_aggregation_point]] == transformation_processes & .data[[flow]] == non_spec
    ) %>%
    dplyr::mutate(
      "{negzeropos}" := dplyr::case_when(
        .data[[e_dot]] < 0 ~ "neg",
        .data[[e_dot]] == 0 ~ "zero",
        .data[[e_dot]] > 0 ~ "pos"
      )
    ) %>%
    dplyr::filter(!(stringr::str_c(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product]], .data[[negzeropos]], sep = "_")
                    %in% list_not_included_total_input_output_by_prod_tps)) %>%
    dplyr::group_by(
      .data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]], .data[[flow_aggregation_point]],
      .data[[ledger_side]], .data[[product]], .data[[negzeropos]]
    ) %>%
    tidyr::crossing(
      destination_flow := list_tp_flows_excl_nonspec
    ) %>%
    dplyr::mutate(
      "{flow}" := .data[["destination_flow"]]
    ) %>%
    dplyr::select(-destination_flow) %>%
    dplyr::inner_join(
      share_input_output_by_prod_per_tp,
      by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {flow_aggregation_point}, {flow}, {unit}, {ledger_side}, {product}, {negzeropos})
    ) %>%
    dplyr::mutate(
      "{e_dot}" := .data[[e_dot]] * Share_input_output_by_prod_per_tp_From_Func
    ) %>%
    dplyr::select(-Share_input_output_by_prod_per_tp_From_Func,
                  -Input_output_by_prod_per_tp_From_Func,
                  -Total_input_output_by_prod_excl_nonspec_From_Func)
  
  
  # When tps with the given product and sign are NOT available in the data frame
  routed_nonspec_tp_without_io_by_prod <- .tidy_iea_df %>%
    dplyr::filter(
      .data[[flow_aggregation_point]] == transformation_processes & .data[[flow]] == non_spec
    ) %>%
    dplyr::mutate(
      "{negzeropos}" := dplyr::case_when(
        .data[[e_dot]] < 0 ~ "neg",
        .data[[e_dot]] == 0 ~ "zero",
        .data[[e_dot]] > 0 ~ "pos"
      )
    ) %>%
    dplyr::filter(stringr::str_c(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product]], .data[[negzeropos]], sep = "_")
                  %in% list_not_included_total_input_output_by_prod_tps)
  
  
  
  routed_nonspec_tp <- dplyr::bind_rows(routed_nonspec_tp_with_io_by_prod, routed_nonspec_tp_without_io_by_prod)
  
  # All other cases
  tidy_iea_df_routed_nonspec_tp <- .tidy_iea_df %>%
    dplyr::filter(
      ! (.data[[flow_aggregation_point]] == transformation_processes & .data[[flow]] == non_spec )
    ) %>%
    dplyr::mutate(
      "{negzeropos}" := dplyr::case_when(
        .data[[e_dot]] < 0 ~ "neg",
        .data[[e_dot]] == 0 ~ "zero",
        .data[[e_dot]] > 0 ~ "pos"
      )
    ) %>%
    dplyr::bind_rows(routed_nonspec_tp) %>%
    #Aggregating. We need to add a pos/neg/null column to add up differently positive and negative values, otherwise we'd only get NET flows.
    # Now sum similar rows using summarise.
    # Group by everything except the energy flow rate column, "E.dot".
    matsindf::group_by_everything_except(e_dot) %>%
    dplyr::summarise(
      "{e_dot}" := sum(.data[[e_dot]])
    ) %>%
    dplyr::mutate(
      #Eliminate the column we added.
      "{negzeropos}" := NULL
    ) %>%
    dplyr::ungroup()
  
  return(tidy_iea_df_routed_nonspec_tp)
  
}
