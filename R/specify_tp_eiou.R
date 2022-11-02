
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
#'                           Default is `IEATools::main_act_plants$autoprod_elect_plants`.
#' @param autoproducer_chp A string identifying "Autoproducer CHP plants" in the `flow` column of the `.tidy_iea_df`.
#'                         Default is `IEATools::transformation_processes$autoproducer_CHP_plants`.
#' @param autoproducer_heat A string identifying "Autoproducer heat plants" in the `flow` column of the `.tidy_iea_df`.
#'                          Default is `IEATools::transformation_processes$autoproducer_heat_plants`.
#' @param main_act_producer_elect A string identifying "Main activity producer electricity plants" in the `flow` column of the `.tidy_iea_df`.
#'                                Default is `IEATools::main_act_plants$main_act_prod_elect_plants`.
#' @param main_act_producer_heat A string identifying "Main activity producer heat plants" in the `flow` column of the `.tidy_iea_df`.
#'                               Default is `IEATools::main_act_plants$main_act_prod_heat_plants`.
#' @param main_act_producer_chp A string identifying "Main activity producer CHP plants" in the `flow` column of the `.tidy_iea_df`.
#'                              Default is `IEATools::main_act_plants$main_act_prod_chp_plants`.
#'
#' @return The `tidy_iea_df` with autoproducer plants merged with main activity producer plants.
#' @export
#'
#' @examples
#' library(dplyr)
#' load_tidy_iea_df() %>% 
#'   gather_producer_autoproducer()
gather_producer_autoproducer <- function(.tidy_iea_df,
                                         # Column names
                                         flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                                         flow = IEATools::iea_cols$flow,
                                         e_dot = IEATools::iea_cols$e_dot,
                                         # Other parameters
                                         transformation_processes = IEATools::aggregation_flows$transformation_processes,
                                         negzeropos = ".negzeropos",
                                         # Autoproducer industries names
                                         autoproducer_elect = IEATools::main_act_plants$autoprod_elect_plants,
                                         autoproducer_chp = IEATools::transformation_processes$autoproducer_CHP_plants,
                                         autoproducer_heat = IEATools::transformation_processes$autoproducer_heat_plants,
                                         # Main activity industries names
                                         main_act_producer_elect = IEATools::main_act_plants$main_act_prod_elect_plants,
                                         main_act_producer_heat = IEATools::main_act_plants$main_act_prod_heat_plants,
                                         main_act_producer_chp = IEATools::main_act_plants$main_act_prod_chp_plants){
  
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


#' Route pumped storage to Main activity electricity producer plant
#' 
#' The function routes Energy industry own use by Pumped storage plants 
#' to Energy industry own use by Main activity producer electricity plants
#' when the value is negative, as it should be.
#' However, for Japan, there are a few years where
#' Energy industry own use by Pumped storage plants is positive.
#' In those instances, the Flow is changed from 
#' Energy industry own use to
#' Main activity producer electricity plants *and* 
#' the Flow.aggregation.point is changed to "Transformation processes"
#' the positive value is retained.
#' This approach preserves the overall energy balance.
#' This approach implicitly assumes that Japan's reported
#' Energy industry own use for Pumped storage plants 
#' is a net value, not a total value.
#' All other countries seemingly report total values for 
#' Energy industry own use by Pumped Storage plants, 
#' as those values are all negative.
#' This function is called within the `specify_all()` function.
#'
#' @param .tidy_iea_df The `.tidy_iea_df` which flows need to be specified.
#' @param flow_aggregation_point The name of the flow aggregation point column in the `.tidy_iea_df`.
#'                               Default is `IEATools::iea_cols$flow_aggregation_point`.
#' @param flow The name of the flow column in the `.tidy_iea_df`.
#'             Default is `IEATools::iea_cols$flow`.
#' @param e_dot The name of the energy column in the `.tidy_iea_df`.
#'              Default is `IEATools::iea_cols$flow`.
#' @param eiou A string identifying "Energy industry own use" in the `flow_aggregation_point` column of the `.tidy_iea_df`.
#'             Default is `IEATools::aggregation_flows$energy_industry_own_use`.
#' @param pumped_storage A string identifying "Pumped storage plants" in the `flow` column of the `.tidy_iea_df`.
#'                       Default is `IEATools::eiou_flows$pumped_storage_plants`.
#' @param main_act_producer_elect A string identifying "Main activity producer electricity plants" in the `flow` column of the `.tidy_iea_df`.
#'                                Default is `IEATools::main_act_plants$main_act_prod_elect_plants`.
#' @param negzeropos The name of a temporary column created in `.tidy_iea_df`. 
#'                   Default is ".negzeropos".
#'
#' @return A modified `.tidy_iea_df` with "Pumped storage plants" industry routed 
#'         to the "Main activity producer electricity plant" industry.
#' @export
#'
#' @examples
#' library(dplyr)
#' load_tidy_iea_df() %>% 
#'   gather_producer_autoproducer()
route_pumped_storage <- function(.tidy_iea_df,
                                 # Column names
                                 flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                                 flow = IEATools::iea_cols$flow,
                                 e_dot = IEATools::iea_cols$e_dot,
                                 # Flow and flow aggregation point names
                                 eiou = IEATools::aggregation_flows$energy_industry_own_use,
                                 pumped_storage = IEATools::eiou_flows$pumped_storage_plants,
                                 main_act_producer_elect = IEATools::main_act_plants$main_act_prod_elect_plants,
                                 # Temporary column name
                                 negzeropos = ".negzeropos"){
  
  .tidy_iea_df %>%
    dplyr::mutate(
      "{flow}" := dplyr::case_when(
        (.data[[flow]] == pumped_storage & .data[[flow_aggregation_point]] == eiou) ~ main_act_producer_elect,
        TRUE ~ .data[[flow]]
      )
    ) %>%
    # Now sum similar rows using summarise.
    # Group by everything except the energy flow rate column, "E.dot".
    matsindf::group_by_everything_except(e_dot) %>%
    dplyr::summarise(
      "{e_dot}" := sum(.data[[e_dot]])
    ) %>%
    dplyr::ungroup()
}


#' Separates EIOU flows of oil and gas extraction
#' 
#' This function separates the EIOU flows of the Oil and gas extraction industry into EIOU flows 
#' for the Oil extraction industry and EIOU flows for the Natural gas extraction industry.
#' It uses the shares of production of each of these two industries to separate EIOU flows. 
#' As such, the EIOU consumed per unit of output will be the same for 
#' the Oil extraction and Natural gas extraction industries.
#'
#' @param .tidy_iea_df The `.tidy_iea_df` on which oil and gas extraction EIOU flows need to be separated.
#' @param eiou The name of the Energy industry own use flow aggregation point.
#'             Default is `IEATools::aggregation_flows$energy_industry_own_use`.
#' @param country,energy_type,method,last_stage,ledger_side,year,flow,flow_aggregation_point,e_dot See `IEATools::iea_cols`.
#' @param oil_gas_extraction The name of the Oil and gas extraction EIOU flow.
#'                           Default is `IEATools::eiou_flows$oil_and_gas_extraction`.
#' @param transformation_processes The name of the flow aggregation point referring to transformation processes.
#'                                 Default is `IEATools::aggregation_flows$transformation_processes`.
#' @param oil_extraction The name of the Oil extraction industry.
#'                       Default is `IEATools::industry_flows$oil_extraction`.
#' @param gas_extraction The name of the Natural gas extraction industry.
#'                       Default is `IEATools::industry_flows$natural_gas_extraction`.
#' @param .share The name of a temporary column that is added to the data frame.
#'               Default is ".share". 
#'
#' @return A `.tidy_iea_df` with "Oil and gas extraction" EIOU flows split into 'Oil extraction"
#'         and "Natural gas extraction" EIOU flows.
#' @export
#'
#' @examples
#' library(dplyr)
#' load_tidy_iea_df() %>% 
#'   split_oil_gas_extraction_eiou()
split_oil_gas_extraction_eiou <- function(.tidy_iea_df,
                                          eiou = IEATools::aggregation_flows$energy_industry_own_use,
                                          country = IEATools::iea_cols$country,
                                          energy_type = IEATools::iea_cols$energy_type,
                                          method = IEATools::iea_cols$method,
                                          last_stage = IEATools::iea_cols$last_stage,
                                          ledger_side = IEATools::iea_cols$ledger_side,
                                          year = IEATools::iea_cols$year,
                                          flow = IEATools::iea_cols$flow,
                                          flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                                          e_dot = IEATools::iea_cols$e_dot,
                                          oil_gas_extraction = IEATools::eiou_flows$oil_and_gas_extraction,
                                          transformation_processes = IEATools::aggregation_flows$transformation_processes,
                                          oil_extraction = IEATools::industry_flows$oil_extraction,
                                          gas_extraction = IEATools::industry_flows$natural_gas_extraction,
                                          .share = ".share"){
  
  # Calculates shares of output for each of the Oil extraction and Natural gas extraction industries
  shares_oil_gas_output <- .tidy_iea_df %>% 
    dplyr::filter(
      .data[[flow_aggregation_point]] == transformation_processes & 
      (.data[[flow]] == oil_extraction | .data[[flow]] == gas_extraction) 
      ) %>% 
    dplyr::filter(.data[[e_dot]] > 0) %>% 
    dplyr::group_by(
      .data[[country]], .data[[energy_type]], .data[[method]], .data[[last_stage]], .data[[ledger_side]], .data[[year]], .data[[flow]]
    ) %>% 
    dplyr::summarise(
      "{e_dot}" := sum(.data[[e_dot]])
     ) %>% 
    dplyr::mutate(
      "{.share}" := .data[[e_dot]] / sum(.data[[e_dot]])
    ) %>% 
    dplyr::select(dplyr::all_of(c(country, energy_type, method, last_stage, ledger_side, year, .share, flow)))
  
  # Check that sum of shares is one
  sum_shares <- shares_oil_gas_output %>% 
    dplyr::group_by(
      .data[[country]], .data[[energy_type]], .data[[method]], .data[[last_stage]], .data[[ledger_side]], .data[[year]],
    ) %>% 
    dplyr::summarise(
      sum_shares = sum(.data[[.share]])
    )
  
  assertthat::assert_that(all(abs(sum_shares$sum_shares - 1) < 1e-4))
  
  # Find out EIOU flows corresponding to Oil and gas extraction, and modify them using shares previously calculated
  modified_eiou_flows <- .tidy_iea_df %>% 
    dplyr::filter(
      .data[[flow_aggregation_point]] == eiou,
      .data[[flow]] == oil_gas_extraction
    ) %>% 
    dplyr::left_join(
      shares_oil_gas_output,
      by = c({country}, {energy_type}, {method}, {last_stage}, {ledger_side}, {year}),
      suffix = c("", ".y")
    ) %>% 
    dplyr::mutate(
      "{.share}" := tidyr::replace_na(.data[[.share]], 1)
    ) %>% 
    dplyr::mutate(
      "{e_dot}" := .data[[e_dot]] * .data[[.share]],
      "{flow}" := .data[[paste0(flow, ".y")]],
      "{flow}" := tidyr::replace_na(.data[[flow]], oil_gas_extraction)
    ) %>% 
    # dplyr::select(-.data[[.share]], -.data[[paste0(flow, ".y")]])
    dplyr::select(-dplyr::any_of(c(.share, paste0(flow, ".y"))))
  
  
  # Filter out former EIOU flows from .tidy_iea_df, and bind the rows calculated above
  split_oil_gas_df <- .tidy_iea_df %>% 
    dplyr::filter(
      ! (.data[[flow_aggregation_point]] == eiou & .data[[flow]] == oil_gas_extraction)
    ) %>% 
    dplyr::bind_rows(
      modified_eiou_flows
    )
  
  # Return new data frame
  return(split_oil_gas_df)
}




#' Routes own use in electricity, chp, and heat plants EIOU flow to main activity producer flows
#' 
#' This function routes the "Own use in electricity, CHP and heat plants" 
#' Energy Industry Own Use flow to each of the three electricity, CHP and heat
#' main activity producer plants. 
#' The function is called within the `specify_all()` function.
#' 
#' The function either performs the routing using the share of outputs or of inputs of each
#' of the three main activity producer industries. 
#' 
#' The method is selected using the `split_using_shares_of` argument, 
#' to which either the have the "input" or "output" value can be passed.
#' 
#' When none of the main activity producer industries is present in the data frame,
#' the "Own use in electricity, CHP and heat plants" flow is ascribed by default to the
#' "Main activity producer electricity plant".
#'
#' @param .tidy_iea_df The `.tidy_iea_df` which flows need to be specified.
#' @param split_using_shares_of A string that identifies which method is to be used for splitting the `own_use_elect_chp_heat` flow.
#'                              Default is "input". The other valid value is "output". See details for more information.
#' @param country The name of the country column in the `.tidy_iea_df`.
#'                Default is `IEATools::iea_cols$country`.
#' @param flow_aggregation_point The name of the flow aggregation point column in the `.tidy_iea_df`.
#'                               Default is `IEATools::iea_cols$flow_aggregation_point`.
#' @param flow The name of the flow column in the `.tidy_iea_df`.
#'             Default is `IEATools::iea_cols$flow`.
#' @param ledger_side The name of the ledger side column in the `.tidy_iea_df`.
#'                    Default is `IEATools::iea_cols$ledger_side`.
#' @param method The name of the method column in the `.tidy_iea_df`.
#'               Default is `IEATools::iea_cols$method`.
#' @param energy_type The name of the energy_type column in the `.tidy_iea_df`.
#'                    Default is `IEATools::iea_cols$energy_type`.
#' @param last_stage The name of the last stage column in the `.tidy_iea_df`.
#'                   Default is `IEATools::iea_cols$last_stage`.
#' @param year The name of the year column in the `.tidy_iea_df`.
#'             Default is `IEATools::iea_cols$flow`.
#' @param product The name of the product column in the `.tidy_iea_df`.
#'                Default is `IEATools::iea_cols$product`.
#' @param unit The name of the unit column in the `.tidy_iea_df`.
#'             Default is `IEATools::iea_cols$unit`.
#' @param e_dot The name of the energy column in the `.tidy_iea_df`.
#'              Default is `IEATools::iea_cols$flow`.
#' @param supply A string identifying "Supply" in the `ledger_side` column of the `.tidy_iea_df`.
#'               Default is `IEATools::ledger_sides$supply`.
#' @param eiou A string identifying "Energy industry own use" in the `flow_aggregation_point` column of the `.tidy_iea_df`.
#'             Default is `IEATools::aggregation_flows$energy_industry_own_use`.
#' @param transformation_processes A string identifying transformation processes in the `flow_aggregation_point` column of the `.tidy_iea_df`
#'                                 Default is `IEATools::aggregation_flows$flow_aggregation_point`.
#' @param own_use_elect_chp_heat A string identifying "Own use in electricity, CHP and heat plants" in the `flow` column of the `.tidy_iea_df`.
#'                               Default is `IEATools::eiou_flows$own_use_elect_chp_heat_plants`.
#' @param main_act_producer_elect A string identifying "Main activity producer electricity plants" in the `flow` column of the `.tidy_iea_df`.
#'                                Default is `IEATools::main_act_plants$main_act_prod_elect_plants`.
#' @param main_act_producer_chp A string identifying "Main activity producer CHP plants" in the `flow` column of the `.tidy_iea_df`.
#'                              Default is `IEATools::main_act_plants$main_act_prod_chp_plants`.
#' @param main_act_producer_heat A string identifying "Main activity producer electricity plants" in the `flow` column of the `.tidy_iea_df`.
#'                               Default is `IEATools::main_act_plants$main_act_prod_heat_plants`.
#' @param n_counting The name of a temporary column created in `.tidy_iea_df`. 
#'                   Default is ".n_counting".
#' @param destination_flow The name of a temporary column created in `.tidy_iea_df`. 
#'                         Default is ".destination_flow".
#' @param Total_main_activity_From_Func The name of a temporary column created in `.tidy_iea_df`. 
#'                                      Default is ".Total_main_activity_From_Func".
#' @param Total_per_main_activity_From_Func The name of a temporary column created in `.tidy_iea_df`. 
#'                                          Default is ".Total_per_main_activity_From_Func".
#' @param Share_per_main_activity_From_Func The name of a temporary column created in `.tidy_iea_df`. 
#'                                          Default is ".Share_per_main_activity_From_Func".
#'
#' @return A modified version of the `.tidy_iea_df`, in which the `own_use_elect_chp_heat` flow has been routed to the main activity producer industries.
#' @export
#'
#' @examples
#' library(dplyr)
#' load_tidy_iea_df() %>% 
#'   route_own_use_elect_chp_heat()
route_own_use_elect_chp_heat <- function(.tidy_iea_df,
                                         split_using_shares_of = c("input", "output"),
                                         # Column names
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
                                         # Strings identifying flows, ledger sides, and flow aggregation points
                                         supply = IEATools::ledger_sides$supply,
                                         eiou =  IEATools::aggregation_flows$energy_industry_own_use,
                                         transformation_processes = IEATools::aggregation_flows$transformation_processes,
                                         own_use_elect_chp_heat = IEATools::eiou_flows$own_use_elect_chp_heat_plants,
                                         main_act_producer_elect = IEATools::main_act_plants$main_act_prod_elect_plants,
                                         main_act_producer_chp = IEATools::main_act_plants$main_act_prod_chp_plants,
                                         main_act_producer_heat = IEATools::main_act_plants$main_act_prod_heat_plants,
                                         # Temporary column names
                                         # negzeropos = ".negzeropos",
                                         n_counting = ".n_counting",
                                         destination_flow = ".destination_flow",
                                         Total_main_activity_From_Func = ".Total_main_activity_From_Func",
                                         Total_per_main_activity_From_Func = ".Total_per_main_activity_From_Func",
                                         Share_per_main_activity_From_Func = ".Share_per_main_activity_From_Func"){
  
  split_using_shares_of <- match.arg(split_using_shares_of)
  
  # The function check whether one of the three main activity elect, heat, and/or chp exists in the TP - supply,
  # for each (Country, Method, Energy.type, Last.stage, Year)
  # If not, then it routes "Own use in electricity, CHP and heat plants" to "Main activity producer electricity plants".
  # If one of the three main activities elect, heat, and/or CHP EXISTS as a supplying transformation process,
  # Then it ascribes ... 
  
  # Returns all the combinations of (Country, Method, Energy.type, Last.stage, Year) present in the .tidy_iea_df
  df_observations_included_tidy_iea_df <- .tidy_iea_df %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]]) %>%
    dplyr::summarise(
      "{n_counting}" := dplyr::n()
    ) %>%
    dplyr::select(-dplyr::any_of(n_counting))
  
  
  # Calculates total input or output per main activity producer, according to the value passed to the split_using_shares_of argument
  
  if (split_using_shares_of == "input"){
    total_main_activity <- .tidy_iea_df %>%
      dplyr::filter(
        .data[[flow]] %in% c(main_act_producer_elect, main_act_producer_chp, main_act_producer_heat)
        & .data[[flow_aggregation_point]] == transformation_processes
        & .data[[ledger_side]] == supply
        & .data[[e_dot]] < 0
      ) %>%
      dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]], .data[[ledger_side]], .data[[flow_aggregation_point]]) %>%
      dplyr::summarise(
        Total_main_activity_From_Func = sum(.data[[e_dot]])
      )
  } else if (split_using_shares_of == "output"){
    total_main_activity <- .tidy_iea_df %>%
      dplyr::filter(
        .data[[flow]] %in% c(main_act_producer_elect, main_act_producer_chp, main_act_producer_heat)
        & .data[[flow_aggregation_point]] == transformation_processes
        & .data[[ledger_side]] == supply
        & .data[[e_dot]] > 0
      ) %>%
      dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]], .data[[ledger_side]], .data[[flow_aggregation_point]]) %>%
      dplyr::summarise(
        Total_main_activity_From_Func = sum(.data[[e_dot]])
      )
  }
  
  # Find out which observations (Country, Method, Energy.type, Last.stage, Year) are NOT in the total computed
  list_not_included_total_main_activity <- df_observations_included_tidy_iea_df %>%
    dplyr::anti_join(total_main_activity, by = c({country}, {method}, {energy_type}, {last_stage}, {year})) %>%
    tidyr::unite(col = "ID", .data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]]) %>%
    dplyr::pull()
  
  # Now, finds input or output per main activity, according to the value passed to the split_using_shares_of argument
  
  if (split_using_shares_of == "input"){
    total_per_main_activity <- .tidy_iea_df %>%
      dplyr::filter(
        .data[[flow]] %in% c(main_act_producer_elect, main_act_producer_chp, main_act_producer_heat)
        & .data[[flow_aggregation_point]] == transformation_processes
        & .data[[ledger_side]] == supply
        & .data[[e_dot]] < 0
      ) %>%
      dplyr::group_by(
        .data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[flow]], .data[[unit]], .data[[ledger_side]], .data[[flow_aggregation_point]]
      ) %>%
      dplyr::summarise(
        Total_per_main_activity_From_Func = sum(.data[[e_dot]])
      )
  } else if (split_using_shares_of == "output"){
    total_per_main_activity <- .tidy_iea_df %>%
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
        Total_per_main_activity_From_Func = sum(.data[[e_dot]])
      )
  }

  # Now, figure out the shares of input or output per main activity
  share_total_per_main_activity <- total_per_main_activity %>%
    dplyr::left_join(
      total_main_activity, by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {unit}, {ledger_side}, {flow_aggregation_point})
    ) %>%
    dplyr::mutate(
      Share_per_main_activity_From_Func = Total_per_main_activity_From_Func / Total_main_activity_From_Func
    ) %>%
    # dplyr::select(-.data[[flow_aggregation_point]])
    dplyr::select(-dplyr::any_of(flow_aggregation_point))
  
  
  # Then, routes the "Own use in electricity, CHP and heat plants" EIOU flow to the different main activity producer plants,
  # According to the shares previously determined.
  routed_own_use_with_main_activity <- .tidy_iea_df %>%
    dplyr::filter(.data[[flow]] == own_use_elect_chp_heat) %>%
    dplyr::filter(!(stringr::str_c(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], sep = "_")
                    %in% list_not_included_total_main_activity)) %>%
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
      share_total_per_main_activity, by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {flow}, {unit}, {ledger_side})
    ) %>%
    dplyr::mutate(
      "{e_dot}" := .data[[e_dot]] * Share_per_main_activity_From_Func
    ) %>%
    dplyr::select(-Share_per_main_activity_From_Func, -Total_per_main_activity_From_Func, -Total_main_activity_From_Func)
  
  
  # Routes the "Own use in electricity, CHP and heat plants" to "Main activity producer electricity plants"
  # When no Main activity producer plants are in transformation processes.
  routed_own_use_without_main_activity <- .tidy_iea_df %>%
    dplyr::filter(.data[[flow]] == own_use_elect_chp_heat) %>%
    dplyr::filter(stringr::str_c(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], sep = "_")
                  %in% list_not_included_total_main_activity) %>%
    dplyr::mutate(
      "{flow}" := dplyr::case_when(
        (.data[[flow]] == own_use_elect_chp_heat & .data[[flow_aggregation_point]] == eiou) ~ main_act_producer_elect,
        TRUE ~ .data[[flow]]
      )
    )
  
  # Binding rows.
  routed_own_use <- dplyr::bind_rows(routed_own_use_with_main_activity, routed_own_use_without_main_activity)
  
  # Adding up and returning data frame.
  tidy_iea_df_routed_own_use <- .tidy_iea_df %>%
    dplyr::filter(.data[[flow]] != own_use_elect_chp_heat) %>%
    dplyr::bind_rows(routed_own_use) %>%
    
    
    # We no longer want to discriminate between positive and negative values.
    # That's because Japan has some EIOU for Pumped storage plants that is positive.
    # We want to pull the positive values into Main activity producer electricity plants.
    # ---MKH, 6 Sept 2021
    # 
    # Aggregating. We need to add a pos/neg/null column to add up differently positive and negative values, otherwise we'd only get NET flows.
    # dplyr::mutate(
    #   "{negzeropos}" := dplyr::case_when(
    #     .data[[e_dot]] < 0 ~ "neg",
    #     .data[[e_dot]] == 0 ~ "zero",
    #     .data[[e_dot]] > 0 ~ "pos"
    #   )
    # ) %>%
    
    
    
    
    # Now sum similar rows using summarise.
    # Group by everything except the energy flow rate column, "E.dot".
    matsindf::group_by_everything_except(e_dot) %>%
    dplyr::summarise(
      "{e_dot}" := sum(.data[[e_dot]])
    ) %>%
    
    
    
    # dplyr::mutate(
    #   #Eliminate the column we added.
    #   "{negzeropos}" := NULL
    # ) %>%
    
    
    
    dplyr::ungroup()
  
  return(tidy_iea_df_routed_own_use)
}



#' Adds a nuclear industry
#' 
#' In the IEA World Energy Extended Balances, there is a "Nuclear industry" Energy industry own use flow, 
#' but there is no "Nuclear industry" in transformation processes flows, 
#' which prevents from defining a nuclear industry in the PSUT.
#' However, using the World Energy Extended Balances documentation, one can deduce from the amount of nuclear fuel used
#' by "Main activity producer electricity plants" and "Main activity producer CHP plants" 
#' the energy transformation due to the nuclear industry. This function performs that task.
#' The function is called within the `specify_all()` function.
#' 
#' The World Energy Extended Balances documentation states that "The primary energy equivalent of nuclear electricity is
#' calculated from the gross generation by assuming a 33% conversion efficiency. The calculation to be carried out
#' is the following: gross electricity generation in TWh x0.086 / 0.33 = primary energy equivalent in Mtoe."
#' 
#' Hence this function does the following:
#'  * the Nuclear fuel consumed by Main activity producer electricity & heat plants is ascribed to nuclear industry plants;
#'  * the output of Main activity producer electricity and heat plants to be directed to nuclear plants is determined by
#'        multiplying their nuclear fuel consumption per 0.33. In the case of CHP plants, that output is divided into
#'        heat and electricity according to the shares of output of each of these two products;
#'  * the output ascribed to nuclear plants is subtracted from Main activity producer electricity and heat plants.
#' 
#' @param .tidy_iea_df The `.tidy_iea_df` which flows need to be specified.
#' @param flow_aggregation_point The name of the flow aggregation point column in the `.tidy_iea_df`.
#'                               Default is `IEATools::iea_cols$flow_aggregation_point`.
#' @param flow The name of the flow column in the `.tidy_iea_df`.
#'             Default is `IEATools::iea_cols$flow`.
#' @param e_dot The name of the energy column in the `.tidy_iea_df`.
#'              Default is `IEATools::iea_cols$e_dot`.
#' @param product The name of the product column in the `.tidy_iea_df`.
#'                Default is `IEATools::iea_cols$product`.
#' @param method The name of the method column in the `.tidy_iea_df`.
#'               Default is `IEATools::iea_cols$method`.
#' @param ledger_side The name of the ledger side column in the `.tidy_iea_df`.
#'                    Default is `IEATools::iea_cols$ledger_side`.
#' @param last_stage The name of the last stage column in the `.tidy_iea_df`.
#'                   Default is `IEATools::iea_cols$last_stage`.
#' @param energy_type The name of the energy type column in the `.tidy_iea_df`.
#'                    Default is `IEATools::iea_cols$energy_type`.
#' @param country The name of the country column in the `.tidy_iea_df`.
#'                Default is `IEATools::iea_cols$country`.
#' @param year The name of the year column in the `.tidy_iea_df`.
#'             Default is `IEATools::iea_cols$year`.
#' @param unit The name of the unit column in the `.tidy_iea_df`.
#'             Default is `IEATools::iea_cols$unit`.
#' @param eiou A string identifying the energy industry own use in the `flow_aggregation_point` column in the `.tidy_iea_df`.
#'             Default is `IEATools::aggregation_flows$energy_industry_own_use`.
#' @param transformation_processes A string identifying the transformation processes in the `flow_aggregation_point` column in the `.tidy_iea_df`.
#'                                 Default is `IEATools::aggregation_flows$transformation_processes`.
#' @param nuclear_industry A string identifying "Nuclear industry" in the `flow` column of the `.tidy_iea_df`.
#'                         Default is `IEATools::eiou_flows$nuclear_industry`.
#' @param main_act_producer_elect A string identifying "Main activity producer electricity plants" in the `flow` column of the `.tidy_iea_df`.
#'                                Default is `IEATools::main_act_plants$main_act_prod_elect_plants`.
#' @param main_act_producer_chp A string identifying "Main activity producer CHP plants" in the `flow` column of the `.tidy_iea_df`.
#'                              Default is `IEATools::main_act_plants$main_act_prod_chp_plants`.
#' @param autoproducer_elect A string identifying "Autoproducer electricity plants" in the `flow` column of the `.tidy_iea_df`.
#'                           Default is `IEATools::main_act_plants$autoprod_elect_plants`.
#' @param autoproducer_chp A string identifying "Autoproducer CHP plants" in the `flow` column of the `.tidy_iea_df`.
#'                         Default is `IEATools::transformation_processes$autoproducer_CHP_plants`.
#' @param nuclear A string identifying the "Nuclear" product in the `product` column of the `tidy_iea_df`.
#'                Default is "Nuclear".
#' @param electricity A string identifying the "Electricity" product in the `product` column of the `tidy_iea_df`.
#'                    Default is "Electricity".
#' @param heat A string identifying the "Heat" product in the `product` column of the `tidy_iea_df`.
#'             Default is "Heat".
#' @param negzeropos The name of a temporary column added to the data frame.
#'                   Default is ".negzeropos".
#' @param share_elect_output_From_Func A temporary column added to the data frame.
#'                                     Default is ".share_elect_output_From_Func".
#' @param Electricity_Nuclear A temporary column and product name added to the data frame, which identifies the production of electricity by nuclear plants.
#'                            Default is "Electricity_Nuclear".
#' @param Heat_Nuclear A temporary column and product name added to the data frame, which identifies the production of heat by nuclear plants.
#'                     Default is "Heat_Nuclear".
#' @param ratio_output_to_nuclear_fuel A parameter that describes the correspondance between input of nuclear fuel and output of electricity and/or heat.
#'                                     The IEA World Energy Extended Balances state that the value adopted in the balances is 0.33, which is therefore
#'                                     the default value of the parameter.
#'
#' @return A modified version of the `.tidy_iea_df`, with a nuclear industry added as an additional transformation process.
#' @export
#'
#' @examples
#' library(dplyr)
#' load_tidy_iea_df() %>% 
#'   add_nuclear_industry()
add_nuclear_industry <- function(.tidy_iea_df,
                                 # Column names
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
                                 # Strings identifying flows, ledger sides, flow aggregation points, and products
                                 eiou = IEATools::aggregation_flows$energy_industry_own_use,
                                 transformation_processes = IEATools::aggregation_flows$transformation_processes,
                                 nuclear_industry = IEATools::eiou_flows$nuclear_industry,
                                 main_act_producer_elect = IEATools::main_act_plants$main_act_prod_elect_plants,
                                 main_act_producer_chp = IEATools::main_act_plants$main_act_prod_chp_plants,
                                 autoproducer_elect = IEATools::main_act_plants$autoprod_elect_plants,
                                 autoproducer_chp = IEATools::transformation_processes$autoproducer_CHP_plants,
                                 nuclear = "Nuclear",
                                 electricity = "Electricity",
                                 heat = "Heat",
                                 # Strings identifying temporary column names
                                 negzeropos = ".negzeropos",
                                 share_elect_output_From_Func = ".share_elect_output_From_Func",
                                 Electricity_Nuclear = "Electricity_Nuclear",
                                 Heat_Nuclear = "Heat_Nuclear",
                                 # Constant
                                 ratio_output_to_nuclear_fuel = 0.33){
  
  products_tibble <- tibble::tibble("{nuclear}" := NA,
                                    "{electricity}" := NA,
                                    "{heat}" := NA)
  
  # Here we keep only the flows that we are going to modify:
  intermediary_modified_flows <- .tidy_iea_df %>%
    dplyr::filter(
      .data[[flow_aggregation_point]] == transformation_processes &
        ((.data[[flow]] %in% c(main_act_producer_elect, autoproducer_elect) & .data[[product]] %in% c(nuclear, electricity)) |
           (.data[[flow]] %in% c(main_act_producer_chp, autoproducer_chp) & .data[[product]] %in% c(nuclear, electricity, heat)))
    ) %>%
    tidyr::pivot_wider(names_from = .data[[product]], values_from = .data[[e_dot]]) %>%
    # dplyr::select(-tidyselect::any_of({e_dot})) 
    dplyr::select(-tidyselect::any_of(e_dot)) 
  
  
  names_intermediary_modified_flows <- names(intermediary_modified_flows)
  
  modified_flows <- intermediary_modified_flows %>% 
    tibble::add_column(!!products_tibble[! names(products_tibble) %in% names_intermediary_modified_flows]) %>%
    dplyr::mutate(
      "{nuclear}" := tidyr::replace_na(.data[[nuclear]], 0),
      "{electricity}" := tidyr::replace_na(.data[[electricity]], 0),
      "{heat}" := tidyr::replace_na(.data[[heat]], 0)
    ) %>%
    dplyr::mutate(
      "{share_elect_output_From_Func}" := .data[[electricity]] / (.data[[electricity]] + .data[[heat]]),
      "{electricity}" := .data[[electricity]] + (.data[[nuclear]] * ratio_output_to_nuclear_fuel) * .data[[share_elect_output_From_Func]],
      "{heat}" := .data[[heat]] + (.data[[nuclear]] * ratio_output_to_nuclear_fuel) * (1 - .data[[share_elect_output_From_Func]]),
      "{Electricity_Nuclear}" := - .data[[nuclear]] * ratio_output_to_nuclear_fuel * .data[[share_elect_output_From_Func]],
      "{Heat_Nuclear}" := - .data[[nuclear]] * ratio_output_to_nuclear_fuel * (1 - .data[[share_elect_output_From_Func]])
    ) %>%
    dplyr::select(-.data[[share_elect_output_From_Func]]) %>%
    tidyr::pivot_longer(cols = c({electricity}, {heat}, {nuclear}, {Electricity_Nuclear}, {Heat_Nuclear}), values_to = {e_dot}, names_to = {product}) %>%
    dplyr::filter(.data[[e_dot]] != 0) %>%
    dplyr::mutate(
      "{flow}" := dplyr::case_when(
        stringr::str_detect(.data[[product]], nuclear) ~ nuclear_industry,
        TRUE ~ .data[[flow]]
      ),
      "{product}" := stringr::str_remove(.data[[product]], stringr::str_c("_", nuclear))
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



#' Routes non specified flows
#'
#' This function is a wrapper of the functions 
#' `route_non_specified_eiou` and `route_non_specified_tp`.
#' It is called within the `specify_all()` function.
#' 
#' See `route_non_specified_eiou` and `route_non_specified_tp` functions documentations for additional details.
#'
#' @param .tidy_iea_df The `.tidy_iea_df` which flows need to be specified.
#' @param route_non_specified_eiou A boolean that indicates whether non specified EIOU flows should be routed to existing
#'                                     industries or kept as non specified.
#' @param route_non_specified_tp A boolean that indicates whether non specified transformation processes flows should be routed to existing
#'                                   industries or kept as non specified.
#'
#' @return A modified version of the `.tidy_iea_df` with non specified flows routed to existing industries.
#' @export
#'
#' @examples
#' library(dplyr)
#' load_tidy_iea_df() %>% 
#'   route_non_specified_flows()
route_non_specified_flows <- function(.tidy_iea_df,
                                      route_non_specified_eiou = TRUE,
                                      route_non_specified_tp = TRUE
                                      ){
  .tidy_iea_df %>%
    route_non_specified_eiou(
      route_non_specified_eiou = route_non_specified_eiou
    ) %>%
    route_non_specified_tp(
      route_non_specified_tp = route_non_specified_tp
    )
}


#' Routes non specified EIOU flows to existing industries
#' 
#' This function routes the non-specified EIOU flow to other existing industries.
#' It does so using the shares of EIOU use of the other EIOU industries.
#' If no EIOU flow different from "Non-specified" is available in the `.tidy_iea_df`,
#' then the "Non-specified" EIOU flow is kept as it is.
#' The function is called within the `route_non_specified_flows()` function.
#' Note that the `routing_non_specified_eiou` parameter enables to switch on and off the routing of the non-specified EIOU flow.
#'
#' @param .tidy_iea_df The `.tidy_iea_df` which flows need to be specified.
#' @param route_non_specified_eiou A boolean indicating whether non-specified EIOU flows should be redirected to other existing industries.
#'                                   If FALSE, the function returns the input data frame.
#'                                   Default is TRUE.
#' @param country The name of the country column in the `.tidy_iea_df`.
#'                Default is `IEATools::iea_cols$country`.
#' @param flow_aggregation_point The name of the flow aggregation point column in the `.tidy_iea_df`.
#'                               Default is `IEATools::iea_cols$flow_aggregation_point`.
#' @param flow The name of the flow column in the `.tidy_iea_df`.
#'                Default is `IEATools::iea_cols$flow`.
#' @param ledger_side The name of the ledger side column in the `.tidy_iea_df`.
#'                Default is `IEATools::iea_cols$ledger_side`.
#' @param method The name of the method column in the `.tidy_iea_df`.
#'                Default is `IEATools::iea_cols$method`.
#' @param energy_type The name of the energy type column in the `.tidy_iea_df`.
#'                Default is `IEATools::iea_cols$energy_type`.
#' @param last_stage The name of the last stage column in the `.tidy_iea_df`.
#'                Default is `IEATools::iea_cols$last_stage`.
#' @param year The name of the year column in the `.tidy_iea_df`.
#'                Default is `IEATools::iea_cols$year`.
#' @param product The name of the product column in the `.tidy_iea_df`.
#'                Default is `IEATools::iea_cols$product`.
#' @param unit The name of the unit column in the `.tidy_iea_df`.
#'                Default is `IEATools::iea_cols$unit`.
#' @param e_dot The name of the energy column in the `.tidy_iea_df`.
#'                Default is `IEATools::iea_cols$e_dot`.
#' @param eiou A string identifying "Energy industry own use" in the `flow_aggregation_point` column of the `.tidy_iea_df`.
#'             Default is `IEATools::aggregation_flows$energy_industry_own_use`.
#' @param non_spec A string identifying "Non-specified" in the `flow` column of the `.tidy_iea_df`.
#'                 Default is "Non-specified".
#' @param negzeropos The name of a temporary column added to the data frame.
#'                   Default is ".negzeropos".
#' @param n_counting The name of a temporary column added to the data frame.
#'                   Default is ".n_counting".
#' @param Total_eiou_excl_nonspec_From_Func The name of a temporary column added to the data frame.
#'                   Default is ".Total_eiou_excl_nonspec_From_Func".
#' @param EIOU_per_industry_From_Func The name of a temporary column added to the data frame.
#'                   Default is ".EIOU_per_industry_From_Func".
#' @param Share_eiou_per_industry_From_Func The name of a temporary column added to the data frame.
#'                   Default is ".Share_eiou_per_industry_From_Func".
#' @param destination_flow The name of a temporary column added to the data frame.
#'                   Default is ".destination_flow".
#'
#' @return A modified version of the `.tidy_iea_df` with the non-specified EIOU flow routed to existing industries.
#' @export
#'
#' @examples
#' library(dplyr)
#' load_tidy_iea_df() %>% 
#'   route_non_specified_eiou()
route_non_specified_eiou <- function(.tidy_iea_df,
                                     route_non_specified_eiou = TRUE,
                                     # Column names
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
                                     # String identifying flow aggregation point and flows
                                     eiou = IEATools::aggregation_flows$energy_industry_own_use,
                                     non_spec = "Non-specified",
                                     # Temporary columns
                                     negzeropos = ".negzeropos",
                                     n_counting = ".n_counting",
                                     Total_eiou_excl_nonspec_From_Func = ".Total_eiou_excl_nonspec_From_Func",
                                     EIOU_per_industry_From_Func = ".EIOU_per_industry_From_Func",
                                     Share_eiou_per_industry_From_Func = ".Share_eiou_per_industry_From_Func",
                                     destination_flow = ".destination_flow"){
  
  
  if (isFALSE(route_non_specified_eiou)){
    return(.tidy_iea_df)
  }
  
  # Figuring out lists of observations (Country, Method, Energy_type, Last_stage, Year)
  df_observations_included_tidy_iea_df <- .tidy_iea_df %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]]) %>%
    dplyr::summarise(
      "{n_counting}" := dplyr::n()
    ) %>%
    dplyr::select(-.data[[n_counting]])

  # Calculating total EIOU flows excluding non-specified flows.  
  total_eiou_excl_nonspec <- .tidy_iea_df %>%
    dplyr::filter(
      .data[[flow_aggregation_point]] == eiou & .data[[flow]] != non_spec
    ) %>%
    dplyr::group_by(
      .data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]], .data[[ledger_side]], .data[[flow_aggregation_point]]
    ) %>%
    dplyr::summarise(
      "{Total_eiou_excl_nonspec_From_Func}" := sum(.data[[e_dot]])
    )
  
  # Figuring out which observations (Country, Method, Energy_type, Last_stage, Year) do not have an EIOU flow other than non-specified
  list_not_included_total_eiou <- df_observations_included_tidy_iea_df %>%
    dplyr::anti_join(total_eiou_excl_nonspec, by = c({country}, {method}, {energy_type}, {last_stage}, {year})) %>%
    tidyr::unite(col = "ID", .data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]]) %>%
    dplyr::pull()
  
  # Figuring out the total EIOU per industry
  eiou_per_industry <- .tidy_iea_df %>%
    dplyr::filter(
      .data[[flow_aggregation_point]] == eiou & .data[[flow]] != non_spec
    ) %>%
    dplyr::group_by(
      .data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[flow]], .data[[unit]], .data[[ledger_side]], .data[[flow_aggregation_point]]
    ) %>%
    dplyr::summarise(
      "{EIOU_per_industry_From_Func}" := sum(.data[[e_dot]])
    )
  
  # Figuring out the shares of EIOU per industry
  share_eiou_per_industry <- eiou_per_industry %>%
    dplyr::left_join(
      total_eiou_excl_nonspec, by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {unit}, {ledger_side}, {flow_aggregation_point})
    ) %>%
    dplyr::mutate(
      "{Share_eiou_per_industry_From_Func}" := .data[[EIOU_per_industry_From_Func]] / .data[[Total_eiou_excl_nonspec_From_Func]]
    ) %>%
    dplyr::select(-.data[[flow_aggregation_point]])
  
  
  # Pulling the list of EIOU flows excluding the non-specified one
  list_eiou_flows_excl_nonspec <- .tidy_iea_df %>%
    dplyr::filter(.data[[flow_aggregation_point]] == eiou & .data[[flow]] != non_spec) %>%
    tidyr::expand(.data[[flow]]) %>%
    dplyr::pull()
  
  # First, when EIOU flows other than non-specified are available, then split according to the shares
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
      "{destination_flow}" := list_eiou_flows_excl_nonspec
    ) %>%
    dplyr::mutate(
      "{flow}" := .data[[destination_flow]]
    ) %>%
    # dplyr::select(-destination_flow) %>%
    dplyr::select(-dplyr::all_of(destination_flow)) %>%
    dplyr::inner_join(
      share_eiou_per_industry, by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {flow}, {unit}, {ledger_side})
    ) %>%
    dplyr::mutate(
      "{e_dot}" := .data[[e_dot]] * .data[[Share_eiou_per_industry_From_Func]]
    ) %>%
    dplyr::select(-.data[[Share_eiou_per_industry_From_Func]], -.data[[EIOU_per_industry_From_Func]], -.data[[Total_eiou_excl_nonspec_From_Func]])
  
  
  # Second, when EIOU flows other than non-specified are not available, then keep non-specified
  # That's operated in the filter.
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



#' Routes non-specified transformation processes flows to existing industries
#' 
#' This function routes non-specified transformation processes flows to existing industries.
#' It does so using the shares of product use and supply of the other transformation processes.
#' If no transformation processes consume or supply a product that is present in the non-specified flows,
#' then the flow remains non-specified.
#' The function is called within the `route_non_specified_flows()` function.
#' Note that the `routing_non_specified_eiou` parameter enables to switch on and off the routing of the non-specified EIOU flow.
#'
#' @param .tidy_iea_df The `.tidy_iea_df` which flows need to be specified.
#' @param route_non_specified_tp A boolean indicating whether non-specified EIOU flows should be redirected to other existing industries.
#'                                 If FALSE, the function returns the input data frame.
#'                                 Default is TRUE.
#' @param country The name of the country column in the `.tidy_iea_df`.
#'                Default is `IEATools::iea_cols$country`.
#' @param flow_aggregation_point The name of the flow aggregation point column in the `.tidy_iea_df`.
#'                Default is `IEATools::iea_cols$flow_aggregation_point`.
#' @param flow The name of the flow column in the `.tidy_iea_df`.
#'                Default is `IEATools::iea_cols$flow`.
#' @param ledger_side The name of the ledger side column in the `.tidy_iea_df`.
#'                Default is `IEATools::iea_cols$ledger_side`.
#' @param method The name of the method column in the `.tidy_iea_df`.
#'                Default is `IEATools::iea_cols$method`.
#' @param energy_type The name of the energy type column in the `.tidy_iea_df`.
#'                Default is `IEATools::iea_cols$energy_type`.
#' @param last_stage The name of the last stage column in the `.tidy_iea_df`.
#'                Default is `IEATools::iea_cols$last_stage`.
#' @param year The name of the country year in the `.tidy_iea_df`.
#'                Default is `IEATools::iea_cols$year`.
#' @param product The name of the product column in the `.tidy_iea_df`.
#'                Default is `IEATools::iea_cols$product`.
#' @param unit The name of the unit column in the `.tidy_iea_df`.
#'                Default is `IEATools::iea_cols$unit`.
#' @param e_dot The name of the energy column in the `.tidy_iea_df`.
#'                Default is `IEATools::iea_cols$energy`.
#' @param transformation_processes A string that identifies "Transformation processes" in the `flow_aggregation_point` column of the `.tidy_iea_df`.
#'                                 Default is `IEATools::aggregation_flows$transformation_processes`.
#' @param non_spec A string that identifies "Non-specified" flows in the `flow` column of the `.tidy_iea_df`.
#'                 Default is "Non-specified".
#' @param negzeropos The name of a temporary column added to the data frame.
#'                   Default is ".negzeropos".
#' @param n_counting The name of a temporary column added to the data frame.
#'                   Default is ".n_counting".
#' @param Total_input_output_by_prod_excl_nonspec The name of a temporary column added to the data frame.
#'                                                          Default is ".Total_input_output_by_prod_excl_nonspec_From_Func".
#' @param Input_output_by_prod_per_tp The name of a temporary column added to the data frame.
#'                                              Default is ".Input_output_by_prod_per_tp_From_Func".
#' @param Share_input_output_by_prod_per_tp The name of a temporary column added to the data frame.
#'                                                    Default is ".Share_input_output_by_prod_per_tp_From_Func".
#' @param destination_flow The name of a temporary column added to the data frame.
#'                         Default is ".destination_flow".
#'
#' @return A modified version of the `.tidy_iea_df` with non-specified transformation processes flows routes to existing industries.
#' @export
#'
#' @examples
#' library(dplyr)
#' load_tidy_iea_df() %>% 
#'   route_non_specified_tp()
route_non_specified_tp <- function(.tidy_iea_df,
                                   route_non_specified_tp = TRUE,
                                   # Column names
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
                                   # Strings identifying flow aggregation point and flow
                                   transformation_processes = IEATools::aggregation_flows$transformation_processes,
                                   non_spec = "Non-specified",
                                   # Temporary column names
                                   negzeropos = ".negzeropos",
                                   n_counting = ".n_counting",
                                   Total_input_output_by_prod_excl_nonspec = ".Total_input_output_by_prod_excl_nonspec",
                                   Input_output_by_prod_per_tp = ".Input_output_by_prod_per_tp",
                                   Share_input_output_by_prod_per_tp = ".Share_input_output_by_prod_per_tp",
                                   destination_flow = ".destination_flow"){
  
  if (isFALSE(route_non_specified_tp)){
    return(.tidy_iea_df)
  }
  
  # Getting a list of all observations (Country, Method, Energy type, LAst stage, Year, Product, and Sign) included in transformation processes
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
      "{n_counting}" := dplyr::n()
    ) %>%
    dplyr::select(-.data[[n_counting]])
  
  # Figuring out total input and output by product in transformation processes, excluding non-specified flows
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
      "{Total_input_output_by_prod_excl_nonspec}" := sum(.data[[e_dot]])
    )
  
  # Figuring out the list of products and signs not available in the transformation processes elsewhere than in non-specified
  list_not_included_total_input_output_by_prod_tps <- df_observations_included_tidy_iea_df %>%
    dplyr::anti_join(total_input_output_by_prod_tps, by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {product}, {negzeropos})) %>%
    tidyr::unite(col = "ID", .data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product]], .data[[negzeropos]]) %>%
    dplyr::pull()
  
  # Figuring out input and output by product for each transformation process
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
      "{Input_output_by_prod_per_tp}" := sum(.data[[e_dot]])
    )
  
  # Figuring out the shares of input and output for each product by transformation process
  share_input_output_by_prod_per_tp <- input_output_by_prod_per_tp %>%
    dplyr::left_join(
      total_input_output_by_prod_tps,
      by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {unit}, {ledger_side}, {flow_aggregation_point}, {product}, {negzeropos})
    ) %>%
    dplyr::mutate(
      "{Share_input_output_by_prod_per_tp}" := .data[[Input_output_by_prod_per_tp]] / .data[[Total_input_output_by_prod_excl_nonspec]]
    ) #%>%
    # dplyr::ungroup() %>% 
    # dplyr::select(-.data[[flow_aggregation_point]])
  
  # Figuring out the list of observations excluding non-specified
  list_tp_flows_excl_nonspec <- .tidy_iea_df %>%
    dplyr::filter(.data[[flow_aggregation_point]] == transformation_processes & .data[[flow]] != non_spec) %>%
    tidyr::expand(.data[[flow]]) %>%
    dplyr::pull()
  
  
  # When tps with the given product and sign are available in the data frame, then we split the flow
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
      "{destination_flow}" := list_tp_flows_excl_nonspec
    ) %>%
    dplyr::mutate(
      "{flow}" := .data[[destination_flow]]
    ) %>%
    # dplyr::select(-destination_flow) %>%
    dplyr::select(-dplyr::all_of(destination_flow)) %>%
    dplyr::inner_join(
      share_input_output_by_prod_per_tp,
      by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {flow_aggregation_point}, {flow}, {unit}, {ledger_side}, {product}, {negzeropos})
    ) %>%
    dplyr::mutate(
      "{e_dot}" := .data[[e_dot]] * .data[[Share_input_output_by_prod_per_tp]]
    ) %>%
    dplyr::select(-.data[[Share_input_output_by_prod_per_tp]],
                  -.data[[Input_output_by_prod_per_tp]],
                  -.data[[Total_input_output_by_prod_excl_nonspec]])
  
  
  # When tps with the given product and sign are NOT available in the data frame, then we keep the flow as it is
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
