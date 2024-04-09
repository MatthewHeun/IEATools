
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
#' @param specify_renewable_plants A boolean indicating whether renewable energy plants should be specified or not.
#'                                 Default is FALSE.
#' @param flow_aggregation_point The name of the flow aggregation point column in the `.tidy_iea_df`.
#'                               Default is `IEATools::iea_cols$flow_aggregation_point`.
#' @param country,method,energy_type,last_stage,unit,year,product See `IEATools::iea_cols`.
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
#' @param hydro_plants The name of the newly created hydropower industry.
#'                     Default is `IEATools::renewable_industries$hydro_plants`.
#' @param hydro The name of the "Hydro" product.
#'              Default is `IEATools::renewable_products$hydro`.
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
                                 specify_renewable_plants = FALSE,
                                 # Column names
                                 flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                                 country = IEATools::iea_cols$country,
                                 method = IEATools::iea_cols$method,
                                 energy_type = IEATools::iea_cols$energy_type,
                                 year = IEATools::iea_cols$year,
                                 last_stage = IEATools::iea_cols$last_stage,
                                 unit = IEATools::iea_cols$unit,
                                 flow = IEATools::iea_cols$flow,
                                 e_dot = IEATools::iea_cols$e_dot,
                                 product = IEATools::iea_cols$product,
                                 # Flow and flow aggregation point names
                                 eiou = IEATools::aggregation_flows$energy_industry_own_use,
                                 pumped_storage = IEATools::eiou_flows$pumped_storage_plants,
                                 main_act_producer_elect = IEATools::main_act_plants$main_act_prod_elect_plants,
                                 hydro_plants = IEATools::renewable_industries$hydro_plants,
                                 hydro = IEATools::renewable_products$hydro,
                                 # Temporary column name
                                 negzeropos = ".negzeropos"){
  
  if (isFALSE(specify_renewable_plants)){
    routed_phs <- .tidy_iea_df %>%
      dplyr::mutate(
        "{flow}" := dplyr::case_when(
          (.data[[flow]] == pumped_storage & .data[[flow_aggregation_point]] == eiou) ~ main_act_producer_elect,
          TRUE ~ .data[[flow]]
        )
      ) %>%
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
      dplyr::mutate(
        # Eliminate the column we added.
        "{negzeropos}" := NULL
      ) %>%
      dplyr::ungroup()
    
  } else {
    
    # Listing observations for which pumped hydro EIOU should be routed to the new "Hydro" industry
    hydro_observations <- .tidy_iea_df |> 
      dplyr::filter(.data[[product]] == hydro) |> 
      tidyr::expand(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]])
    
    # Routing pumped hydro to the "Hydro" industry for these observations
    routed_to_hydropower <- .tidy_iea_df |> 
      dplyr::inner_join(hydro_observations, by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {unit})) |> 
      dplyr::mutate(
        "{flow}" := dplyr::case_when(
          (.data[[flow]] == pumped_storage & .data[[flow_aggregation_point]] == eiou) ~ hydro_plants,
          TRUE ~ .data[[flow]]
        )
      )
    
    # Routing pumped hydro to Main activity producer electricity plants for remaining observations
    routed_to_elec_plants <- .tidy_iea_df |> 
      dplyr::anti_join(hydro_observations, by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {unit})) |> 
      dplyr::mutate(
        "{flow}" := dplyr::case_when(
          (.data[[flow]] == pumped_storage & .data[[flow_aggregation_point]] == eiou) ~ main_act_producer_elect,
          TRUE ~ .data[[flow]]
        )
      )
    
    # Binding both data frames and clearing up
    routed_phs <- routed_to_hydropower |> 
      dplyr::bind_rows(routed_to_elec_plants) |> 
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
      dplyr::mutate(
        # Eliminate the column we added.
        "{negzeropos}" := NULL
      ) %>%
      dplyr::ungroup()
  }
  return(routed_phs)
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
      suffix = c("", ".y"), 
      # The new policy in dplyr is to warn about multiple rows being created.
      # Creating multiple rows is the desired behavior here.
      # Setting multiple = "all" eliminates the warning.
      # multiple = "all"
      # The latest approach by dplyr to solve this problem involves the relationahip argument
      relationship = "many-to-many"
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
        "{Total_main_activity_From_Func}" := sum(.data[[e_dot]])
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
        "{Total_main_activity_From_Func}" := sum(.data[[e_dot]])
      )
  }
  
  # Find out which observations (Country, Method, Energy.type, Last.stage, Year) are NOT in the total computed
  list_not_included_total_main_activity <- df_observations_included_tidy_iea_df %>%
    dplyr::anti_join(total_main_activity, by = c({country}, {method}, {energy_type}, {last_stage}, {year})) %>%
    # tidyr::unite(col = "ID", .data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]]) %>%
    tidyr::unite(col = "ID", dplyr::all_of(c(country, method, energy_type, last_stage, year))) %>%
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
        "{Total_per_main_activity_From_Func}" := sum(.data[[e_dot]])
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
        "{Total_per_main_activity_From_Func}" := sum(.data[[e_dot]])
      )
  }

  # Now, figure out the shares of input or output per main activity
  share_total_per_main_activity <- total_per_main_activity %>%
    dplyr::left_join(
      total_main_activity, by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {unit}, {ledger_side}, {flow_aggregation_point})
    ) %>%
    dplyr::mutate(
      "{Share_per_main_activity_From_Func}" := .data[[Total_per_main_activity_From_Func]] / .data[[Total_main_activity_From_Func]]
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
      "{destination_flow}" := c(main_act_producer_elect, main_act_producer_chp, main_act_producer_heat)
    ) %>%
    dplyr::mutate(
      "{flow}" := .data[[destination_flow]]
    ) %>%
    # dplyr::select(-.data[[destination_flow]]) %>%
    dplyr::select(-dplyr::any_of(destination_flow)) %>%
    dplyr::inner_join(
      share_total_per_main_activity, by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {flow}, {unit}, {ledger_side})
    ) %>%
    dplyr::mutate(
      "{e_dot}" := .data[[e_dot]] * .data[[Share_per_main_activity_From_Func]]
    ) %>%
    # dplyr::select(-.data[[Share_per_main_activity_From_Func]], -.data[[Total_per_main_activity_From_Func]], -.data[[Total_main_activity_From_Func]])
    dplyr::select(-dplyr::any_of(c(Share_per_main_activity_From_Func, Total_per_main_activity_From_Func, Total_main_activity_From_Func)))
  
  
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
#' @param ascribe_eiou_to_nuclear A boolean defining whether a fraction of the EIOU of electricity, CHP and heat plants
#'                                should be ascribed to the new nuclear industry. Default is FALSE.
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
#' @param own_use_elect_chp_heat A string identifying "Own use in electricity, CHP and heat plants" in the `flow` column of the `.tidy_iea_df`.
#'                               Default is `IEATools::eiou_flows$own_use_elect_chp_heat_plants`.
#' @param nuclear A string identifying the "Nuclear" product in the `product` column of the `tidy_iea_df`.
#'                Default is "Nuclear".
#' @param electricity A string identifying the "Electricity" product in the `product` column of the `tidy_iea_df`.
#'                    Default is `IEATools::electricity_products$electricity`.
#' @param heat A string identifying the "Heat" product in the `product` column of the `tidy_iea_df`.
#'             Default is `IEATools::heat_products$heat`.
#' @param negzeropos The name of a temporary column added to the data frame.
#'                   Default is ".negzeropos".
#' @param share_elect_output_From_Func The name of a temporary column added to the data frame.
#'                                     Default is ".share_elect_output_From_Func".
#' @param share_nuclear_output The name of a temporary column added to the data frame.
#'                             Default is ".share_nuclear_output". 
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
                                 ascribe_eiou_to_nuclear = FALSE,
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
                                 own_use_elect_chp_heat = IEATools::eiou_flows$own_use_elect_chp_heat_plants,
                                 nuclear = IEATools::nuclear_products$nuclear,
                                 electricity = IEATools::electricity_products$electricity,
                                 heat = IEATools::heat_products$heat,
                                 # Strings identifying temporary column names
                                 negzeropos = ".negzeropos",
                                 share_elect_output_From_Func = ".share_elect_output_From_Func",
                                 share_nuclear_output = ".share_nuclear_output",
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
    # tidyr::pivot_wider(names_from = .data[[product]], values_from = .data[[e_dot]]) %>%
    tidyr::pivot_wider(names_from = dplyr::all_of(product), values_from = dplyr::all_of(e_dot)) %>%
    # dplyr::select(-tidyselect::any_of({e_dot})) 
    dplyr::select(-tidyselect::any_of(e_dot))
  
  # Select names of wide data frame just built, so we can add missing products as additional columns
  names_intermediary_modified_flows <- names(intermediary_modified_flows)
  
  # Modify selected flows
  # (a) Temporary df to help specifying EIOU flows after
  temp <- intermediary_modified_flows %>% 
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
      "{electricity}_{nuclear}" := - .data[[nuclear]] * ratio_output_to_nuclear_fuel * .data[[share_elect_output_From_Func]],
      "{heat}_{nuclear}" := - .data[[nuclear]] * ratio_output_to_nuclear_fuel * (1 - .data[[share_elect_output_From_Func]])
    )
  
  # Then modified input/output flows for nuclear and elec/heat/chp plants
  modified_flows <- temp |> 
    dplyr::select(-dplyr::any_of(share_elect_output_From_Func)) %>%
    tidyr::pivot_longer(cols = c({electricity}, {heat}, {nuclear}, glue::glue("{electricity}_{nuclear}"), glue::glue("{heat}_{nuclear}")), values_to = {e_dot}, names_to = {product}) %>%
    dplyr::filter(.data[[e_dot]] != 0) %>%
    dplyr::mutate(
      "{flow}" := dplyr::case_when(
        stringr::str_detect(.data[[product]], nuclear) ~ nuclear_industry,
        TRUE ~ .data[[flow]]
      ),
      "{product}" := stringr::str_remove(.data[[product]], stringr::str_c("_", nuclear))
    )
  
  # Dealing with EIOU flows
  eiou_elec_heat_CHP_plants <- .tidy_iea_df |> 
    dplyr::filter(.data[[flow]] == own_use_elect_chp_heat & .data[[flow_aggregation_point]] == eiou)
  
  # First case, we don't do anything
  if (isFALSE(ascribe_eiou_to_nuclear)){
    modified_flows <- modified_flows |> 
      dplyr::bind_rows(eiou_elec_heat_CHP_plants)
  # Second case, we determine the share of the output supplied by nuclear plants,
  # and ascribe the corresponding EIOU to nuclear plants
  } else if(isTRUE(ascribe_eiou_to_nuclear)){
    
    # Share nuclear output
    share_nuclear_output_df <- temp |> 
      dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]]) |> 
      dplyr::summarise(dplyr::across(tidyselect::any_of(c(electricity, heat, nuclear, glue::glue("{electricity}_{nuclear}"), glue::glue("{heat}_{nuclear}"))), sum)) |> 
      dplyr::mutate(
        "{share_nuclear_output}" := (.data[[glue::glue("{electricity}_{nuclear}")]] + .data[[glue::glue("{heat}_{nuclear}")]])/(.data[[electricity]] + .data[[heat]] + .data[[glue::glue("{electricity}_{nuclear}")]]  + .data[[glue::glue("{heat}_{nuclear}")]])
      ) |>
      dplyr::select(-tidyselect::any_of(c(share_elect_output_From_Func, electricity, heat, nuclear, glue::glue("{electricity}_{nuclear}"), glue::glue("{heat}_{nuclear}"), ledger_side, flow_aggregation_point, flow, product)))
    
    # Definining nuclear EIOU
    nuclear_eiou <- eiou_elec_heat_CHP_plants |> 
      dplyr::left_join(share_nuclear_output_df, by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {unit})) |> 
      dplyr::mutate(
        "{e_dot}" := .data[[e_dot]] * .data[[share_nuclear_output]],
        "{flow}" := nuclear_industry
      ) |> 
      dplyr::select(-tidyselect::any_of(c(share_nuclear_output)))
    
    # Defining elec/CHP/heat plants total EIOU
    elec_chp_heat_plants_eiou <- eiou_elec_heat_CHP_plants |> 
      dplyr::left_join(share_nuclear_output_df, by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {unit})) |> 
      dplyr::mutate(
        "{e_dot}" := .data[[e_dot]] * (1 - .data[[share_nuclear_output]]),
        "{flow}" := own_use_elect_chp_heat
      ) |> 
      dplyr::select(-tidyselect::any_of(c(share_nuclear_output)))
    
    # Adding modified EIOU flows to modified flows
    modified_flows <- modified_flows |> 
      dplyr::bind_rows(
        elec_chp_heat_plants_eiou,
        nuclear_eiou
      )
  }
  
  # Builds output data frame by filtering out input data frame (take out modified flows), and collating modified data.
  to_return <- .tidy_iea_df %>%
    dplyr::filter(
      ! (.data[[flow_aggregation_point]] == transformation_processes &
           ((.data[[flow]] %in% c(main_act_producer_elect, autoproducer_elect) & .data[[product]] %in% c(nuclear, electricity)) |
              (.data[[flow]] %in% c(main_act_producer_chp, autoproducer_chp) & .data[[product]] %in% c(nuclear, electricity, heat))))
    ) %>%
    dplyr::filter(! (.data[[flow]] == own_use_elect_chp_heat & .data[[flow_aggregation_point]] == eiou)) |> 
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


#' Specifies renewable electricity and heat
#'
#' This function specifies hydropower, geothermal, solar photovoltaic, solar thermal, oceanic, and wind power industries.
#'
#' The primary energy use of hydro, geothermal, solar photovoltaic, solar thermal, oceanic, and wind power energy by main activity and autoproducer plants are used
#' to create new renewable industries that produce electricity and heat (heat only in the case of geothermal and solar thermal). The physical content method is used
#' to derive the electricity produced by renewable industries, except in the case of geothermal and solar thermal, for which the IEA uses other factors in its balances.
#' In the case of CHP plants (which can be relevant for geothermal and solar thermal), the output of the new renewable industry follows the same heat vs electricity
#' breakdown as the main industry from which it is derived.
#'
#' @param .tidy_iea_df The `.tidy_iea_df` which flows need to be specified.
#' @param specify_renewable_plants A boolean indicating whether renewable energy plants should be specified or not.
#'                                 Default is FALSE.
#' @param ascribe_eiou_to_renewable_plants A boolean defining whether a fraction of the EIOU of electricity, CHP and heat plants
#'                                         should be ascribed to the new renewable industries. Default is FALSE.
#' @param flow_aggregation_point,flow,e_dot,product,method,ledger_side,last_stage,energy_type,country,year,unit See `IEATools::iea_cols`.
#' @param transformation_processes A string identifying the transformation processes in the `flow_aggregation_point` column in the `.tidy_iea_df`.
#'                                 Default is `IEATools::aggregation_flows$transformation_processes`.
#' @param eiou A string identifying the energy industry own use in the `flow_aggregation_point` column in the `.tidy_iea_df`.
#'             Default is `IEATools::aggregation_flows$energy_industry_own_use`.
#' @param main_act_producer_elect A string identifying "Main activity producer electricity plants" in the `flow` column of the `.tidy_iea_df`.
#'                                Default is `IEATools::main_act_plants$main_act_prod_elect_plants`.
#' @param main_act_producer_chp A string identifying "Main activity producer CHP plants" in the `flow` column of the `.tidy_iea_df`.
#'                              Default is `IEATools::main_act_plants$main_act_prod_chp_plants`.
#' @param main_act_producer_heat A string identifying "Main activity producer heat plants" in the `flow` column of the `.tidy_iea_df`.
#'                              Default is `IEATools::main_act_plants$main_act_prod_heat_plants`.
#' @param autoproducer_elect A string identifying "Autoproducer electricity plants" in the `flow` column of the `.tidy_iea_df`.
#'                           Default is `IEATools::main_act_plants$autoprod_elect_plants`.
#' @param autoproducer_chp A string identifying "Autoproducer CHP plants" in the `flow` column of the `.tidy_iea_df`.
#'                         Default is `IEATools::transformation_processes$autoproducer_CHP_plants`.
#' @param autoproducer_heat A string identifying "Autoproducer CHP plants" in the `flow` column of the `.tidy_iea_df`.
#'                         Default is `IEATools::transformation_processes$autoprod_heat_plants`.
#' @param own_use_elect_chp_heat A string identifying the "Own use in electricity, CHP and heat plants" EIOU flow in the `.tidy_iea_df`.
#'                               Default is `IEATools::eiou_flows$own_use_elect_chp_heat_plants`.
#' @param geothermal,hydro,solar_pv,solar_th,oceanic,wind Renewable energy product names. See `IEATools::renewable_products`.
#' @param electricity The name of the electricity product.
#'                    Default is `IEATools::electricity_products$electricity`.
#' @param heat The name of the heat product.
#'             Default is `IEATools::heat_products$heat`.
#' @param ratio_solar_th_elec The ratio of primary energy to electricity to use for solar thermal.
#'                            Default is 0.33 as this is the value assumed in the IEA's energy balances.
#' @param ratio_solar_th_heat The ratio of primary energy to heat to use for solar thermal.
#'                            Default is 1 as this is the value assumed in the IEA's energy balances.
#' @param ratio_geothermal_elec The ratio of primary energy to electricity to use for geothermal.
#'                              Default is 0.1 as this is the value assumed in the IEA's energy balances.
#' @param ratio_geothermal_heat The ratio of primary energy to heat to use for geothermal.
#'                              Default is 0.5 as this is the value assumed in the IEA's energy balances.
#' @param ratio_other_renewable_elec The ratio of primary energy to electricity to use for hydropower, solar photovoltaic, oceanic and wind power.
#'                                   Default is 1 as this is the value assumed in the IEA's energy balances.
#' @param geothermal_plants,hydro_plants,solar_pv_plants,solar_th_plants,oceanic_plants,wind_power_plants Names of renewable industries added. See `IEATools::renewable_industries`.
#' @param negzeropos The name of a temporary column added to the data frame.
#'                   Default is ".negzeropos".
#' @param ratio_elec_to_heat A temporary column added to the data frame.
#'                                     Default is ".ratio_elec_to_heat".
#' @param .share_industry The name of a temporary column added to specify the renewable industry 
#'                        for which the share of output is calculated. Default is ".share_industry".
#' @param .share The name of a temporary column where the share of output is calculated for each renewable industry.
#'               Default is ".share".
#'
#' @return Returns a .tidy_iea_df with renewable electricity and heat from geothermal, hydropower, solar thermal, solar photovoltaic, wind power, and oceanic power specified.
#' @export
#'
#' @examples
#' library(dplyr)
#' load_tidy_iea_df() %>% 
#'   specify_renewable_plants()
specify_renewable_plants <- function(.tidy_iea_df,
                                     specify_renewable_plants = FALSE,
                                     ascribe_eiou_to_renewable_plants = FALSE,
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
                                     main_act_producer_elect = IEATools::main_act_plants$main_act_prod_elect_plants,
                                     main_act_producer_chp = IEATools::main_act_plants$main_act_prod_chp_plants,
                                     main_act_producer_heat = IEATools::main_act_plants$main_act_prod_heat_plants,
                                     autoproducer_elect = IEATools::main_act_plants$autoprod_elect_plants,
                                     autoproducer_chp = IEATools::transformation_processes$autoproducer_CHP_plants,
                                     autoproducer_heat = IEATools::main_act_plants$autoprod_heat_plants,
                                     own_use_elect_chp_heat = IEATools::eiou_flows$own_use_elect_chp_heat_plants,
                                     # Input products
                                     geothermal = IEATools::renewable_products$geothermal,
                                     hydro = IEATools::renewable_products$hydro,
                                     solar_pv = IEATools::renewable_products$solar_photovoltaics,
                                     solar_th = IEATools::renewable_products$solar_thermal,
                                     oceanic = IEATools::renewable_products$tide_wave_and_ocean,
                                     wind = IEATools::renewable_products$wind,
                                     # Output products
                                     electricity = IEATools::electricity_products$electricity,
                                     heat = IEATools::heat_products$heat,
                                     # Ratios of final to primary energy
                                     ratio_solar_th_elec = 0.33,
                                     ratio_solar_th_heat = 1,
                                     ratio_geothermal_elec = 0.1,
                                     ratio_geothermal_heat = 0.5,
                                     ratio_other_renewable_elec = 1,
                                     # New industry names
                                     geothermal_plants = IEATools::renewable_industries$geothermal_plants,
                                     hydro_plants = IEATools::renewable_industries$hydro_plants,
                                     solar_pv_plants = IEATools::renewable_industries$solar_pv_plants,
                                     solar_th_plants = IEATools::renewable_industries$solar_th_plants,
                                     oceanic_plants = IEATools::renewable_industries$oceanic_plants,
                                     wind_power_plants = IEATools::renewable_industries$wind_power_plants,
                                     # Strings identifying temporary column names
                                     negzeropos = ".negzeropos",
                                     ratio_elec_to_heat = ".ratio_elec_to_heat",
                                     .share_industry = ".share_industry",
                                     .share = ".share"){
  
  # Check if renewable energy should be specified. If yes, then the code carries on.
  if (isFALSE(specify_renewable_plants)){
    return(.tidy_iea_df)
  }
  
  # Tibble of products of interest
  products_tibble <- tibble::tibble("{geothermal}" := NA,
                                    "{hydro}" := NA,
                                    "{solar_pv}" := NA,
                                    "{solar_th}" := NA,
                                    "{oceanic}" := NA,
                                    "{wind}" := NA,
                                    "{electricity}" := NA,
                                    "{heat}" := NA)
  
  # Potentially move to using the IEATools constant, if "Other sources" are removed
  renewable_products <- c(geothermal, hydro, solar_pv, solar_th, oceanic, wind)
  
  # Relevant products
  relevant_products <- c(geothermal, hydro, solar_pv, solar_th, oceanic, wind, electricity, heat)
  
  # (1) Here we select only the flows that we are going to modify, and pivot them to wide format for modification
  selected_io_flows <- .tidy_iea_df %>%
    dplyr::filter(
      .data[[flow_aggregation_point]] == transformation_processes &
        ((.data[[flow]] %in% c(main_act_producer_elect, autoproducer_elect) & .data[[product]] %in% c(renewable_products, electricity)) |
           (.data[[flow]] %in% c(main_act_producer_chp, autoproducer_chp) & .data[[product]] %in% c(renewable_products, electricity, heat)) |
           (.data[[flow]] %in% c(main_act_producer_heat, autoproducer_heat) & .data[[product]] %in% c(renewable_products, heat)))
    ) %>%
    # tidyr::pivot_wider(names_from = .data[[product]], values_from = .data[[e_dot]]) %>%
    tidyr::pivot_wider(names_from = dplyr::all_of(product), values_from = dplyr::all_of(e_dot)) %>%
    # dplyr::select(-tidyselect::any_of({e_dot}))
    dplyr::select(-tidyselect::any_of(e_dot))
  
  # (2.a) Select names of wide data frame just built, so we can add missing products as additional columns
  names_selected_io_flows <- names(selected_io_flows)
  
  # (2.b) Modify selected flows
  # (i) Temporary df to help specifying EIOU flows after
  temp <- selected_io_flows %>% 
    tibble::add_column(!!products_tibble[! names(products_tibble) %in% names_selected_io_flows]) %>%
    # Replacing NAs by zeros in all columns
    dplyr::mutate(dplyr::across(tidyselect::all_of(relevant_products), ~tidyr::replace_na(.x, 0))) |> 
    # Defining renewable electricity for products for which all inputs deliver electricity
    dplyr::mutate(
      "{hydro}_{electricity}" := -.data[[hydro]] * ratio_other_renewable_elec,
      "{solar_pv}_{electricity}" := -.data[[solar_pv]] * ratio_other_renewable_elec,
      "{oceanic}_{electricity}" := -.data[[oceanic]] * ratio_other_renewable_elec,
      "{wind}_{electricity}" := -.data[[wind]] * ratio_other_renewable_elec,
    ) |> 
    # Defining renewable electricity and heat for products with potential joint production
    dplyr::mutate(
      "{ratio_elec_to_heat}" := .data[[electricity]] / .data[[heat]],
      "{geothermal}_{electricity}" := dplyr::case_match(
        .data[[ratio_elec_to_heat]],
        Inf ~ -(.data[[geothermal]] * ratio_geothermal_elec),
        0 ~ 0,
        .default = -(.data[[geothermal]]) / (1 + ratio_geothermal_elec/(ratio_geothermal_heat * .data[[ratio_elec_to_heat]])) * ratio_geothermal_elec
      ),
      "{geothermal}_{heat}" := dplyr::case_match(
        .data[[ratio_elec_to_heat]],
        Inf ~ 0,
        0 ~ -.data[[geothermal]] * ratio_geothermal_heat,
        .default = -(.data[[geothermal]]) / (1 + ratio_geothermal_heat/ratio_geothermal_elec*.data[[ratio_elec_to_heat]]) * ratio_geothermal_heat
      ),
      "{solar_th}_{electricity}" := dplyr::case_match(
        .data[[ratio_elec_to_heat]],
        Inf ~ -(.data[[solar_th]] * ratio_solar_th_elec),
        0 ~ 0,
        .default = -(.data[[solar_th]]) / (1 + ratio_solar_th_elec/(ratio_solar_th_heat * .data[[ratio_elec_to_heat]])) * ratio_solar_th_elec
      ),
      "{solar_th}_{heat}" := dplyr::case_match(
        .data[[ratio_elec_to_heat]],
        Inf ~ 0,
        0 ~ -.data[[solar_th]] * ratio_solar_th_heat,
        .default = -(.data[[solar_th]]) / (1 + ratio_solar_th_heat/ratio_solar_th_elec*.data[[ratio_elec_to_heat]]) * ratio_solar_th_heat
      ),
    )
  
  # (ii) Subtracting specified electricity and heat flows from existing plants output; specifying product output
  modified_flows <-  temp |> 
    dplyr::mutate(
      "{electricity}" := .data[[electricity]] - (.data[[glue::glue("{hydro}_{electricity}")]] + .data[[glue::glue("{solar_pv}_{electricity}")]] + .data[[glue::glue("{oceanic}_{electricity}")]] + 
                                                   .data[[glue::glue("{wind}_{electricity}")]] + .data[[glue::glue("{geothermal}_{electricity}")]] + .data[[glue::glue("{solar_th}_{electricity}")]]),
      "{heat}" := .data[[heat]] - (.data[[glue::glue("{geothermal}_{heat}")]] + .data[[glue::glue("{solar_th}_{heat}")]])
    ) |> 
    # Removing columns if needed
    dplyr::select(-dplyr::any_of(ratio_elec_to_heat)) %>%
    # Back to tidy, long format
    tidyr::pivot_longer(cols = -c({country}, {method}, {energy_type}, {last_stage}, {year}, {ledger_side}, {flow_aggregation_point}, {flow}, {unit}), 
                        values_to = {e_dot}, names_to = {product}) |> 
    dplyr::filter(.data[[e_dot]] != 0) %>%
    # Adjusting product and flow names:
    dplyr::mutate(
      "{flow}" := dplyr::case_when(
        stringr::str_detect(.data[[product]], geothermal) ~ IEATools::renewable_industries$geothermal_plants,
        stringr::str_detect(.data[[product]], hydro) ~ IEATools::renewable_industries$hydro_plants,
        stringr::str_detect(.data[[product]], solar_pv) ~ IEATools::renewable_industries$solar_pv_plants,
        stringr::str_detect(.data[[product]], solar_th) ~ IEATools::renewable_industries$solar_th_plants,
        stringr::str_detect(.data[[product]], oceanic) ~ IEATools::renewable_industries$oceanic_plants,
        stringr::str_detect(.data[[product]], wind) ~ IEATools::renewable_industries$wind_power_plants,
        TRUE ~ .data[[flow]]
      ),
      "{product}" := stringr::str_remove(.data[[product]], ".*_")
    )
  
  # (3) Dealing with EIOU flows
  eiou_elec_heat_CHP_plants <- .tidy_iea_df |> 
    dplyr::filter(.data[[flow]] == own_use_elect_chp_heat & .data[[flow_aggregation_point]] == eiou)
  
  # (i) First case, we don't do anything
  if (isFALSE(ascribe_eiou_to_renewable_plants)){
    modified_flows <- modified_flows |> 
      dplyr::bind_rows(eiou_elec_heat_CHP_plants)
    # (ii) Second case, we determine the share of the output supplied by each renewable energy industry,
    # and ascribe the corresponding EIOU to each renewable energy industry
  } else if(isTRUE(ascribe_eiou_to_renewable_plants)){
    
    # Defining a vector of products of interest
    products_of_interest <- c(electricity, heat, geothermal, hydro, solar_pv, solar_th, oceanic, wind,
                              glue::glue("{hydro}_{electricity}"), glue::glue("{solar_pv}_{electricity}"), glue::glue("{oceanic}_{electricity}"), glue::glue("{wind}_{electricity}"),
                              glue::glue("{geothermal}_{electricity}"), glue::glue("{geothermal}_{heat}"), glue::glue("{solar_th}_{electricity}"), glue::glue("{solar_th}_{heat}"))
    
    # Share each renewable energy plant to total elec/chp/heat plants output
    share_renewable_output_df <- temp |> 
      dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]]) |> 
      dplyr::summarise(dplyr::across(tidyselect::any_of(products_of_interest), sum)) |> 
      dplyr::mutate(
        "{.share}_{geothermal_plants}" := (.data[[glue::glue("{geothermal}_{electricity}")]] + .data[[glue::glue("{geothermal}_{heat}")]])/(.data[[electricity]] + .data[[heat]]),
        "{.share}_{hydro_plants}" := (.data[[glue::glue("{hydro}_{electricity}")]])/(.data[[electricity]] + .data[[heat]]),
        "{.share}_{solar_pv_plants}" := (.data[[glue::glue("{solar_pv}_{electricity}")]])/(.data[[electricity]] + .data[[heat]]),
        "{.share}_{solar_th_plants}" := (.data[[glue::glue("{solar_th}_{electricity}")]] + .data[[glue::glue("{solar_th}_{heat}")]])/(.data[[electricity]] + .data[[heat]]),
        "{.share}_{oceanic_plants}" := (.data[[glue::glue("{oceanic}_{electricity}")]])/(.data[[electricity]] + .data[[heat]]),
        "{.share}_{wind_power_plants}" := (.data[[glue::glue("{wind}_{electricity}")]])/(.data[[electricity]] + .data[[heat]]),
      ) |>
      dplyr::select(-tidyselect::any_of(c(ratio_elec_to_heat, products_of_interest, ledger_side, flow_aggregation_point, flow, product)))
    
    # Defining shares of interest
    shares_of_interest <- c(glue::glue("{.share}_{geothermal_plants}"), glue::glue("{.share}_{hydro_plants}"), glue::glue("{.share}_{solar_pv_plants}"),
                            glue::glue("{.share}_{solar_th_plants}"), glue::glue("{.share}_{oceanic_plants}"), glue::glue("{.share}_{wind_power_plants}"))
    
    # Defining renewable industry EIOU
    renewable_industry_eiou <- eiou_elec_heat_CHP_plants |> 
      dplyr::left_join(share_renewable_output_df, by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {unit})) |> 
      tidyr::pivot_longer(cols = tidyselect::any_of(shares_of_interest), names_to = .share_industry, values_to = .share) |> 
      dplyr::mutate(
        "{e_dot}" := .data[[e_dot]] * .data[[.share]],
        "{flow}" := stringr::str_extract(.data[[.share_industry]], "_.*") |> 
          stringr::str_remove("_")
      ) |> 
      dplyr::select(-tidyselect::any_of(c(.share, .share_industry)))
    
    # Defining elec/CHP/heat plants total EIOU
    elec_chp_heat_plants_eiou <- eiou_elec_heat_CHP_plants |> 
      dplyr::left_join(share_renewable_output_df, by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {unit})) |> 
      dplyr::mutate(
        "{e_dot}" := .data[[e_dot]] * (1 - (.data[[glue::glue("{.share}_{geothermal_plants}")]]+.data[[glue::glue("{.share}_{hydro_plants}")]]+.data[[glue::glue("{.share}_{solar_pv_plants}")]]
                                            +.data[[glue::glue("{.share}_{solar_th_plants}")]]+.data[[glue::glue("{.share}_{oceanic_plants}")]]+.data[[glue::glue("{.share}_{wind_power_plants}")]])),
        "{flow}" := own_use_elect_chp_heat
      ) |> 
      dplyr::select(-dplyr::starts_with(.share))
    
    # Adding modified EIOU flows to modified flows
    modified_flows <- modified_flows |> 
      dplyr::bind_rows(
        elec_chp_heat_plants_eiou,
        renewable_industry_eiou
      )
  }
  
  # (4) Builds output data frame by filtering out input data frame (take out modified flows), and collating modified data.
  to_return <- .tidy_iea_df %>%
    # Inverse of the condition that was filtered in "modified_flows"
    dplyr::filter(
      ! (.data[[flow_aggregation_point]] == transformation_processes &
        ((.data[[flow]] %in% c(main_act_producer_elect, autoproducer_elect) & .data[[product]] %in% c(renewable_products, electricity)) |
           (.data[[flow]] %in% c(main_act_producer_chp, autoproducer_chp) & .data[[product]] %in% c(renewable_products, electricity, heat)) |
           (.data[[flow]] %in% c(main_act_producer_heat, autoproducer_heat) & .data[[product]] %in% c(renewable_products, heat))))
    ) %>%
    dplyr::filter(! (.data[[flow]] == own_use_elect_chp_heat & .data[[flow_aggregation_point]] == eiou)) |> 
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


#' Specifies electricity grid
#' 
#' Adds an electricity grid industry that takes as input all electricity produced by any industry,
#' which is now specified by producing industry (e.g., "Electricity \[from Wind power plants\]"),
#' and converts it into Electricity.
#'
#' @param .tidy_iea_df The `.tidy__iea_df` for which an electricity grid industry should be added.
#' @param specify_electricity_grid A boolean stating whether an electricity grid industry should be created or not.
#'                     Default is FALSE.
#' @param supplying_industry_notation Notation to use to specify the electricity supplying industry.
#'                                    Default is `RCLabels::from_notation`.
#' @param flow_aggregation_point,flow,e_dot,product,method,ledger_side,last_stage,energy_type,country,year,unit See `IEATools::iea_cols`.
#' @param losses The name of the "Losses" flows in the input data frame.
#'               Default is `IEATools::tfc_compare_flows$losses`.
#' @param grid_industry The name of the electricity grid industry to be added.
#'                      Default is `IEATools::grid_industries$electricity_grid`.
#' @param supply The name of the supply ledger side.
#'               Default is `IEATools::ledger_sides$supply`.
#' @param transformation_processes The name of transformation processes in the flow aggregation point column.
#'                                 Default is `IEATools::tfc_compare_flows$transformation_processes`.
#' @param electricity The name of the product name for "Electricity".
#'                    Default is `IEATools::electricity_products$electricity`.
#' @param negzeropos The name of a temporary column added to the data frame.
#'                   Default is ".negzeropos".
#'
#' @return The `.tidy__iea_df` to which an electricity grid industry has been added.
#' @export
#'
#' @examples
#' load_tidy_iea_df() %>% 
#'   specify_electricity_grid()
specify_electricity_grid <- function(.tidy_iea_df,
                                     specify_electricity_grid = FALSE,
                                     supplying_industry_notation = RCLabels::from_notation,
                                     # IEA col names
                                     country = IEATools::iea_cols$country,
                                     method = IEATools::iea_cols$method,
                                     energy_type = IEATools::iea_cols$energy_type,
                                     last_stage = IEATools::iea_cols$last_stage,
                                     year = IEATools::iea_cols$year,
                                     ledger_side = IEATools::iea_cols$ledger_side,
                                     flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                                     flow = IEATools::iea_cols$flow,
                                     product = IEATools::iea_cols$product,
                                     unit = IEATools::iea_cols$unit,
                                     e_dot = IEATools::iea_cols$e_dot,
                                     # Constants
                                     losses = IEATools::tfc_compare_flows$losses,
                                     grid_industry = IEATools::grid_industries$electricity_grid,
                                     supply = IEATools::ledger_sides$supply,
                                     transformation_processes = IEATools::tfc_compare_flows$transformation_processes,
                                     electricity = IEATools::electricity_products$electricity,
                                     # Strings identifying temporary column names
                                     negzeropos = ".negzeropos"){
  
  # maybe change pattern_to_remove to RCLabels::of_notation$suff_start
  
  # Check if electricity grid should be specified. If yes, then the code carries on.
  if (isFALSE(specify_electricity_grid)){
    return(.tidy_iea_df)
  }
  
  # (1) Select production flows
  selected_production_flows <- .tidy_iea_df |> 
    dplyr::filter(.data[[ledger_side]] == supply & .data[[e_dot]] > 0 & .data[[product]] == electricity)
  
  # (2) Select losses flows
  selected_losses_flows <- .tidy_iea_df |> 
    dplyr::filter(.data[[flow]] == losses & .data[[product]] == electricity)
  
  # (3) Modify production flows
  modified_production_flows <- selected_production_flows |> 
    # Change this with RCLabels!!
    dplyr::mutate(
      "{product}" := stringr::str_c(.data[[product]], 
                                    supplying_industry_notation[["suff_start"]], 
                                    .data[[flow]], 
                                    supplying_industry_notation[["suff_end"]],
                                    sep = "")
    )
  
  # (4) Adding inputs to grid industry
  added_inputs_to_grid <- modified_production_flows |> 
    dplyr::mutate(
      "{flow}" := grid_industry,
      "{e_dot}" := - .data[[e_dot]]
    )
  
  # (5) Adding supply of the grid industry
  added_supply_by_grid <- selected_production_flows |> 
    dplyr::bind_rows(selected_losses_flows) |> 
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product]], .data[[unit]]) |> 
    dplyr::summarise(
      "{e_dot}" := sum(.data[[e_dot]])
    ) |> 
    dplyr::mutate(
      "{flow_aggregation_point}" := transformation_processes,
      "{ledger_side}" := supply,
      "{flow}" := grid_industry,
    )
  
  # (5) Bind data frame and get ready to return values
  to_return <- .tidy_iea_df |> 
    dplyr::filter(! (.data[[ledger_side]] == supply & .data[[e_dot]] > 0 & .data[[product]] == electricity)) |> 
    dplyr::filter(! (.data[[flow]] == losses & .data[[product]] == electricity)) |> 
    dplyr::bind_rows(
      modified_production_flows,
      added_inputs_to_grid,
      added_supply_by_grid
    ) |> 
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
    # dplyr::select(-.data[[n_counting]])
    dplyr::select(-dplyr::any_of(n_counting))
  
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
    # tidyr::unite(col = "ID", .data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]]) %>%
    tidyr::unite(col = "ID", dplyr::all_of(c(country, method, energy_type, last_stage, year))) %>%
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
    # dplyr::select(-.data[[flow_aggregation_point]])
    dplyr::select(-dplyr::any_of(flow_aggregation_point))
  
  
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
    # dplyr::select(-.data[[Share_eiou_per_industry_From_Func]], -.data[[EIOU_per_industry_From_Func]], -.data[[Total_eiou_excl_nonspec_From_Func]])
    dplyr::select(-dplyr::any_of(c(Share_eiou_per_industry_From_Func,
                                   EIOU_per_industry_From_Func, 
                                   Total_eiou_excl_nonspec_From_Func)))
  
  
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
    # dplyr::select(-.data[[n_counting]])
    dplyr::select(-dplyr::any_of(n_counting))
  
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
    # tidyr::unite(col = "ID", .data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product]], .data[[negzeropos]]) %>%
    tidyr::unite(col = "ID", dplyr::all_of(c(country, method, energy_type, last_stage, year, product, negzeropos))) %>%
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
    # dplyr::select(-.data[[Share_input_output_by_prod_per_tp]],
    #               -.data[[Input_output_by_prod_per_tp]],
    #               -.data[[Total_input_output_by_prod_excl_nonspec]])
    dplyr::select(-dplyr::any_of(c(Share_input_output_by_prod_per_tp,
                                   Input_output_by_prod_per_tp,
                                   Total_input_output_by_prod_excl_nonspec)))
  
  
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
