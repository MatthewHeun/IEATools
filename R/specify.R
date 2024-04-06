
#' Specify primary production industries
#' 
#' The IEA extended energy balances include 
#' some `Flow`s that identify `Energy industry own use` (EIOU), 
#' the consumption of energy by energy-producing industries.
#' But some primary production industries that receive EIOU do not produce anything.
#' For example, `Coal mines` receive electricity
#' but there are no `Coal mines` that produce coal.
#' Rather, the generic `Production` industry produces coal.
#' This function solves that problem by 
#' replacing the generic `Production` industry with 
#' specific industries.
#' 
#' By default, the following changes are made to `.tidy_iea_df`:
#' 
#' 1. Energy industry own use for `Liquefaction (LNG) / regasification plants` is 
#'    reassigned to `Oil and gas extraction`.
#' 2. Each `Production` flow is replaced by a `Resources [of Product]` flow,
#'    which produces `Product [from Resources]`.
#' 3. For each `Production` flow, a manufacturing flow, that takes as input
#'    the `Product [from Resources]` supplied by the new `Resources [of Product]`,
#'    and that produces the given `Product`, is added. The name of the manufacturing industry
#'    is `Coal mines` for `coal_and_coal_products`, `Oil and gas extraction` for `oil_and_gas_products`,
#'    and `Manufacture [of Product]` for all other products.
#' 
#' Users can specify other changes by adjusting the default argument values.
#' 
#' Be sure to call this function _after_ calling `augment_iea_df()` or
#' `load_tidy_iea_df()`.
#'
#' @param .tidy_iea_df An IEA data frame whose columns have been renamed by `rename_iea_df_cols()`.
#' @param liquefaction_regas A string identifying liquefaction and regasification plants. 
#'                           Default is "Liquefaction (LNG) / regasification plants".
#' @param liquefaction_regas_reassign A string identifying the industry to which EIOU into `liquefaction_regas` will be reassigned.
#'                                    Default is "Natural gas extraction".
#' @param transformation_processes A string identifying transformation processes in the flow column of `.tidy_iea_df`. 
#'                                 Default is "Transformation processes".
#' @param ledger_side,flow,product,flow_aggregation_point See `IEATools::iea_cols`.
#' @param resources A string identifying resource industries to be added to `.tidy_iea_df`. 
#'                  Default is "`Resources`".
#' @param production A string identifying production in the flow column. Default is "`Production`".
#' @param e_dot The name of the energy column in `.tidy_iea_df`. Default is "`E.dot`".
#' @param list_primary_coal_products The list of primary coal products for which the production industry needs to be changed.
#'                                   Default is `IEATools::primary_coal_products`.
#' @param list_primary_oil_products The list of primary oil products for which the production industry needs to be changed.
#'                                  Default is `IEATools::primary_oil_products`.
#' @param list_primary_gas_products The list of primary gas products for which the production industry needs to be changed.
#'                                  Default is `IEATools::primary_gas_products`.
#' @param coal_mines The name of the new industry that produces primary coal products.
#'                   Default is `IEATools::industry_flows$coal_mines`.
#' @param oil_extraction The name of the new industry that produces primary oil and gas products.
#'                       Default is `IEATools::industry_flows$oil_extraction`.
#' @param gas_extraction The name of the new industry that produces primary oil and gas products.
#'                       Default is `IEATools::industry_flows$natural_gas_extraction`.
#' @param resource_products_notation The notation to be used for defining products coming from the new resource industries.
#'                                   E.g., the Crude oil product will be called "Crude oil \[from Resources\]".
#'                                   Default is `RCLabels::from_notation`.
#' @param resources_flow_notation The notation to be used for defining the new resource industries.
#'                                E.g., the Crude oil resource will be called "Resources \[of Crude oil\]".
#'                                Default is `RCLabels::of_notation`.
#' @param manufacture The name of the industries that convert resource-products \(inputs\) into actual products \(outputs\), 
#'                    when a corresponding a corresponding industry does not exist by default in IEA data.
#'                    Default is "Manufacture".
#' @param manufacture_flow_notation The notation to be used for the newly created manufacture industries 
#'                                  \(each manufacturing industry is specified\) by the product it manufactures.
#'                                  Default is `RCLabels::of_notation`.
#'
#' @return A `.tidy_iea_df` with adjusted production information for primary energy 
#'         for both coal and coal products and oil and gas extraction
#' 
#' @export
#'
#' @examples
#' library(dplyr)
#' load_tidy_iea_df() %>% 
#'   specify_primary_production() %>% 
#'   add_psut_matnames() %>% 
#'   dplyr::filter(Flow == "Coal mines" | stringr::str_detect(Flow, "Resources")) %>% 
#'   select(-Method, -Last.stage, -Ledger.side, -Unit)
#' # EIOU by "Liquefaction (LNG) / regasification plants" is reassigned to "Oil and gas extraction"
#' data.frame(
#'   Flow.aggregation.point = c("Energy industry own use"),
#'   Flow = c("Liquefaction (LNG) / regasification plants"), 
#'   Product = c("Natural gas"),
#'   E.dot = c(-42), 
#'   stringsAsFactors = FALSE
#' ) %>% 
#'   specify_primary_production()
specify_primary_production <- function(.tidy_iea_df,
                                       ledger_side = IEATools::iea_cols$ledger_side,
                                       flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                                       flow = IEATools::iea_cols$flow,
                                       product = IEATools::iea_cols$product,
                                       e_dot = IEATools::iea_cols$e_dot,
                                       list_primary_coal_products = IEATools::primary_coal_products,
                                       list_primary_oil_products = IEATools::primary_oil_products,
                                       list_primary_gas_products = IEATools::primary_gas_products,
                                       production = IEATools::tpes_flows$production,
                                       coal_mines = IEATools::industry_flows$coal_mines,
                                       oil_extraction = IEATools::industry_flows$oil_extraction,
                                       gas_extraction = IEATools::industry_flows$natural_gas_extraction,
                                       liquefaction_regas = "Liquefaction (LNG) / regasification plants",
                                       liquefaction_regas_reassign = IEATools::industry_flows$natural_gas_extraction,
                                       transformation_processes = IEATools::aggregation_flows$transformation_processes,
                                       resources = IEATools::tpes_flows$resources,
                                       resource_products_notation = RCLabels::from_notation,
                                       resources_flow_notation = RCLabels::of_notation,
                                       manufacture = "Manufacture",
                                       manufacture_flow_notation = RCLabels::of_notation){
  
  production_products <- c(list_primary_coal_products, list_primary_oil_products, list_primary_gas_products)
  
  # First, we define resource flows, i.e. flows supplied by resources
  resource_outputs_flows <- .tidy_iea_df %>% 
    dplyr::filter(
      .data[[flow]] == production #& .data[[product]] %in% production_products
    ) %>% 
    dplyr::mutate(
      "{flow}" := stringr::str_c(
        resources, 
        resources_flow_notation[["suff_start"]], 
        .data[[product]], 
        resources_flow_notation[["suff_end"]], 
        sep = ""
      ),
      "{product}" := stringr::str_c(.data[[product]], 
                                    resource_products_notation[["suff_start"]], 
                                    resources, 
                                    resource_products_notation[["suff_end"]],
                                    sep = "")
    )
  
  # Second, we define extractive industries outputs
  extractive_industries_output_flows <- .tidy_iea_df %>% 
    dplyr::filter(
      .data[[flow]] == production #& .data[[product]] %in% production_products
    ) %>% 
    dplyr::mutate(
      "{flow}" := dplyr::case_when(
        .data[[product]] %in% list_primary_coal_products ~ coal_mines,
        .data[[product]] %in% list_primary_oil_products ~ oil_extraction,
        .data[[product]] %in% list_primary_gas_products ~ gas_extraction,
        TRUE ~ stringr::str_c(
          manufacture,
          manufacture_flow_notation[["suff_start"]],
          .data[[product]],
          manufacture_flow_notation[["suff_end"]],
          sep = ""
        )
      ),
      "{flow_aggregation_point}" := transformation_processes
    )
  
  # Third, we define extractive industries inputs
  extractive_industries_input_flows <- .tidy_iea_df %>% 
    dplyr::filter(
      .data[[flow]] == production #& .data[[product]] %in% production_products
    ) %>% 
    dplyr::mutate(
      "{flow}" := dplyr::case_when(
        .data[[product]] %in% list_primary_coal_products ~ coal_mines,
        .data[[product]] %in% list_primary_oil_products ~ oil_extraction,
        .data[[product]] %in% list_primary_gas_products ~ gas_extraction,
        TRUE ~ stringr::str_c(
          manufacture,
          manufacture_flow_notation[["suff_start"]],
          .data[[product]],
          manufacture_flow_notation[["suff_end"]],
          sep = ""
        )
      ),
      "{product}" := stringr::str_c(.data[[product]], 
                                    resource_products_notation[["suff_start"]], 
                                    resources, 
                                    resource_products_notation[["suff_end"]], 
                                    sep = ""),
      "{e_dot}" := -.data[[e_dot]],
      "{flow_aggregation_point}" := transformation_processes
    )
  
  # Fourth, we add all these flows to the input .tidy_iea_df
  .tidy_iea_df %>% 
    dplyr::filter(
      ! (.data[[flow]] == production)# & .data[[product]] %in% production_products)
    ) %>% 
    dplyr::bind_rows(
      resource_outputs_flows,
      extractive_industries_output_flows,
      extractive_industries_input_flows
    ) %>% 
    # Here we need to reassign EIOU tagged as "Liquefaction (LNG) / regasification plants" to 
    # the Oil and gas extraction sector.
    dplyr::mutate(
      "{flow}" := dplyr::case_when(
        .data[[flow]] == liquefaction_regas ~ liquefaction_regas_reassign, 
        TRUE ~ .data[[flow]]
      )
    ) %>% 
    # After reassigning, we may have multiple rows of liquefaction_regas_reassign,
    # so we need to sum those rows.
    matsindf::group_by_everything_except(e_dot) %>% 
    dplyr::summarise(
      "{e_dot}" := sum(.data[[e_dot]])
      ) %>% 
    dplyr::ungroup()
}
  

#' Convert Production Flows to Resource Flows
#' 
#' The IEA gives resource extraction in rows where the `Flow` is "`Production`".
#' This function changes the "`Production`" string 
#' to "`Resources [of Product]`", 
#' where `product` is the name of the energy carrier for this resource.
#' 
#' This function should be called _after_ `specify_primary_production()`,
#' which adjusts for energy industry own use 
#' of some primary energy producing industries.
#' If this function is called first, 
#' EIOU will not be accounted correctly.
#'
#' @param .tidy_iea_df An IEA data frame whose columns have been renamed by `rename_iea_df_cols()`
#' @param flow The name of the flow column in `.tidy_iea_df`.  
#'             Default is `IEATools::iea_cols$flow`.
#' @param production A string identifying production in the flow column. 
#'                   Default is `IEATools::tpes_flows$production`.
#' @param resources A string identifying resource industries to be added to `.tidy_iea_df`. 
#'                  Default is `IEATools::tpes_flows$resources`.
#' @param product The name of the product column in `.tidy_iea_df`. 
#'                Default is `IEATools::iea_cols$product`.
#' @param notation A list of specification notations. 
#'                 Default is `IEATools::bracket_notation`.
#'
#' @return A `.tidy_iea_df` with `Production` changed to `resources .resources_open product .resources_close` in the `flow` column
#' 
#' @export
#'
#' @examples
#' load_tidy_iea_df() %>% 
#'   specify_primary_production() %>% 
#'   specify_production_to_resources()
specify_production_to_resources <- function(.tidy_iea_df, 
                                            flow = IEATools::iea_cols$flow,
                                            product = IEATools::iea_cols$product,
                                            production = IEATools::tpes_flows$production,
                                            resources = IEATools::tpes_flows$resources,
                                            notation = RCLabels::from_notation){
  # Take any remaining "Production" rows and convert them to Resources (Product).
  .tidy_iea_df %>% 
    dplyr::mutate(
      "{flow}" := dplyr::case_when(
        .data[[flow]] == production ~ RCLabels::paste_pref_suff(pref = resources, suff = .data[[product]], notation = notation), 
        TRUE ~ .data[[flow]]
      )
    )
}


#' Specify interface industries
#' 
#' An interface industry is one that moves energy carriers into or out of a country.
#' When `flow` is any of the interface industries, we need to be more specific.
#' If we don't separate these `flow`s by `product`, we run into trouble with
#' upstream swims (e.g., all `product`s are produced even if only one is needed) and
#' embodied energy calculations (many types of energy are embodied, even if only one should be).
#' This function adds a suffix "\[of Product\]" to each of these interface industries.
#' 
#' Note that "Production" also needs to be specified, 
#' but that is accomplished in the `specify_primary_production()` and
#' `specify_production_to_resources()` functions.
#' 
#' Resource flows and manufacture flows are included by default, 
#' because they need a place where they are specified, too.
#'
#' @param .tidy_iea_df A tidy data frame containing IEA extended energy balance data.
#' @param flow The name of the flow column in `.tidy_iea_df`.  Default is "`Flow`".
#' @param int_industries A string vector of industries involved in exchanges with other countries,
#'                       bunkers, or stock changes. 
#'                       Default is `c(IEATools::interface_industries, IEATools::tpes_flows$resources, manufacture = "Manufacture")`.
#' @param product The name of the product column in `.tidy_iea_df`. 
#'                Default is "`Product`".
#' @param flow_notation The notation for the `flow` column. 
#'                      Default is `RCLabels::of_notation`.
#' @param product_notation The notation for the `product` column.
#'                         Default is `RCLabels::bracket_notation`, meaning that 
#'                         any type of "X \[from Resources\]" or "X \[to Y\]" 
#'                         will be parsed correctly for its prefix.
#'
#' @return A modified version of `.tidy_iea_df` with specified interface industries.
#' 
#' @export
#'
#' @examples
#' load_tidy_iea_df() %>% 
#'   specify_interface_industries()
specify_interface_industries <- function(.tidy_iea_df,
                                         flow = IEATools::iea_cols$flow, 
                                         int_industries = c(IEATools::interface_industries,
                                                            IEATools::tpes_flows$resources,
                                                            manufacture = "Manufacture"),
                                         product = IEATools::iea_cols$product, 
                                         flow_notation = RCLabels::of_notation, 
                                         product_notation = RCLabels::bracket_notation){
  # Find the rows where flow is an interface industry.
  int_ind_rows <- .tidy_iea_df %>% 
    dplyr::filter(.data[[flow]] %in% int_industries)
  # Specify those rows
  int_ind_rows_specified <- int_ind_rows %>% 
    dplyr::mutate(
      "{flow}" := RCLabels::paste_pref_suff(pref = .data[[flow]], 
                                            suff = RCLabels::get_pref_suff(.data[[product]],
                                                                           which = "pref",
                                                                           notation = product_notation),
                                            notation = flow_notation), 
      "{flow}" := as.character(.data[[flow]])
    )
  
  .tidy_iea_df %>% 
    # Subtract the interface industry rows from the incoming data frame
    dplyr::filter(!(.data[[flow]] %in% int_industries)) %>% 
    # Add back the specified rows
    dplyr::bind_rows(int_ind_rows_specified)
}


#' Specify destinations for energy industry own use flows into transformation processes 
#' 
#' The extended energy balance data from the IEA includes 
#' "Energy industry own use" (EIOU) for many transformation processes.
#' Unfortunately, in some cases
#' the EIOU flows into industries that aren't included in transformation processes.
#' For example, "Electricity" is consumed by 
#' "Own use in electricity, CHP and heat plants", 
#' which is not a transformation process.
#' We have to make some decisions to ensure that 
#' EIOU is routed to actual transformation processes.
#' See details for a list of changes made to the `.tidy_iea_df` data frame.
#' 
#' The following changes are made to the `.tidy_iea_df` data frame:
#' 1. EIOU classified as `own_use_elect_chp_heat` is sent to `main_act_producer_elect`.
#' 2. EIOU classified as `pumped_storage` is sent to `main_act_producer_elect`.
#' 3. EIOU classified as `nuclear_industry` is sent to `main_act_producer_elect`.
#' 4. EIOU classified as `non_spec_energy` is sent to `nonspecenergy_reclassify`.
#' 
#' After the changes are made, reassigned EIOU may double-up pre-existing EIOU.
#' For example, a country may already have "Electricity" EIOU flowing into "Main activity producer electricity plants".
#' It may also have EIOU flowing into "Nuclear industry". 
#' When we switch the EIOU flow into "Nuclear industry" into "Main activity producer electricity plants", 
#' we now have two rows of electricity EIOU into "Main activity producer electricity plants".
#' To avoid double rows, all like rows are summed before returning.
#'
#' @param .tidy_iea_df An IEA data frame whose columns have been renamed by `rename_iea_df_cols()`.
#' @param split_own_use_elect_chp_heat_using_shares_of Indicates whether the input or outputs to
#'                                                     Main activity producer plants should be use for
#'                                                     splitting the Own use in electricity, chp and heat plants
#'                                                     EIOU flow. Default is "input".
#' @param route_non_specified_eiou Boolean stating whether non-specified EIOU flows should be routed to existing industries.
#'                                 Default is TRUE.
#' @param route_non_specified_tp Boolean stating whether non-specified transformation processes flows should be routed to existing industries.
#'                               Default is TRUE.
#' @param specify_renewable_plants Boolean stating whether renewable energy industries should be specified or not.
#'                                 Default is FALSE.
#' @param specify_electricity_grid Boolean stating whether an electricity grid industry should be specified or not.
#'                                 Default is FALSE.
#' @param flow_aggregation_point The name of the flow aggregation point column in `.tidy_iea_df`. Default is "Flow.aggregation.point".
#' @param eiou A string identifying energy industry own use in the flow aggregation point column. Default is "Energy industry own use".
#' @param transformation_processes A string identifying transformation processes in the flow aggregation point column. Default is "Transformation processes".
#' @param flow The name of the flow column in `.tidy_iea_df`. Default is "Flow".
#' @param own_use_elect_chp_heat A string identifying own use in electricity, CHP and heat plants in the flow column. Default is "Own use in electricity, CHP and heat plants".
#' @param pumped_storage A string identifying pumped storage plants in the flow column. Default is "Pumped storage plants".
#' @param nuclear_industry A string identifying nuclear plants in the flow column. Default is "Nuclear industry".
#' @param e_dot The name of the energy flow column in `.tidy_iea_df`. Default is "E.dot".
#' @param negzeropos The name of a temporary column created in `.tidy_iea_df`. Default is ".negzeropos".
#' @param main_act_producer_elect A string identifying main activity producer electricity plants. Default is "Main activity producer electricity plants".
#'
#' @return A modified version of `.tidy_iea_df`.
#' 
#' @export
#'
#' @examples
#' library(dplyr)
#' load_tidy_iea_df() %>% 
#'   specify_tp_eiou() %>% 
#'   filter(Flow.aggregation.point == "Energy industry own use" & 
#'            Flow == "Main activity producer electricity plants")
specify_tp_eiou <- function(.tidy_iea_df,
                            split_own_use_elect_chp_heat_using_shares_of = c("input", "output"),
                            route_non_specified_eiou = TRUE,
                            route_non_specified_tp = TRUE,
                            specify_renewable_plants = FALSE,
                            specify_electricity_grid = FALSE,
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
  .tidy_iea_df |> 
    matsindf::verify_cols_missing(negzeropos)
  
  split_own_use_elect_chp_heat_using_shares_of <- match.arg(split_own_use_elect_chp_heat_using_shares_of)
  
  .tidy_iea_df |> 
    gather_producer_autoproducer() %>% 
    route_pumped_storage(
      specify_renewable_plants = specify_renewable_plants
    ) |> 
    split_oil_gas_extraction_eiou() %>% 
    route_own_use_elect_chp_heat(
      split_using_shares_of = split_own_use_elect_chp_heat_using_shares_of
    ) |> 
    add_nuclear_industry() %>% 
    specify_renewable_plants(
      specify_renewable_plants = specify_renewable_plants
    ) |> 
    route_non_specified_flows(
      route_non_specified_eiou = route_non_specified_eiou,
      route_non_specified_tp = route_non_specified_tp
    ) |> 
    specify_electricity_grid(
      specify_electricity_grid = specify_electricity_grid
    )
}


#' Find transformation sinks and sources
#' 
#' In the IEA extended energy balance data, 
#' transformation processes (tp) ought to both consume and produce energy.
#' But some transformation processes consume energy without producing any energy; 
#' others produce without consuming.
#' Such transformation processes can be called "transformation sinks" and 
#' "transformation sources," respectively.
#' This function finds and identifies transformation processes that act as sinks or sources.
#' 
#' It is important to identify transformation sinks, 
#' because they cause two problems for physical supply-use table (PSUT) analysis. 
#' First, when swimming upstream, a PSUT analysis cannot "see" the sunk energy carriers,
#' because they have no downstream effects.
#' Thus, upstream swims cannot conserve energy.
#' Second, when calculating embodied energy for each downstream energy carrier,
#' the sunk energy carriers cannot be embodied in any final demand energy carriers.
#' Thus, embodied energy calculations cannot conserve energy.
#' 
#' Transformation sources can also cause problems for physical supply-use table (PSUT) analysis. 
#' In particular, when swimming upstream, a PSUT analysis will "see" the final energy sources,
#' but cannot see any associated primary energy carriers.
#' 
#' Transformation sinks and sources are identified by the following algorithm:
#' 
#' 1. Identify (per group in `.tidy_iea_df`) all `Transformation processes` that consume energy (negative value for `E.dot`).
#'    Energy consumption can be for the transformation process itself or for Energy industry own use.
#' 2. Identify (per group in `.tidy_iea_df`) all `Transformation processes` that produce energy (positive value for `E.dot`).
#' 3. Take the set difference between the two (consumers less producers for sinks and producers less consumers for sources). 
#'    The set difference is the list of transformation sinks or sources, respectively.
#' 
#' [tp_sinks_sources()] is a function not unlike [dplyr::summarise()];
#' it returns a summary containing grouping variables and industries that are transformation sinks or sources.
#' So be sure to specify (or accept defaults for) 
#' the `grouping_vars` argument.
#' Typical grouping variables are `Method`, `Last.stage`, `Country`, `Year`, `Energy.type`.
#' Don't group on `Flow.aggregation.point`, because energy from different aggregation points
#' (`Energy industry own use` and `Transformation processes`) flows into each machine.
#' Don't group on `Flow`, `Product`, or `E.dot`, either.
#' If groups are not set, 
#' `flow`s will be analyzed together, possibly leading to missed transformation sinks or sources.
#' 
#' The various `specify_*()` functions should also be called _before_ calling [tp_sinks_sources()].
#' The `specify_*()` functions clean up the IEA data, ensuring that energy is routed to the right places.
#' 
#' Note that this function only identifies transformation sinks or sources;
#' it does not fix the problem. 
#' To solve the problem of transformation sinks, 
#' see the [tp_sinks_to_nonenergy()] function.
#' [tp_sinks_to_nonenergy()] uses the output of [tp_sinks_sources()]
#' to route energy consumed by transformation sinks to `Non-energy use industry/transformation/energy`.
#' There is no function to solve the problem of transformation sources at this time.
#'
#' @param .tidy_iea_df a tidy data frame containing IEA extended energy balance data
#' @param type one of "sinks" or "sources"
#' @param flow_aggregation_point the name of the flow aggregation point column in `.tidy_iea_df`. Default is "`Flow.aggregation.point`".
#' @param transformation_processes a string that identifies transformation processes in the `flow_aggregation_point` column. Default is "`Transformation processes`".
#' @param eiou a string that identifies energy industry own use in the `flow_aggregation_point` column. Default is "`Energy industry own use`".
#' @param flow the name of the flow column in `.tidy_iea_df`. Default is "`Flow`".
#' @param product the name of the product column in `.tidy_iea_df`. Default is "`Product`".
#' @param e_dot the name of the energy rate column in `.tidy_iea_df`. Default is "`E.dot`".
#'
#' @return The `grouping_vars` and the `flow` column, 
#'         with one row for each industry that is a transformation sink or source.
#'         Industries that are transformation sinks or sources are named in the `flow` column.
#' 
#' @export
#'
#' @examples
#' library(dplyr)
#' load_tidy_iea_df() %>% 
#'   specify_primary_production() %>% 
#'   specify_interface_industries() %>% 
#'   specify_tp_eiou() %>% 
#'   tp_sinks_sources()
#' load_tidy_iea_df() %>% 
#'   specify_primary_production() %>% 
#'   specify_interface_industries() %>% 
#'   specify_tp_eiou() %>% 
#'   tp_sinks_sources(type = "sources")
tp_sinks_sources <- function(.tidy_iea_df, 
                             type = c("sinks", "sources"),
                             flow_aggregation_point = "Flow.aggregation.point",
                             transformation_processes = "Transformation processes",
                             eiou = "Energy industry own use",
                             flow = "Flow", 
                             product = "Product",
                             e_dot = "E.dot"){
  type <- match.arg(type)
  grouping_vars <- matsindf::everything_except(.tidy_iea_df, flow_aggregation_point, flow, product, e_dot)
  use_rows <- .tidy_iea_df %>% 
    dplyr::group_by(!!!grouping_vars) %>% 
    dplyr::filter((!!as.name(flow_aggregation_point) == transformation_processes | !!as.name(flow_aggregation_point) == eiou) & !!as.name(e_dot) < 0) %>% 
    # dplyr::select(dplyr::group_cols(), flow) %>% 
    dplyr::select(dplyr::group_cols(), dplyr::all_of(flow)) %>% 
    unique() %>% 
    dplyr::ungroup()
  make_rows <- .tidy_iea_df %>% 
    dplyr::group_by(!!!grouping_vars) %>% 
    dplyr::filter(!!as.name(flow_aggregation_point) == transformation_processes & !!as.name(e_dot) > 0) %>% 
    # dplyr::select(dplyr::group_cols(), flow) %>% 
    dplyr::select(dplyr::group_cols(), dplyr::all_of(flow)) %>% 
    unique() %>% 
    dplyr::ungroup()
  # setdiff gives the rows that are IN use_rows but NOT in make_rows.
  if (type == "sinks") {
    return(dplyr::setdiff(use_rows, make_rows))
  } else {
    return(dplyr::setdiff(make_rows, use_rows))
  }
}


#' Reassign Transformation process sinks to Non-energy use
#' 
#' Transformation processes that consume energy without producing any energy are called 
#' "transformation process sinks".
#' See [tp_sinks_sources()] for information about why transformation process sinks are problematic.
#' This function reclassifies energy flowing into transformation process sinks
#' as `non_energy_flow`, by default "Non-energy use in industry/transformation/energy".
#'
#' @param .tidy_iea_df a tidy data frame containing IEA extended energy balance data
#' @param ledger_side the name of the ledger side column in `.tidy_iea_df`. Default is "`Ledger.side`".
#' @param consumption a string identifying the consumption side of the ledger. Default is "`Consumption`".
#' @param flow_aggregation_point the name of the flow aggregation point column in `.tidy_iea_df`. Default is "`Flow.aggregation.point`".
#' @param non_energy_flow_agg_point the name of the aggregation point where transformation process sinks will be reassigned. Default is "`Non-energy use`".
#' @param transformation_processes a string that identifies transformation processes in the `flow_aggregation_point` column. Default is "`Transformation processes`".
#' @param eiou a string that identifies energy industry own use in the `flow_aggregation_point` column. Default is "`Energy industry own use`".
#' @param flow the name of the flow column in `.tidy_iea_df`. Default is "`Flow`".
#' @param non_energy_flow a sting identifying non-energy flows. Default is "`Non-energy use industry/transformation/energy`".
#' @param product the name of the product column in `.tidy_iea_df`. Default is "`Product`".
#' @param e_dot the name of the energy rate column in `.tidy_iea_df`. Default is "`E.dot`".
#'
#' @return `.tidy_iea_df` with energy sunk in Transformation processes sinks reassigned to Non-energy use
#' 
#' @export
#'
#' @examples
#' library(dplyr)
#' DF <- data.frame(
#'   Ledger.side = c("Supply", "Supply", "Supply", "Consumption"),
#'   Flow.aggregation.point = c("Transformation processes", 
#'                              "Transformation processes", 
#'                              "Transformation processes", 
#'                              "Non-energy use"), 
#'   Flow = c("Automobiles", "Automobiles", "Furnaces", 
#'            "Non-energy use industry/transformation/energy"),
#'   Product = c("Petrol", "MD", "Coal", "Coal"),
#'   E.dot = c(-1, 1, -2, 8), 
#'   stringsAsFactors = FALSE
#' ) %>% 
#'   mutate(
#'     Method = "PCM", 
#'     Last.stage = "Final",
#'     Energy.type = "E",
#'     Country = "Bogus",
#'     Year = 1971
#'   )
#' DF
#' DF %>% 
#'   tp_sinks_to_nonenergy()
tp_sinks_to_nonenergy <- function(.tidy_iea_df, 
                                  ledger_side = "Ledger.side",
                                  consumption = "Consumption",
                                  flow_aggregation_point = "Flow.aggregation.point",
                                  non_energy_flow_agg_point = "Non-energy use",
                                  transformation_processes = "Transformation processes",
                                  eiou = "Energy industry own use",
                                  flow = "Flow", 
                                  non_energy_flow = "Non-energy use industry/transformation/energy",
                                  product = "Product",
                                  e_dot = "E.dot"){
                                  # grouping_vars = c("Method", "Last.stage", "Country", "Year", "Energy.type")){
  # First step is to find all Transformation process sinks.
  # These items need to removed from the IEAData data frame, eventually.
  Sinks <- .tidy_iea_df %>% 
    tp_sinks_sources(type = "sinks", 
                     flow_aggregation_point = flow_aggregation_point,
                     transformation_processes = transformation_processes,
                     eiou = eiou,
                     flow = flow, 
                     product = product,
                     e_dot = e_dot)
  # Figure out which rows have sinks in them.
  # They will need to be removed later.
  # But a modified version of Sinks will be routed to final demand.
  # (semi_join keeps only those rows in .tidy_iea_df that match rows in Sinks.)
  Remove_later <- dplyr::semi_join(.tidy_iea_df, Sinks, by = names(Sinks))
  # When modified, the rows in Remove_later will be added to final demand
  # Change the metadata for these items.
  To_add_to_final_demand <- Remove_later %>% 
    dplyr::mutate(
      !!as.name(ledger_side) := consumption, 
      !!as.name(flow_aggregation_point) := non_energy_flow_agg_point,
      !!as.name(flow) := non_energy_flow,
      # The Remove_later entries are all negative 
      # (because they were on the Consumption side of the ledger in Transformation processes). 
      # But they need to be positive when they are moved to final demand.
      !!as.name(e_dot) := abs(!!as.name(e_dot))
    )
  Temp <- .tidy_iea_df %>% 
    # Eliminate rows in .tidy_iea_df that match Remove_later,
    dplyr::anti_join(Remove_later, by = names(Remove_later)) %>% 
    # rbind the new rows to the data frame
    dplyr::bind_rows(To_add_to_final_demand)
  # Look at Temp to find the rows Non-energy use rows
  Nonenergy <- Temp %>% 
    dplyr::filter(!!as.name(flow) == non_energy_flow)
  # Summarize these Non-energy flows
  # This has the effect of adding the new Non-energy use to any existing non-energy use
  # in the same group.
  SummarizedNonenergy <- Nonenergy %>% 
    # Group by all columns except for E.dot
    matsindf::group_by_everything_except(e_dot) %>% 
    dplyr::summarise(!!as.name(e_dot) := sum(!!as.name(e_dot))) %>%
    dplyr::ungroup()
  # Return after removing the non-summarized Non-energy flows and inserting the summarized Non-energy flows.
  Temp %>% 
    # Remove the Nonenergy rows
    dplyr::anti_join(Nonenergy, by = names(Temp)) %>% 
    dplyr::bind_rows(SummarizedNonenergy)
}


#' Specify all industries
#' 
#' This is a convenience function.
#' It bundles several others:
#' 1. [specify_primary_production()]
#' 2. [specify_tp_eiou()]
#' 3. [specify_interface_industries()]
#' 4. [tp_sinks_to_nonenergy()]
#' 
#' Each bundled function is called in turn using default arguments.
#' See examples for two ways to achieve the same result.
#'
#' @param .tidy_iea_df A tidy data frame containing IEA extended energy balance data
#' @param split_own_use_elect_chp_heat_using_shares_of Indicates whether the input or outputs to
#'                                                     Main activity producer plants should be use for
#'                                                     splitting the Own use in electricity, CHP and heat plants
#'                                                     EIOU flow. Default is "input".
#' @param route_non_specified_eiou Boolean stating whether non-specified EIOU flows should be routed to existing industries
#'                                 Default is TRUE.
#' @param route_non_specified_tp Boolean stating whether non-specified transformation processes flows should be routed to existing industries
#'                               Default is TRUE.
#' @param specify_renewable_plants A boolean indicating whether renewable energy plants should be specified or not.
#'                                 Default is FALSE.
#' @param specify_electricity_grid Boolean stating whether an electricity grid industry should be specified or not.
#'                                 Default is FALSE.
#'
#' @return An enhanced and corrected version of `.tidy_iea_df` 
#'         That is ready for physical supply-use table (PSUT) analysis.
#' 
#' @export
#'
#' @examples
#' # Simple
#' load_tidy_iea_df() %>% 
#'   specify_all()
#' # Complicated
#' load_tidy_iea_df() %>% 
#'   specify_primary_production() %>% 
#'   specify_tp_eiou() %>% 
#'   specify_bunkers() %>% 
#'   specify_interface_industries() %>% 
#'   tp_sinks_to_nonenergy()
specify_all <- function(.tidy_iea_df,
                        split_own_use_elect_chp_heat_using_shares_of = c("input", "output"),
                        route_non_specified_eiou = TRUE,
                        route_non_specified_tp = TRUE,
                        specify_renewable_plants = FALSE,
                        specify_electricity_grid = FALSE){
  
  split_own_use_elect_chp_heat_using_shares_of <- match.arg(split_own_use_elect_chp_heat_using_shares_of)
  
  .tidy_iea_df %>% 
    specify_primary_production() %>% 
    #specify_production_to_resources() %>% 
    specify_tp_eiou(
      split_own_use_elect_chp_heat_using_shares_of = split_own_use_elect_chp_heat_using_shares_of,
      route_non_specified_eiou = route_non_specified_eiou,
      route_non_specified_tp = route_non_specified_tp,
      specify_renewable_plants = specify_renewable_plants,
      specify_electricity_grid = specify_electricity_grid
    ) %>% 
    specify_bunkers() %>%
    specify_interface_industries() %>% 
    tp_sinks_to_nonenergy()
}


#' Remove specification strings from a column
#' 
#' `Flow` and `Product` columns of IEA data frames may have been "specified" 
#' with one of the functions of `specify_all()`.
#' The specifying makes it difficult to sort the columns in IEA order (with `sort_iea_df()`), 
#' as the `Flow` and `Product` columns now contain non-IEA flows and products.
#' To enable sorting, this function de-specifies a column in `.df`.
#' 
#' De-specifying includes the following changes:
#'     * Any "Resource" flows are replaced by "Production". E.g., "Resources \[of Coal\]" becomes "Production".
#'     * All parenthetical decorations are removed.  E.g., "Other bituminous coal \[of Coal mines\]" becomes "Other bituminous coal".
#'     
#' Identification of parenthetical notation delimiters is determined by a notation object.
#'
#' @param .df The data frame in which `col` exists.
#' @param col The string name of the column in `.df` to be de-specified.
#' @param despecified_col The string name of the column in the output data frame to contain the de-specified version of `col`.
#' @param notations The notations used for row and column names. See `matsbyname::notation_vec()`. 
#'                  Default is `list(RCLabels::of_notation, RCLabels::from_notation)`, 
#'                  because both `RCLabels::of_notation` and `RCLabels::from_notation` can be used in the `Flow` column
#'                  of an IEA data frame.
#' @param production,resources See `IEATools::tpes_flows`.
#'
#' @return A de-specified version of `.df` and the result placed in the `despecified_col` column.
#' 
#' @export
#'
#' @examples
#' library(dplyr)
#' load_tidy_iea_df() %>% 
#'   specify_all() %>% 
#'   despecify_col(col = "Flow", despecified_col = "clean_Flow") %>% 
#'   select(Flow, Product, E.dot, clean_Flow) %>% 
#'   filter(endsWith(Flow, RCLabels::bracket_notation[["suff_end"]]))
despecify_col <- function(.df, col, despecified_col, 
                          notations = list(RCLabels::of_notation, RCLabels::from_notation),
                          production = IEATools::tpes_flows$production,
                          resources = IEATools::tpes_flows$resources) {
  out <- .df %>% 
    dplyr::mutate(
      "{despecified_col}" := dplyr::case_when(
        # Change "Resources" back to "Production"
        startsWith(.data[[col]], resources) ~ production, 
        TRUE ~ .data[[col]]
      )
    )
  # Now eliminate all suffixes from despecified_col in out
  # and return the resulting data frame.
  out %>% 
    remove_suffix_specifications(col = despecified_col, unsuffixed_col = despecified_col, 
                                 notations = notations)
}


#' Remove specification suffixes from a column
#' 
#' `Flow` and `Product` columns of IEA data frames may have been "specified" 
#' to contain a suffix of the form " \[of Natural gas\"], for example.
#' This function strips away the suffix.
#'     
#' Identification of parenthetical notation delimiters is determined by the `notations` argument.
#'
#' @param .df The data frame in which `col` exists.
#' @param col The string name of the column in `.df` to be de-specified.
#' @param unsuffixed_col The string name of the column in the output data frame to contain the un-suffixed version of `col`.
#' @param notations The notations used for row and column names. See `matsbyname::notation_vec()`. 
#'                  Default is `list(RCLabels::of_notation, RCLabels::from_notation)`, 
#'                  because both `RCLabels::of_notation` and `RCLabels::from_notation` can be used in the `Flow` column
#'                  of an IEA data frame.
#'
#' @return A version of `.df` with suffixes removed from the `col` column and the result placed in the `despecified_col` column.
#' 
#' @export
#'
#' @examples
#' library(dplyr)
#' load_tidy_iea_df() %>% 
#'   specify_all() %>% 
#'   remove_suffix_specifications(col = "Flow", unsuffixed_col = "clean_Flow") %>% 
#'   select(Flow, Product, E.dot, clean_Flow) %>% 
#'   filter(endsWith(Flow, RCLabels::bracket_notation[["suff_end"]]))
remove_suffix_specifications <- function(.df, col, unsuffixed_col, 
                                         notations = list(RCLabels::of_notation, RCLabels::from_notation)){
  # Eliminate all suffixes from col in the outgoing data frame.
  # Save the result in unsuffixed_col.
  out <- .df %>% 
    dplyr::mutate(
      "{unsuffixed_col}" := .data[[col]]
    )
  for (nota in notations) {
    out <- out %>%
      dplyr::mutate(
        "{unsuffixed_col}" := RCLabels::get_pref_suff(.data[[unsuffixed_col]], which = "pref", notation = nota)
      )
  }
  return(out)
}
