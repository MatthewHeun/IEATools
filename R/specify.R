


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
#' * The `Production` industry for `coal_and_coal_products` 
#'   is replaced by `Coal mines`.
#' * A `Resources (Coal)` industry is created which becomes the 
#'   source of all primary `coal_and_coal_products` in the energy conversion chain.
#' * Flows from `Resources (Coal)` to `Coal mines` are added.
#' * The `Coal mines (energy)` EIOU `Flow` is replaced by `Coal mines`.
#' * The `Production` industry for `oil_and_oil_products` and `Natural gas`
#'   is replaced by `Oil and gas extraction`.
#' * A `Resources (Oil and natural gas)` industry is created which becomes the 
#'   source of all primary `oil_and_oil_products` and `Natural gas` in the energy conversion chain.
#' * Flows from `Resources (Oil and natural gas)` to `Oil and gas extraction` are added.
#' * The `Oil and gas extraction (energy)` EIOU `Flow` is replaced by `Oil and gas extraction`.
#' 
#' Users can specify other changes by adjusting the default argument values.
#' 
#' Be sure to call this function _after_ calling `augment_iea_df()` or
#' `load_tidy_iea_df()`.
#'
#' @param .tidy_iea_df an IEA data frame whose columns have been renamed by [rename_iea_df_cols()]
#' @param eiou_destinations a vector of destinations for EIOU for primary production of coal and coal products and oil and natural gas.
#'        Default is `c("Coal mines", "Oil and gas extraction")`.
#' @param production_products a list of products for which we want to specify primary industries.
#'        Default is `list(coal_and_coal_products, c(oil_and_oil_products, "Natural gas"))`.
#' @param production_products_short_names a vector of short names for primary industries. 
#'        Default is `c("Coal", "Oil and natural gas")`.
#' @param flow_aggregation_point the name of the flow aggregation point column in `.tidy_iea_df`. 
#'        Default is "`Flow.aggregation.point`".
#' @param eiou a string identifying energy industry own use in the `Flow.aggregation.point` column of `.tidy_iea_df`.
#'        Default is "`Energy industry own use`".
#' @param transformation_processes a string identifying transformation processes in the flow column of `.tidy_iea_df`. 
#'        Default is "`Transformation processes`".
#' @param flow the name of the flow column in `.tidy_iea_df`.  Default is "`Flow`".
#' @param resources a string identifying resource industries to be added to `.tidy_iea_df`. 
#'        Default is "`Resources`".
#' @param production a string identifying production in the flow column. Default is "`Production`".
#' @param e_dot the name of the energy column in `.tidy_eia_df`. Default is "`E.dot`".
#' @param product the name of the product column in `.tidy_iea_df`.  Default is "`Product`".
#'
#' @return `.tidy_iea_df` with adjusted production information for primary energy 
#'         for both coal and coal products and oil and gas extraction
#' 
#' @export
#'
#' @examples
#' library(dplyr)
#' load_tidy_iea_df() %>% 
#'   specify_primary_production() %>% 
#'   add_psut_matnames() %>% 
#'   filter(Flow %in% c("Resources (Coal)", "Coal mines")) %>%
#'   select(-Method, -Last.stage, -Ledger.side, -Unit)
specify_primary_production <- function(.tidy_iea_df,
                                       eiou_destinations = c("Coal mines", "Oil and gas extraction"),
                                       production_products = list(IEATools::coal_and_coal_products, 
                                                                  c(IEATools::oil_and_oil_products, "Natural gas")),
                                       production_products_short_names = c("Coal", "Oil and natural gas"),
                                       flow_aggregation_point = "Flow.aggregation.point",
                                       eiou = "Energy industry own use",
                                       transformation_processes = "Transformation processes",
                                       flow = "Flow", 
                                       # eiou_suffix = "(energy)",
                                       resources = "Resources",
                                       production = "Production", 
                                       e_dot = "E.dot",
                                       product = "Product"){
  specify_primary_func <- function(.tidf, eiou_dest, prod_prods, prod_short_name){
    # Convert from Production to Resources (prod_short_name)
    # For example, Production Anthracite becomes Resources (Coal) Anthracite
    res_name <- resources
    if (!endsWith(resources, paste0("(", prod_short_name, ")"))) {
      res_name <- paste0(res_name, " (", prod_short_name, ")")
    }
    # Replace Production with res_name in Production rows
    .tidf <- .tidf %>% 
      dplyr::mutate(
        !!as.name(flow) := dplyr::case_when(
          !!as.name(flow) == production & !!as.name(product) %in% prod_prods ~ res_name,
          TRUE ~ !!as.name(flow)
        )
      )
    
    EIOU <- .tidf %>% 
      dplyr::filter(!!as.name(flow_aggregation_point) == eiou & 
                      !!as.name(flow) == eiou_dest)
    if (nrow(EIOU) > 0) {
      # We have EIOU rows, so we have more work to do.
      # Find rows of production of prods
      Resource_rows <- .tidf %>% 
        dplyr::filter(!!as.name(flow) == res_name & !!as.name(product) %in% prod_prods)
      # Make rows for input of prod into eiou_dest
      Input <- Resource_rows %>% 
        dplyr::mutate(
          !!as.name(flow_aggregation_point) := transformation_processes,
          !!as.name(flow) := eiou_dest,
          # Convert to an input (negative)
          !!as.name(e_dot) := -!!as.name(e_dot)
        )
      # Make rows for production of prod by eiou_dest
      Output <- Input %>% 
        dplyr::mutate(
          !!as.name(e_dot) := -!!as.name(e_dot)
        )
      # Put it all together
      .tidf <- .tidf %>% 
        # Add rows for additional flow from Resources to the EIOU industry to .tidy_iea_df
        dplyr::bind_rows(Input, Output)
    }
    return(.tidf)
  }
  
  # specify_func is called for its side effect of modifying .tidy_iea_df,
  # so we don't assign the result to any value.
  # Rather, we simply return the modified version of .tidy_iea_df.
  for (i in 1:length(eiou_destinations)) {
    .tidy_iea_df <- specify_primary_func(.tidf = .tidy_iea_df,
                                         eiou_dest = eiou_destinations[[i]], 
                                         prod_prods = production_products[[i]],
                                         prod_short_name = production_products_short_names[[i]])
  }
  return(.tidy_iea_df)
}


#' Convert Production Flows to Resource Flows
#' 
#' The IEA gives resource extraction in rows where the `Flow` is "`Production`".
#' This function changes the "`Production`" string 
#' to "`Resources (product)`", 
#' where `product` is the name of the energy carrier for this resource.
#' 
#' This function should be called _after_ `specify_primary_resources()`,
#' which adjusts for energy industry own use 
#' of some primary energy producing industries.
#' If this function is called first, 
#' EIOU will not be accounted correctly.
#'
#' @param .tidy_iea_df an IEA data frame whose columns have been renamed by [rename_iea_df_cols()]
#' @param flow the name of the flow column in `.tidy_iea_df`.  Default is "`Flow`".
#' @param production a string identifying production in the flow column. Default is "`Production`".
#' @param resources a string identifying resource industries to be added to `.tidy_iea_df`. 
#'        Default is "`Resources`".
#' @param product the name of the product column in `.tidy_iea_df`.  Default is "`Product`".
#'
#' @return `.tidy_iea_df` with `Production` changed to `Resources (product)`
#' 
#' @export
#'
#' @examples
#' load_tidy_iea_df() %>% 
#'   specify_primary_production() %>% 
#'   production_to_resources()
specify_production_to_resources <- function(.tidy_iea_df, 
                                    flow = "Flow",
                                    product = "Product",
                                    production = "Production",
                                    resources = "Resources"){
  # Take any remaining "Production" rows and convert them to Resources (Product).
  .tidy_iea_df %>% 
    dplyr::mutate(
      !!as.name(flow) := dplyr::case_when(
        !!as.name(flow) == production ~ paste0(resources, " (", !!as.name(product), ")"), 
        TRUE ~ !!as.name(flow)
      )
    )
}


#' Specify interface industries
#' 
#' An interface industry is one that moves energy resources in or out of a country.
#' When `Flow` is any of the interface industries, we need to be more specific.
#' If we don't separate these Flows, we run into trouble with
#' upstream swims (e.g., all `Product`s are produced even if only one is needed) and
#' embodied energy calculations (many types of energy are embodied, even if only one sould be).
#' This function adds a suffix ` (Product)` to each of these interface industries.
#' 
#' Note that "`Production`" also needs to be specified, 
#' but that is accomplished in the [specify_primary_production()] and
#' [production_to_resources()] functions.
#'
#' @param .tidy_iea_df a tidy data frame containing IEA extended energy balance data
#' @param flow the name of the flow column in `.tidy_iea_df`.  Default is "`Flow`".
#' @param int_industries a string vector of industries involved in exchanges with other countries,
#'        bunkers, or stock changes. Default is [interface_industries].
#' @param product the name of the product column in `.tidy_iea_df`.  Default is "`Product`".
#'
#' @return a modified version of `.tidy_iea_df` with specified interface industries
#' 
#' @export
#'
#' @examples
#' load_tidy_iea_df() %>% 
#'   specify_interface_industries()
specify_interface_industries <- function(.tidy_iea_df,
                                         flow = "Flow", 
                                         int_industries = IEATools::interface_industries,
                                         product = "Product"){
  .tidy_iea_df %>% 
    dplyr::mutate(
      !!as.name(flow) := dplyr::case_when(
        !!as.name(flow) %in% int_industries ~ paste0(!!as.name(flow), " (", !!as.name(product), ")"),
        TRUE ~ !!as.name(flow)
      )
    )
}


#' Specify destinations for energy industry own use flows into transformation processes 
#' 
#' The extended energy balance data from the IEA includes 
#' Energy industry own use (EIOU) for many transformation processes.
#' Unfortunately, the EIOU flows into industries that aren't included in transformation processes.
#' For example, `Electricity` is consumed by 
#' `Own use in electricity, CHP and heat plants`, 
#' which is not transformation process.
#' We have to make some decisions to ensure that 
#' EIOU is routed to actual transformation processes.
#' See details for a list of actual changes made to the `.tidy_iea_df` data frame.
#' 
#' The following changes are made to the `.tidy_iea_df` data frame:
#' 1. EIOU classified as `own_use_elect_chp_heat` is sent to `main_act_producer_elect`.
#' 2. EIOU classified as `pumped_storage` is sent to `main_act_producer_elect`.
#' 3. EIOU classified as `nuclear_industry` is sent to `main_act_producer_elect`.
#' 4. EIOU classified as `liquefaction_regas` is sent to `liquefaction_regas_reclassify`.
#' 4. EIOU classified as `non_spec_energy` is sent to `nonspecenergy_reclassify`.
#'
#' @param .tidy_iea_df an IEA data frame whose columns have been renamed by [rename_iea_df_cols()]
#' @param flow_aggregation_point the name of the flow aggregation point column in `.tidy_iea_df`. Default is "`Flow.aggregation.point`".
#' @param eiou a string identifying energy industry own use in the flow aggregation point column. Default is "`Energy industry own use`".
#' @param flow the name of the flow column in `.tidy_iea_df`. Default is "`Flow`".
#' @param own_use_elect_chp_heat a string identifying own use in electricity, CHP and heat plants in the flow column. Default is "`Own use in electricity, CHP and heat plants`".
#' @param pumped_storage a string identifying pumped storage plants in the flow column. Default is "`Pumped storage plants`".
#' @param nuclear_industry a string identifying nuclear plants in the flow column. Default is "`Nuclear industry`".
#' @param liquefaction_regas a string identify liquefaction and regasification plants. Default is "`Liquefaction (LNG) / regasification plants`".
#' @param non_spec_energy a string identifying non-specified energy in the flow solumn. Default is "`Non-specified (energy)`".
#' @param main_act_producer_elect a string identifying main activity producer electricity plants. Default is "`Main activity producter electricity plants`".
#' @param liquefaction_regas_reclassify a string identifying the reclassified liquefaction and regasification industry. Default is "`Oil refineries`".
#' @param nonspecenergy_reclassify a string identifying the reclassified non-specified (energy) industry. Default is "`Non-specified (transformation)`".
#'
#' @return a modified version of `.tidy_iea_df`
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
                            flow_aggregation_point = "Flow.aggregation.point",
                            eiou = "Energy industry own use",
                            flow = "Flow", 
                            # Industries that receive EIOU but are not in Transformation processes
                            own_use_elect_chp_heat = "Own use in electricity, CHP and heat plants",
                            pumped_storage = "Pumped storage plants",
                            nuclear_industry = "Nuclear industry",
                            liquefaction_regas = "Liquefaction (LNG) / regasification plants",
                            non_spec_energy = "Non-specified (energy)",
                            # Places where the EIOU will e reassigned
                            main_act_producer_elect = "Main activity producer electricity plants",
                            liquefaction_regas_reclassify = "Oil refineries",
                            nonspecenergy_reclassify = "Non-specified (transformation)"){
  .tidy_iea_df %>% 
    dplyr::mutate(
      !!as.name(flow) := dplyr::case_when(
        # Apply "Own use in electricity, CHP and heat plants" to "Main activity producer electricity plants"
        # This solves a problem in Ghana where "Own use in electricity, CHP and heat plants" 
        # would lead to a zero row in the make matrix.
        # There is no Industry that makes "Own use in electricity, CHP and heat plants".
        # In the absence of further information, and assuming that there is more 
        # electricity production than CHP or heat production in each country, 
        # we apply "Own use in electricity, CHP and heat plants" to
        # "Main activity producer electricity plants".
        !!as.name(flow) == own_use_elect_chp_heat & !!as.name(flow_aggregation_point) == eiou ~ main_act_producer_elect,
        
        # When pumped storage is in the mix, 
        # the IEA data helpfully indicates EIOU assigned to "Pumped storage plants".
        # However, Pumped storage plants do not make any electricity, 
        # so there is no appropriate Industry for its EIOU.
        # To fix this problem, 
        # apply EIOU by Pumped storage plants to 
        # the Industry in which production from Pumped storage plants is accounted:
        # Main activity producer electricity plants.
        !!as.name(flow) == pumped_storage & !!as.name(flow_aggregation_point) == eiou ~ main_act_producer_elect,
        
        # If Nuclear is used, we need to reclassify EIOU by Nuclear plants
        # to Main activity producer electricity plants.
        !!as.name(flow) == nuclear_industry & !!as.name(flow_aggregation_point) == eiou ~ main_act_producer_elect,
        
        # Some EIOU flows into "Liquefaction (LNG) / regasification plants".
        # But there is no "Liquefaction (LNG) / regasification plants" in Transformation processes.
        # So this EIOU flow needs to be reassigned.
        # We choose to reassign "Liquefaction (LNG) / regasification plants" to liquefaction_regas_reclassify.
        !!as.name(flow) == liquefaction_regas & !!as.name(flow_aggregation_point) == eiou ~ liquefaction_regas_reclassify,
        
        # Non-specified (energy) is an Industry that receives EIOU.
        # However, Non-specified (energy) is not an Industry that makes anything.
        # So, we need to reassign these EIOU flows somwehere.
        # For the UK, the numbers for "Non-sepcified (energy)" are rather small.
        # In the absence of any better information, we apply 
        # "Non-specified (energy)" to nonspecenergy_reclassify.  
        !!as.name(flow) == non_spec_energy & !!as.name(flow_aggregation_point) == eiou ~ nonspecenergy_reclassify,
        
        # Otherwise, just keep the same value for the flow column.
        TRUE ~ !!as.name(flow)
      )
    )
}


#' Find transformation sinks
#' 
#' In the IEA extended energy balance data, 
#' transformation processes ought to both consume and produce energy.
#' But some transformation processes consume energy without producing any energy.
#' Those transformation processes can be called "transformation sinks."
#' This function finds and identifies industries that act as transformation sinks.
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
#' Transformation sinks are identified by the following algorithm:
#' 
#' 1. Identify (per group in `.tidy_iea_df`) all `Transformation processes` that consume energy (negative value for `E.dot`).
#'    Energy consumption can be for the transformation process itself or for Energy industry own use.
#' 2. Identify (per group in `.tidy_iea_df`) all `Transformation processes` that produce energy (positive value for `E.dot`).
#' 3. Take the set difference between the two (producers less consumers). 
#'    The set difference is the list of transformation sinks.
#' 
#' [transformation_sinks()] is a function not unlike [dplyr::summarise()];
#' it returns a summary containing grouping variables and industries that are transformation sinks.
#' So be sure to specify (or accept with the defaults) 
#' the `grouping_vars` argument.
#' Typical grouping variables are `Method`, `Last.stage`, `Country`, `Year`, `Energy.type`.
#' Don't group on `Flow.aggregation.point`, because energy from different aggregation points
#' (`Energy industry own use` and `Transformation processes`) flows into each machine.
#' Don't group on `Flow`, `Product`, or `E.dot`, either.
#' If groups are not set, 
#' `flow`s will be analyzed together, possibly leading to missed transformation sinks.
#' 
#' The various `specify_*()` functions should also be called _before_ calling [transformation_sinks()].
#' The `specify_*()` functions clean up the IEA data, ensuring that energy is routed to the right places.
#' 
#' Note that this function only identifies the problem of transformation sinks;
#' it does not fix the problem. 
#' To solve the problem of transformation sinks, 
#' see the [tp_sinks_to_nonenergy()] function.
#' [tp_sinks_to_nonenergy()] uses the output of [transformation_sinks()]
#' to route energy consumed by transformation sinks to `Non-energy use industry/transformation/energy`.
#'
#' @param .tidy_iea_df a tidy data frame containing IEA extended energy balance data
#' @param flow_aggregation_point the name of the flow aggregation point column in `.tidy_iea_df`. Default is "`Flow.aggregation.point`".
#' @param transformation_processes a string that identifies transformation processes in the `flow_aggregation_point` column. Default is "`Transformation processes`".
#' @param eiou a string that identifies energy industry own use in the `flow_aggregation_point` column. Default is "`Energy industry own use`".
#' @param flow the name of the flow column in `.tidy_iea_df`. Default is "`Flow`".
#' @param product the name of the product column in `.tidy_iea_df`. Default is "`Product`".
#' @param e_dot the name of the energy rate column in `.tidy_iea_df`. Default is "`E.dot`".
#' @param grouping_vars a string vector of column names by which `.tidy_iea_df` will be grouped before finding transformation sinks. Default is `c("Method", "Last.stage", "Country", "Year", "Energy.type")`.
#'
#' @return the `grouping_vars` of `.tidy_iea_df` plus the `flow` column, 
#'         with one row for each industry that is a transformation sink.
#'         Industries that are transformation sinks are named in the `flow` column.
#' 
#' @export
#'
#' @examples
#' library(dplyr)
#' load_tidy_iea_df() %>% 
#'   specify_primary_production() %>% 
#'   specify_interface_industries() %>% 
#'   specify_tp_eiou() %>% 
#'   transformation_sinks()
transformation_sinks <- function(.tidy_iea_df, 
                                 flow_aggregation_point = "Flow.aggregation.point",
                                 transformation_processes = "Transformation processes",
                                 eiou = "Energy industry own use",
                                 flow = "Flow", 
                                 product = "Product",
                                 e_dot = "E.dot", 
                                 grouping_vars = c("Method", "Last.stage", "Country", "Year", "Energy.type")){
  assertthat::assert_that(!(flow_aggregation_point %in% grouping_vars), msg = paste(flow_aggregation_point, "cannot be a grouping variable of .tidy_iea_df in transformation_sinks()"))
  assertthat::assert_that(!(flow %in% grouping_vars), msg = paste(flow, "cannot be a grouping variable of .tidy_iea_df in transformation_sinks()"))
  assertthat::assert_that(!(product %in% grouping_vars), msg = paste(product, "cannot be a grouping variable of .tidy_iea_df in transformation_sinks()"))
  assertthat::assert_that(!(e_dot %in% grouping_vars), msg = paste(e_dot, "cannot be a grouping variable of .tidy_iea_df in transformation_sinks()"))
  use_rows <- .tidy_iea_df %>% 
    dplyr::group_by(!!!lapply(grouping_vars, as.name)) %>% 
    dplyr::filter((!!as.name(flow_aggregation_point) == transformation_processes | !!as.name(flow_aggregation_point) == eiou) & !!as.name(e_dot) < 0) %>% 
    dplyr::select(dplyr::group_cols(), flow) %>% 
    unique()
  make_rows <- .tidy_iea_df %>% 
    dplyr::group_by(!!!lapply(grouping_vars, as.name)) %>% 
    dplyr::filter(!!as.name(flow_aggregation_point) == transformation_processes & !!as.name(e_dot) > 0) %>% 
    dplyr::select(dplyr::group_cols(), flow) %>% 
    unique()
  # setdiff gives the rows that are IN use_rows but not in make_rows.
  dplyr::setdiff(use_rows, make_rows) %>% 
    dplyr::ungroup()
}


#' Prepare for PSUT analysis
#' 
#' This is a convenience function.
#' This function bundles several others:
#' 
#' 1. [specify_primary_production()]
#' 2. [specify_production_to_resources()]
#' 3. [specify_tp_eiou()]
#' 4. [specify_interface_industries()]
#' 
#' Each bundled function is called in turn using default arguments.
#' See examples for two ways to achieve the same result.
#'
#' @param .tidy_iea_df a tidy data frame containing IEA extended energy balance data
#'
#' @return an enhanced and corrected version of `.tidy_iea_df` 
#'         that is ready for physical supply-use table (PSUT) analysis.
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
#'   specify_production_to_resources() %>% 
#'   specify_tp_eiou() %>% 
#'   specify_interface_industries()
specify_all <- function(.tidy_iea_df){
  .tidy_iea_df %>% 
    specify_primary_production() %>% 
    specify_production_to_resources() %>% 
    specify_tp_eiou() %>% 
    specify_interface_industries()
}
