


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
production_to_resources <- function(.tidy_iea_df, 
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


#' Prepare for PSUT analysis
#' 
#' This is a convenience function.
#' This function bundles several others:
#' 
#' 1. [specify_primary_production()]
#' 2. [production_to_resources()]
#' 3. [specify_interface_industries()]
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
#'   prep_psut()
#' # Complicated
#' load_tidy_iea_df() %>% 
#'   specify_primary_production() %>% 
#'   production_to_resources()
prep_psut <- function(.tidy_iea_df){
  .tidy_iea_df %>% 
    specify_primary_production() %>% 
    production_to_resources() %>% 
    specify_interface_industries()
}
