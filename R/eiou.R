#' Specify production industries
#' 
#' The IEA extended energy balance data include 
#' some `Flow`s that identify `Energy industry own use` (EIOU), 
#' the consumption of energy by energy-producing industries.
#' But some industries that receive EIOU do not produce anything.
#' For example, `Coal mines` receive electricity
#' but there are no `Coal mines` that produce coal.
#' Rather, the generic `Production` industry produces coal.
#' This function solves that problem by 
#' replacing the generic `Production` industry with 
#' specific industries.
#' 
#' In particular, the following changes are made:
#' 
#' * The `Production` industry for `coal_and_coal_products` 
#'   is replaced by `Coal mines`.
#' * A `Resources (coal)` industry is created.
#'
#' @param .tidy_iea_data 
#' @param flow 
#' @param product 
#' @param production 
#' @param coal_mines 
#' @param coals 
#'
#' @return
#' 
#' @export
#'
#' @examples
specify_production <- function(.tidy_iea_data,
                               flow_aggregation_point = "Flow.aggregation.point",
                               transformation_processes = "Transformation processes",
                               flow = "Flow", 
                               product = "Product",
                               production = "Production", 
                               coal_mines = "Coal mines",
                               coals = coal_and_coal_products,
                               resources_coal = "Resources (Coal)",
                               e_dot = "E.dot"){
  # Find rows of Production of coals 
  Coal_production <- .tidy_iea_data %>% 
    dplyr::filter(!!as.name(flow) == production & !!as.name(product) %in% coals)
  # Make resource rows for these rows.
  Coal_resources <- Coal_production %>% 
    dplyr::mutate(
      !!as.name(flow) := resources_coal
    )
  # Make rows for input of coal into mines
  Mine_input <- Coal_production %>% 
    dplyr::mutate(
      !!as.name(flow_aggregation_point) := transformation_processes,
      !!as.name(flow) := coal_mines,
      !!as.name(e_dot) := -!!as.name(e_dot)
    )
  # Make rows for production of coal by coal mines
  Mine_output <- Mine_input %>% 
    dplyr::mutate(
      !!as.name(e_dot) := -!!as.name(e_dot)
    )
  # Replace the "Coal mines (energy)" Flow in EIOU with "Coal mines"

  # Bundle everything and return
  .tidy_iea_data %>% 
    # Delete the original Coal production rows
    anti_join(Coal_production) %>% 
    # And bind the replacements.
    bind_rows(Coal_resources, Mine_input, Mine_output)
}