#' Specify production industries
#' 
#' The IEA extended energy balances include 
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
#' The following changes are made to `.tidy_iea_df`:
#' 
#' * The `Production` industry for `coal_and_coal_products` 
#'   is replaced by `Coal mines`.
#' * A `Resources (coal)` industry is created which becomes the 
#'   originator of `coal_and_coal_products` in the energy conversion chain.
#' * Flows from `Resources (Coal)` to `Coal mines` are added.
#' * The `Coal mines (energy)` `Flow` is replaced by `Coal mines`.
#'
#' @param .tidy_iea_df an IEA data frame whose columns have been renamed by [rename_iea_df_cols()]
#' @param flow_aggregation_point the name of the flow aggregation point column in `.tidy_iea_df`. Default is "`Flow.aggregation.point`".
#' @param flow the name of the flow column in `.tidy_iea_df`.  Default is "`Flow`".
#' @param production a string identifying production in the flow column. Default is "`Production`".
#' @param transformation_processes a string identifying transformation processes in the flow column of `.tidy_iea_df`. Default is "`Transformation processes`".
#' @param product the name of the product column in `.tidy_iea_df`.  Default is "`Product`".
#' @param coal_mines a string identifying coal mines in the flow column. Default is "`Coal mines`".
#' @param coal_mines_eiou a string identifying energy industry by coal mines. Default is "`coal_mines` + `(energy)`".
#' @param coals a vector of strings identifying coal and coal products. Default is `coal_and_coal_products`.
#' @param e_dot the name of the energy column in `.tidy_eia_df`. Default is "`E.dot`".
#'
#' @return `.tidy_iea_df` with adjusted coal and coal mine information
#' 
#' @export
#'
#' @examples
#' load_tidy_iea_df() %>% 
#'   specify_coal_production() %>% 
#'   add_psut_matnames() %>% 
#'   filter(Flow %in% c("Resources (Coal)", "Coal mines")) %>%
#'   select(-Method, -Last.stage, -Ledger.side, -Unit)
specify_coal_production <- function(.tidy_iea_df,
                               flow_aggregation_point = "Flow.aggregation.point",
                               flow = "Flow", 
                               production = "Production", 
                               transformation_processes = "Transformation processes",
                               product = "Product",
                               coal_mines = "Coal mines",
                               coal_mines_eiou = paste(coal_mines, "(energy)"),
                               coals = coal_and_coal_products,
                               resources_coal = "Resources (Coal)",
                               e_dot = "E.dot"){
  # Find rows of EIOU by Coal mines
  CoalMine_EIOU <- .tidy_iea_df %>% 
    filter(!!as.name(flow) == coal_mines_eiou)
  # Find rows of Production of coals 
  Coal_production <- .tidy_iea_df %>% 
    dplyr::filter(!!as.name(flow) == production & !!as.name(product) %in% coals)
  # Make resource rows for these rows.
  Coal_resources <- Coal_production %>% 
    dplyr::mutate(
      !!as.name(flow) := resources_coal
    )
  if (nrow(CoalMine_EIOU) == 0) {
    # We have no EIOU for coal mines.
    # We need only to change Production to Resources (Coal)
    .tidy_iea_df %>% 
      # Replace Production with Resources (Coal)
      dplyr::mutate(
        !!as.name(flow) := case_when(
          !!as.name(flow) == production & !!as.name(product) %in% coals ~ resources_coal,
          TRUE ~ !!as.name(flow)
        )
      ) %>% 
      return()
  }
  # We have EIOU rows, so we have more work to do.
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

  # Bundle everything and return
  .tidy_iea_df %>% 
    # Replace the "Coal mines (energy)" Flow in EIOU with "Coal mines"
    dplyr::mutate(
      !!as.name(flow) := case_when(
        !!as.name(flow) == "Coal mines (energy)" ~ "Coal mines"
      )
    ) %>% 
    # Delete the original Coal production rows
    dplyr::anti_join(Coal_production, by = names(.tidy_iea_df)) %>% 
    # And bind the replacements.
    dplyr::bind_rows(Coal_resources, Mine_input, Mine_output)
}