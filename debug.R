
library(dplyr)

tidy_iea_df <- load_tidy_iea_df(.iea_file = "/home/eeear/Documents/Datasets/IEA/WEEBs/IEA Extended Energy Balances 2022 (TJ).csv")


glimpse(tidy_iea_df)

# Testing and checking data
tidy_iea_df |> 
  filter(Flow == "Main activity producer electricity plants") |>
  filter(Product == "Solar thermal") |> 
  glimpse()
  filter(E.dot != 0) |> 
  tidyr::expand(Product) |> 
  View()


# First specification process
partially_specified_data <- tidy_iea_df |> 
  specify_primary_production() |> 
  gather_producer_autoproducer() %>% 
  route_pumped_storage() %>% 
  split_oil_gas_extraction_eiou() %>% 
  route_own_use_elect_chp_heat() %>% 
  add_nuclear_industry()




# Running function
res <- partially_specified_data |> 
  specify_renewable_plants()



# Debugging function

# THE ODD THING TO CHECK IS WHY SOLAR THERMAL ONLY APPEARS AS EIOU AT GLOBAL LEVEL IN CHP PLANTS. THIS DOESNT MAKE SENSE.

.tidy_iea_df <- .tidy_iea_df |> filter(Country == "WRLD" & Year > 1990)

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
  #dplyr::group_by(tidyselect::all_of(c(country, method, energy_type, last_stage, year, product, unit))) |> 
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