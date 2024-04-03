
library(dplyr)

tidy_iea_df <- load_tidy_iea_df(.iea_file = "/home/eeear/Documents/Datasets/IEA/WEEBs/IEA Extended Energy Balances 2022 (TJ).csv")


glimpse(tidy_iea_df)

# Testing and checking data
tidy_iea_df |> 
  filter(Flow == "Autoproducer CHP plants") |> 
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

.tidy_iea_df <- .tidy_iea_df |> filter(Country == "WRLD" & Year > 1990)

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
  dplyr::select(-tidyselect::any_of(e_dot)) |> 
  select(-Wind)

# (2.a) Select names of wide data frame just built, so we can add missing products as additional columns
names_selected_io_flows <- names(selected_io_flows)

# (2.b) Modify selected flows
modified_flows <- selected_io_flows %>% 
  tibble::add_column(!!products_tibble[! names(products_tibble) %in% names_selected_io_flows]) %>%
  glimpse()
  
  
  # Replacing NAs by zeros in all columns
  dplyr::mutate(across(tidyselect::all_of(relevant_products), ~tidyr::replace_na(.x, 0))) |> 
  # Defining renewable electricity for products for which all inputs deliver electricity
  dplyr::mutate(
    "{hydro}_{electricity}" := -.data[[hydro]] * ratio_other_renewable_elec,
    "{solar_pv}_{electricity}" := -.data[[solar_pv]] * ratio_other_renewable_elec,
    "{oceanic}_{electricity}" := -.data[[oceanic]] * ratio_other_renewable_elec,
    "{wind}_{electricity}" := -.data[[wind]] * ratio_other_renewable_elec,
  ) |> 
  # Defining renewable electricity and heat for products with potential joint production
  # NOT SURE ABOUT THIS BELOW...!
  dplyr::mutate(
    # Share of electricity output (1 for elec plants, 0 for heat plants, something else for CHP plants)
    "{share_elect_output_From_Func}" := .data[[electricity]] / (.data[[electricity]] + .data[[heat]]),
    # Specifying elec flows
    "{geothermal}_{electricity}" := -(.data[[geothermal]] * .data[[share_elect_output_From_Func]] * ratio_geothermal_heat / (1 + .data[[share_elect_output_From_Func]] * (ratio_geothermal_heat - ratio_geothermal_elec))),
    "{solar_th}_{electricity}" := -(.data[[solar_th]] * .data[[share_elect_output_From_Func]] * ratio_solar_th_heat / (1 + .data[[share_elect_output_From_Func]] * (ratio_solar_th_heat - ratio_solar_th_elec))),
    # Specifying heat flows
    "{geothermal}_{heat}" := -(.data[[geothermal]] * (1 - .data[[share_elect_output_From_Func]] * ratio_geothermal_heat / (1 + .data[[share_elect_output_From_Func]] * (ratio_geothermal_heat - ratio_geothermal_elec)))),
    "{solar_th}_{heat}" := -(.data[[solar_th]] * (1 - .data[[share_elect_output_From_Func]] * ratio_solar_th_heat / (1 + .data[[share_elect_output_From_Func]] * (ratio_solar_th_heat - ratio_solar_th_elec)))),
  ) |> 
  # Subtracting specified electricity and heat flows from existing plants output
  dplyr::mutate(
    "{electricity}" := .data[[electricity]] - (.data[[glue::glue("{hydro}_{electricity}")]] + .data[[glue::glue("{solar_pv}_{electricity}")]] + .data[[glue::glue("{oceanic}_{electricity}")]] + 
                                                 .data[[glue::glue("{wind}_{electricity}")]] + .data[[glue::glue("{geothermal}_{electricity}")]] + .data[[glue::glue("{solar_th}_{electricity}")]]),
    "{heat}" := .data[[heat]] - (.data[[glue::glue("{geothermal}_{heat}")]] + .data[[glue::glue("{solar_th}_{heat}")]])
  ) |> 
  # Removing columns if needed
  dplyr::select(-dplyr::any_of(share_elect_output_From_Func)) %>%
  # Back to tidy, long format
  tidyr::pivot_longer(cols = -c({country}, {method}, {energy_type}, {last_stage}, {year}, {ledger_side}, {flow_aggregation_point}, {flow}, {unit}), 
                      values_to = {e_dot}, names_to = {product}) |> 
  dplyr::filter(.data[[e_dot]] != 0) %>%
  # Adjusting product and flow names:
  dplyr::mutate(
    "{flow}" := dplyr::case_when(
      stringr::str_detect(.data[[product]], geothermal) ~ "Geothermal plants",
      stringr::str_detect(.data[[product]], hydro) ~ "Hydropower plants",
      stringr::str_detect(.data[[product]], solar_pv) ~ "Solar photovoltaic plants",
      stringr::str_detect(.data[[product]], solar_th) ~ "Solar thermal plants",
      stringr::str_detect(.data[[product]], oceanic) ~ "Tide, wave and ocean plants",
      stringr::str_detect(.data[[product]], wind) ~ "Wind power plants",
      TRUE ~ .data[[flow]]
    ),
    "{product}" := stringr::str_remove(.data[[product]], ".*_")
  )

# (3) Builds output data frame by filtering out input data frame (take out modified flows), and collating modified data.
to_return <- .tidy_iea_df %>%
  # Inverse of the condition that was filtered in "modified_flows"
  dplyr::filter(
    ! (.data[[flow_aggregation_point]] == transformation_processes &
      ((.data[[flow]] %in% c(main_act_producer_elect, autoproducer_elect) & .data[[product]] %in% c(renewable_products, electricity)) |
         (.data[[flow]] %in% c(main_act_producer_chp, autoproducer_chp) & .data[[product]] %in% c(renewable_products, electricity, heat)) |
         (.data[[flow]] %in% c(main_act_producer_heat, autoproducer_heat) & .data[[product]] %in% c(renewable_products, heat))))
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