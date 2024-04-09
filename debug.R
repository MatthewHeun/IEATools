
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


renewable_industry_eiou <- eiou_elec_heat_CHP_plants |> 
  dplyr::left_join(share_renewable_output_df, by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {unit})) |> 
  tidyr::pivot_longer(cols = tidyselect::any_of(shares_of_interest), names_to = .share_industry, values_to = .share) |> 
  
  
  
  dplyr::mutate(
    "{e_dot}" := .data[[e_dot]] * .data[[.share]],
    "{flow}" := stringr::str_extract(.data[[.share_industry]], "_.*") |> 
      stringr::str_remove("_")
  ) |> 
  dplyr::select(-tidyselect::any_of(c(.share, .share_industry)))

