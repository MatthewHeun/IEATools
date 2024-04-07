
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
