# Tests regarding the regional aggregation process
test_that("Loading regional aggregation table works as intended", {
  
  # First using default 2019 aggregation table
  aggregation_table <- read_aggregation_region_table()
  
  # Testing empty celles are gotten rid of
  expect_equal(nrow(aggregation_table %>% dplyr::filter(IEA_regions == "OECD Americas")), 0)
  expect_equal(nrow(aggregation_table %>% dplyr::filter(IEA_regions == "Memo: Mali")), 0) 
  
  # Testing correct destinations
  expect_equal(nrow(aggregation_table %>%
                      dplyr::filter(IEA_regions == "France" & Destination_regions == "France") %>%
                      dplyr::select(Destination_regions)), 1)
  
  expect_equal(nrow(aggregation_table %>%
                      dplyr::filter(IEA_regions == "Paraguay" & Destination_regions == "RoW America") %>%
                      dplyr::select(Destination_regions)), 1)
  
  
  # Changing file, now using 2020 aggregation table
  aggregation_table <- read_aggregation_region_table(file_path = system.file("extdata", 
                                                                             "aggregation_table_iea_exiobase_2020.xlsx",
                                                                             package = "IEATools"))
  
  # Testing empty cells are gotten rid of
  expect_equal(nrow(aggregation_table %>% dplyr::filter(IEA_regions == "OECD Americas")), 0)
  expect_equal(nrow(aggregation_table %>% dplyr::filter(IEA_regions == "Memo: Mali")), 0) 
  
  # Testing correct destinations
  expect_equal(nrow(aggregation_table %>%
                      dplyr::filter(IEA_regions == "France" & Destination_regions == "France") %>%
                      dplyr::select(Destination_regions)), 1)
  
  expect_equal(nrow(aggregation_table %>%
                      dplyr::filter(IEA_regions == "Paraguay" & Destination_regions == "RoW America") %>%
                      dplyr::select(Destination_regions)), 1)
})
#--- EAR, 02/09/2020


# This code tests that South Africa (ZAF) and Ghana (GHA) get aggregated properly into a GHA_ZAF country.
# So there is only one aggregation region as output in this test.
test_that("Aggregating South Africa and Ghana works as intended", {
  
  tidy_GHA_ZAF_df_2021 <- sample_iea_data_path(version = 2021) |> 
    load_tidy_iea_df(apply_fixes = FALSE) |> 
    specify_all()
  
  tidy_GHA_ZAF_df <- sample_iea_data_path() |> 
    load_tidy_iea_df(apply_fixes = FALSE) |> 
    specify_all()
  
  ### 0. Checking that the aggregation works with the default aggregation table (iea -> exiobase; 2019 iea data)
  default_aggregation_2021 <- tidy_GHA_ZAF_df_2021 %>% 
    aggregate_regions()
  
  expect_equal(default_aggregation_2021 %>% nrow(), 428)
  
  default_aggregation <- tidy_GHA_ZAF_df %>% 
    aggregate_regions()
  
  # Added a row for imports of Charcoal
  expect_equal(default_aggregation %>% nrow(), 429)
  
  ### 1. First, checking that it works well when net_trade flag is FALSE.
  
  aggregation_table_GHA_ZAF <- tibble::tribble(
    ~IEA_regions, ~Destination_regions, ~Country,
    "Ghana", "GHAZAF", "GHA",
    "South Africa", "GHAZAF", "ZAF", 
    "Spain", "Spain", "ESP",
    "France", "France", "FRA"
  )
  
  aggregated_regions <- aggregate_regions(tidy_GHA_ZAF_df,
                                          aggregation_table = aggregation_table_GHA_ZAF,
                                          net_trade = FALSE)
  
  manual_aggregation <- tidy_GHA_ZAF_df %>%
    dplyr::group_by(Year, LedgerSide, FlowAggregationPoint, Flow, Product, Unit) %>% 
    dplyr::summarise(Edot.aggregated = sum(Edot), .groups = "drop")
  
  # Testing that all rows are perfectly equal and that there are the same number of rows
  comparing <- aggregated_regions %>%
    dplyr::full_join(manual_aggregation, 
                     by = dplyr::join_by(Year, LedgerSide, FlowAggregationPoint, Flow, Product, Unit)) %>% 
    dplyr::mutate(
      is_equal = Edot.aggregated == Edot
    )
  
  expect_true(all(comparing$is_equal))
  expect_equal(nrow(aggregated_regions), nrow(manual_aggregation))
  
  
  ### 2. Second, now we check that it works well when net_trade is TRUE.
  
  aggregated_regions <- aggregate_regions(tidy_GHA_ZAF_df,
                                          aggregation_table = aggregation_table_GHA_ZAF,
                                          net_trade = TRUE)
  
  
  manual_aggregation_excl_ie <- tidy_GHA_ZAF_df %>% 
    #remove_suffix_specifications(col = IEATools::iea_cols$flow, unsuffixed_col = IEATools::iea_cols$flow) %>% 
    dplyr::filter(! (stringr::str_detect(Flow, IEATools::interface_industries$imports) | stringr::str_detect(Flow, IEATools::interface_industries$exports)) |
                    stringr::str_detect(Flow, IEATools::tpes_flows$exports_to_world_aviation_bunkers) |
                    stringr::str_detect(Flow, IEATools::tpes_flows$exports_to_world_marine_bunkers)) %>%
    #specify_interface_industries() %>% 
    dplyr::group_by(Method, LastStage, EnergyType, Year, LedgerSide, FlowAggregationPoint, Flow, Product, Unit) %>%
    dplyr::summarise(Edot.aggregated = sum(Edot), .groups = "drop") %>% 
    dplyr::mutate(
      Country = "GHAZAF"
    )
  
  # This here needs being modified.
  manual_aggregation_ie <- tidy_GHA_ZAF_df %>%
    dplyr::filter(stringr::str_detect(Flow, IEATools::interface_industries$imports) | stringr::str_detect(Flow, IEATools::interface_industries$exports)) %>%
    dplyr::filter(! (stringr::str_detect(Flow, IEATools::tpes_flows$exports_to_world_marine_bunkers) |
                       stringr::str_detect(Flow, IEATools::tpes_flows$exports_to_world_aviation_bunkers))) %>%
    dplyr::group_by(Method, EnergyType, LastStage, Year, LedgerSide, FlowAggregationPoint, Flow, Product, Unit) %>%
    dplyr::summarise(Edot.aggregated = sum(Edot), .groups = "drop") %>%
    remove_suffix_specifications(col = IEATools::iea_cols$flow, unsuffixed_col = IEATools::iea_cols$flow) %>%
    tidyr::pivot_wider(names_from = Flow, values_from = Edot.aggregated) %>% 
    dplyr::mutate(
      Imports = tidyr::replace_na(Imports, 0),
      Exports = tidyr::replace_na(Exports, 0),
      Net_Imports = Imports + Exports
    ) %>%
    dplyr::select(-c("Imports", "Exports")) %>%
    tidyr::pivot_longer(cols = Net_Imports, names_to = "Flow", values_to = "Edot.aggregated") %>%
    dplyr::mutate(
      Flow = dplyr::case_when(
        Edot.aggregated > 0 ~ "Imports",
        Edot.aggregated < 0 ~ "Exports",
        Edot.aggregated == 0 ~ "Net_Imports"
      )
    ) %>%
    dplyr::filter(Edot.aggregated != 0) %>% 
    specify_interface_industries() %>% 
    dplyr::mutate(
      Country = "GHAZAF"
    )
    
  manual_aggregation <- dplyr::bind_rows(manual_aggregation_excl_ie, manual_aggregation_ie)
  
  comparing <- aggregated_regions %>%
    dplyr::full_join(manual_aggregation, by = c("Country", "Method", "EnergyType", "LastStage", "Year", "LedgerSide", "FlowAggregationPoint", "Flow", "Product", "Unit")) %>%
    dplyr::mutate(
      is_equal = Edot.aggregated == Edot
    )

  # Testing that all rows are perfectly equal and that there are the same number of rows
  
  expect_true(all(comparing$is_equal))
  expect_equal(nrow(aggregated_regions), nrow(manual_aggregation))
})
# --- EAR, 01/10/2020


# This code tests that aggregation by regions works as intended by;
# (1) Aggregating South Africa (ZAF) and Matt's Great Country (MGC), a duplicate of South Africa;
# (2) Aggregating Ghana (GHA) and Emmanuel's Great Country (EGC), a duplicate of Ghana;
# There are therefore two aggregation regions as output.
test_that("Aggregating ZAF and MGC, and GHA and EGC, works as intended", {
  
  ### 1. First, checking that it works well when net_trade flag is FALSE.
  tidy_GHA_ZAF_df <- load_tidy_iea_df()
  
  
  tidy_GHA_ZAF_EGC_MGC_df <- dplyr::bind_rows(
    tidy_GHA_ZAF_df,
    tidy_GHA_ZAF_df %>% 
      dplyr::mutate(
        "{IEATools::iea_cols$country}" := dplyr::case_when(
          .data[[IEATools::iea_cols$country]] == "GHA" ~ "Emmanuel Great Country",
          .data[[IEATools::iea_cols$country]] == "ZAF" ~ "Matt Great Country"
        ))) %>% 
    specify_all()
  
  aggregation_table_GHA_ZAF_EGC_MGC <- tibble::tribble(
    ~IEA_regions, ~Destination_regions, ~Country,
    "Ghana", "GHA_EGC", "GHA",
    "South Africa", "ZAF_MGC", "ZAF", 
    "Matt Great Country", "ZAF_MGC", "Matt Great Country",
    "Emmanuel Great Country", "GHA_EGC", "Emmanuel Great Country",
    "Spain", "Spain", "ESP",
    "France", "France", "FRA"
  )
  
  aggregated_regions <- aggregate_regions(tidy_GHA_ZAF_EGC_MGC_df,
                                          aggregation_table = aggregation_table_GHA_ZAF_EGC_MGC,
                                          net_trade = FALSE)
  
  manual_aggregation <- tidy_GHA_ZAF_EGC_MGC_df %>%
    dplyr::mutate(
      Country = dplyr::case_when(
        Country == "ZAF" ~ "ZAF_MGC",
        Country == "Matt Great Country" ~ "ZAF_MGC",
        Country == "GHA" ~ "GHA_EGC",
        Country == "Emmanuel Great Country" ~ "GHA_EGC"
      )
    ) %>%
    dplyr::group_by(Country, Year, LedgerSide, FlowAggregationPoint, Flow, Product) %>%
    dplyr::summarise(Edot.aggregated = sum(Edot), .groups = "drop")
  
  comparing <- aggregated_regions %>%
    dplyr::full_join(manual_aggregation, 
                     by = dplyr::join_by(Country, Year, LedgerSide, FlowAggregationPoint, Flow, Product)) %>%
    dplyr::mutate(
      difference = Edot.aggregated - Edot
    )
  
  # Testing that all rows are perfectly equal and that there are the same number of rows
  
  count_non_null_differences <- comparing %>%
    dplyr::filter(difference != 0) %>%
    nrow()
  
  expect_equal(count_non_null_differences, 0)
  expect_equal(nrow(aggregated_regions), nrow(manual_aggregation))
  
  
  ### 2. Second, now we check that it works well when net_trade is TRUE.
  
  aggregated_regions <- aggregate_regions(tidy_GHA_ZAF_EGC_MGC_df,
                                          aggregation_table = aggregation_table_GHA_ZAF_EGC_MGC,
                                          net_trade = TRUE)
  
  manual_aggregation_excl_ie <- tidy_GHA_ZAF_EGC_MGC_df %>%
    dplyr::filter(! (stringr::str_detect(Flow, IEATools::interface_industries$imports) | stringr::str_detect(Flow, IEATools::interface_industries$exports)) |
                    stringr::str_detect(Flow, IEATools::tpes_flows$exports_to_world_aviation_bunkers) |
                    stringr::str_detect(Flow, IEATools::tpes_flows$exports_to_world_marine_bunkers)) %>%
    dplyr::mutate(
      Country = dplyr::case_when(
        Country == "ZAF" ~ "ZAF_MGC",
        Country == "Matt Great Country" ~ "ZAF_MGC",
        Country == "GHA" ~ "GHA_EGC",
        Country == "Emmanuel Great Country" ~ "GHA_EGC"
      )
    ) %>%
    dplyr::group_by(Country, Method, EnergyType, LastStage, Year, LedgerSide, FlowAggregationPoint, Flow, Product, Unit) %>%
    dplyr::summarise(Edot.aggregated = sum(Edot), .groups = "drop")
  
  manual_aggregation_ie <- tidy_GHA_ZAF_EGC_MGC_df %>%
    dplyr::filter(stringr::str_detect(Flow, IEATools::interface_industries$imports) | stringr::str_detect(Flow, IEATools::interface_industries$exports)) %>%
    dplyr::filter(! (stringr::str_detect(Flow, IEATools::tpes_flows$exports_to_world_marine_bunkers) |
                       stringr::str_detect(Flow, IEATools::tpes_flows$exports_to_world_aviation_bunkers))) %>%
    dplyr::mutate(
      Country = dplyr::case_when(
        Country == "ZAF" ~ "ZAF_MGC",
        Country == "Matt Great Country" ~ "ZAF_MGC",
        Country == "GHA" ~ "GHA_EGC",
        Country == "Emmanuel Great Country" ~ "GHA_EGC"
      )
    ) %>%
    dplyr::group_by(Country, Method, EnergyType, LastStage, Year, LedgerSide, FlowAggregationPoint, Flow, Product, Unit) %>%
    dplyr::summarise(Edot.aggregated = sum(Edot), 
                     .groups = "drop") %>%
    dplyr::mutate(
      Flow = dplyr::case_when(
        stringr::str_detect(Flow, "Imports") ~ "Imports",
        stringr::str_detect(Flow, "Exports") ~ "Exports",
        TRUE ~ Flow
      )
    ) %>% 
    tidyr::pivot_wider(names_from = Flow, values_from = Edot.aggregated) %>%
    dplyr::mutate(
      Imports = tidyr::replace_na(Imports, 0),
      Exports = tidyr::replace_na(Exports, 0),
      Net_Imports = Imports + Exports
    ) %>%
    dplyr::select(-c("Imports", "Exports")) %>%
    tidyr::pivot_longer(cols = Net_Imports, names_to = "Flow", values_to = "Edot.aggregated") %>%
    dplyr::mutate(
      Flow = dplyr::case_when(
        Edot.aggregated > 0 ~ "Imports",
        Edot.aggregated < 0 ~ "Exports",
        Edot.aggregated == 0 ~ "Net_Imports"
      )
    ) %>%
    dplyr::filter(Edot.aggregated != 0) %>% 
    specify_interface_industries()
    
  manual_aggregation <- dplyr::bind_rows(manual_aggregation_excl_ie, manual_aggregation_ie)
  
  comparing <- aggregated_regions %>%
    dplyr::full_join(manual_aggregation, 
                     by = dplyr::join_by(Country, Method, EnergyType, LastStage, Year,
                                         LedgerSide, FlowAggregationPoint, Flow, Product, Unit)) %>%
    dplyr::mutate(
      difference = Edot.aggregated - Edot
    )
  
  # Testing that all rows are perfectly equal and that there are the same number of rows
  
  count_non_null_differences <- comparing %>%
    dplyr::filter(difference != 0) %>%
    nrow()
  
  expect_equal(count_non_null_differences, 0)
  expect_equal(nrow(aggregated_regions), nrow(manual_aggregation))
  
})
# --- EAR, 01/10/2020


test_that("primary_aggregates() works as expected", {

  result <- load_tidy_iea_df() %>% 
    primary_aggregates()
  
  # Do our own aggregation
  expected <- load_tidy_iea_df() %>% 
    dplyr::filter(.data[[IEATools::iea_cols$flow_aggregation_point]] == IEATools::aggregation_flows$total_primary_energy_supply) %>% 
    dplyr::group_by(.data[[IEATools::iea_cols$country]], 
                    .data[[IEATools::iea_cols$method]], 
                    .data[[IEATools::iea_cols$energy_type]], 
                    .data[[IEATools::iea_cols$last_stage]], 
                    .data[[IEATools::iea_cols$year]]) %>% 
    dplyr::summarise(EXp = sum(.data[["Edot"]]), .groups = "drop")
  
  expect_equal(result, expected)
})


test_that("finaldemand_aggregates() works as expected", {
  result <- load_tidy_iea_df() %>% 
    finaldemand_aggregates()
  
  # Do our own aggregation
  net_energy <- load_tidy_iea_df() %>% 
    dplyr::filter(.data[[IEATools::iea_cols$ledger_side]] == IEATools::ledger_sides$consumption) %>% 
    dplyr::group_by(.data[[IEATools::iea_cols$country]], 
                    .data[[IEATools::iea_cols$method]], 
                    .data[[IEATools::iea_cols$energy_type]], 
                    .data[[IEATools::iea_cols$last_stage]], 
                    .data[[IEATools::iea_cols$year]]) %>% 
    dplyr::summarise(
      # Net energy
      EXfdnet = sum(Edot), 
      .groups = "drop"
    )
  
  gross_less_net <- load_tidy_iea_df() %>% 
    dplyr::filter(.data[[IEATools::iea_cols$flow_aggregation_point]] == IEATools::tfc_compare_flows$energy_industry_own_use) %>% 
    dplyr::group_by(.data[[IEATools::iea_cols$country]], 
                    .data[[IEATools::iea_cols$method]], 
                    .data[[IEATools::iea_cols$energy_type]], 
                    .data[[IEATools::iea_cols$last_stage]], 
                    .data[[IEATools::iea_cols$year]]) %>% 
    dplyr::summarise(
      # Net energy
      eiou = abs(sum(Edot)), 
      .groups = "drop"
    )
  
  expected <- dplyr::full_join(net_energy, gross_less_net, 
                               by = c(IEATools::iea_cols$country, 
                                      IEATools::iea_cols$method, 
                                      IEATools::iea_cols$energy_type, 
                                      IEATools::iea_cols$last_stage, 
                                      IEATools::iea_cols$year)) %>% 
    dplyr::mutate(
      "{IEATools::aggregate_cols$gross_aggregate_demand}" := .data[[IEATools::aggregate_cols$net_aggregate_demand]] + eiou, 
      eiou = NULL
    )
    
  # Make sure we get the same thing.
  expect_equal(result, expected)
})


