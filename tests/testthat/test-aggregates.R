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
})
#--- EAR, 02/09/2020


# This code tests that South Africa (ZAF) and Ghana (GHA) get aggregated properly into a GHA_ZAF country.
# So there is only one aggregation region as output in this test.
test_that("Aggregating South Africa and Ghana works as intended", {
  
  tidy_GHA_ZAF_df <- load_tidy_iea_df() %>% 
    specify_all()
  
  ### 0. Checking that the aggregation works with the default aggregation table (iea -> exiobase; 2019 iea data)
  default_aggregation_2019 <- tidy_GHA_ZAF_df %>% 
    aggregate_regions()
  
  expect_equal(default_aggregation_2019 %>% nrow(), 409)
  
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
    dplyr::group_by(Year, Ledger.side, Flow.aggregation.point, Flow, Product) %>%
    dplyr::summarise(E.dot.aggregated = sum(E.dot))
  
  # Testing that all rows are perfectly equal and that there are the same number of rows
  comparing <- aggregated_regions %>%
    dplyr::full_join(manual_aggregation) %>%
    dplyr::mutate(
      is_equal = E.dot.aggregated == E.dot
    )
  
  expect_true(all(comparing$is_equal))
  expect_equal(nrow(aggregated_regions), nrow(manual_aggregation))
  
  
  ### 2. Second, now we check that it works well when net_trade is TRUE.
  
  aggregated_regions <- aggregate_regions(tidy_GHA_ZAF_df,
                                          aggregation_table = aggregation_table_GHA_ZAF,
                                          net_trade = TRUE)
  
  manual_aggregation_excl_ie <- tidy_GHA_ZAF_df %>%
    dplyr::filter(! (stringr::str_detect(Flow, "Imports") | stringr::str_detect(Flow, "Exports"))) %>%
    dplyr::group_by(Method, Last.stage, Energy.type, Year, Ledger.side, Flow.aggregation.point, Flow, Product, Unit) %>%
    dplyr::summarise(E.dot.aggregated = sum(E.dot)) %>% 
    dplyr::mutate(
      Country = "GHAZAF"
    )
  
  # This here needs being modified.
  manual_aggregation_ie <- tidy_GHA_ZAF_df %>%
    dplyr::filter(stringr::str_detect(Flow, "Imports") | stringr::str_detect(Flow, "Exports")) %>%
    dplyr::mutate(
      Flow = dplyr::case_when(
        stringr::str_detect(Flow, "Imports") ~ "Imports",
        stringr::str_detect(Flow, "Exports") ~ "Exports",
        TRUE ~ Flow
      )
    ) %>% 
    dplyr::group_by(Method, Energy.type, Last.stage, Year, Ledger.side, Flow.aggregation.point, Flow, Product, Unit) %>%
    dplyr::summarise(E.dot.aggregated = sum(E.dot)) %>%
    tidyr::pivot_wider(names_from = Flow, values_from = E.dot.aggregated) %>%
    dplyr::mutate(
      Imports = tidyr::replace_na(Imports, 0),
      Exports = tidyr::replace_na(Exports, 0),
      Net_Imports = Imports + Exports
    ) %>%
    dplyr::select(-c("Imports", "Exports")) %>%
    tidyr::pivot_longer(cols = Net_Imports, names_to = "Flow", values_to = "E.dot.aggregated") %>%
    dplyr::mutate(
      Flow = dplyr::case_when(
        E.dot.aggregated > 0 ~ "Imports",
        E.dot.aggregated < 0 ~ "Exports",
        E.dot.aggregated == 0 ~ "Net_Imports"
      )
    ) %>%
    dplyr::filter(E.dot.aggregated != 0) %>% 
    dplyr::mutate(
      Flow = stringr::str_c(Flow, " [of ", Product, "]")
    ) %>% 
    dplyr::mutate(
      Country = "GHAZAF"
    )
  
  manual_aggregation <- dplyr::bind_rows(manual_aggregation_excl_ie, manual_aggregation_ie)
  
  comparing <- aggregated_regions %>%
    dplyr::full_join(manual_aggregation, by = c("Country", "Method", "Energy.type", "Last.stage", "Year", "Ledger.side", "Flow.aggregation.point", "Flow", "Product", "Unit")) %>%
    dplyr::mutate(
      is_equal = E.dot.aggregated == E.dot
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
    dplyr::group_by(Country, Year, Ledger.side, Flow.aggregation.point, Flow, Product) %>%
    dplyr::summarise(E.dot.aggregated = sum(E.dot))
  
  comparing <- aggregated_regions %>%
    dplyr::full_join(manual_aggregation) %>%
    dplyr::mutate(
      difference = E.dot.aggregated - E.dot
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
    dplyr::filter(! (stringr::str_detect(Flow, "Imports") | stringr::str_detect(Flow, "Exports"))) %>%
    dplyr::mutate(
      Country = dplyr::case_when(
        Country == "ZAF" ~ "ZAF_MGC",
        Country == "Matt Great Country" ~ "ZAF_MGC",
        Country == "GHA" ~ "GHA_EGC",
        Country == "Emmanuel Great Country" ~ "GHA_EGC"
      )
    ) %>%
    dplyr::group_by(Country, Method, Energy.type, Last.stage, Year, Ledger.side, Flow.aggregation.point, Flow, Product, Unit) %>%
    dplyr::summarise(E.dot.aggregated = sum(E.dot))
  
  manual_aggregation_ie <- tidy_GHA_ZAF_EGC_MGC_df %>%
    dplyr::filter((stringr::str_detect(Flow, "Imports") | stringr::str_detect(Flow, "Exports"))) %>%
    dplyr::mutate(
      Country = dplyr::case_when(
        Country == "ZAF" ~ "ZAF_MGC",
        Country == "Matt Great Country" ~ "ZAF_MGC",
        Country == "GHA" ~ "GHA_EGC",
        Country == "Emmanuel Great Country" ~ "GHA_EGC"
      )
    ) %>%
    dplyr::group_by(Country, Method, Energy.type, Last.stage, Year, Ledger.side, Flow.aggregation.point, Flow, Product, Unit) %>%
    dplyr::summarise(E.dot.aggregated = sum(E.dot)) %>%
    dplyr::mutate(
      Flow = dplyr::case_when(
        stringr::str_detect(Flow, "Imports") ~ "Imports",
        stringr::str_detect(Flow, "Exports") ~ "Exports",
        TRUE ~ Flow
      )
    ) %>% 
    tidyr::pivot_wider(names_from = Flow, values_from = E.dot.aggregated) %>%
    dplyr::mutate(
      Imports = tidyr::replace_na(Imports, 0),
      Exports = tidyr::replace_na(Exports, 0),
      Net_Imports = Imports + Exports
    ) %>%
    dplyr::select(-c("Imports", "Exports")) %>%
    tidyr::pivot_longer(cols = Net_Imports, names_to = "Flow", values_to = "E.dot.aggregated") %>%
    dplyr::mutate(
      Flow = dplyr::case_when(
        E.dot.aggregated > 0 ~ "Imports",
        E.dot.aggregated < 0 ~ "Exports",
        E.dot.aggregated == 0 ~ "Net_Imports"
      )
    ) %>%
    dplyr::filter(E.dot.aggregated != 0) %>% 
    dplyr::mutate(
      Flow = stringr::str_c(Flow, " [of ", Product, "]")
    )
  
  manual_aggregation <- dplyr::bind_rows(manual_aggregation_excl_ie, manual_aggregation_ie)
  
  comparing <- aggregated_regions %>%
    dplyr::full_join(manual_aggregation) %>%
    dplyr::mutate(
      difference = E.dot.aggregated - E.dot
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
    dplyr::summarise(EX.p = sum(.data[["E.dot"]]), .groups = "drop")
  
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
      EX.d_net = sum(E.dot), 
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
      eiou = abs(sum(E.dot)), 
      .groups = "drop"
    )
  
  expected <- dplyr::full_join(net_energy, gross_less_net, 
                               by = c(IEATools::iea_cols$country, 
                                      IEATools::iea_cols$method, 
                                      IEATools::iea_cols$energy_type, 
                                      IEATools::iea_cols$last_stage, 
                                      IEATools::iea_cols$year)) %>% 
    dplyr::mutate(
      EX.d_gross = EX.d_net + eiou, 
      eiou = NULL
    )
    
  # Make sure we get the same thing.
  expect_equal(result, expected)
})


