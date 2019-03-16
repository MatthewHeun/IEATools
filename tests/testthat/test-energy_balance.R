###########################################################
context("IEA energy balance")
###########################################################

test_that("calc_tidy_iea_df_balance works correctly", {
  Ebal <- file.path("extdata", "GH-ZA-ktoe-Extended-Energy-Balances-sample.csv") %>% 
    system.file(package = "IEAData") %>% 
    iea_df() %>%
    rename_iea_df_cols() %>% 
    use_iso_countries() %>% 
    remove_agg_memo_flows() %>% 
    augment_iea_df() %>% 
    tidy_iea_df() %>% 
    group_by(Country, Year, Energy.type, Units, Product) %>% 
    calc_tidy_iea_df_balances()
  expect_false(all(Ebal$balance_OK))
  expect_true(Ebal %>% filter(Country == "GH", Year == 1971, Product == "Aviation gasoline") %>% extract2("balance_OK"))
  expect_false(Ebal %>% filter(Country == "GH", Year == 1971, Product == "Electricity") %>% extract2("balance_OK"))
  expect_true(Ebal %>% filter(Country == "ZA", Year == 1971, Product == "Hydro") %>% extract2("balance_OK"))
  expect_false(Ebal %>% filter(Country == "ZA", Year == 2000, Product == "Other bituminous coal") %>% extract2("balance_OK"))
})

test_that("fix_IEA_df_energy_balance works correctly", {
  unbalanced <- file.path("extdata", "GH-ZA-ktoe-Extended-Energy-Balances-sample.csv") %>% 
    system.file(package = "IEAData") %>% 
    iea_df() %>%
    rename_iea_df_cols() %>% 
    use_iso_countries() %>% 
    remove_agg_memo_flows() %>% 
    augment_iea_df() %>% 
    tidy_iea_df() %>% 
    group_by(Country, Year, Energy.type, Units, Product) %>% 
    calc_tidy_iea_df_balances() %>% 
    tidy_iea_df_balanced()
  expect_false(unbalanced)
})


