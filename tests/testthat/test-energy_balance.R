library(dplyr)
library(magrittr)

###########################################################
context("IEA energy balance")
###########################################################

test_that("calc_tidy_iea_df_balance works correctly", {
  Ebal <- file.path("extdata", "GH-ZA-ktoe-Extended-Energy-Balances-sample.csv") %>% 
    system.file(package = "IEATools") %>% 
    iea_df() %>%
    rename_iea_df_cols() %>% 
    use_iso_countries() %>% 
    remove_agg_memo_flows() %>% 
    augment_iea_df() %>% 
    tidy_iea_df() %>% 
    group_by(Country, Year, Energy.type, Unit, Product) %>% 
    calc_tidy_iea_df_balances()
  expect_false(all(Ebal$balance_OK))
  expect_true(Ebal %>% filter(Country == "GH", Year == 1971, Product == "Aviation gasoline") %>% extract2("balance_OK"))
  expect_false(Ebal %>% filter(Country == "GH", Year == 1971, Product == "Electricity") %>% extract2("balance_OK"))
  expect_true(Ebal %>% filter(Country == "ZA", Year == 1971, Product == "Hydro") %>% extract2("balance_OK"))
  expect_false(Ebal %>% filter(Country == "ZA", Year == 2000, Product == "Other bituminous coal") %>% extract2("balance_OK"))
})

test_that("fix_tidy_iea_df_balance works correctly", {
  unbalanced <- file.path("extdata", "GH-ZA-ktoe-Extended-Energy-Balances-sample.csv") %>% 
    system.file(package = "IEATools") %>% 
    iea_df() %>%
    rename_iea_df_cols() %>% 
    use_iso_countries() %>% 
    remove_agg_memo_flows() %>% 
    augment_iea_df() %>% 
    tidy_iea_df()
  # This should fail.
  unbalanced %>% 
    group_by(Country, Year, Energy.type, Unit, Product) %>% 
    calc_tidy_iea_df_balances() %>% 
    tidy_iea_df_balanced() %>% 
    expect_false()
  # But if we fix the energy balances, it should come back balanced.
  unbalanced %>% 
    group_by(Country, Year, Energy.type, Unit, Product) %>% 
    fix_tidy_iea_df_balances() %>% 
    calc_tidy_iea_df_balances() %>% 
    tidy_iea_df_balanced() %>% 
    expect_true()
  # We can also try the calculation overall (not per Product) by changing the grouping.
  unbalanced %>% 
    group_by(Country, Year, Energy.type, Unit, Product) %>% 
    fix_tidy_iea_df_balances() %>% 
    # Change grouping. Now, we're calculating energy balances at the country level, not at the product level within countries.
    group_by(Country, Year, Energy.type, Unit) %>% 
    calc_tidy_iea_df_balances() %>% 
    tidy_iea_df_balanced() %>% 
    expect_true()
})


