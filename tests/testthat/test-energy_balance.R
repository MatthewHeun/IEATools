library(dplyr)
library(magrittr)

###########################################################
context("IEA energy balance")
###########################################################

test_that("calc_tidy_iea_df_balance works correctly", {
  Ebal <- load_tidy_iea_df() %>%
    calc_tidy_iea_df_balances()
  expect_false(all(Ebal$balance_OK))
  expect_true(Ebal %>% filter(Country == "GHA", Year == 1971, Product == "Aviation gasoline") %>% extract2("balance_OK"))
  expect_false(Ebal %>% filter(Country == "GHA", Year == 1971, Product == "Electricity") %>% extract2("balance_OK"))
  expect_true(Ebal %>% filter(Country == "ZAF", Year == 1971, Product == "Hydro") %>% extract2("balance_OK"))
  expect_false(Ebal %>% filter(Country == "ZAF", Year == 2000, Product == "Other bituminous coal") %>% extract2("balance_OK"))
})

test_that("fix_tidy_iea_df_balance works correctly", {
  unbalanced <- load_tidy_iea_df()
  # This should fail.
  unbalanced %>% 
    calc_tidy_iea_df_balances() %>% 
    tidy_iea_df_balanced() %>% 
    expect_false()
  # But if we fix the energy balances, it should come back balanced.
  unbalanced %>% 
    fix_tidy_iea_df_balances() %>% 
    calc_tidy_iea_df_balances() %>% 
    tidy_iea_df_balanced() %>% 
    expect_true()
  # We can also try the calculation overall (not per Product) by changing the grouping.
  unbalanced %>% 
    fix_tidy_iea_df_balances() %>% 
    # Change grouping. Now, we're calculating energy balances at the country level, not at the product level within countries.
    calc_tidy_iea_df_balances(grouping_vars = c("Country", "Year", "Energy.type", "Unit")) %>% 
    tidy_iea_df_balanced() %>% 
    expect_true()
})

