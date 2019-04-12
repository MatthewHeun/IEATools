library(dplyr)
library(magrittr)

###########################################################
context("IEA energy balance")
###########################################################

test_that("calc_tidy_iea_df_balance works correctly", {
  Ebal <- load_tidy_iea_df() %>%
    calc_tidy_iea_df_balances()
  expect_false(all(Ebal$balance_OK))
  expect_true(Ebal %>% dplyr::filter(Country == "GHA", Year == 1971, Product == "Aviation gasoline") %>% magrittr::extract2("balance_OK"))
  expect_false(Ebal %>% dplyr::filter(Country == "GHA", Year == 1971, Product == "Electricity") %>% magrittr::extract2("balance_OK"))
  expect_true(Ebal %>% dplyr::filter(Country == "ZAF", Year == 1971, Product == "Hydro") %>% magrittr::extract2("balance_OK"))
  expect_false(Ebal %>% dplyr::filter(Country == "ZAF", Year == 2000, Product == "Other bituminous coal") %>% magrittr::extract2("balance_OK"))
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
})


