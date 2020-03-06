library(dplyr)
library(magrittr)

###########################################################
context("IEA energy balance")
###########################################################

test_that("calc_tidy_iea_df_balance works correctly for 2018 data", {
  Ebal_2018 <- load_tidy_iea_df(sample_iea_data_path(2018)) %>%
    calc_tidy_iea_df_balances()
  expect_false(all(Ebal$balance_OK))
  expect_true(Ebal_2018 %>% dplyr::filter(Country == "GHA", Year == 1971, Product == "Aviation gasoline") %>% magrittr::extract2("balance_OK"))
  expect_false(Ebal_2018 %>% dplyr::filter(Country == "GHA", Year == 1971, Product == "Electricity") %>% magrittr::extract2("balance_OK"))
  expect_true(Ebal_2018 %>% dplyr::filter(Country == "ZAF", Year == 1971, Product == "Hydro") %>% magrittr::extract2("balance_OK"))
  expect_false(Ebal_2018 %>% dplyr::filter(Country == "ZAF", Year == 2000, Product == "Other bituminous coal") %>% magrittr::extract2("balance_OK"))
})


test_that("calc_tidy_iea_df_balance works correctly for 2019 data", {
  Ebal_2019 <- load_tidy_iea_df(sample_iea_data_path(2019)) %>%
    calc_tidy_iea_df_balances()
  expect_false(all(Ebal$balance_OK))
  expect_true(Ebal_2019 %>% dplyr::filter(Country == "GHA", Year == 1971, Product == "Aviation gasoline") %>% magrittr::extract2("balance_OK"))
  expect_false(Ebal_2019 %>% dplyr::filter(Country == "GHA", Year == 1971, Product == "Electricity") %>% magrittr::extract2("balance_OK"))
  expect_true(Ebal_2019 %>% dplyr::filter(Country == "ZAF", Year == 1971, Product == "Hydro") %>% magrittr::extract2("balance_OK"))
  expect_false(Ebal_2019 %>% dplyr::filter(Country == "ZAF", Year == 2000, Product == "Other bituminous coal") %>% magrittr::extract2("balance_OK"))
})


test_that("fix_tidy_iea_df_balance works correctly for all valid years", {
  for (year in valid_iea_release_years) {
    unbalanced <- load_tidy_iea_df(sample_iea_data_path(year))
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
  }
})


