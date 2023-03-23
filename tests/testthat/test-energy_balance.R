
# test_that("calc_tidy_iea_df_balance() works correctly for 2018 data", {
#   Ebal_2018 <- load_tidy_iea_df(sample_iea_data_path(2018)) %>%
#     calc_tidy_iea_df_balances()
#   expect_false(all(Ebal_2018$balance_OK))
#   expect_true(Ebal_2018 %>% dplyr::filter(Country == "GHA", Year == 1971, Product == "Aviation gasoline") %>% magrittr::extract2("balance_OK"))
#   expect_false(Ebal_2018 %>% dplyr::filter(Country == "GHA", Year == 1971, Product == "Electricity") %>% magrittr::extract2("balance_OK"))
#   expect_true(Ebal_2018 %>% dplyr::filter(Country == "ZAF", Year == 1971, Product == "Hydro") %>% magrittr::extract2("balance_OK"))
#   expect_false(Ebal_2018 %>% dplyr::filter(Country == "ZAF", Year == 2000, Product == "Other bituminous coal") %>% magrittr::extract2("balance_OK"))
# })
# 
# 
# test_that("calc_tidy_iea_df_balance() works correctly for 2019 data", {
#   Ebal_2019 <- load_tidy_iea_df(sample_iea_data_path(2019)) %>%
#     calc_tidy_iea_df_balances()
#   expect_false(all(Ebal_2019$balance_OK))
#   expect_true(Ebal_2019 %>% dplyr::filter(Country == "GHA", Year == 1971, Product == "Aviation gasoline") %>% magrittr::extract2("balance_OK"))
#   expect_false(Ebal_2019 %>% dplyr::filter(Country == "GHA", Year == 1971, Product == "Electricity") %>% magrittr::extract2("balance_OK"))
#   # This one was OK in the 2018 data, but is off by 1e-4 in the 2019 data.
#   expect_false(Ebal_2019 %>% dplyr::filter(Country == "ZAF", Year == 1971, Product == "Hydro") %>% magrittr::extract2("balance_OK"))
#   expect_false(Ebal_2019 %>% dplyr::filter(Country == "ZAF", Year == 2000, Product == "Other bituminous coal") %>% magrittr::extract2("balance_OK"))
# })
# 
# 
# test_that("calc_tidy_iea_df_balance() works correctly for 2021 data", {
#   Ebal_2021 <- load_tidy_iea_df(sample_iea_data_path(2021)) %>%
#     calc_tidy_iea_df_balances(tol = 4e-4)
#   expect_true(all(Ebal_2021$balance_OK))
# })


test_that("calc_tidy_iea_df_balance() works correctly for 2022 data", {
  Ebal_2022 <- load_tidy_iea_df(sample_iea_data_path(2022)) %>%
    calc_tidy_iea_df_balances(tol = 0.1)
  expect_true(all(Ebal_2022$balance_OK))
})


test_that("fix_tidy_iea_df_balance() works correctly for no-row data frame", {
  Ebal_2022 <- load_tidy_iea_df(sample_iea_data_path(2022))
  Ebal_2022 <- Ebal_2022[0, ]
  res <- fix_tidy_iea_df_balances(Ebal_2022)
  expect_equal(nrow(res), 0)
  expect_equal(colnames(res), colnames(Ebal_2022))
})


test_that("fix_tidy_iea_df_balance() works correctly for all valid years", {
  for (rel_year in valid_iea_release_years) {
    unbalanced <- load_tidy_iea_df(sample_iea_data_path(rel_year))
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


test_that("fix_tidy_iea_df_balances() gives a helpful error message", {
  for (rel_year in valid_iea_release_years) {
    unbalanced <- load_tidy_iea_df(sample_iea_data_path(rel_year))
    # Mess up the data
    unbalanced[[IEATools::iea_cols$e_dot]][[1]] <- -9999
    # Try to fix the energy balance. Should receive an error.
    expect_error(unbalanced %>% 
      fix_tidy_iea_df_balances(max_fix = 10), 
      "GHA, 1971, Primary solid biofuels")
  }
})
