test_that("bunker specification works", {
  tidy_iea_df <- load_tidy_iea_df() %>% 
    specify_primary_production() %>% 
    specify_tp_eiou()
  # Verify that we have international_X_bunkers flows.
  imb_rows <- tidy_iea_df %>% 
    dplyr::filter(.data[[IEATools::iea_cols$flow]] == IEATools::tpes_flows$international_marine_bunkers) %>% 
    nrow()
  expect_true(imb_rows == 7)
  iab_rows <- tidy_iea_df %>% 
    dplyr::filter(.data[[IEATools::iea_cols$flow]] == IEATools::tpes_flows$international_aviation_bunkers) %>% 
    nrow()
  expect_true(iab_rows == 8)
  # Rename those flows
  specified <- tidy_iea_df %>% 
    specify_bunkers()
  # Check that we got rid of International_X_bunker flows.
  imb_rows_specified <- specified %>% 
    dplyr::filter(.data[[IEATools::iea_cols$flow]] == IEATools::tpes_flows$international_marine_bunkers) %>% 
    nrow()
  expect_true(imb_rows_specified == 0)
  iab_rows_specified <- specified %>% 
    dplyr::filter(.data[[IEATools::iea_cols$flow]] == IEATools::tpes_flows$international_aviation_bunkers) %>% 
    nrow()
  expect_true(iab_rows_specified == 0)

  # Ensure that the International_X_bunkers flows have been renamed correctly.
  etwmb_rows_specified <- specified %>% 
    dplyr::filter(.data[[IEATools::iea_cols$flow]] == IEATools::tpes_flows$exports_to_world_marine_bunkers) %>% 
    nrow()
  expect_true(etwmb_rows_specified == 7)
  etwab_rows_specified <- specified %>% 
    dplyr::filter(.data[[IEATools::iea_cols$flow]] == IEATools::tpes_flows$exports_to_world_aviation_bunkers) %>% 
    nrow()
  expect_true(etwab_rows_specified == 8)
  
  
  
  
})
