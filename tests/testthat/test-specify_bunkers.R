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

test_that("interface industries are correctly specified after specifying bunkers", {
  
  int_inds_wout_int_bunkers <- setdiff(interface_industries, c(IEATools::interface_industries$international_aviation_bunkers, 
                                                               IEATools::interface_industries$international_marine_bunkers))
  
  specified <- load_tidy_iea_df() %>% 
    specify_bunkers() %>%
    specify_interface_industries()
  # We should have no more Imports, Exports, International aviation bunkers, International marine bunkers, or Stock changes.
  # Rather, everything should be specified as X (Product).
  for (i in int_inds_wout_int_bunkers) {
    # Ensure that there are no interface_industries remaining
    expect_equal(nrow(specified %>% dplyr::filter(Flow == i)), 0)
    # Ensure that every interface_industry ends with "]", indicating that it has been specified.
    expect_true(specified %>% dplyr::filter(startsWith(Flow, i) & endsWith(Flow, of_notation[["suff_end"]])) %>% nrow() > 0)
  }
})


test_that("World_X_bunkers are specfied correctly", {
  # Make a IEA-style fake data frame that contains World marine and aviation bunkers
  empty <- sample_iea_data_path() %>% 
    iea_df() %>%
    rename_iea_df_cols() %>%
    # Empty the data frame
    dplyr::slice(0) %>%
    dplyr::select(-`2000`)
  # Now add some rows that contain WMB and WAB countries and flows.
  to_add <- tibble::tribble(
    ~Country, ~Flow, ~Product, ~`1971`, 
    IEATools::transport_flows$world_marine_bunkers, 
    IEATools::transport_flows$world_marine_bunkers, 
    "Matt's cool product", 
    42, 
    #
    IEATools::transport_flows$world_aviation_bunkers, 
    IEATools::transport_flows$world_aviation_bunkers, 
    "Zeke's cool product", 
    43)
  raw <- empty %>%
    dplyr::bind_rows(to_add)
  specified <- raw %>%
    use_iso_countries() %>%
    specify_bunkers()
  
  expect_equal(specified[[IEATools::iea_cols$country]][[1]], "WMB")
  expect_equal(specified[[IEATools::iea_cols$country]][[2]], "WAB")
  expect_equal(specified[[IEATools::iea_cols$flow]][[1]], "International navigation")
  expect_equal(specified[[IEATools::iea_cols$flow]][[2]], "International aviation")
})

