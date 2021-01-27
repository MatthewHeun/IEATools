###########################################################
context("PSUT functions")
###########################################################

test_that("S_units_from_tidy works as expected", {
  S_units <- load_tidy_iea_df() %>% 
    extract_S_units_from_tidy()

  for (i in nrow(S_units)) {
    su <- S_units$S_units[[i]]
    expect_true(all(su[ , "ktoe"] == 1))
  }
})


test_that("add_psut_matnames works as expected", {
  With_matnames <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    add_psut_matnames()
  # Ensure that none of the matnames are NA
  expect_false(any(is.na(With_matnames$matnames)))
  # Specific checks
  # Resources
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow, "Resources")) %>% magrittr::extract2("matnames") %>% magrittr::equals("R") %>% all())
  # Imports
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow, "Imports")) %>% magrittr::extract2("matnames") %>% magrittr::equals("V") %>% all())
  # Exports
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow, "Exports")) %>% magrittr::extract2("matnames") %>% magrittr::equals("Y") %>% all())
  # International marine bunkers
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow, "International marine bunkers") & E.dot < 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("Y") %>% all())
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow, "International marine bunkers") & E.dot > 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("V") %>% all())
  # International aviation bunkers
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow, "International aviation bunkers") & E.dot < 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("Y") %>% all())
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow, "International aviation bunkers") & E.dot > 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("V") %>% all())
  # Stock changes
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow, "Stock changes") & E.dot < 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("Y") %>% all())
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow, "Stock changes") & E.dot > 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("V") %>% all())
  # Transformation processes
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow.aggregation.point, "Transformation processes") & E.dot < 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("U_feed") %>% all())
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow.aggregation.point, "Transformation processes") & E.dot > 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("V") %>% all())
  # EIOU
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow.aggregation.point, "Energy industry own use") & E.dot < 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("U_EIOU") %>% all())
  # Consumption
  expect_true(With_matnames %>% dplyr::filter(startsWith(Ledger.side, "Consumption")) %>% magrittr::extract2("matnames") %>% magrittr::equals("Y") %>% all())
  # Transfers
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow, "Transfers") & E.dot < 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("U_feed") %>% all())
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow, "Transfers") & E.dot > 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("V") %>% all())
  
  With_matnames <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    add_psut_matnames() %>% 
    tibble::add_row(
      Country = c("GHA", "GHA"),
      Method = c("PCM", "PCM"),
      Energy.type = c("E", "E"),
      Last.stage = c("Final", "Final"),
      Year = c(1971, 1971),
      Ledger.side = c("Consumption", "Supply"),
      Flow.aggregation.point = c("Industry", "Total primary energy supply"),
      Flow = c("Non-ferrous metals", "Stock changes [of Crude oil]"),
      Product = c("Electricity", "Crude oil"),
      Unit = c("ktoe", "ktoe"),
      E.dot = c(100, -100),
      matnames = psut_cols$Epsilon
    ) %>% 
    add_psut_matnames()
  
  expect_equal(With_matnames %>% dplyr::filter(matnames == "Epsilon") %>% nrow(), 2)
  
  With_matnames_2 <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    add_psut_matnames() %>% 
    tibble::add_row(
      Country = c("GHA", "GHA"),
      Method = c("PCM", "PCM"),
      Energy.type = c("E", "E"),
      Last.stage = c("Final", "Final"),
      Year = c(1971, 1971),
      Ledger.side = c("Consumption", "Supply"),
      Flow.aggregation.point = c("Industry", "Total primary energy supply"),
      Flow = c("Non-ferrous metals", "Stock changes [of Crude oil]"),
      Product = c("Electricity", "Crude oil"),
      Unit = c("ktoe", "ktoe"),
      E.dot = c(100, -100),
      matnames = psut_cols$Epsilon)
  
  expect_true(all(With_matnames == With_matnames_2))
})


test_that("add_row_col_meta works as expected", {
  With_meta <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    add_psut_matnames() %>% 
    tibble::add_row(
      Country = c("GHA", "GHA"),
      Method = c("PCM", "PCM"),
      Energy.type = c("E", "E"),
      Last.stage = c("Final", "Final"),
      Year = c(1971, 1971),
      Ledger.side = c("Consumption", "Supply"),
      Flow.aggregation.point = c("Industry", "Total primary energy supply"),
      Flow = c("Non-ferrous metals", "Stock changes [of Crude oil]"),
      Product = c("Electricity", "Crude oil"),
      Unit = c("ktoe", "ktoe"),
      E.dot = c(100, -100),
      matnames = psut_cols$Epsilon
    ) %>% 
    add_row_col_meta()
  # Ensure that every row is filled in the new columns.
  expect_false(any(is.na(With_meta$rownames)))
  expect_false(any(is.na(With_meta$colnames)))
  expect_false(any(is.na(With_meta$rowtypes)))
  expect_false(any(is.na(With_meta$coltypes)))
  # Ensure that row and column types are correct
  expect_true(With_meta %>% dplyr::filter(matnames == "R" | matnames == "V") %>% magrittr::extract2("rowtypes") %>% magrittr::equals("Industry") %>% all())
  expect_true(With_meta %>% dplyr::filter(matnames == "R" | matnames == "V") %>% magrittr::extract2("coltypes") %>% magrittr::equals("Product") %>% all())
  expect_true(With_meta %>% dplyr::filter(startsWith(matnames, "U") | matnames == "Y") %>% magrittr::extract2("rowtypes") %>% magrittr::equals("Product") %>% all())
  expect_true(With_meta %>% dplyr::filter(startsWith(matnames, "U") | matnames == "Y") %>% magrittr::extract2("coltypes") %>% magrittr::equals("Industry") %>% all())
  expect_true(With_meta %>% dplyr::filter(startsWith(matnames, "Epsilon")) %>% magrittr::extract2("rowtypes") %>% magrittr::equals("Product") %>% all())
  expect_true(With_meta %>% dplyr::filter(startsWith(matnames, "Epsilon")) %>% magrittr::extract2("coltypes") %>% magrittr::equals("Industry") %>% all())
  # Ensure that row and column identifiers are correct
  # Rows of R and V are the Flow names
  magrittr::equals(With_meta %>% dplyr::filter(matnames == "R" | matnames == "V") %>% magrittr::extract2("rownames"), 
                   With_meta %>% dplyr::filter(matnames == "R" | matnames == "V") %>% magrittr::extract2("Flow")) %>% 
    all() %>% 
    expect_true()
  # Columns of R and V are the Product names
  magrittr::equals(With_meta %>% dplyr::filter(matnames == "R" | matnames == "V") %>% magrittr::extract2("colnames"), 
                   With_meta %>% dplyr::filter(matnames == "R" | matnames == "V") %>% magrittr::extract2("Product")) %>% 
    all() %>% 
    expect_true()
  # Rows of U and Y are the Product names
  magrittr::equals(With_meta %>% dplyr::filter(matnames == "U" | matnames == "Y") %>% magrittr::extract2("rownames"), 
                   With_meta %>% dplyr::filter(matnames == "U" | matnames == "Y") %>% magrittr::extract2("Product")) %>% 
    all() %>% 
    expect_true()
  # Rows of Epsilon are the Product names
  magrittr::equals(With_meta %>% dplyr::filter(matnames == "Epsilon") %>% magrittr::extract2("rownames"), 
                   With_meta %>% dplyr::filter(matnames == "Epsilon") %>% magrittr::extract2("Product")) %>% 
    all() %>% 
    expect_true()
  
  # Columns of U and Y are the Flow names
  magrittr::equals(With_meta %>% dplyr::filter(matnames == "U" | matnames == "Y") %>% magrittr::extract2("colnames"), 
                   With_meta %>% dplyr::filter(matnames == "U" | matnames == "Y") %>% magrittr::extract2("Flow")) %>% 
    all() %>% 
    expect_true()
  
  magrittr::equals(With_meta %>% dplyr::filter(matnames == "Epsilon") %>% magrittr::extract2("colnames"), 
                   With_meta %>% dplyr::filter(matnames == "Epsilon") %>% magrittr::extract2("Flow")) %>% 
    all() %>% 
    expect_true()
  
  # If columns are already there, add_row_col_meta() shouldn't do anything.
  With_meta <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    add_psut_matnames() %>% 
    tibble::add_row(
      Country = c("GHA", "GHA"),
      Method = c("PCM", "PCM"),
      Energy.type = c("E", "E"),
      Last.stage = c("Final", "Final"),
      Year = c(1971, 1971),
      Ledger.side = c("Consumption", "Supply"),
      Flow.aggregation.point = c("Industry", "Total primary energy supply"),
      Flow = c("Non-ferrous metals", "Stock changes [of Crude oil]"),
      Product = c("Electricity", "Crude oil"),
      Unit = c("ktoe", "ktoe"),
      E.dot = c(100, -100),
      matnames = psut_cols$Epsilon
    ) %>% 
    tibble::add_column(
      rownames = "a very odd name",
      colnames = "a WEIRD name",
      rowtypes = "a damn weird row type",
      coltypes = "a bloody strange col type"
    )
  
  With_meta_2 <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    add_psut_matnames() %>% 
    tibble::add_row(
      Country = c("GHA", "GHA"),
      Method = c("PCM", "PCM"),
      Energy.type = c("E", "E"),
      Last.stage = c("Final", "Final"),
      Year = c(1971, 1971),
      Ledger.side = c("Consumption", "Supply"),
      Flow.aggregation.point = c("Industry", "Total primary energy supply"),
      Flow = c("Non-ferrous metals", "Stock changes [of Crude oil]"),
      Product = c("Electricity", "Crude oil"),
      Unit = c("ktoe", "ktoe"),
      E.dot = c(100, -100),
      matnames = psut_cols$Epsilon
    ) %>% 
    tibble::add_column(
      rownames = "a very odd name",
      colnames = "a WEIRD name",
      rowtypes = "a damn weird row type",
      coltypes = "a bloody strange col type"
    ) %>% 
    add_row_col_meta()
  
  expect_true(all(With_meta == With_meta_2))
  
  
  # Now, expect error if only one of the 4 expected columns is present.
  With_meta <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    add_psut_matnames() %>% 
    dplyr::mutate(
      "{IEATools::mat_meta_cols$rownames}" := "a very odd rowname"
    ) %>% 
    add_row_col_meta() %>% 
    expect_error(regexp = "'rownames' is")
  
  
  # Verify that any added column triggers the error. 
  # This time do "colnames"
  load_tidy_iea_df() %>% 
    specify_all() %>% 
    add_psut_matnames() %>% 
    dplyr::mutate(
      "{IEATools::mat_meta_cols$colnames}" := "a bad colname"
    ) %>% 
    add_row_col_meta() %>% 
    testthat::expect_error(regexp = "'colnames' is ")

  # rowtypes  
  load_tidy_iea_df() %>% 
    specify_all() %>% 
    add_psut_matnames() %>% 
    dplyr::mutate(
      "{IEATools::mat_meta_cols$rowtypes}" := "a bad rowtype"
    ) %>% 
    add_row_col_meta() %>% 
    testthat::expect_error(regexp = "'rowtypes' is ")

  # coltypes  
  load_tidy_iea_df() %>% 
    specify_all() %>% 
    add_psut_matnames() %>% 
    dplyr::mutate(
      "{IEATools::mat_meta_cols$coltypes}" := "a bad coltype"
    ) %>% 
    add_row_col_meta() %>% 
    testthat::expect_error(regexp = "'coltypes' is ")

})


test_that("collapse_to_psut works expected", {
  With_mats <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    add_psut_matnames() %>% 
    tibble::add_row(
      Country = c("GHA", "GHA"),
      Method = c("PCM", "PCM"),
      Energy.type = c("E", "E"),
      Last.stage = c("Final", "Final"),
      Year = c(1971, 1971),
      Ledger.side = c("Consumption", "Supply"),
      Flow.aggregation.point = c("Industry", "Total primary energy supply"),
      Flow = c("Non-ferrous metals", "Stock changes [of Crude oil]"),
      Product = c("Electricity", "Crude oil"),
      Unit = c("ktoe", "ktoe"),
      E.dot = c(100, -100),
      matnames = psut_cols$Epsilon
    ) %>% 
    add_row_col_meta() %>% 
    collapse_to_tidy_psut()
  expect_equal(nrow(With_mats), 21)
  # Ensure that all values in the matrices (excluding Epsilon) are positive.
  With_mats %>%
    dplyr::filter(matnames != "Epsilon") %>% 
    dplyr::mutate(
      gezero = matsbyname::compare_byname(matvals, ">=", 0) %>% matsbyname::all_byname()
    ) %>% 
    extract2("gezero") %>% 
    as.logical() %>% 
    all() %>% 
    expect_true()
})


test_that("prep_psut() works as expected", {
  Simple <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    prep_psut() %>% 
    tidyr::pivot_longer(cols = c("R", "U_EIOU", "U_feed", "V", "Y", "S_units"),
                        names_to = "matnames", values_to = "matvals") %>% 
    dplyr::rename(matval_simple = matvals)
  S_units <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    extract_S_units_from_tidy()
  Complicated <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    add_psut_matnames() %>% 
    add_row_col_meta() %>% 
    collapse_to_tidy_psut() %>% 
    tidyr::spread(key = matnames, value = matvals) %>% 
    dplyr::full_join(S_units, by = c("Method", "Energy.type", "Last.stage", "Country", "Year")) %>% 
    tidyr::gather(key = matnames, value = matvals, R, U_EIOU, U_feed, V, Y, S_units) %>% 
    dplyr::rename(matval_complicated = matvals)
  # Simple and Complicated ought to be the same.
  dplyr::full_join(Simple, Complicated, by = c("Method", "Energy.type", "Last.stage", "Country", "Year", "matnames")) %>% 
    dplyr::mutate(
      same = matsbyname::equal_byname(matval_simple, matval_complicated)
    ) %>% 
    magrittr::extract2("same") %>% 
    as.logical() %>% 
    all() %>% 
    expect_true()
  # Verify that S_units has the correct row and column types. 
  # On 21 July 2020, it did not!
  S_units[[IEATools::psut_cols$s_units]][[1]] %>% 
    matsbyname::rowtype() %>% 
    expect_equal(IEATools::row_col_types$product)
  S_units[[IEATools::psut_cols$s_units]][[1]] %>% 
    matsbyname::coltype() %>% 
    expect_equal(IEATools::row_col_types$unit)
})


test_that("prep_psut() works as expected with empty .tidy_iea_df", {
  iea_data <- load_tidy_iea_df() %>% 
    specify_all()
  empty_iea_data <- iea_data[0, ]
  
  zero_psut <- prep_psut(empty_iea_data)
  cn <- colnames(zero_psut)
  for (col_name in c(IEATools::iea_cols$country, 
                     IEATools::iea_cols$method,
                     IEATools::iea_cols$energy_type,
                     IEATools::iea_cols$last_stage,
                     IEATools::iea_cols$year,
                     "R", "U_EIOU", "U_feed", "V", "Y", "S_units", "Epsilon")) {
    expect_true(col_name %in% cn)
  }
  expect_equal(nrow(zero_psut), 0)
})


test_that("prep_psut() correctly makes columns of U and r_EIOU matrices", {
  psut <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    prep_psut()
  
  psut_with_test_cols <- psut %>% 
    dplyr::mutate(
      U_test = matsbyname::sum_byname(U_feed, U_EIOU), 
      r_EIOU_test = matsbyname::quotient_byname(U_EIOU, U),
      r_EIOU_test = matsbyname::replaceNaN_byname(r_EIOU, val = 0)
    )
  expect_equal(psut_with_test_cols[["U_test"]], psut_with_test_cols[["U"]])
  expect_equal(psut_with_test_cols[["r_EIOU_test"]], psut_with_test_cols[["r_EIOU"]])
})


test_that("prep_psut() correctly works with Epsilon flows", {
  
  PSUT_flows_with_Epsilon <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    add_psut_matnames() %>% 
    tibble::add_row(
      Country = c("GHA", "GHA"),
      Method = c("PCM", "PCM"),
      Energy.type = c("E", "E"),
      Last.stage = c("Final", "Final"),
      Year = c(1971, 1971),
      Ledger.side = c("Consumption", "Supply"),
      Flow.aggregation.point = c("Industry", "Total primary energy supply"),
      Flow = c("Non-ferrous metals", "Stock changes [of Crude oil]"),
      Product = c("Electricity", "Crude oil"),
      Unit = c("ktoe", "ktoe"),
      E.dot = c(100, -100),
      matnames = c(psut_cols$Epsilon, psut_cols$Epsilon)
    ) %>%
    prep_psut()

  expect_true(
    all(c("R", "V", "U_feed", "U_EIOU", "Y", "S_units", "Epsilon") %in% colnames(PSUT_flows_with_Epsilon))
  )
  
  epsilon_expected_value = matrix(nrow = 2, ncol = 2,
                                  c(0, 100, -100, 0))
  
  expect_true(
    all(epsilon_expected_value == (PSUT_flows_with_Epsilon %>%
                                     dplyr::select(Epsilon) %>% 
                                     dplyr::pull() %>% 
                                     dplyr::first())
  ))
})





