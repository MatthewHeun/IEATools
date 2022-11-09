test_that("S_units_from_tidy works as expected", {
  S_units <- load_tidy_iea_df() %>% 
    extract_S_units_from_tidy()

  for (i in nrow(S_units)) {
    su <- S_units$S_units[[i]]
    expect_true(all(su[ , "ktoe"] == 1))
  }
})


test_that("add_psut_matnames() works as expected for R_includes_all_exogenous_flows = FALSE", {
  With_matnames <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    add_psut_matnames(R_includes_all_exogenous_flows = FALSE)
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
      matnames = psut_cols$B
    ) %>% 
    add_psut_matnames()
  
  expect_equal(With_matnames %>% dplyr::filter(matnames == "B") %>% nrow(), 2)
  
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
      matnames = psut_cols$B)
  
  expect_true(all(With_matnames == With_matnames_2))
})


test_that("add_psut_matnames() works as expected for R_includes_all_exogenous_flows = TRUE", {
  With_matnames <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    add_psut_matnames()
  # Ensure that none of the matnames are NA
  expect_false(any(is.na(With_matnames$matnames)))
  # Specific checks
  # Resources
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow, "Resources")) %>% magrittr::extract2("matnames") %>% magrittr::equals("R") %>% all())
  # Imports
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow, "Imports")) %>% magrittr::extract2("matnames") %>% magrittr::equals("R") %>% all())
  # Exports
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow, "Exports")) %>% magrittr::extract2("matnames") %>% magrittr::equals("Y") %>% all())
  # International marine bunkers
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow, "International marine bunkers") & E.dot < 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("Y") %>% all())
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow, "International marine bunkers") & E.dot > 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("R") %>% all())
  # International aviation bunkers
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow, "International aviation bunkers") & E.dot < 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("Y") %>% all())
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow, "International aviation bunkers") & E.dot > 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("R") %>% all())
  # Stock changes
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow, "Stock changes") & E.dot < 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("Y") %>% all())
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow, "Stock changes") & E.dot > 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("R") %>% all())
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
})


test_that("add_row_col_meta() works as expected", {
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
      matnames = psut_cols$B
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
  expect_true(With_meta %>% dplyr::filter(startsWith(matnames, "B")) %>% magrittr::extract2("rowtypes") %>% magrittr::equals("Product") %>% all())
  expect_true(With_meta %>% dplyr::filter(startsWith(matnames, "B")) %>% magrittr::extract2("coltypes") %>% magrittr::equals("Industry") %>% all())
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
  # Rows of B are the Product names
  magrittr::equals(With_meta %>% dplyr::filter(matnames == "B") %>% magrittr::extract2("rownames"), 
                   With_meta %>% dplyr::filter(matnames == "B") %>% magrittr::extract2("Product")) %>% 
    all() %>% 
    expect_true()
  
  # Columns of U and Y are the Flow names
  magrittr::equals(With_meta %>% dplyr::filter(matnames == "U" | matnames == "Y") %>% magrittr::extract2("colnames"), 
                   With_meta %>% dplyr::filter(matnames == "U" | matnames == "Y") %>% magrittr::extract2("Flow")) %>% 
    all() %>% 
    expect_true()
  
  magrittr::equals(With_meta %>% dplyr::filter(matnames == "B") %>% magrittr::extract2("colnames"), 
                   With_meta %>% dplyr::filter(matnames == "B") %>% magrittr::extract2("Flow")) %>% 
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
      matnames = psut_cols$B
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
      matnames = psut_cols$B
    ) %>% 
    tibble::add_column(
      rownames = "a very odd name",
      colnames = "a WEIRD name",
      rowtypes = "a damn weird row type",
      coltypes = "a bloody strange col type"
    ) %>% 
    add_row_col_meta() %>% 
    magrittr::equals(With_meta) %>% 
    all() %>% 
    expect_true()
  
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


test_that("collapse_to_psut() works expected", {
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
      matnames = psut_cols$B
    ) %>% 
    add_row_col_meta() %>% 
    collapse_to_tidy_psut()
  expect_equal(nrow(With_mats), 21)
  # Ensure that all values in the matrices (excluding B) are positive.
  With_mats %>%
    dplyr::filter(matnames != "B") %>% 
    dplyr::mutate(
      gezero = matsbyname::compare_byname(matvals, ">=", 0) %>% matsbyname::all_byname()
    ) %>% 
    magrittr::extract2("gezero") %>% 
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
                     "R", "U_EIOU", "U_feed", "V", "Y", "S_units", "B")) {
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


test_that("replace_null_RUV() works correctly with NULL R and U", {
  # Set up so that the psut data frame has NULL for
  # R, U_feed, and U_EIOU in 1971 for GHA.
  psut <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    prep_psut() %>% 
    tidyr::pivot_longer(cols = c("R", "U_EIOU", "U_feed", "U", "r_EIOU", "V", "Y", "S_units"), names_to = "matnames", values_to = "matvals") %>% 
    dplyr::filter(!(Country == "GHA" & Year == 1971 & matnames == "R")) %>% 
    dplyr::filter(!(Country == "GHA" & Year == 1971 & matnames == "U_feed")) %>% 
    dplyr::filter(!(Country == "GHA" & Year == 1971 & matnames == "U_EIOU")) %>% 
    tidyr::pivot_wider(names_from = "matnames", values_from = "matvals")
  # Check that replace_null_RUV() works as expected.
  res <- psut %>% 
    replace_null_RUV()
  
  expected_R <- psut$Y[[1]] %>% 
    matsbyname::transpose_byname() %>% 
    matsbyname::colsums_byname() %>% 
    matsbyname::hadamardproduct_byname(0) %>% 
    matsbyname::setrownames_byname(IEATools::tpes_flows$resources)
  expected_U <- psut$V[[1]] %>% 
    matsbyname::transpose_byname() %>% 
    matsbyname::hadamardproduct_byname(0)
  # Verify that the NULL R matrix has been replaced with the correct 0 matrix.
  expect_equal(res$R[[1]], expected_R)
  # Verify that U_feed and U_EIOU are no longer NULL and is rather that transposed V matrix full of zeroes.
  expect_equal(res$U_feed[[1]], expected_U)
  expect_equal(res$U_EIOU[[1]], expected_U)
  # We haven't removed the U or r_EIOU matrices. So those should be same as before
  expect_equal(res$U[[1]], psut$U[[1]])
  expect_equal(res$r_EIOU[[1]], psut$r_EIOU[[1]])
  
  # Test that everything works correctly with a list. 
  mats_list <- list(U = NULL, r_EIOU = NULL, V = psut$V[[1]], 
                    Y = psut$Y[[1]], S_units = psut$S_units[[1]], 
                    R = NULL, U_EIOU = NULL, U_feed = NULL)
  res_list <- replace_null_RUV(mats_list)
  expect_equal(res_list$R, expected_R)
  expect_equal(res_list$U_feed, expected_U)
  expect_equal(res_list$U_EIOU, expected_U)
  expect_equal(res_list$U, expected_U)
  expect_equal(res_list$r_EIOU, expected_U)

  # Test that everything works correctly with individual matrices passed in the ... argument
  res_indiv <- replace_null_RUV(U = mats_list$U, r_eiou = mats_list$r_EIOU, V = mats_list$V,
                                Y = mats_list$Y, 
                                R = mats_list$R, U_eiou = mats_list$U_EIOU, U_feed = mats_list$U_feed)
  expect_equal(res_indiv$R, expected_R)
  expect_equal(res_indiv$U_feed, expected_U)
  expect_equal(res_indiv$U_EIOU, expected_U)
  expect_equal(res_indiv$U, expected_U)
  expect_equal(res_indiv$r_EIOU, expected_U)
})


test_that("prep_psut() correctly works with Balancing flows", {
  
  PSUT_flows_with_Balancing <- load_tidy_iea_df() %>% 
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
      matnames = c(psut_cols$B, psut_cols$B)
    ) %>%
    prep_psut()

  expect_true(
    all(c("R", "V", "U_feed", "U_EIOU", "Y", "S_units", "B") %in% colnames(PSUT_flows_with_Balancing))
  )
  
  balancing_expected_value = matrix(nrow = 2, ncol = 2,
                                  c(0, 100, -100, 0))
  
  expect_true(
    all(balancing_expected_value == (PSUT_flows_with_Balancing %>%
                                     dplyr::select(B) %>% 
                                     dplyr::pull() %>% 
                                     dplyr::first())
  ))
})


test_that("prep_psut() works when there is no energy industry own use", {
  # Set up so that the psut data frame has NULL for
  # R, U_feed, and U_EIOU in 1971 for GHA.
  psut <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    # Eliminate energy industry own use, so we do not get a U_EIOU matrix.
    dplyr::filter(.data[[IEATools::iea_cols$flow_aggregation_point]] != IEATools::tfc_compare_flows$energy_industry_own_use) %>% 
    prep_psut()
  # In this case, the U_EIOU matrix should be 0.
  for (i in 1:nrow(psut)) {
    expect_true(psut$U_EIOU[[i]] %>% matsbyname::iszero_byname())
  }
})





