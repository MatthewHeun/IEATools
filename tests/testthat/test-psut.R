test_that("extract_S_units_from_tidy() works as expected", {
  S_units <- load_tidy_iea_df() %>% 
    extract_S_units_from_tidy()

  for (i in nrow(S_units)) {
    su <- S_units$S_units[[i]]
    expect_true(all(su[ , "TJ"] == 1))
  }
})


test_that("extract_S_units_from_tidy() works with Matrix objects", {
  S_units <- load_tidy_iea_df() %>% 
    extract_S_units_from_tidy(matrix_class = "Matrix")
  
  for (i in nrow(S_units)) {
    su <- S_units$S_units[[i]]
    expect_true(matsbyname::is.Matrix(su))
    expect_true(all(su[ , "TJ"] == 1))
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
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow, "International marine bunkers") & Edot < 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("Y") %>% all())
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow, "International marine bunkers") & Edot > 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("V") %>% all())
  # International aviation bunkers
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow, "International aviation bunkers") & Edot < 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("Y") %>% all())
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow, "International aviation bunkers") & Edot > 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("V") %>% all())
  # Stock changes
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow, "Stock changes") & Edot < 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("Y") %>% all())
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow, "Stock changes") & Edot > 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("V") %>% all())
  # Transformation processes
  expect_true(With_matnames %>% dplyr::filter(startsWith(FlowAggregationPoint, "Transformation processes") & Edot < 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("U_feed") %>% all())
  expect_true(With_matnames %>% dplyr::filter(startsWith(FlowAggregationPoint, "Transformation processes") & Edot > 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("V") %>% all())
  # EIOU
  expect_true(With_matnames %>% dplyr::filter(startsWith(FlowAggregationPoint, "Energy industry own use") & Edot < 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("U_EIOU") %>% all())
  # Consumption
  expect_true(With_matnames %>% dplyr::filter(startsWith(LedgerSide, "Consumption")) %>% magrittr::extract2("matnames") %>% magrittr::equals("Y") %>% all())
  # Transfers
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow, "Transfers") & Edot < 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("U_feed") %>% all())
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow, "Transfers") & Edot > 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("V") %>% all())
  
  With_matnames <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    add_psut_matnames() %>% 
    tibble::add_row(
      Country = c("GHA", "GHA"),
      Method = c("PCM", "PCM"),
      EnergyType = c("E", "E"),
      LastStage = c("Final", "Final"),
      Year = c(1971, 1971),
      LedgerSide = c("Consumption", "Supply"),
      FlowAggregationPoint = c("Industry", "Total primary energy supply"),
      Flow = c("Non-ferrous metals", "Stock changes [of Crude oil]"),
      Product = c("Electricity", "Crude oil"),
      Unit = c("ktoe", "ktoe"),
      Edot = c(100, -100),
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
      EnergyType = c("E", "E"),
      LastStage = c("Final", "Final"),
      Year = c(1971, 1971),
      LedgerSide = c("Consumption", "Supply"),
      FlowAggregationPoint = c("Industry", "Total primary energy supply"),
      Flow = c("Non-ferrous metals", "Stock changes [of Crude oil]"),
      Product = c("Electricity", "Crude oil"),
      Unit = c("ktoe", "ktoe"),
      Edot = c(100, -100),
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
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow, "International marine bunkers") & Edot < 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("Y") %>% all())
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow, "International marine bunkers") & Edot > 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("R") %>% all())
  # International aviation bunkers
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow, "International aviation bunkers") & Edot < 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("Y") %>% all())
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow, "International aviation bunkers") & Edot > 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("R") %>% all())
  # Stock changes
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow, "Stock changes") & Edot < 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("Y") %>% all())
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow, "Stock changes") & Edot > 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("R") %>% all())
  # Transformation processes
  expect_true(With_matnames %>% dplyr::filter(startsWith(FlowAggregationPoint, "Transformation processes") & Edot < 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("U_feed") %>% all())
  expect_true(With_matnames %>% dplyr::filter(startsWith(FlowAggregationPoint, "Transformation processes") & Edot > 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("V") %>% all())
  # EIOU
  expect_true(With_matnames %>% dplyr::filter(startsWith(FlowAggregationPoint, "Energy industry own use") & Edot < 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("U_EIOU") %>% all())
  # Consumption
  expect_true(With_matnames %>% dplyr::filter(startsWith(LedgerSide, "Consumption")) %>% magrittr::extract2("matnames") %>% magrittr::equals("Y") %>% all())
  # Transfers
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow, "Transfers") & Edot < 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("U_feed") %>% all())
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow, "Transfers") & Edot > 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("V") %>% all())
})


test_that("add_row_col_meta() works as expected", {
  With_meta <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    add_psut_matnames() %>% 
    tibble::add_row(
      Country = c("GHA", "GHA"),
      Method = c("PCM", "PCM"),
      EnergyType = c("E", "E"),
      LastStage = c("Final", "Final"),
      Year = c(1971, 1971),
      LedgerSide = c("Consumption", "Supply"),
      FlowAggregationPoint = c("Industry", "Total primary energy supply"),
      Flow = c("Non-ferrous metals", "Stock changes [of Crude oil]"),
      Product = c("Electricity", "Crude oil"),
      Unit = c("ktoe", "ktoe"),
      Edot = c(100, -100),
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
      EnergyType = c("E", "E"),
      LastStage = c("Final", "Final"),
      Year = c(1971, 1971),
      LedgerSide = c("Consumption", "Supply"),
      FlowAggregationPoint = c("Industry", "Total primary energy supply"),
      Flow = c("Non-ferrous metals", "Stock changes [of Crude oil]"),
      Product = c("Electricity", "Crude oil"),
      Unit = c("ktoe", "ktoe"),
      Edot = c(100, -100),
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
      EnergyType = c("E", "E"),
      LastStage = c("Final", "Final"),
      Year = c(1971, 1971),
      LedgerSide = c("Consumption", "Supply"),
      FlowAggregationPoint = c("Industry", "Total primary energy supply"),
      Flow = c("Non-ferrous metals", "Stock changes [of Crude oil]"),
      Product = c("Electricity", "Crude oil"),
      Unit = c("ktoe", "ktoe"),
      Edot = c(100, -100),
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
      EnergyType = c("E", "E"),
      LastStage = c("Final", "Final"),
      Year = c(1971, 1971),
      LedgerSide = c("Consumption", "Supply"),
      FlowAggregationPoint = c("Industry", "Total primary energy supply"),
      Flow = c("Non-ferrous metals", "Stock changes [of Crude oil]"),
      Product = c("Electricity", "Crude oil"),
      Unit = c("ktoe", "ktoe"),
      Edot = c(100, -100),
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


test_that("collapse_to_psut() works with Matrix objects", {
  With_mats <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    add_psut_matnames() %>% 
    tibble::add_row(
      Country = c("GHA", "GHA"),
      Method = c("PCM", "PCM"),
      EnergyType = c("E", "E"),
      LastStage = c("Final", "Final"),
      Year = c(1971, 1971),
      LedgerSide = c("Consumption", "Supply"),
      FlowAggregationPoint = c("Industry", "Total primary energy supply"),
      Flow = c("Non-ferrous metals", "Stock changes [of Crude oil]"),
      Product = c("Electricity", "Crude oil"),
      Unit = c("ktoe", "ktoe"),
      Edot = c(100, -100),
      matnames = psut_cols$B
    ) %>% 
    add_row_col_meta() %>% 
    collapse_to_tidy_psut(matrix_class = "Matrix")
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
  
  for (i in 1:nrow(With_mats)) {
    expect_true(matsbyname::is.Matrix(With_mats$matvals[[i]]))
  }
})


test_that("prep_psut() works as expected", {
  Simple <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    prep_psut() %>% 
    tidyr::pivot_longer(cols = c("R", "U", "U_EIOU", "U_feed", "r_EIOU", "V", "Y", "S_units"),
                        names_to = "matnames", values_to = "matvals") %>% 
    dplyr::rename(matval_simple = matvals)
  expect_equal(names(Simple), 
               c("Country", "Method", "EnergyType", "LastStage", "Year", "matnames", "matval_simple"))
  S_units <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    extract_S_units_from_tidy()
  Complicated <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    add_psut_matnames() %>% 
    add_row_col_meta() %>% 
    collapse_to_tidy_psut() %>% 
    tidyr::spread(key = matnames, value = matvals) %>%
    dplyr::mutate(
      U = matsbyname::sum_byname(U_feed, U_EIOU), 
      r_EIOU = matsbyname::quotient_byname(U_EIOU, U) %>% 
        matsbyname::replaceNaN_byname(0)
    ) %>%
    dplyr::full_join(S_units, by = c("Country", "Method", "EnergyType", "LastStage", "Year")) %>% 
    tidyr::gather(key = matnames, value = matvals, R, U, U_EIOU, U_feed, r_EIOU, V, Y, S_units) %>% 
    dplyr::rename(matval_complicated = matvals)
  # Simple and Complicated ought to be the same.
  dplyr::full_join(Simple, Complicated, by = c("Country", "Method", "EnergyType", "LastStage", "Year", "matnames")) %>% 
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


test_that("prep_psut() works with Matrix objects", {
  Simple <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    prep_psut(matrix_class = "Matrix") %>% 
    tidyr::pivot_longer(cols = c("R", "U", "U_EIOU", "U_feed", "r_EIOU", "V", "Y", "S_units"),
                        names_to = "matnames", values_to = "matvals") %>% 
    dplyr::rename(matval_simple = matvals)
  for (i in 1:nrow(Simple)) {
    expect_true(matsbyname::is.Matrix(Simple$matval_simple[[i]]))
  }
  expect_equal(names(Simple), 
               c("Country", "Method", "EnergyType", "LastStage", "Year", "matnames", "matval_simple"))
  S_units <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    extract_S_units_from_tidy(matrix_class = "Matrix")
  Complicated <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    add_psut_matnames() %>% 
    add_row_col_meta() %>% 
    collapse_to_tidy_psut(matrix_class = "Matrix") %>% 
    tidyr::spread(key = matnames, value = matvals) %>%
    dplyr::mutate(
      U = matsbyname::sum_byname(U_feed, U_EIOU), 
      r_EIOU = matsbyname::quotient_byname(U_EIOU, U) %>% 
        matsbyname::replaceNaN_byname(0)
    ) %>%
    dplyr::full_join(S_units, by = c("Country", "Method", "EnergyType", "LastStage", "Year")) %>% 
    tidyr::gather(key = matnames, value = matvals, R, U, U_EIOU, U_feed, r_EIOU, V, Y, S_units) %>% 
    dplyr::rename(matval_complicated = matvals)
  # Simple and Complicated ought to be the same.
  dplyr::full_join(Simple, Complicated, by = c("Country", "Method", "EnergyType", "LastStage", "Year", "matnames")) %>% 
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
                     "R", "U", "U_EIOU", "U_feed", "V", "Y", "S_units")) {
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


test_that("prep_psut() correctly makes columns of U and r_EIOU matrices with Matrix objects", {
  psut <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    prep_psut(matrix_class = "Matrix")
  
  psut_with_test_cols <- psut %>% 
    dplyr::mutate(
      U_test = matsbyname::sum_byname(U_feed, U_EIOU), 
      r_EIOU_test = matsbyname::quotient_byname(U_EIOU, U),
      r_EIOU_test = matsbyname::replaceNaN_byname(r_EIOU, val = 0)
    )
  # Verify that we made Matrix objects everywhere
  for (j in 6:ncol(psut_with_test_cols)) {
    for (i in 1:nrow(psut_with_test_cols)) {
      expect_true(matsbyname::is.Matrix(psut_with_test_cols[[i, j]][[1]]))
    }
  }
  
  expect_equal(psut_with_test_cols[["U_test"]], psut_with_test_cols[["U"]])
  expect_equal(psut_with_test_cols[["r_EIOU_test"]], psut_with_test_cols[["r_EIOU"]])
})


test_that("replace_null_UV() correctly adds Manufacture industries", {
  # Build R and Y matrices
  R <- matrix(c(10, 0, 
                0, 200), nrow = 2, ncol = 2, byrow = TRUE, 
              dimnames = list(c("Imports [of Fuel oil]", 
                                "Imports [of Aviation gasoline]"), 
                              c("Fuel oil", "Aviation gasoline"))) |> 
    matsbyname::setrowtype("Industry") |> 
    matsbyname::setcoltype("Product")
  Y <- matrix(c(10, 0, 
                0, 200), nrow = 2, ncol = 2, byrow = TRUE, 
              dimnames = list(c( "Fuel oil", "Aviation gasoline"), 
                              c("International marine bunkers", 
                                "International aviation bunkers"))) |> 
    matsbyname::setrowtype("Product") |> 
    matsbyname::setcoltype("Industry")
  
  # Test that an error is raised if energy is not conserved originally.
  replace_null_UV(R = R - 1, Y = Y) |> 
    expect_error("Energy is not balanced originally in replace_null_UV")
  
  # Check that nothing is changed when U and V are not NULL
  U_placeholder <- matrix(0)
  V_placeholder <- matrix(0)
  res_do_nothing <- replace_null_UV(R = R, 
                                    U = U_placeholder, U_feed = U_placeholder, 
                                    U_eiou = U_placeholder, r_eiou = U_placeholder,
                                    V = V_placeholder, Y = Y)
  expect_equal(res_do_nothing, 
               list(R = R, U = U_placeholder, U_feed = U_placeholder, 
                    U_EIOU = U_placeholder, r_EIOU = U_placeholder,
                    V = V_placeholder))
  
  # Generate U and V matrices
  res <- replace_null_UV(R = R, Y = Y)
  
  # Check that results are as expected.
  expectedR <- matrix(c(200, 0, 
                        0, 10), nrow = 2, ncol = 2, byrow = TRUE, 
                      dimnames = list(c("Imports [of Aviation gasoline]", 
                                        "Imports [of Fuel oil]"), 
                                      c("Aviation gasoline [from Imports]", 
                                        "Fuel oil [from Imports]"))) |> 
    matsbyname::setrowtype("Industry") |> 
    matsbyname::setcoltype("Product")
  matsbyname::equal_byname(res$R, expectedR) |> 
    expect_true()
  
  expectedU <- matrix(c(200, 0, 
                        0, 10), nrow = 2, ncol = 2, byrow = TRUE, 
                      dimnames = list(c("Aviation gasoline [from Imports]",
                                        "Fuel oil [from Imports]"), 
                                      c("Manufacture [of Aviation gasoline]", 
                                        "Manufacture [of Fuel oil]"))) |> 
    matsbyname::setrowtype("Product") |> 
    matsbyname::setcoltype("Industry")
  matsbyname::equal_byname(res$U, expectedU) |> 
    expect_true()
  
  expectedUfeed <- expectedU
  matsbyname::equal_byname(res$U_feed, expectedUfeed) |> 
    expect_true()
  
  expectedUeiou <- expectedU * 0
  matsbyname::equal_byname(res$U_EIOU, expectedUeiou) |> 
    expect_true()
  
  expectedreiou <- expectedUeiou
  matsbyname::equal_byname(res$r_EIOU, expectedreiou) |> 
    expect_true()
  
  expectedV <- matrix(c(200, 0,
                        0, 10), nrow = 2, ncol = 2, byrow = TRUE, 
                      dimnames = list(c("Manufacture [of Aviation gasoline]", 
                                        "Manufacture [of Fuel oil]"), 
                                      c("Aviation gasoline", 
                                        "Fuel oil"))) |> 
    matsbyname::setrowtype("Industry") |> 
    matsbyname::setcoltype("Product")
  matsbyname::equal_byname(res$V, expectedV) |> 
    expect_true()
  
  # Check that it works correctly in a data frame
  # and that it works correctly with both
  # matrix objects (row 1) and 
  # Matrix objects (row 2).
  RM <- Matrix::Matrix(R) |> 
    matsbyname::setrowtype("Industry") |> 
    matsbyname::setcoltype("Product")
  YM <- Matrix::Matrix(Y) |> 
    matsbyname::setrowtype("Product") |> 
    matsbyname::setcoltype("Industry")
  res_df <- tibble::tibble(R = list(R, RM), Y = list(Y, YM)) |> 
    replace_null_UV()
  
  for (i in 1:nrow(res_df)) {
    matsbyname::equal_byname(res_df$R[[i]], expectedR) |> 
      expect_true()
    matsbyname::equal_byname(res_df$U[[i]], expectedU) |> 
      expect_true()
    matsbyname::equal_byname(res_df$U_feed[[i]], expectedUfeed) |> 
      expect_true()
    matsbyname::equal_byname(res_df$U_EIOU[[i]], expectedUeiou) |> 
      expect_true()
    matsbyname::equal_byname(res_df$r_EIOU[[i]], expectedreiou) |> 
      expect_true()
    matsbyname::equal_byname(res_df$V[[i]], expectedV) |> 
      expect_true()
  }
  # Check that all of the first row are matrix objects
  res_df |> 
    dplyr::slice(1) |> 
    purrr::transpose() |> 
    purrr::flatten() |> 
    lapply(is.matrix) |> 
    unlist() |> 
    all() |> 
    expect_true()
  # Check that all of the 2nd row are Matrix objects
  res_df |> 
    dplyr::slice(2) |> 
    purrr::transpose() |> 
    purrr::flatten() |> 
    lapply(matsbyname::is.Matrix) |> 
    unlist() |> 
    all() |> 
    expect_true()
})


test_that("replace_null_UV() works when there is only one Y column", {
  # Build R and Y matrices
  R <- matrix(c(10, 0, 
                0, 200), nrow = 2, ncol = 2, byrow = TRUE, 
              dimnames = list(c("Imports [of Fuel oil]", 
                                "Imports [of Hard coal (if no detail)]"), 
                              c("Fuel oil", "Hard coal (if no detail)"))) |> 
    matsbyname::setrowtype("Industry") |> 
    matsbyname::setcoltype("Product")
  Y <- matrix(c(10,
                200), nrow = 2, ncol = 1, byrow = TRUE, 
              dimnames = list(c( "Fuel oil", "Hard coal (if no detail)"), 
                              c("International marine bunkers"))) |> 
    matsbyname::setrowtype("Product") |> 
    matsbyname::setcoltype("Industry")
  
  # Generate U and V matrices
  res <- replace_null_UV(R = R, Y = Y)
  
  # Check that results are as expected.
  expectedR <- matrix(c(200, 0, 
                        0, 10), nrow = 2, ncol = 2, byrow = TRUE, 
                      dimnames = list(c("Imports [of Hard coal (if no detail)]", 
                                        "Imports [of Fuel oil]"), 
                                      c("Hard coal (if no detail) [from Imports]", 
                                        "Fuel oil [from Imports]"))) |> 
    matsbyname::setrowtype("Industry") |> 
    matsbyname::setcoltype("Product")
  matsbyname::equal_byname(res$R, expectedR) |> 
    expect_true()
  
  expectedU <- matrix(c(200, 0, 
                        0, 10), nrow = 2, ncol = 2, byrow = TRUE, 
                      dimnames = list(c("Hard coal (if no detail) [from Imports]",
                                        "Fuel oil [from Imports]"), 
                                      c("Manufacture [of Hard coal (if no detail)]", 
                                        "Manufacture [of Fuel oil]"))) |> 
    matsbyname::setrowtype("Product") |> 
    matsbyname::setcoltype("Industry")
  matsbyname::equal_byname(res$U, expectedU) |> 
    expect_true()
  
  expectedUfeed <- expectedU
  matsbyname::equal_byname(res$U_feed, expectedUfeed) |> 
    expect_true()
  
  expectedUeiou <- expectedU * 0
  matsbyname::equal_byname(res$U_EIOU, expectedUeiou) |> 
    expect_true()
  
  expectedreiou <- expectedUeiou
  matsbyname::equal_byname(res$r_EIOU, expectedreiou) |> 
    expect_true()
  
  expectedV <- matrix(c(200, 0,
                        0, 10), nrow = 2, ncol = 2, byrow = TRUE, 
                      dimnames = list(c("Manufacture [of Hard coal (if no detail)]", 
                                        "Manufacture [of Fuel oil]"), 
                                      c("Hard coal (if no detail)", 
                                        "Fuel oil"))) |> 
    matsbyname::setrowtype("Industry") |> 
    matsbyname::setcoltype("Product")
  matsbyname::equal_byname(res$V, expectedV) |> 
    expect_true()
})


test_that("prep_psut() correctly works with Balancing flows", {
  
  PSUT_flows_with_Balancing <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    add_psut_matnames() %>% 
    tibble::add_row(
      Country = c("GHA", "GHA"),
      Method = c("PCM", "PCM"),
      EnergyType = c("E", "E"),
      LastStage = c("Final", "Final"),
      Year = c(1971, 1971),
      LedgerSide = c("Consumption", "Supply"),
      FlowAggregationPoint = c("Industry", "Total primary energy supply"),
      Flow = c("Non-ferrous metals", "Stock changes [of Crude oil]"),
      Product = c("Electricity", "Crude oil"),
      Unit = c("ktoe", "ktoe"),
      Edot = c(100, -100),
      matnames = c(psut_cols$B, psut_cols$B)
    ) %>% 
    prep_psut()

  expect_true(
    all(c("R", "V", "U_feed", "U_EIOU", "Y", "S_units", "B") %in% colnames(PSUT_flows_with_Balancing))
  )
  
  a <- PSUT_flows_with_Balancing %>%
   dplyr::select(B) %>%
   dplyr::pull() %>%
   dplyr::first()

  balancing_expected_value <- matrix(nrow = 2, ncol = 2,
                                  c(0, 100, -100, 0)) |> 
    matsbyname::setcolnames_byname(c("Non-ferrous metals", "Stock changes [of Crude oil]")) |> 
    matsbyname::setrownames_byname(c("Crude oil", "Electricity")) |> 
    matsbyname::setcoltype("Industry") |> 
    matsbyname::setrowtype("Product")
  
  expect_true(matsbyname::equal_byname(a, balancing_expected_value))
  
  # expect_true(
  #   all(balancing_expected_value == (PSUT_flows_with_Balancing %>%
  #                                    dplyr::select(B) %>% 
  #                                    dplyr::pull() %>% 
  #                                    dplyr::first())
  # ))
})


test_that("prep_psut() correctly works with Balancing flows and Matrix objects", {
  
  PSUT_flows_with_Balancing <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    add_psut_matnames() %>% 
    tibble::add_row(
      Country = c("GHA", "GHA"),
      Method = c("PCM", "PCM"),
      EnergyType = c("E", "E"),
      LastStage = c("Final", "Final"),
      Year = c(1971, 1971),
      LedgerSide = c("Consumption", "Supply"),
      FlowAggregationPoint = c("Industry", "Total primary energy supply"),
      Flow = c("Non-ferrous metals", "Stock changes [of Crude oil]"),
      Product = c("Electricity", "Crude oil"),
      Unit = c("ktoe", "ktoe"),
      Edot = c(100, -100),
      matnames = c(psut_cols$B, psut_cols$B)
    ) %>%
    prep_psut(matrix_class = "Matrix")
  
  expect_true(
    all(c("R", "V", "U_feed", "U_EIOU", "Y", "S_units", "B") %in% colnames(PSUT_flows_with_Balancing))
  )
  
  a <- PSUT_flows_with_Balancing %>%
    dplyr::select(B) %>%
    dplyr::pull() %>%
    dplyr::first()
  
  balancing_expected_value <- matrix(nrow = 2, ncol = 2,
                                     c(0, 100, -100, 0)) |> 
    matsbyname::setcolnames_byname(c("Non-ferrous metals", "Stock changes [of Crude oil]")) |> 
    matsbyname::setrownames_byname(c("Crude oil", "Electricity")) |> 
    matsbyname::setcoltype("Industry") |> 
    matsbyname::setrowtype("Product")
  
  expect_true(matsbyname::equal_byname(a, balancing_expected_value))
  
  # expect_true(
  #   all(balancing_expected_value == (PSUT_flows_with_Balancing %>%
  #                                      dplyr::select(B) %>% 
  #                                      dplyr::pull() %>% 
  #                                      dplyr::first())
  #   ))
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
    expect_true(psut$U_EIOU[[i]] |> matsbyname::iszero_byname())
    expect_true(psut$r_EIOU[[i]] |> matsbyname::iszero_byname())
  }
})


test_that("prep_psut() works when there is no energy industry own use with Matrix objects", {
  # Set up so that the psut data frame has NULL for
  # R, U_feed, and U_EIOU in 1971 for GHA.
  psut <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    # Eliminate energy industry own use, so we do not get a U_EIOU matrix.
    dplyr::filter(.data[[IEATools::iea_cols$flow_aggregation_point]] != IEATools::tfc_compare_flows$energy_industry_own_use) %>% 
    prep_psut(matrix_class = "Matrix")
  # In this case, the U_EIOU matrix should be 0.
  for (i in 1:nrow(psut)) {
    expect_true(psut$U_EIOU[[i]] |> matsbyname::iszero_byname())
    expect_true(psut$r_EIOU[[i]] |> matsbyname::iszero_byname())
  }
})





