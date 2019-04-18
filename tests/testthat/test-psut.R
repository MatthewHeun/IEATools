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
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow.aggregation.point, "Transformation processes") & E.dot < 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("U_excl_EIOU") %>% all())
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow.aggregation.point, "Transformation processes") & E.dot > 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("V") %>% all())
  # EIOU
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow.aggregation.point, "Energy industry own use") & E.dot < 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("U_EIOU") %>% all())
  # Consumption
  expect_true(With_matnames %>% dplyr::filter(startsWith(Ledger.side, "Consumption")) %>% magrittr::extract2("matnames") %>% magrittr::equals("Y") %>% all())
  # Transfers
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow, "Transfers") & E.dot < 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("U_excl_EIOU") %>% all())
  expect_true(With_matnames %>% dplyr::filter(startsWith(Flow, "Transfers") & E.dot > 0) %>% magrittr::extract2("matnames") %>% magrittr::equals("V") %>% all())
})

test_that("add_row_col_meta works as expected", {
  With_meta <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    add_psut_matnames() %>% 
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
  # Columns of U and Y are the Flow names
  magrittr::equals(With_meta %>% dplyr::filter(matnames == "U" | matnames == "Y") %>% magrittr::extract2("colnames"), 
                   With_meta %>% dplyr::filter(matnames == "U" | matnames == "Y") %>% magrittr::extract2("Flow")) %>% 
    all() %>% 
    expect_true()
})

test_that("collapse_to_psut works expected", {
  With_mats <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    add_psut_matnames() %>% 
    add_row_col_meta() %>% 
    collapse_to_tidy_psut()
  expect_equal(nrow(With_mats), 20)
  # Ensure that all values in the matrices are positive.
  With_mats %>%
    dplyr::mutate(
      gezero = matsbyname::compare_byname(matvals, ">=", 0) %>% matsbyname::all_byname()
    ) %>% 
    extract2("gezero") %>% 
    as.logical() %>% 
    all() %>% 
    expect_true()
})

test_that("prep_psut works as expected", {
  Simple <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    prep_psut() %>% 
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
    tidyr::gather(key = matnames, value = matvals, R, U_EIOU, U_excl_EIOU, V, Y, S_units) %>% 
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
})
