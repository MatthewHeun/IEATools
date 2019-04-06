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
  expect_false(any(is.na(With_matnames$matname)))
  # Specific checks
  # Resources
  expect_true(With_matnames %>% filter(startsWith(Flow, "Resources")) %>% extract2("matname") %>% equals("R") %>% all())
  # Imports
  expect_true(With_matnames %>% filter(startsWith(Flow, "Imports")) %>% extract2("matname") %>% equals("V") %>% all())
  # Exports
  expect_true(With_matnames %>% filter(startsWith(Flow, "Exports")) %>% extract2("matname") %>% equals("Y") %>% all())
  # International marine bunkers
  expect_true(With_matnames %>% filter(startsWith(Flow, "International marine bunkers") & E.dot < 0) %>% extract2("matname") %>% equals("Y") %>% all())
  expect_true(With_matnames %>% filter(startsWith(Flow, "International marine bunkers") & E.dot > 0) %>% extract2("matname") %>% equals("V") %>% all())
  # International aviation bunkers
  expect_true(With_matnames %>% filter(startsWith(Flow, "International aviation bunkers") & E.dot < 0) %>% extract2("matname") %>% equals("Y") %>% all())
  expect_true(With_matnames %>% filter(startsWith(Flow, "International aviation bunkers") & E.dot > 0) %>% extract2("matname") %>% equals("V") %>% all())
  # Stock changes
  expect_true(With_matnames %>% filter(startsWith(Flow, "Stock changes") & E.dot < 0) %>% extract2("matname") %>% equals("Y") %>% all())
  expect_true(With_matnames %>% filter(startsWith(Flow, "Stock changes") & E.dot > 0) %>% extract2("matname") %>% equals("V") %>% all())
  # Transformation processes
  expect_true(With_matnames %>% filter(startsWith(Flow.aggregation.point, "Transformation processes") & E.dot < 0) %>% extract2("matname") %>% equals("U_excl_EIOU") %>% all())
  expect_true(With_matnames %>% filter(startsWith(Flow.aggregation.point, "Transformation processes") & E.dot > 0) %>% extract2("matname") %>% equals("V") %>% all())
  # EIOU
  expect_true(With_matnames %>% filter(startsWith(Flow.aggregation.point, "Energy industry own use") & E.dot < 0) %>% extract2("matname") %>% equals("U_EIOU") %>% all())
  # Consumption
  expect_true(With_matnames %>% filter(startsWith(Ledger.side, "Consumption")) %>% extract2("matname") %>% equals("Y") %>% all())
  # Transfers
  expect_true(With_matnames %>% filter(startsWith(Flow, "Transfers") & E.dot < 0) %>% extract2("matname") %>% equals("U_excl_EIOU") %>% all())
  expect_true(With_matnames %>% filter(startsWith(Flow, "Transfers") & E.dot > 0) %>% extract2("matname") %>% equals("V") %>% all())
})

test_that("add_row_col_meta works as expected", {
  With_meta <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    add_psut_matnames() %>% 
    add_row_col_meta()
  # Ensure that every row is filled in the new columns.
  expect_false(any(is.na(With_meta$rowname)))
  expect_false(any(is.na(With_meta$colname)))
  expect_false(any(is.na(With_meta$rowtype)))
  expect_false(any(is.na(With_meta$coltype)))
  # Ensure that row and column types are correct
  expect_true(With_meta %>% filter(matname == "R" | matname == "V") %>% extract2("rowtype") %>% equals("Industry") %>% all())
  expect_true(With_meta %>% filter(matname == "R" | matname == "V") %>% extract2("coltype") %>% equals("Product") %>% all())
  expect_true(With_meta %>% filter(startsWith(matname, "U") | matname == "Y") %>% extract2("rowtype") %>% equals("Product") %>% all())
  expect_true(With_meta %>% filter(startsWith(matname, "U") | matname == "Y") %>% extract2("coltype") %>% equals("Industry") %>% all())
  # Ensure that row and column identifiers are correct
  # Rows of R and V are the Flow names
  magrittr::equals(With_meta %>% filter(matname == "R" | matname == "V") %>% magrittr::extract2("rowname"), 
                   With_meta %>% filter(matname == "R" | matname == "V") %>% magrittr::extract2("Flow")) %>% 
    all() %>% 
    expect_true()
  # Columns of R and V are the Product names
  magrittr::equals(With_meta %>% filter(matname == "R" | matname == "V") %>% magrittr::extract2("colname"), 
                   With_meta %>% filter(matname == "R" | matname == "V") %>% magrittr::extract2("Product")) %>% 
    all() %>% 
    expect_true()
  # Rows of U and Y are the Product names
  magrittr::equals(With_meta %>% filter(matname == "U" | matname == "Y") %>% magrittr::extract2("rowname"), 
                   With_meta %>% filter(matname == "U" | matname == "Y") %>% magrittr::extract2("Product")) %>% 
    all() %>% 
    expect_true()
  # Columns of U and Y are the Flow names
  magrittr::equals(With_meta %>% filter(matname == "U" | matname == "Y") %>% magrittr::extract2("colname"), 
                   With_meta %>% filter(matname == "U" | matname == "Y") %>% magrittr::extract2("Flow")) %>% 
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
})

test_that("prep_psut works as expected", {
  # S_units <- load_tidy_iea_df() %>% 
  #   extract_S_units_from_tidy()
  # Simple <- load_tidy_iea_df() %>% 
  #   specify_all() %>% 
  #   prep_psut()
  # Complicated <- load_tidy_iea_df() %>% 
  #   specify_all() %>% 
  #   add_psut_matnames() %>% 
  #   add_row_col_meta() %>% 
  #   collapse_to_tidy_psut()
  # all(Simple == Complicated)
  # 
  # 
  
  
  Simple <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    prep_psut() %>% 
    rename(matval_simple = matval)
  S_units <- load_tidy_iea_df() %>% 
    extract_S_units_from_tidy()
  Complicated <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    add_psut_matnames() %>% 
    add_row_col_meta() %>% 
    collapse_to_tidy_psut() %>% 
    tidyr::spread(key = matname, value = matval) %>% 
    dplyr::full_join(S_units, by = c("Method", "Energy.type", "Last.stage", "Country", "Year")) %>% 
    tidyr::gather(key = matname, value = matval, R, U_EIOU, U_excl_EIOU, V, Y, S_units) %>% 
    dplyr::rename(matval_complicated = matval)
  # Simple and Complicated ought to be the same.
  dplyr::full_join(Simple, Complicated, by = c("Method", "Energy.type", "Last.stage", "Country", "Year", "matname")) %>% 
    dplyr::mutate(
      same = matsbyname::equal_byname(matval_simple, matval_complicated)
    ) %>% 
    magrittr::extract2("same") %>% 
    as.logical() %>% 
    all() %>% 
    expect_true()

})
