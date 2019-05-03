###########################################################
context("Testing utilities")
###########################################################

test_that("starts_with_any_of works properly", {
  expect_true(starts_with_any_of(x = "prefix - suffix", target = c("a", "b", "prefix")))
  expect_false(starts_with_any_of(x = "prefix - suffix", target = c("a", "b", "c")))
  expect_false(starts_with_any_of(x = "prefix - suffix", target = "suffix"))
  expect_equal(starts_with_any_of(x = c("Production - Crude", "Production - NG", "Exports - Oil", "Exports - Crude"),
                                  target = c("Production", "Imports")),
               c(TRUE, TRUE, FALSE, FALSE))
  # Does it also work with lists?
  expect_equal(starts_with_any_of(x = list("Production - Crude", "Production - NG", "Exports - Oil", "Exports - Crude"),
                                  target = c("Production", "Imports")),
               c(TRUE, TRUE, FALSE, FALSE))
})

test_that("year_cols works as expected", {
  DF <- data.frame(a = c(1, 2), `1967` = c(3, 4), `-10` = c(5, 6), check.names = FALSE)
  expect_equal(DF %>% year_cols(), c(2, 3))
  expect_equal(DF %>% year_cols(return_names = TRUE), c("1967", "-10"))
})

test_that("insert_after works as expected", {
  l <- list("a", "b", "c", "d", "c")
  expect_equal(insert_after(l, after = "c", values = "1"), list("a", "b", "c", "1", "d", "c", "1"))
  expect_equal(l %>% insert_after(after = "c", values = "1"), list("a", "b", "c", "1", "d", "c", "1"))
  # Try with some special cases
  expect_equal(l %>% insert_after(after = "c", values = "1", .after_all = FALSE), list("a", "b", "c", "1", "d", "c"))
  expect_equal(l %>% insert_after(after = "c", values = "1", .equals_function = magrittr::equals), list("a", "b", "c", "1", "d", "c", "1"))
  expect_equal(l %>% insert_after(after = NULL, "1"), list("a", "b", "c", "d", "c", "1"))
  expect_equal(l %>% insert_after(values = "1"), list("a", "b", "c", "d", "c", "1"))
})

test_that("extract_TK works as expected", {
  heats1 <- c("HTH.600.C", "MTH.200.C", "MTH.100.C", "LTH.20.C", "LTH.-20.C")
  expect_equal(extract_TK(heats1), c(600, 200, 100, 20, -20) + 273.15)
  
  expect_true(is.na(extract_TK("LMH.20.C")))
  expect_equal(extract_TK(c("MMH.20.C", "HTH.600.C")), c(NA_real_, 600 + 273.15))
  expect_true(is.na(extract_TK("MTH.100.J")))
  
  expect_equal(extract_TK("HTH.600.F"), 588.70555556)
  expect_equal(extract_TK("STH.600.F"), 588.70555556)
  expect_equal(extract_TK("6TH.600.F"), 588.70555556)
  expect_equal(extract_TK("$TH.600.F"), 588.70555556)
  expect_equal(extract_TK("LTH.-104.75.F"), 197.177777778)
  expect_equal(extract_TK("LTH.55.R"), 30.555555555)
  expect_equal(extract_TK("LTH.-79.2.C"), 193.95)
  expect_equal(extract_TK("LTH.1089.15.K"), 1089.15)
  expect_equal(extract_TK("LTH.-40.F"), extract_TK("LTH.-40.C"))
})

test_that("carnot_efficiency works as expected", {
  expect_equal(carnot_efficiency("HTH.298.15.K"), 0)

  heats1 <- c("HTH.600.C", "MTH.200.C", "MTH.100.C", "LTH.20.C", "LTH.-20.C")
  expect_equal(carnot_efficiency(heats1), c(0.65853519, 0.36986157, 0.20099156, 0.016770082, 0.15093074))
  
  expect_equal(carnot_efficiency("HTH.600.C", T_0 = 500), 1 - 500 / (600 + 273.15))
  expect_equal(carnot_efficiency(c("HTH.600.C", "LTH.600.C"), T_0 = c(298.15, 273.15)), c(1 - 298.15 / (600 + 273.15), 1 - 273.15 / (600 + 273.15)))
  
  expect_true(is.na(carnot_efficiency("$$H.50.C")))
})
