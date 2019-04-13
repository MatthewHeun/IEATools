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
