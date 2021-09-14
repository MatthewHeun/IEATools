test_that("loading the phi constants table works as expected", {
  colnames <- load_phi_constants_table() %>%
    names()
  expect_equal(colnames, c("Product", "phi", "is.useful"))
})


