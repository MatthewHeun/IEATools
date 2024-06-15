test_that("loading the phi constants table works as expected", {
  phi_table <- load_phi_constants_table()
  colnames <- phi_table %>%
    names()
  expect_equal(colnames, c("Product", "phi", "IsUseful"))
})


