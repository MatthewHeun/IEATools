context("test-eiou")

###########################################################
context("Specify EIOU")
###########################################################

test_that("specify_eiou works as expected for Own use in electricity, CHP and heat plants", {
  # Make a bogus data frame
  EIOU <- data.frame(Country = c("US", "US"), 
                     Flow.aggregation.point = c("Nothing", "Nothing"),
                     Flow = c("Own use in electricity, CHP and heat plants", "Own use in electricity, CHP and heat plants"), 
                     Product = c("Electricity", "Nothing"), stringsAsFactors = FALSE)
  EIOU_fixed <- specify_eiou(EIOU)
  # The first row is expected to change, because its Product is "Electricity"
  expect_equal(EIOU_fixed$Flow[[1]], "Main activity producer electricity plants")
  # The second row will not change, because its Product is "Nothing"
  expect_equal(EIOU_fixed$Flow[[2]], "Own use in electricity, CHP and heat plants")
})

test_that("specify_eiou works as expected for pumped storage plants", {
  # Make a bogus data frame
  EIOU <- data.frame(Country = c("US", "US"), 
                     Flow.aggregation.point = c("Energy industry own use", "Nothing"),
                     Product = c("Electricity", "Electricity"),
                     Flow = c("Pumped storage plants", "Pumped storage plants"), 
                     stringsAsFactors = FALSE)
  EIOU_fixed <- specify_eiou(EIOU)
  # The first row is expected to change, because its Product is "Electricity"
  expect_equal(EIOU_fixed$Flow[[1]], "Main activity producer electricity plants")
  # The second row will not change, because its Product is "Nothing"
  expect_equal(EIOU_fixed$Flow[[2]], "Pumped storage plants")
})

