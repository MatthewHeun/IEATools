context("test-eiou")

###########################################################
context("Specify EIOU")
###########################################################

test_that("specify_eiou works as expected for Own use in electricity, CHP and heat plants", {
  # Make a bogus data frame
  EIOU <- data.frame(Country = c("US", "US"), 
                     Flow.aggregation.point = c("Energy industry own use", "nothing"),
                     Flow = c("Own use in electricity, CHP and heat plants", "Own use in electricity, CHP and heat plants"), 
                     stringsAsFactors = FALSE)
  EIOU_fixed <- specify_tp_eiou(EIOU)
  # The first row is expected to change, because its Product is "Electricity"
  expect_equal(EIOU_fixed$Flow[[1]], "Main activity producer electricity plants")
  # The second row will not change, because its Product is "Nothing"
  expect_equal(EIOU_fixed$Flow[[2]], "Own use in electricity, CHP and heat plants")
})

test_that("specify_eiou works as expected for pumped storage plants", {
  # Make a bogus data frame
  EIOU <- data.frame(Country = c("US", "US"), 
                     Flow.aggregation.point = c("Energy industry own use", "Nothing"),
                     Flow = c("Pumped storage plants", "Pumped storage plants"), 
                     stringsAsFactors = FALSE)
  EIOU_fixed <- specify_tp_eiou(EIOU)
  # The first row is expected to change, because its Product is "Electricity"
  expect_equal(EIOU_fixed$Flow[[1]], "Main activity producer electricity plants")
  # The second row will not change, because its Product is "Nothing"
  expect_equal(EIOU_fixed$Flow[[2]], "Pumped storage plants")
})

test_that("specify_eiou works as expected for nuclear industry", {
  # Make a bogus data frame
  EIOU <- data.frame(Country = c("US", "US"), 
                     Flow.aggregation.point = c("Energy industry own use", "Nothing"),
                     Flow = c("Nuclear industry", "Nuclear industry"), 
                     stringsAsFactors = FALSE)
  EIOU_fixed <- specify_tp_eiou(EIOU)
  # The first row is expected to change, because its Product is "Electricity"
  expect_equal(EIOU_fixed$Flow[[1]], "Main activity producer electricity plants")
  # The second row will not change, because its Product is "Nothing"
  expect_equal(EIOU_fixed$Flow[[2]], "Nuclear industry")
})

test_that("specify_eiou works as expected for non-specified (energy)", {
  # Make a bogus data frame
  EIOU <- data.frame(Country = c("US", "US"), 
                     Flow.aggregation.point = c("Energy industry own use", "Nothing"),
                     Flow = c("Non-specified (energy)", "Non-specified (energy)"), 
                     stringsAsFactors = FALSE)
  EIOU_fixed <- specify_tp_eiou(EIOU)
  # The first row is expected to change, because its Product is "Electricity"
  expect_equal(EIOU_fixed$Flow[[1]], "Oil and gas extraction")
  # The second row will not change, because its Product is "Nothing"
  expect_equal(EIOU_fixed$Flow[[2]], "Non-specified (energy)")
})
