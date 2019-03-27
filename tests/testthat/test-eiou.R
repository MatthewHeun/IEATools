library(dplyr)
library(magrittr)

###########################################################
context("Specify EIOU")
###########################################################

test_that("specify_tp_eiou works as expected for Own use in electricity, CHP and heat plants", {
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

test_that("specify_tp_eiou works as expected for pumped storage plants", {
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

test_that("specify_tp_eiou works as expected for nuclear industry", {
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

test_that("specify_tp_eiou works as expected for non-specified (energy)", {
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

test_that("specify_tp_eiou works for sample data", {
  # This test is failing, because the (energy) suffix is still present in the Flow for Own use in electricity, CHP and heat plants
  # and the function assumes it has been stripped away. 
  # Solution: strip away "(energy)" and "(transf.)" during processing of these data.
  # Also, Flow.aggregation.point is not found. Need to do more to the data frame before calling specify_tp_eiou().
  specified <- load_tidy_iea_df() %>% 
    specify_tp_eiou() %>% 
    filter(Flow.aggregation.point == "Energy industry own use" & 
             Flow == "Main activity producer electricity plants")
  expect_equal(nrow(specified), 5)
})
