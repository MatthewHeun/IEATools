library(dplyr)

###########################################################
context("EIOU functions")
###########################################################

test_that("multiplication works", {
  Specific_production <- load_tidy_iea_df() %>% 
    specify_production()
  Prod_coal_oilng <- Specific_production %>% 
    filter(Flow == "Production" & Product %in% coal_and_coal_products)
  expect_equal(nrow(Prod_coal_oilng), 0)
  Res_coal_oilng <- Specific_production %>% 
    filter(startsWith(Flow, "Resources"))
  expect_equal(nrow(Res_coal_oilng), 6)
  expect_true(all(Res_coal_oilng$Flow.aggregation.point == "Total primary energy supply"))
  # There are none of these flows for Ghana (GH)
  expect_true(all(Res_coal_oilng$Country == "ZA"))
})
