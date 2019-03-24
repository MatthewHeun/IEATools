library(dplyr)
library(magrittr)

###########################################################
context("EIOU functions")
###########################################################

test_that("EIOU is replaced correctly", {
  Specific_production <- load_tidy_iea_df() %>% 
    specify_primary_production()
  Prod_coal_oilng <- Specific_production %>% 
    filter(Flow == "Production" & Product %in% coal_and_coal_products)
  expect_equal(nrow(Prod_coal_oilng), 0)
  Res_coal_oilng <- Specific_production %>% 
    filter(startsWith(Flow, "Resources"))
  expect_equal(nrow(Res_coal_oilng), 6)
  expect_true(all(Res_coal_oilng$Flow.aggregation.point == "Total primary energy supply"))
  # There are none of these flows for Ghana (GH)
  expect_true(all(Res_coal_oilng$Country == "ZA"))
  # Check for new rows of Coal mines
  Mines <- Specific_production %>% 
    filter(Flow == "Coal mines")
  expect_equal(nrow(Mines), 8)
  # Check that EIOU flows correctly remove the "(energy)" suffix.
  eiou <- Specific_production %>% 
    filter(Flow.aggregation.point == "Energy industry own use") %>% 
    extract2("Flow") %>% 
    unique()
  expect_false(eiou %>% endsWith("(energy)") %>% any())
})
