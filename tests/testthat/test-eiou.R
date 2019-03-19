###########################################################
context("EIOU functions")
###########################################################

test_that("multiplication works", {
  load_tidy_iea_df() %>% 
    specify_production()
})
