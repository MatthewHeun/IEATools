library(magrittr)
library(testthat)

###########################################################
context("Final to useful")
###########################################################

sample_allocation_table <- tibble::tribble(
  ~Destination, ~Ef.product, ~Machine, ~Eu.product, ~C,
  "Residential", "Elect", "Lights", "Light", 0.5,
  "Residential", "Elect", "Water heaters", "MTH", 0.5,
  "Construction", "Elect", "Elect motors", "MD", 0.25,
  "Construction", "Elect", "Lights", "Light", 0.25,
  "Construction", "Elect", "Heaters", "MTH", 0.5,
  "Residential", "Petrol", "Engines", "MD", 0.25,
  "Residential", "Petrol", "Burner", "MTH", 0.75,
  "Construction", "Petrol", "Autos", "MD", 0.6,
  "Construction", "Petrol", "Furnace", "LTH", 0.4)
  
sample_efficiency_table <- tibble::tribble(
  ~Machine, ~Eu.product, ~eta_fu,
  "Lights", "Light", 0.45,
  "Water heaters", "MTH", 0.9,
  "Elect motors", "MD", 0.95,
  "Heaters", "MTH", 0.9,
  "Engines", "MD", 0.25,
  "Burner", "MTH", 0.98,
  "Autos", "MD", 0.15,
  "Furnace", "LTH", 0.97)
  

test_that("make_C works as expected", {
  alloc_table <- load_fu_allocation_data()
  C <- make_C_mats(alloc_table)
})