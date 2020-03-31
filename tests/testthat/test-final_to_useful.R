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
  

test_that("form_C_mats works as expected", {
  C_df <- load_fu_allocation_data() %>% 
    form_C_mats()
  # Check some values.
  C_EIOU_GHA_1971 <- C_df %>% 
    dplyr::filter(Country == "GHA", Year == 1971, matnames == IEATools::template_cols$C_eiou) %>% 
    magrittr::extract2(IEATools::mat_meta_cols$matvals) %>% 
    magrittr::extract2(1)
  r1 <- "Electricity -> Main activity producer electricity plants"
  r2 <- "Refinery gas -> Oil refineries"
  c1 <- "Electric lights -> Light"
  c2 <- "Electric motors -> MD"
  c3 <- "Industrial furnaces -> HTH.600.C"
  expect_equal(C_EIOU_GHA_1971[[r1, c1]], 0.5)
  expect_equal(C_EIOU_GHA_1971[[r1, c2]], 0.5)
  expect_equal(C_EIOU_GHA_1971[[r1, c3]], 0)
  expect_equal(C_EIOU_GHA_1971[[r2, c1]], 0)
  expect_equal(C_EIOU_GHA_1971[[r2, c2]], 0)
  expect_equal(C_EIOU_GHA_1971[[r2, c3]], 1)
  
  C_Y_ZAF_2000 <- C_df %>% 
    dplyr::filter(Country == "ZAF", Year == 2000, matnames == IEATools::template_cols$C_Y) %>% 
    magrittr::extract2(IEATools::mat_meta_cols$matvals) %>% 
    magrittr::extract2(1)
  r1 <- "Blast furnace gas -> Iron and steel"
  c1 <- "Airplanes -> MD"
  r_kerosene <- "Kerosene type jet fuel excl. biofuels -> Domestic aviation"
  c_stoves <- "Industrial furnaces -> HTH.600.C"
  expect_equal(C_Y_ZAF_2000[[r1, c_stoves]], 1)
  expect_equal(C_Y_ZAF_2000[[r_kerosene, c1]], 1)
  
  # Set a wrong value and expect a warning.
  
})
