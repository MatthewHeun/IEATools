library(magrittr)
library(testthat)

###########################################################
context("Final to useful")
###########################################################

test_that("form_C_mats works as expected", {
  allocation_table <- load_fu_allocation_data()
  C_df <- form_C_mats(allocation_table)
  # Check type of year column
  expect_true(is.numeric(C_df$Year))
  # Check that the Unit column is missing.  It has no meaning for allocations.
  expect_true(is.null(C_df[[IEATools::iea_cols$unit]]))
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
  allocation_table_wrong <- allocation_table
  allocation_table_wrong[[3, "1971"]] <- 0.9 # Was 1.0. This change should trigger an error on this row.
  expect_warning(diagnostic_df <- form_C_mats(allocation_table_wrong), "Not all rows in the C matrices sum to 1.")
  # Check that the diagnostic data frame is correct
  expect_equal(diagnostic_df[[1, IEATools::mat_meta_cols$rownames]], "Refinery gas -> Oil refineries")
  expect_equal(diagnostic_df[[1, ".should_be_1_vector"]], 0.9)
})


test_that("form_eta_fu_phi_vecs works as expected", {
  efficiency_table <- load_eta_fu_data()
  eta_fu_phi_u_df <- form_eta_fu_phi_u_vecs(efficiency_table)
  # Check type of year column
  expect_true(is.numeric(eta_fu_phi_u_df$Year))
  # Check that the Unit column is missing.  It has no meaning for allocations.
  expect_true(is.null(eta_fu_phi_u_df[[IEATools::iea_cols$unit]]))

  # Check some values
  eta_GHA_1971 <- eta_fu_phi_u_df$matvals[[1]]
  expect_equal(eta_GHA_1971[["Irons -> MTH.200.C", 1]], 0.85)
  expect_equal(eta_GHA_1971[["Trucks -> MD", 1]], 0.30)
  expect_equal(eta_GHA_1971[["Fans -> MD", 1]], 0.10)
  expect_equal(eta_GHA_1971[["Boat engines -> MD", 1]], 0.30)

  phi_ZAR_2000 <- eta_fu_phi_u_df$matvals[[8]]  
  expect_equal(phi_ZAR_2000[["Generators -> MD", 1]], 1)
  expect_equal(phi_ZAR_2000[["LPG stoves -> LTH.20.C", 1]], 0.01677008)
  expect_equal(phi_ZAR_2000[["Industrial furnaces -> HTH.600.C", 1]], 0.65853519)
  expect_equal(phi_ZAR_2000[["Electric lights -> Light", 1]], 0.18301611)
})


test_that("move_to_useful_last_stage works as expected", {
  psut_mats <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    prep_psut()
  C_data <- load_fu_allocation_data() %>% 
    form_C_mats()
  eta_fu_data <- load_eta_fu_data() %>% 
    form_eta_fu_phi_u_vecs()
  
  with_useful <- psut_mats %>% 
    move_last_stage_to_useful(tidy_C_data = C_data, tidy_eta_fu_data = eta_fu_data)

  # Check some of the values  
  
})
