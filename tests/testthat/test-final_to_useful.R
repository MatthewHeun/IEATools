library(magrittr)
library(testthat)

test_that("form_C_mats works as expected", {
  allocation_table <- load_fu_allocation_data()
  C_df <- form_C_mats(allocation_table)
  # Check type of year column
  expect_true(is.numeric(C_df$Year))
  # Check that the Unit column is missing.  It has no meaning for allocations.
  expect_true(is.null(C_df[[IEATools::iea_cols$unit]]))
  # Check some values.
  C_EIOU_GHA_1971 <- C_df %>% 
    dplyr::filter(Country == "GHA", Year == 1971) %>% 
    magrittr::extract2(IEATools::template_cols$C_eiou) %>% 
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
    dplyr::filter(Country == "ZAF", Year == 2000) %>% 
    magrittr::extract2(IEATools::template_cols$C_Y) %>% 
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
  
  # Check the row and column types
  for (i in 1:nrow(C_df)) {
    expect_equal(matsbyname::rowtype(C_df$C_EIOU[[i]]), "Product -> Industry")
    expect_equal(matsbyname::coltype(C_df$C_EIOU[[i]]), "Industry -> Product")
    expect_equal(matsbyname::rowtype(C_df$C_Y[[i]]), "Product -> Industry")
    expect_equal(matsbyname::coltype(C_df$C_Y[[i]]), "Industry -> Product")
  }
})


test_that("form_C_mats works with Matrix objects", {
  allocation_table <- load_fu_allocation_data()
  C_df <- form_C_mats(allocation_table, matrix.class = "Matrix")
  # Check that we made Matrix objects.
  expect_true(all(sapply(C_df$C_EIOU, matsbyname::is.Matrix)))
  expect_true(all(sapply(C_df$C_Y, matsbyname::is.Matrix)))
  # Check type of year column
  expect_true(is.numeric(C_df$Year))
  # Check that the Unit column is missing.  It has no meaning for allocations.
  expect_true(is.null(C_df[[IEATools::iea_cols$unit]]))
  # Check some values.
  C_EIOU_GHA_1971 <- C_df %>% 
    dplyr::filter(Country == "GHA", Year == 1971) %>% 
    magrittr::extract2(IEATools::template_cols$C_eiou) %>% 
    magrittr::extract2(1)
  r1 <- "Electricity -> Main activity producer electricity plants"
  r2 <- "Refinery gas -> Oil refineries"
  c1 <- "Electric lights -> Light"
  c2 <- "Electric motors -> MD"
  c3 <- "Industrial furnaces -> HTH.600.C"
  expect_equal(C_EIOU_GHA_1971[r1, c1], 0.5)
  expect_equal(C_EIOU_GHA_1971[r1, c2], 0.5)
  expect_equal(C_EIOU_GHA_1971[r1, c3], 0)
  expect_equal(C_EIOU_GHA_1971[r2, c1], 0)
  expect_equal(C_EIOU_GHA_1971[r2, c2], 0)
  expect_equal(C_EIOU_GHA_1971[r2, c3], 1)
  
  C_Y_ZAF_2000 <- C_df %>% 
    dplyr::filter(Country == "ZAF", Year == 2000) %>% 
    magrittr::extract2(IEATools::template_cols$C_Y) %>% 
    magrittr::extract2(1)
  r1 <- "Blast furnace gas -> Iron and steel"
  c1 <- "Airplanes -> MD"
  r_kerosene <- "Kerosene type jet fuel excl. biofuels -> Domestic aviation"
  c_stoves <- "Industrial furnaces -> HTH.600.C"
  expect_equal(C_Y_ZAF_2000[r1, c_stoves], 1)
  expect_equal(C_Y_ZAF_2000[r_kerosene, c1], 1)
  
  # Set a wrong value and expect a warning.
  allocation_table_wrong <- allocation_table
  allocation_table_wrong[[3, "1971"]] <- 0.9 # Was 1.0. This change should trigger an error on this row.
  expect_warning(diagnostic_df <- form_C_mats(allocation_table_wrong, matrix.class = "Matrix"), "Not all rows in the C matrices sum to 1.")
  # Check that the diagnostic data frame is correct
  expect_equal(diagnostic_df[[1, IEATools::mat_meta_cols$rownames]], "Refinery gas -> Oil refineries")
  expect_equal(diagnostic_df[[1, ".should_be_1_vector"]], 0.9)
  
  # Check the row and column types
  for (i in 1:nrow(C_df)) {
    expect_equal(matsbyname::rowtype(C_df$C_EIOU[[i]]), "Product -> Industry")
    expect_equal(matsbyname::coltype(C_df$C_EIOU[[i]]), "Industry -> Product")
    expect_equal(matsbyname::rowtype(C_df$C_Y[[i]]), "Product -> Industry")
    expect_equal(matsbyname::coltype(C_df$C_Y[[i]]), "Industry -> Product")
  }
})


test_that("form_eta_fu_phi_u_vecs() works as expected", {
  efficiency_table <- load_eta_fu_data()
  eta_fu_phi_u_df <- form_eta_fu_phi_u_vecs(efficiency_table)
  # Check type of year column
  expect_true(is.numeric(eta_fu_phi_u_df$Year))
  # Check that the Unit column is missing.  It has no meaning for allocations.
  expect_true(is.null(eta_fu_phi_u_df[[IEATools::iea_cols$unit]]))

  # Check some values
  eta_GHA_1971 <- eta_fu_phi_u_df$eta.fu[[1]]
  expect_equal(eta_GHA_1971[["Irons -> MTH.200.C", 1]], 0.85)
  expect_equal(eta_GHA_1971[["Trucks -> MD", 1]], 0.30)
  expect_equal(eta_GHA_1971[["Fans -> MD", 1]], 0.10)
  expect_equal(eta_GHA_1971[["Boat engines -> MD", 1]], 0.30)
  
  
  phi_ZAR_2000 <- eta_fu_phi_u_df$phi.u[[4]]  
  expect_equal(phi_ZAR_2000[["MD", 1]], 1)
  expect_equal(phi_ZAR_2000[["LTH.20.C", 1]], 0.01677008)
  expect_equal(phi_ZAR_2000[["HTH.600.C", 1]], 0.65853519)
  expect_equal(phi_ZAR_2000[["Light", 1]], 0.18301611)

  # Check row and column types
  for (i in 1:nrow(eta_fu_phi_u_df)) {
    expect_equal(matsbyname::rowtype(eta_fu_phi_u_df$eta.fu[[i]]), "Industry -> Product")
    expect_equal(matsbyname::coltype(eta_fu_phi_u_df$eta.fu[[i]]), "eta.fu")
  }
  for (i in 1:nrow(eta_fu_phi_u_df)) {
    expect_equal(matsbyname::rowtype(eta_fu_phi_u_df$phi.u[[i]]), "Product")
    expect_equal(matsbyname::coltype(eta_fu_phi_u_df$phi.u[[i]]), "phi")
  }
})


test_that("form_eta_fu_phi_u_vecs() errors when phi values are different", {
  efficiency_table <- load_eta_fu_data()
  # Change a value to be different. Change a MD phi value from 1 to 0.99.
  expect_equal(efficiency_table[32, "1971"], 1.0)
  efficiency_table[32, "1971"] <- 0.9999
  expect_equal(efficiency_table[32, "1971"], 0.9999)
  expect_error(form_eta_fu_phi_u_vecs(efficiency_table), "Found useful products with different phi values in form_eta_fu_phi_u_vecs")
})


test_that("form_eta_fu_phi_u_vecs() works when the incoming data frame has no rows", {
  efficiency_table <- load_eta_fu_data()
  efficiency_table <- efficiency_table[0, ]
  eta_fu_phi_u_df <- form_eta_fu_phi_u_vecs(efficiency_table)
  
  expect_true(IEATools::template_cols$eta_fu %in% names(eta_fu_phi_u_df))
  expect_true(IEATools::template_cols$phi_u %in% names(eta_fu_phi_u_df))
})


test_that("extend_to_useful_helper() works as intended", {
  # These tests come from the "Pushing Y to Useful" tab in file "Matrix f->u example calcs.xlsx"
  
  # Set up some matrices and data frames
  Y_f <- matrix(c(100, 50, 
                  200, 25), byrow = TRUE, nrow = 2, ncol = 2, 
                dimnames = list(c("Elect", "Petrol"), c("Residential", "Construction"))) %>% 
    matsbyname::setrowtype(IEATools::row_col_types$product) %>% 
    matsbyname::setcoltype(IEATools::row_col_types$industry)
  
  Allocation_Table <- tibble::tribble(~Destination, ~Ef.product, ~Machine, ~Eu.product, ~C, 
                                      "Residential", "Elect", "Lights", "Light", 0.5,
                                      "Residential", "Elect", "Water heaters", "MTH", 0.5,
                                      "Construction", "Elect", "Elect motors", "MD", 0.25, 
                                      "Construction", "Elect", "Lights", "Light", 0.25,
                                      "Construction", "Elect", "Heaters", "MTH", 0.5,
                                      "Residential", "Petrol", "Engines", "MD", 0.25,
                                      "Residential", "Petrol", "Burner", "MTH", 0.75,
                                      "Construction", "Petrol", "Autos", "MD", 0.6,
                                      "Construction", "Petrol", "Furnace", "LTH", 0.4)
  
  Efficiency_Table <- tibble::tribble(~Machine, ~Eu.product, ~eta.fu,
                                      "Lights", "Light", 0.45,
                                      "Water heaters", "MTH", 0.9,
                                      "Elect motors", "MD", 0.95,
                                      "Heaters", "MTH", 0.9,
                                      "Engines", "MD", 0.25,
                                      "Burner", "MTH", 0.98,
                                      "Autos", "MD", 0.15,
                                      "Furnace", "LTH", 0.97)
  
  
  # Calculate some matrices
  
  C_Y <- Allocation_Table %>% 
    dplyr::mutate(
      rownames = RCLabels::paste_pref_suff(pref = Ef.product, suff = Destination, notation = RCLabels::arrow_notation), 
      colnames = RCLabels::paste_pref_suff(pref = Machine, suff = Eu.product, notation = RCLabels::arrow_notation), 
      rowtypes = RCLabels::paste_pref_suff(pref = row_col_types$product, suff = row_col_types$industry, notation = RCLabels::arrow_notation),
      coltypes = RCLabels::paste_pref_suff(pref = row_col_types$industry, suff = row_col_types$product, notation = RCLabels::arrow_notation),
      matnames = "C_Y", 
      Destination = NULL, 
      Ef.product = NULL, 
      Machine = NULL, 
      Eu.product = NULL
    ) %>%
    dplyr::rename(
      matvals = C
    ) %>% 
    dplyr::group_by(matnames) %>% 
    matsindf::collapse_to_matrices(matrix.class = "Matrix") %>% 
    magrittr::extract2("matvals") %>% 
    magrittr::extract2(1)
  
  eta.fu_rownames <- Efficiency_Table %>% 
    dplyr::mutate(
      rownames = RCLabels::paste_pref_suff(pref = Machine, suff = Eu.product, notation = RCLabels::arrow_notation)
    ) %>% 
    magrittr::extract2("rownames")
  
  eta_fu <- Efficiency_Table %>% 
    magrittr::extract2("eta.fu") %>% 
    matrix(ncol = 1, dimnames = list(eta.fu_rownames, "eta.fu")) %>% 
    matsbyname::setrowtype(RCLabels::paste_pref_suff(pref = row_col_types$industry, 
                                                       suff = row_col_types$product, 
                                                       notation = RCLabels::arrow_notation))
  
  # Calculate actual results
  
  res <- IEATools:::extend_to_useful_helper(dest_mat = Y_f, C_mat = C_Y, eta_fu_vec = eta_fu)
  
  
  # Calculate expected results
  
  ## Step 1
  
  Y_f_vec <- matsbyname::vectorize_byname(Y_f, notation = RCLabels::arrow_notation)
  Y_f_vec_hat <- matsbyname::hatize_byname(Y_f_vec, keep = "rownames")
  Y_f_vec_hat_C_Y <- matsbyname::matrixproduct_byname(Y_f_vec_hat, C_Y)
  
  eta_fu_hat <- matsbyname::hatize_byname(eta_fu, keep = "rownames") %>% 
    arrow_to_from_byname(margin = 2)
  
  expect_equal(eta_fu_hat[["Engines -> MD", "MD [from Engines]"]], 
               Efficiency_Table %>% dplyr::filter(Machine == "Engines") %>% 
                 magrittr::extract2("eta.fu"))
  
  
  ## Step 2
  
  add_to_U_feed_expected <- Y_f_vec_hat_C_Y %>% 
    matsbyname::aggregate_to_pref_suff_byname(keep = "pref", margin = 1, notation = RCLabels::arrow_notation) %>%
    matsbyname::clean_byname(margin = 1) %>% 
    matsbyname::setcoltype(row_col_types$industry)
  
  expect_equal(res$add_to_U, add_to_U_feed_expected)
  
  
  ## Step 3
  
  add_to_V_expected <- Y_f_vec_hat_C_Y %>% 
    matsbyname::colsums_byname() %>%
    matsbyname::hatize_byname(keep = "colnames") %>%
    matsbyname::matrixproduct_byname(eta_fu_hat) %>% 
    matsbyname::setrowtype(row_col_types$industry) %>% 
    matsbyname::setcoltype(row_col_types$product)
  
  expect_equal(res$add_to_V, add_to_V_expected)
  
  
  ## Step 4
  
  add_to_Y_expected <- matsbyname::matrixproduct_byname(Y_f_vec_hat_C_Y, eta_fu_hat) %>%
    matsbyname::transpose_byname() %>%
    matsbyname::aggregate_to_pref_suff_byname(keep = "suff", margin = 2, notation = RCLabels::arrow_notation) %>%
    matsbyname::clean_byname() %>% 
    matsbyname::setrowtype(row_col_types$product) %>% 
    matsbyname::setcoltype(row_col_types$industry)
  
  expect_equal(res$add_to_dest, add_to_Y_expected)
})


test_that("extend_to_useful() works as expected", {
  C_data <- load_fu_allocation_data() %>% 
    form_C_mats()
  eta_fu_data <- load_eta_fu_data() %>% 
    form_eta_fu_phi_u_vecs()
  m_cols <- eta_fu_data %>% 
    IEATools::meta_cols(return_names = TRUE,
                        years_to_keep = IEATools::iea_cols$year,
                        not_meta = c(IEATools::template_cols$eta_fu, IEATools::template_cols$phi_u))
  psut_mats <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    prep_psut() %>% 
    dplyr::full_join(C_data, by = m_cols) %>% 
    dplyr::full_join(eta_fu_data, by = m_cols)
  
  with_useful <- psut_mats %>% 
    extend_to_useful()

  # Check some of the values  
  
  # Allocation of ZAF EIOU electricity for lighting and mechanical drive in 2000
  
  EIOU_Electricity_Coal_mines <- psut_mats %>% 
    dplyr::filter(Country == "ZAF", Year == 2000) %>% 
    magrittr::extract2("U_EIOU") %>% 
    magrittr::extract2(1) %>% 
    magrittr::extract("Electricity", "Coal mines")
  
  alloc_EIOU_ZA_2000 <- C_data %>% 
    dplyr::filter(Country == "ZAF", Year == 2000) %>% 
    magrittr::extract2("C_EIOU") %>% 
    magrittr::extract2(1)
  C_lights <- alloc_EIOU_ZA_2000 %>% 
    magrittr::extract("Electricity -> Coal mines", "Electric lights -> Light")
  C_motors <- alloc_EIOU_ZA_2000 %>% 
    magrittr::extract("Electricity -> Coal mines", "Electric motors -> MD")
  
  expected_elect_into_lights_in_mines <- EIOU_Electricity_Coal_mines * C_lights
  expected_elect_into_motors_in_mines <- EIOU_Electricity_Coal_mines * C_motors
  
  eta_lights <- eta_fu_data %>% 
    dplyr::filter(Country == "ZAF", Year == 2000) %>% 
    magrittr::extract2("eta.fu") %>% 
    magrittr::extract2(1) %>% 
    magrittr::extract("Electric lights -> Light", 1)
  
  eta_motors <- eta_fu_data %>% 
    dplyr::filter(Country == "ZAF", Year == 2000) %>% 
    magrittr::extract2("eta.fu") %>% 
    magrittr::extract2(1) %>% 
    magrittr::extract("Electric motors -> MD", 1)
  
  expected_light_into_mines <- expected_elect_into_lights_in_mines * eta_lights
  expected_md_into_mines <- expected_elect_into_motors_in_mines * eta_motors

  actual_light_into_mines <- with_useful %>% 
    dplyr::filter(Country == "ZAF", Last.stage == "Useful", Year == 2000) %>% 
    magrittr::extract2("U_EIOU") %>% 
    magrittr::extract2(1) %>% 
    magrittr::extract("Light [from Electric lights]", "Coal mines")
  
  actual_md_into_mines <- with_useful %>% 
    dplyr::filter(Country == "ZAF", Last.stage == "Useful", Year == 2000) %>% 
    magrittr::extract2("U_EIOU") %>% 
    magrittr::extract2(1) %>% 
    magrittr::extract("MD [from Electric motors]", "Coal mines")

  expect_equal(actual_light_into_mines, expected_light_into_mines)
  expect_equal(actual_md_into_mines, expected_md_into_mines)
  
  
  # ZAF allocation of Other bituminous coal to MTH.200.C
  OBC_to_Chem <- psut_mats %>% 
    dplyr::filter(Country == "ZAF", Year == 2000) %>% 
    magrittr::extract2("Y") %>% 
    magrittr::extract2(1) %>% 
    magrittr::extract("Other bituminous coal", "Chemical and petrochemical")
  
  alloc_Y_ZA_2000 <- C_data %>% 
    dplyr::filter(Country == "ZAF", Year == 2000) %>% 
    magrittr::extract2("C_Y") %>% 
    magrittr::extract2(1)
  
  C_heaters <- alloc_Y_ZA_2000 %>% 
    magrittr::extract("Other bituminous coal -> Chemical and petrochemical", "Electric heaters -> MTH.200.C")
  
  expected_OBC_into_heaters_in_chem <- OBC_to_Chem * C_heaters

  eta_heaters <- eta_fu_data %>% 
    dplyr::filter(Country == "ZAF", Year == 2000) %>% 
    magrittr::extract2("eta.fu") %>% 
    magrittr::extract2(1) %>% 
    magrittr::extract("Electric heaters -> MTH.200.C", 1)

  expected_200C_into_chem <- expected_OBC_into_heaters_in_chem * eta_heaters
  
  actual_200C_into_chem <- with_useful %>% 
    dplyr::filter(Country == "ZAF", Last.stage == "Useful", Year == 2000) %>% 
    magrittr::extract2("Y") %>% 
    magrittr::extract2(1) %>% 
    magrittr::extract("MTH.200.C [from Electric heaters]", "Chemical and petrochemical")

  expect_equal(actual_200C_into_chem, expected_200C_into_chem)

  # Light is a little more complicated, because some electricity is also used for Light in Chem
  
  # First work on OBC
  Elect_to_Chem <- psut_mats %>% 
    dplyr::filter(Country == "ZAF", Year == 2000) %>% 
    magrittr::extract2("Y") %>% 
    magrittr::extract2(1) %>% 
    magrittr::extract("Electricity", "Chemical and petrochemical")
  
  C_OBC_light <- alloc_Y_ZA_2000 %>% 
    magrittr::extract("Other bituminous coal -> Chemical and petrochemical", "Electric lights -> Light")
  C_Elect_light <- alloc_Y_ZA_2000 %>% 
    magrittr::extract("Electricity -> Chemical and petrochemical", "Electric lights -> Light")

  expected_OBC_into_lights_in_chem <- OBC_to_Chem * C_OBC_light
  expected_Elect_into_lights_in_chem <- Elect_to_Chem * C_Elect_light
  
  expected_OBC_light_into_chem <- expected_OBC_into_lights_in_chem * eta_lights
  expected_Elect_light_into_chem <- expected_Elect_into_lights_in_chem * eta_lights
  expected_light_into_chem <- expected_OBC_light_into_chem + expected_Elect_light_into_chem
  
  actual_light_into_chem <- with_useful %>% 
    dplyr::filter(Country == "ZAF", Last.stage == "Useful", Year == 2000) %>% 
    magrittr::extract2("Y") %>% 
    magrittr::extract2(1) %>% 
    magrittr::extract("Light [from Electric lights]", "Chemical and petrochemical")

  expect_equal(actual_light_into_chem, expected_light_into_chem)
    
  # Allocation of GHA final demand electricity for lighting and mechanical drive Non-ferrous metals in 1971
  Elect_to_NFM <- psut_mats %>% 
    dplyr::filter(Country == "GHA", Year == 1971) %>% 
    magrittr::extract2("Y") %>% 
    magrittr::extract2(1) %>% 
    magrittr::extract("Electricity", "Non-ferrous metals")
  
  alloc_Y_GH_1971 <- C_data %>% 
    dplyr::filter(Country == "GHA", Year == 1971) %>% 
    magrittr::extract2("C_Y") %>% 
    magrittr::extract2(1)
  
  C_motors <- alloc_Y_GH_1971 %>% 
    magrittr::extract("Electricity -> Non-ferrous metals", "Electric motors -> MD")
  C_lights <- alloc_Y_GH_1971 %>% 
    magrittr::extract("Electricity -> Non-ferrous metals", "Electric lights -> Light")
  
  expected_elect_into_motors_in_NFM <- Elect_to_NFM * C_motors
  expected_elect_into_lights_in_NFM <- Elect_to_NFM * C_lights
  
  eta_motors <- eta_fu_data %>% 
    dplyr::filter(Country == "GHA", Year == 1971) %>% 
    magrittr::extract2("eta.fu") %>% 
    magrittr::extract2(1) %>% 
    magrittr::extract("Electric motors -> MD", 1)

  eta_lights <- eta_fu_data %>% 
    dplyr::filter(Country == "GHA", Year == 1971) %>% 
    magrittr::extract2("eta.fu") %>% 
    magrittr::extract2(1) %>% 
    magrittr::extract("Electric lights -> Light", 1)
  
  
  expected_md_into_NFM <- expected_elect_into_motors_in_NFM * eta_motors
  expected_light_into_NFM <- expected_elect_into_lights_in_NFM * eta_lights
  
  actual_md_into_NFM <- with_useful %>% 
    dplyr::filter(Country == "GHA", Last.stage == "Useful", Year == 1971) %>% 
    magrittr::extract2("Y") %>% 
    magrittr::extract2(1) %>% 
    magrittr::extract("MD [from Electric motors]", "Non-ferrous metals")
  
  actual_light_into_NFM <- with_useful %>% 
    dplyr::filter(Country == "GHA", Last.stage == "Useful", Year == 1971) %>% 
    magrittr::extract2("Y") %>% 
    magrittr::extract2(1) %>% 
    magrittr::extract("Light [from Electric lights]", "Non-ferrous metals")
  
  expect_equal(actual_md_into_NFM, expected_md_into_NFM)
  expect_equal(actual_light_into_NFM, expected_light_into_NFM)
  
  # Check that a warning is emitted when the energy balance check fails.
  # The default 2019 psut_mats (before fixing energy balance) has a slight energy balance mismatch.
  # This test sets the tolerance especially tight to force an energy balance failure.
  expect_warning(with_useful_warn <- psut_mats %>% 
                   extend_to_useful(tol = 1e-10))
  
  # Verify that we have correct r_EIOU matrices for the useful stage.
  r_EIOU_check <- with_useful %>% 
    dplyr::mutate(
      r_EIOU_check = matsbyname::quotient_byname(U_EIOU, U) %>% 
        matsbyname::replaceNaN_byname(val = 0), 
      should_be_zero = matsbyname::difference_byname(r_EIOU_check, r_EIOU), 
      is_zero = matsbyname::iszero_byname(should_be_zero)
    )
  expect_true(all(unlist(r_EIOU_check$is_zero)))
})


test_that("extend_to_useful() works with Matrix objects", {
  C_data <- load_fu_allocation_data() %>% 
    form_C_mats(matrix.class = "Matrix")
  eta_fu_data <- load_eta_fu_data() %>% 
    form_eta_fu_phi_u_vecs(matrix.class = "Matrix")
  m_cols <- eta_fu_data %>% 
    IEATools::meta_cols(return_names = TRUE,
                        years_to_keep = IEATools::iea_cols$year,
                        not_meta = c(IEATools::template_cols$eta_fu, IEATools::template_cols$phi_u))
  psut_mats <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    prep_psut(matrix.class = "Matrix") %>% 
    dplyr::full_join(C_data, by = m_cols) %>% 
    dplyr::full_join(eta_fu_data, by = m_cols)
  
  with_useful <- psut_mats %>% 
    extend_to_useful()
  
  # Check some of the values  
  
  # Allocation of ZAF EIOU electricity for lighting and mechanical drive in 2000
  
  EIOU_Electricity_Coal_mines <- psut_mats %>% 
    dplyr::filter(Country == "ZAF", Year == 2000) %>% 
    magrittr::extract2("U_EIOU") %>% 
    magrittr::extract2(1) %>% 
    magrittr::extract("Electricity", "Coal mines")
  
  alloc_EIOU_ZA_2000 <- C_data %>% 
    dplyr::filter(Country == "ZAF", Year == 2000) %>% 
    magrittr::extract2("C_EIOU") %>% 
    magrittr::extract2(1)
  C_lights <- alloc_EIOU_ZA_2000 %>% 
    magrittr::extract("Electricity -> Coal mines", "Electric lights -> Light")
  C_motors <- alloc_EIOU_ZA_2000 %>% 
    magrittr::extract("Electricity -> Coal mines", "Electric motors -> MD")
  
  expected_elect_into_lights_in_mines <- EIOU_Electricity_Coal_mines * C_lights
  expected_elect_into_motors_in_mines <- EIOU_Electricity_Coal_mines * C_motors
  
  eta_lights <- eta_fu_data %>% 
    dplyr::filter(Country == "ZAF", Year == 2000) %>% 
    magrittr::extract2("eta.fu") %>% 
    magrittr::extract2(1) %>% 
    magrittr::extract("Electric lights -> Light", 1)
  
  eta_motors <- eta_fu_data %>% 
    dplyr::filter(Country == "ZAF", Year == 2000) %>% 
    magrittr::extract2("eta.fu") %>% 
    magrittr::extract2(1) %>% 
    magrittr::extract("Electric motors -> MD", 1)
  
  expected_light_into_mines <- expected_elect_into_lights_in_mines * eta_lights
  expected_md_into_mines <- expected_elect_into_motors_in_mines * eta_motors
  
  actual_light_into_mines <- with_useful %>% 
    dplyr::filter(Country == "ZAF", Last.stage == "Useful", Year == 2000) %>% 
    magrittr::extract2("U_EIOU") %>% 
    magrittr::extract2(1) %>% 
    magrittr::extract("Light [from Electric lights]", "Coal mines")
  
  actual_md_into_mines <- with_useful %>% 
    dplyr::filter(Country == "ZAF", Last.stage == "Useful", Year == 2000) %>% 
    magrittr::extract2("U_EIOU") %>% 
    magrittr::extract2(1) %>% 
    magrittr::extract("MD [from Electric motors]", "Coal mines")
  
  expect_equal(actual_light_into_mines, expected_light_into_mines)
  expect_equal(actual_md_into_mines, expected_md_into_mines)
  
  
  # ZAF allocation of Other bituminous coal to MTH.200.C
  OBC_to_Chem <- psut_mats %>% 
    dplyr::filter(Country == "ZAF", Year == 2000) %>% 
    magrittr::extract2("Y") %>% 
    magrittr::extract2(1) %>% 
    magrittr::extract("Other bituminous coal", "Chemical and petrochemical")
  
  alloc_Y_ZA_2000 <- C_data %>% 
    dplyr::filter(Country == "ZAF", Year == 2000) %>% 
    magrittr::extract2("C_Y") %>% 
    magrittr::extract2(1)
  
  C_heaters <- alloc_Y_ZA_2000 %>% 
    magrittr::extract("Other bituminous coal -> Chemical and petrochemical", "Electric heaters -> MTH.200.C")
  
  expected_OBC_into_heaters_in_chem <- OBC_to_Chem * C_heaters
  
  eta_heaters <- eta_fu_data %>% 
    dplyr::filter(Country == "ZAF", Year == 2000) %>% 
    magrittr::extract2("eta.fu") %>% 
    magrittr::extract2(1) %>% 
    magrittr::extract("Electric heaters -> MTH.200.C", 1)
  
  expected_200C_into_chem <- expected_OBC_into_heaters_in_chem * eta_heaters
  
  actual_200C_into_chem <- with_useful %>% 
    dplyr::filter(Country == "ZAF", Last.stage == "Useful", Year == 2000) %>% 
    magrittr::extract2("Y") %>% 
    magrittr::extract2(1) %>% 
    magrittr::extract("MTH.200.C [from Electric heaters]", "Chemical and petrochemical")
  
  expect_equal(actual_200C_into_chem, expected_200C_into_chem)
  
  # Light is a little more complicated, because some electricity is also used for Light in Chem
  
  # First work on OBC
  Elect_to_Chem <- psut_mats %>% 
    dplyr::filter(Country == "ZAF", Year == 2000) %>% 
    magrittr::extract2("Y") %>% 
    magrittr::extract2(1) %>% 
    magrittr::extract("Electricity", "Chemical and petrochemical")
  
  C_OBC_light <- alloc_Y_ZA_2000 %>% 
    magrittr::extract("Other bituminous coal -> Chemical and petrochemical", "Electric lights -> Light")
  C_Elect_light <- alloc_Y_ZA_2000 %>% 
    magrittr::extract("Electricity -> Chemical and petrochemical", "Electric lights -> Light")
  
  expected_OBC_into_lights_in_chem <- OBC_to_Chem * C_OBC_light
  expected_Elect_into_lights_in_chem <- Elect_to_Chem * C_Elect_light
  
  expected_OBC_light_into_chem <- expected_OBC_into_lights_in_chem * eta_lights
  expected_Elect_light_into_chem <- expected_Elect_into_lights_in_chem * eta_lights
  expected_light_into_chem <- expected_OBC_light_into_chem + expected_Elect_light_into_chem
  
  actual_light_into_chem <- with_useful %>% 
    dplyr::filter(Country == "ZAF", Last.stage == "Useful", Year == 2000) %>% 
    magrittr::extract2("Y") %>% 
    magrittr::extract2(1) %>% 
    magrittr::extract("Light [from Electric lights]", "Chemical and petrochemical")
  
  expect_equal(actual_light_into_chem, expected_light_into_chem)
  
  # Allocation of GHA final demand electricity for lighting and mechanical drive Non-ferrous metals in 1971
  Elect_to_NFM <- psut_mats %>% 
    dplyr::filter(Country == "GHA", Year == 1971) %>% 
    magrittr::extract2("Y") %>% 
    magrittr::extract2(1) %>% 
    magrittr::extract("Electricity", "Non-ferrous metals")
  
  alloc_Y_GH_1971 <- C_data %>% 
    dplyr::filter(Country == "GHA", Year == 1971) %>% 
    magrittr::extract2("C_Y") %>% 
    magrittr::extract2(1)
  
  C_motors <- alloc_Y_GH_1971 %>% 
    magrittr::extract("Electricity -> Non-ferrous metals", "Electric motors -> MD")
  C_lights <- alloc_Y_GH_1971 %>% 
    magrittr::extract("Electricity -> Non-ferrous metals", "Electric lights -> Light")
  
  expected_elect_into_motors_in_NFM <- Elect_to_NFM * C_motors
  expected_elect_into_lights_in_NFM <- Elect_to_NFM * C_lights
  
  eta_motors <- eta_fu_data %>% 
    dplyr::filter(Country == "GHA", Year == 1971) %>% 
    magrittr::extract2("eta.fu") %>% 
    magrittr::extract2(1) %>% 
    magrittr::extract("Electric motors -> MD", 1)
  
  eta_lights <- eta_fu_data %>% 
    dplyr::filter(Country == "GHA", Year == 1971) %>% 
    magrittr::extract2("eta.fu") %>% 
    magrittr::extract2(1) %>% 
    magrittr::extract("Electric lights -> Light", 1)
  
  
  expected_md_into_NFM <- expected_elect_into_motors_in_NFM * eta_motors
  expected_light_into_NFM <- expected_elect_into_lights_in_NFM * eta_lights
  
  actual_md_into_NFM <- with_useful %>% 
    dplyr::filter(Country == "GHA", Last.stage == "Useful", Year == 1971) %>% 
    magrittr::extract2("Y") %>% 
    magrittr::extract2(1) %>% 
    magrittr::extract("MD [from Electric motors]", "Non-ferrous metals")
  
  actual_light_into_NFM <- with_useful %>% 
    dplyr::filter(Country == "GHA", Last.stage == "Useful", Year == 1971) %>% 
    magrittr::extract2("Y") %>% 
    magrittr::extract2(1) %>% 
    magrittr::extract("Light [from Electric lights]", "Non-ferrous metals")
  
  expect_equal(actual_md_into_NFM, expected_md_into_NFM)
  expect_equal(actual_light_into_NFM, expected_light_into_NFM)
  
  # Check that a warning is emitted when the energy balance check fails.
  # The default 2019 psut_mats (before fixing energy balance) has a slight energy balance mismatch.
  # This test sets the tolerance especially tight to force an energy balance failure.
  expect_warning(with_useful_warn <- psut_mats %>% 
                   extend_to_useful(tol = 1e-10))
  
  # Verify that we have correct r_EIOU matrices for the useful stage.
  r_EIOU_check <- with_useful %>% 
    dplyr::mutate(
      r_EIOU_check = matsbyname::quotient_byname(U_EIOU, U) %>% 
        matsbyname::replaceNaN_byname(val = 0), 
      should_be_zero = matsbyname::difference_byname(r_EIOU_check, r_EIOU), 
      is_zero = matsbyname::iszero_byname(should_be_zero)
    )
  expect_true(all(unlist(r_EIOU_check$is_zero)))
})


test_that("extend_to_useful() works with individual matrices", {
  C_data <- load_fu_allocation_data() %>% 
    form_C_mats()
  eta_fu_data <- load_eta_fu_data() %>% 
    form_eta_fu_phi_u_vecs()
  m_cols <- eta_fu_data %>% 
    IEATools::meta_cols(return_names = TRUE,
                        years_to_keep = IEATools::iea_cols$year,
                        not_meta = c(IEATools::template_cols$eta_fu, IEATools::template_cols$phi_u))
  psut_mats <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    prep_psut() %>% 
    dplyr::full_join(C_data, by = m_cols) %>% 
    dplyr::full_join(eta_fu_data, by = m_cols)
  
  useful_mats <- extend_to_useful(R = psut_mats$R[[1]], 
                                  U_feed = psut_mats$U_feed[[1]], 
                                  U_eiou = psut_mats$U_EIOU[[1]], 
                                  U = psut_mats$U[[1]], 
                                  r_eiou = psut_mats$r_eiou[[1]], 
                                  V = psut_mats$V[[1]], 
                                  Y = psut_mats$Y[[1]], 
                                  C_eiou = psut_mats$C_EIOU[[1]], 
                                  C_Y = psut_mats$C_Y[[1]], 
                                  eta_fu = psut_mats$eta.fu[[1]], 
                                  phi_u = psut_mats$phi.u[[1]])
  # Ensure that expected matrices are included.
  # There should be no more matrices than these.
  expect_equal(names(useful_mats), 
               c("U_feed_Useful", "U_EIOU_Useful", "U_Useful", 
                 "r_EIOU_Useful", "V_Useful", "Y_Useful"))
  
  # Try with C_eiou missing, thereby ignoring any EIOU.
  # Do the same calculation as above, but don't include 
  # the C_eiou argument.
  # This approach is expected to trip the imbalance warning, 
  # because we're removing EIOU energy from the ECC.
  extend_to_useful(R = psut_mats$R[[1]], 
                   U_feed = psut_mats$U_feed[[1]], 
                   U_eiou = psut_mats$U_EIOU[[1]], 
                   U = psut_mats$U[[1]], 
                   r_eiou = psut_mats$r_eiou[[1]], 
                   V = psut_mats$V[[1]], 
                   Y = psut_mats$Y[[1]], 
                   C_Y = psut_mats$C_Y[[1]], 
                   eta_fu = psut_mats$eta.fu[[1]], 
                   phi_u = psut_mats$phi.u[[1]]) %>% 
    expect_warning(regexp = "Energy is not balanced to within")
})


test_that("extend_to_useful() works with individual Matrix objects", {
  C_data <- load_fu_allocation_data() %>% 
    form_C_mats(matrix.class = "Matrix")
  eta_fu_data <- load_eta_fu_data() %>% 
    form_eta_fu_phi_u_vecs(matrix.class = "Matrix")
  m_cols <- eta_fu_data %>% 
    IEATools::meta_cols(return_names = TRUE,
                        years_to_keep = IEATools::iea_cols$year,
                        not_meta = c(IEATools::template_cols$eta_fu, IEATools::template_cols$phi_u))
  psut_mats <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    prep_psut(matrix.class = "Matrix") %>% 
    dplyr::full_join(C_data, by = m_cols) %>% 
    dplyr::full_join(eta_fu_data, by = m_cols)
  
  useful_mats <- extend_to_useful(R = psut_mats$R[[1]], 
                                  U_feed = psut_mats$U_feed[[1]], 
                                  U_eiou = psut_mats$U_EIOU[[1]], 
                                  U = psut_mats$U[[1]], 
                                  r_eiou = psut_mats$r_eiou[[1]], 
                                  V = psut_mats$V[[1]], 
                                  Y = psut_mats$Y[[1]], 
                                  C_eiou = psut_mats$C_EIOU[[1]], 
                                  C_Y = psut_mats$C_Y[[1]], 
                                  eta_fu = psut_mats$eta.fu[[1]], 
                                  phi_u = psut_mats$phi.u[[1]])
  # Ensure that expected matrices are included.
  # There should be no more matrices than these.
  expect_equal(names(useful_mats), 
               c("U_feed_Useful", "U_EIOU_Useful", "U_Useful", 
                 "r_EIOU_Useful", "V_Useful", "Y_Useful"))
  
  # Try with C_eiou missing, thereby ignoring any EIOU.
  # Do the same calculation as above, but don't include 
  # the C_eiou argument.
  # This approach is expected to trip the imbalance warning, 
  # because we're removing EIOU energy from the ECC.
  extend_to_useful(R = psut_mats$R[[1]], 
                   U_feed = psut_mats$U_feed[[1]], 
                   U_eiou = psut_mats$U_EIOU[[1]], 
                   U = psut_mats$U[[1]], 
                   r_eiou = psut_mats$r_eiou[[1]], 
                   V = psut_mats$V[[1]], 
                   Y = psut_mats$Y[[1]], 
                   C_Y = psut_mats$C_Y[[1]], 
                   eta_fu = psut_mats$eta.fu[[1]], 
                   phi_u = psut_mats$phi.u[[1]]) %>% 
    expect_warning(regexp = "Energy is not balanced to within")
})


test_that("extend_to_useful() works with list of matrices", {
  C_data <- load_fu_allocation_data() %>% 
    form_C_mats()
  eta_fu_data <- load_eta_fu_data() %>% 
    form_eta_fu_phi_u_vecs()
  m_cols <- eta_fu_data %>% 
    IEATools::meta_cols(return_names = TRUE,
                        years_to_keep = IEATools::iea_cols$year,
                        not_meta = c(IEATools::template_cols$eta_fu, IEATools::template_cols$phi_u))
  psut_mats <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    prep_psut() %>% 
    dplyr::full_join(C_data, by = m_cols) %>% 
    dplyr::full_join(eta_fu_data, by = m_cols)
  
  # Make a list out of the first row of matrices
  var_store <- as.list(psut_mats[1, ])
  
  useful_list <- extend_to_useful(var_store)
  
  # When a list is used as the data store, we should get all variables returned.
  expect_equal(names(useful_list), 
               c("Country", "Method", "Energy.type", "Last.stage", "Year",
                 "Y", "S_units", "R", "U", "U_feed",
                 "U_EIOU", "r_EIOU", "V", "C_EIOU", "C_Y",
                 "eta.fu", "phi.u", "U_feed_Useful", "U_EIOU_Useful", "U_Useful", 
                 "r_EIOU_Useful", "V_Useful", "Y_Useful"))
})


test_that("extend_to_useful() works with list of Matrix objects", {
  C_data <- load_fu_allocation_data() %>% 
    form_C_mats(matrix.class = "Matrix")
  eta_fu_data <- load_eta_fu_data() %>% 
    form_eta_fu_phi_u_vecs(matrix.class = "Matrix")
  m_cols <- eta_fu_data %>% 
    IEATools::meta_cols(return_names = TRUE,
                        years_to_keep = IEATools::iea_cols$year,
                        not_meta = c(IEATools::template_cols$eta_fu, IEATools::template_cols$phi_u))
  psut_mats <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    prep_psut(matrix.class = "Matrix") %>% 
    dplyr::full_join(C_data, by = m_cols) %>% 
    dplyr::full_join(eta_fu_data, by = m_cols)
  
  # Make a list out of the first row of matrices
  var_store <- as.list(psut_mats[1, ])
  
  useful_list <- extend_to_useful(var_store)
  # Ensure all are Matrix objects
  for (i in 6:length(useful_list)) {
    expect_true(matsbyname::is.Matrix(useful_list[[i]][[1]]))
  }
  
  # When a list is used as the data store, we should get all variables returned.
  expect_equal(names(useful_list), 
               c("Country", "Method", "Energy.type", "Last.stage", "Year",
                 "Y", "S_units", "R", "U", "U_feed",
                 "U_EIOU", "r_EIOU", "V", "C_EIOU", "C_Y",
                 "eta.fu", "phi.u", "U_feed_Useful", "U_EIOU_Useful", "U_Useful", 
                 "r_EIOU_Useful", "V_Useful", "Y_Useful"))
})


test_that("extend_to_useful() works as expected when clean_up_df = FALSE", {
  C_data <- load_fu_allocation_data() %>% 
    form_C_mats()
  eta_fu_data <- load_eta_fu_data() %>% 
    form_eta_fu_phi_u_vecs()
  m_cols <- eta_fu_data %>% 
    IEATools::meta_cols(return_names = TRUE,
                        years_to_keep = IEATools::iea_cols$year,
                        not_meta = c(IEATools::template_cols$eta_fu, IEATools::template_cols$phi_u))
  psut_mats <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    prep_psut() %>% 
    dplyr::full_join(C_data, by = m_cols) %>% 
    dplyr::full_join(eta_fu_data, by = m_cols)
  
  with_useful <- psut_mats %>% 
    extend_to_useful(clean_up_df = FALSE)
  
  # Check column names. We should get a lot of "_Useful"s here.
  expect_equal(names(with_useful), 
               c("Country", "Method", "Energy.type", "Last.stage", "Year",
                 "Y", "S_units", "R", "U", "U_feed",
                 "U_EIOU", "r_EIOU", "V", "C_EIOU", "C_Y",
                 "eta.fu", "phi.u", "U_feed_Useful", "U_EIOU_Useful", "U_Useful", 
                 "r_EIOU_Useful", "V_Useful", "Y_Useful"))
})


test_that("extend_to_useful() works as expected when clean_up_df = FALSE with Matrix objects", {
  C_data <- load_fu_allocation_data() |>  
    form_C_mats(matrix.class = "Matrix")
  eta_fu_data <- load_eta_fu_data() |> 
    form_eta_fu_phi_u_vecs(matrix.class = "Matrix")
  m_cols <- eta_fu_data |> 
    IEATools::meta_cols(return_names = TRUE,
                        years_to_keep = IEATools::iea_cols$year,
                        not_meta = c(IEATools::template_cols$eta_fu, IEATools::template_cols$phi_u))
  psut_mats <- load_tidy_iea_df() |> 
    specify_all() |> 
    prep_psut(matrix.class = "Matrix") |> 
    dplyr::full_join(C_data, by = m_cols) |> 
    dplyr::full_join(eta_fu_data, by = m_cols)
  
  with_useful <- psut_mats %>% 
    extend_to_useful(clean_up_df = FALSE)
  
  # Check column names. We should get a lot of "_Useful"s here.
  expect_equal(names(with_useful), 
               c("Country", "Method", "Energy.type", "Last.stage", "Year",
                 "Y", "S_units", "R", "U", "U_feed",
                 "U_EIOU", "r_EIOU", "V", "C_EIOU", "C_Y",
                 "eta.fu", "phi.u", "U_feed_Useful", "U_EIOU_Useful", "U_Useful", 
                 "r_EIOU_Useful", "V_Useful", "Y_Useful"))
})


test_that("extend_to_useful() works with empty lists", {
  C_data <- load_fu_allocation_data() |> 
    form_C_mats(matrix.class = "Matrix")
  eta_fu_data <- load_eta_fu_data() %>% 
    form_eta_fu_phi_u_vecs(matrix.class = "Matrix")
  m_cols <- eta_fu_data %>% 
    IEATools::meta_cols(return_names = TRUE,
                        years_to_keep = IEATools::iea_cols$year,
                        not_meta = c(IEATools::template_cols$eta_fu, IEATools::template_cols$phi_u))
  psut_mats <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    prep_psut(matrix.class = "Matrix") %>% 
    dplyr::full_join(C_data, by = m_cols) %>% 
    dplyr::full_join(eta_fu_data, by = m_cols)
  
  # Make a list out of the first row of matrices
  var_store <- as.list(psut_mats[1, ])
  var_store <- var_store[0]
  
  useful_list <- extend_to_useful(var_store)
  
  expect_null(useful_list)
})


test_that("extend_to_useful() returns NULL with empty data frames", {
  C_data <- load_fu_allocation_data() %>% 
    form_C_mats(matrix.class = "Matrix")
  eta_fu_data <- load_eta_fu_data() %>% 
    form_eta_fu_phi_u_vecs(matrix.class = "Matrix")
  m_cols <- eta_fu_data %>% 
    IEATools::meta_cols(return_names = TRUE,
                        years_to_keep = IEATools::iea_cols$year,
                        not_meta = c(IEATools::template_cols$eta_fu, IEATools::template_cols$phi_u))
  psut_mats <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    prep_psut(matrix.class = "Matrix") %>% 
    dplyr::full_join(C_data, by = m_cols) %>% 
    dplyr::full_join(eta_fu_data, by = m_cols)
  
  # Make a no-row data frame.
  psut_mats <- psut_mats[0, ]
  
  with_useful <- psut_mats %>% 
    extend_to_useful()
  
  expect_equal(nrow(with_useful), 0)
  
  expect_setequal(names(with_useful), c("Country", "Method", "Energy.type", "Last.stage", "Year", "Y", "S_units", 
                                        "R", "U", "U_feed", "U_EIOU", "r_EIOU", "V"))
})
