###########################################################
context("Completion functions")
###########################################################

test_that("tidy_fu_allocation_table() works", {
  fu_table <- load_fu_allocation_data()
  years <- year_cols(fu_table, return_names = TRUE)
  tidy <- tidy_fu_allocation_table(fu_table)
  expect_null(tidy[[IEATools::template_cols$maximum_values]])
  for (yr in years) {
    expect_null(tidy[[yr]])
  }
  tidy %>% 
    dplyr::filter(.data[[IEATools::template_cols$quantity]] %in% c(IEATools::iea_cols$e_dot, IEATools::template_cols$e_dot_perc)) %>% 
    nrow() %>% 
    expect_equal(0)
  
  # Test that the original is returned if it is already tidy
  tidy2 <- tidy_fu_allocation_table(tidy)
  expect_equal(tidy2, tidy)
})


test_that("complete_fu_allocation_table works as expected", {
  
  # In this test, the 2nd exemplar isn't needed, and the code should 
  # break out of the for loop.
  
  # The strategy here will be to use Ghana's FU allocation table
  # with South Africa's as an exemplar.
  # We'll remove a row from Ghana's table and make sure South Africa's is inserted.
  fu_table <- load_fu_allocation_data()
  
  fu_table_GHA <- fu_table %>% 
    dplyr::filter(Country == "GHA") %>% 
    # Delete rows from Ghana's table for Residential consumption of PSBs
    dplyr::filter(!(.data[[IEATools::iea_cols$flow_aggregation_point]] == IEATools::tfc_flows$other & 
                      .data[[IEATools::template_cols$ef_product]] == IEATools::biofuels_and_waste_products$primary_solid_biofuels & 
                      .data[[IEATools::template_cols$destination]] == IEATools::other_flows$residential))
  # Verify that those PSB rows have been deleted.
  fu_table_GHA %>% 
    dplyr::filter(.data[[IEATools::iea_cols$flow_aggregation_point]] == IEATools::tfc_flows$other, 
                  .data[[IEATools::template_cols$ef_product]] == IEATools::biofuels_and_waste_products$primary_solid_biofuels,
                  .data[[IEATools::template_cols$destination]] == IEATools::other_flows$residential) %>% 
    nrow() %>% 
    expect_equal(0)
  
  fu_table_ZAF <- fu_table %>% 
    dplyr::filter(Country == "ZAF")
  # Verify that the ZAF table has rows for Residential consumption of PSBs.
  fu_table_ZAF %>% 
    dplyr::filter(.data[[IEATools::iea_cols$flow_aggregation_point]] == IEATools::tfc_flows$other,
                  .data[[IEATools::template_cols$ef_product]] == IEATools::biofuels_and_waste_products$primary_solid_biofuels,
                  .data[[IEATools::template_cols$destination]] == IEATools::other_flows$residential) %>% 
    nrow() %>% 
    expect_gt(0)
  
  # Get the IEA data for GHA and ZAF and specify it.
  tidy_specified_iea_data <- load_tidy_iea_df() %>% 
    specify_all()
  
  # Now send the data into complete_fu_allocation_table()
  completed <- complete_fu_allocation_table(fu_allocation_table = fu_table_GHA, 
                                            # Give 2 exemplars so that we stress test the loop.
                                            exemplar_fu_allocation_tables = list(fu_table_ZAF, fu_table_ZAF), 
                                            tidy_specified_iea_data = tidy_specified_iea_data)
  completed %>% 
    dplyr::filter(.data[[IEATools::template_cols$c_source]] == "ZAF") %>% 
    nrow() %>% 
    expect_equal(4)
  # We should have four Ghana rows for PSBs in Residential
  completed %>% 
    dplyr::filter(Country == "GHA", 
                  .data[[IEATools::iea_cols$flow_aggregation_point]] == IEATools::tfc_flows$other, 
                  .data[[IEATools::template_cols$ef_product]] == IEATools::biofuels_and_waste_products$primary_solid_biofuels, 
                  .data[[IEATools::template_cols$destination]] == IEATools::other_flows$residential) %>% 
    nrow() %>% 
    expect_equal(4)
  # But their source should not be Ghana.
  completed %>% 
    dplyr::filter(.data[[IEATools::template_cols$c_source]] == "GHA", 
                  .data[[IEATools::iea_cols$flow_aggregation_point]] == IEATools::tfc_flows$other, 
                  .data[[IEATools::template_cols$ef_product]] == IEATools::biofuels_and_waste_products$primary_solid_biofuels, 
                  .data[[IEATools::template_cols$destination]] == IEATools::other_flows$residential) %>% 
    nrow() %>% 
    expect_equal(0)
  
  # Try with a missing allocation table.
  expect_error(complete_fu_allocation_table(exemplar_fu_allocation_tables = list(fu_table_ZAF, fu_table_ZAF), 
                                            tidy_specified_iea_data = tidy_specified_iea_data), 
               'argument "fu_allocation_table" is missing, with no default')
  
  # Try with missing exemplars
  expect_error(complete_fu_allocation_table(fu_allocation_table = fu_table_GHA, 
                                            tidy_specified_iea_data = tidy_specified_iea_data), 
               'argument "exemplar_fu_allocation_tables" is missing, with no default')
  
  # Try with missing IEA data
  expect_error(complete_fu_allocation_table(fu_allocation_table = fu_table_GHA, 
                                            # Give 2 exemplars so that we stress test the loop.
                                            exemplar_fu_allocation_tables = list(fu_table_ZAF, fu_table_ZAF)), 
               'argument "tidy_specified_iea_data" is missing, with no default')
})


test_that("complete_fu_allocation_table() works with a tidy incomplete fu table", {
  fu_table <- load_fu_allocation_data()
  year_columns <- year_cols(fu_table, return_names = TRUE)
  
  tidy_fu_table_GHA <- fu_table %>% 
    dplyr::filter(Country == "GHA") %>% 
    # Delete rows from Ghana's table for Residential consumption of PSBs
    dplyr::filter(!(.data[[IEATools::iea_cols$flow_aggregation_point]] == IEATools::tfc_flows$other & 
                      .data[[IEATools::template_cols$ef_product]] == IEATools::biofuels_and_waste_products$primary_solid_biofuels & 
                      .data[[IEATools::template_cols$destination]] == IEATools::other_flows$residential)) %>% 
    # Tidy it
    tidy_fu_allocation_table()
  
  fu_table_ZAF <- fu_table %>% 
    dplyr::filter(Country == "ZAF")

  # Get the IEA data for GHA and ZAF and specify it.
  tidy_specified_iea_data <- load_tidy_iea_df() %>% 
    specify_all()
  
  # Now send the data into complete_fu_allocation_table()
  completed <- complete_fu_allocation_table(fu_allocation_table = tidy_fu_table_GHA, 
                                            # Give 2 exemplars so that we stress test the loop.
                                            exemplar_fu_allocation_tables = list(fu_table_ZAF, fu_table_ZAF), 
                                            tidy_specified_iea_data = tidy_specified_iea_data)
  # Ensure that the correct rows have been picked up for GHA from ZAF.
  completed %>% 
    dplyr::filter(.data[[IEATools::template_cols$c_source]] == "ZAF") %>% 
    nrow() %>% 
    expect_equal(4)
})


test_that("complete_fu_allocation_table() works with 2 exemplars", {
  # In this test, we set up an allocation table that has two missing pieces.
  # The two missing pieces are obtained from 2 exemplars, one missing piece from each.
  
  fu_table <- load_fu_allocation_data()
  fu_table_GHA <- fu_table %>% 
    dplyr::filter(Country == "GHA") %>% 
    # Delete rows from Ghana's table for Residential consumption of PSBs
    dplyr::filter(!(.data[[IEATools::iea_cols$flow_aggregation_point]] == IEATools::tfc_flows$other & 
                      .data[[IEATools::template_cols$ef_product]] == IEATools::biofuels_and_waste_products$primary_solid_biofuels & 
                      .data[[IEATools::template_cols$destination]] == IEATools::other_flows$residential)) %>% 
    dplyr::filter(!(.data[[IEATools::iea_cols$flow_aggregation_point]] == IEATools::tfc_compare_flows$energy_industry_own_use &
                      .data[[IEATools::template_cols$ef_product]] == IEATools::electricity_products$electricity &
                      .data[[IEATools::template_cols$destination]] == IEATools::transformation_processes$main_activity_producer_electricity_plants))
  # Ensure that we removed the correct rows from Ghana.
  fu_table_GHA %>% 
    dplyr::filter(.data[[IEATools::iea_cols$flow_aggregation_point]] == IEATools::tfc_flows$other & 
                    .data[[IEATools::template_cols$ef_product]] == IEATools::biofuels_and_waste_products$primary_solid_biofuels & 
                    .data[[IEATools::template_cols$destination]] == IEATools::other_flows$residential) %>% 
    nrow() %>% 
    expect_equal(0)
  fu_table_GHA %>% 
    dplyr::filter(.data[[IEATools::iea_cols$flow_aggregation_point]] == IEATools::tfc_compare_flows$energy_industry_own_use &
                    .data[[IEATools::template_cols$ef_product]] == IEATools::electricity_products$electricity &
                    .data[[IEATools::template_cols$destination]] == IEATools::transformation_processes$main_activity_producer_electricity_plants) %>% 
    nrow() %>% 
    expect_equal(0)
  
  # Now make two exemplars from South Africa, one with country ZAF, the other whose country is renamed to "World".
  # South Africa has EIOU Electricity consumed by Main activity producer electricity plants but NO Residential PSB consumption.
  # World has Residential PSB consumption but NO EIOU Electricity consumed by Main activity producer electricity plants.
  # Ghana will pick up EIOU Electricity consumed by Main activity producer electricity plants from South Africa.
  # Ghana will pick up Residential PSB consumption from World.
  fu_table_ZAF <- fu_table %>% 
    dplyr::filter(Country == "ZAF") %>% 
    dplyr::filter(!(.data[[IEATools::iea_cols$flow_aggregation_point]] == IEATools::tfc_flows$other & 
                      .data[[IEATools::template_cols$ef_product]] == IEATools::biofuels_and_waste_products$primary_solid_biofuels & 
                      .data[[IEATools::template_cols$destination]] == IEATools::other_flows$residential))
  # Ensure we deleted correct rows from this table.
  fu_table_ZAF %>% 
    dplyr::filter(.data[[IEATools::iea_cols$flow_aggregation_point]] == IEATools::tfc_flows$other,
                  .data[[IEATools::template_cols$ef_product]] == IEATools::biofuels_and_waste_products$primary_solid_biofuels,
                  .data[[IEATools::template_cols$destination]] == IEATools::other_flows$residential) %>% 
    nrow() %>% 
    expect_equal(0)
  
  # Second exemplar
  fu_table_World <- fu_table %>% 
    dplyr::filter(Country == "ZAF") %>% 
    dplyr::mutate(
      Country = "World"
    ) %>% 
    dplyr::filter(!(.data[[IEATools::iea_cols$flow_aggregation_point]] == IEATools::tfc_compare_flows$energy_industry_own_use &
                      .data[[IEATools::template_cols$ef_product]] == IEATools::electricity_products$electricity &
                      .data[[IEATools::template_cols$destination]] == IEATools::transformation_processes$main_activity_producer_electricity_plants))
  # Ensure we removed the correct rows from the World exemplar.
  fu_table_World %>% 
    dplyr::filter(.data[[IEATools::iea_cols$flow_aggregation_point]] == IEATools::tfc_compare_flows$energy_industry_own_use,
                  .data[[IEATools::template_cols$ef_product]] == IEATools::electricity_products$electricity,
                  .data[[IEATools::template_cols$destination]] == IEATools::transformation_processes$main_activity_producer_electricity_plants) %>% 
    nrow() %>% 
    expect_equal(0)
  fu_table_World %>% 
    magrittr::extract2(IEATools::iea_cols$country) %>% 
    unique() %>% 
    expect_equal("World")
  
  # Get the IEA data for GHA and ZAF and specify it.
  tidy_specified_iea_data <- load_tidy_iea_df() %>% 
    specify_all()
  
  # Now run Ghana through the completion process.
  completed <- complete_fu_allocation_table(fu_allocation_table = fu_table_GHA, 
                                            exemplar_fu_allocation_tables = list(fu_table_ZAF, fu_table_World), 
                                            tidy_specified_iea_data = tidy_specified_iea_data)
  
  # Check that Ghana obtained Residential PSB consumption from World.
  completed %>% 
    dplyr::filter(.data[[IEATools::iea_cols$flow_aggregation_point]] == IEATools::tfc_flows$other, 
                  .data[[IEATools::template_cols$ef_product]] == IEATools::biofuels_and_waste_products$primary_solid_biofuels,
                  .data[[IEATools::template_cols$destination]] == IEATools::other_flows$residential) %>% 
    magrittr::extract2(IEATools::template_cols$c_source) %>% 
    unique() %>% 
    expect_equal("World")
  # Check that Ghana obtained EIOU Electricity consumed by Main activity producer electricity plants from South Africa. 
  completed %>% 
    dplyr::filter(.data[[IEATools::iea_cols$flow_aggregation_point]] == IEATools::tfc_compare_flows$energy_industry_own_use,
                  .data[[IEATools::template_cols$ef_product]] == IEATools::electricity_products$electricity,
                  .data[[IEATools::template_cols$destination]] == IEATools::transformation_processes$main_activity_producer_electricity_plants) %>% 
    magrittr::extract2(IEATools::template_cols$c_source) %>% 
    expect_equal(c("ZAF", "ZAF"))
  
  # Try again without enough information to complete the FU Allocation table.
  # Create this situation by dropping the fu_table_World from the list of exemplars.
  # In this situation, we cannot allocate the Residential PSB rows for Ghana.
  # This attempt should emit a warning.
  expect_warning(complete_failure <- complete_fu_allocation_table(fu_allocation_table = fu_table_GHA, 
                                                                  exemplar_fu_allocation_tables = fu_table_ZAF, 
                                                                  tidy_specified_iea_data = tidy_specified_iea_data), 
                 "Didn't complete FU Allocation table for GHA. Returning a data frame of final energy that wasn't allocated.")
  expect_equal(complete_failure[[IEATools::template_cols$ef_product]] %>% unique(), IEATools::biofuels_and_waste_products$primary_solid_biofuels)
})


test_that("fu_allocation_table_completed() works as expected", {
  iea_data <- load_tidy_iea_df() %>% 
    specify_all()
  fu_allocations <- load_fu_allocation_data()
  expect_true(fu_allocation_table_completed(fu_allocations, iea_data))
  
  # Remove a row from fu_allocations. Now the test for completion should fail.
  # In fact, remove a row that has a 1 in it.
  expect_false(fu_allocation_table_completed(fu_allocations[-3, ], iea_data))
  
  # Now remove a row that has a fraction.
  expect_false(fu_allocation_table_completed(fu_allocations[-8, ], iea_data))
  
  # Try with a wide IEAData data frame.
  expect_true(fu_allocation_table_completed(fu_allocations, iea_data %>% 
                                              tidyr::pivot_wider(names_from = IEATools::iea_cols$year, 
                                                                 values_from = IEATools::iea_cols$e_dot)))
})


test_that("tidy_eta_fu_table() works", {
  eta_table <- load_eta_fu_data()
  years <- year_cols(eta_table, return_names = TRUE)
  tidy <- tidy_eta_fu_table(eta_table)
  expect_null(tidy[[IEATools::template_cols$maximum_values]])
  for (yr in years) {
    expect_null(tidy[[yr]])
  }
  tidy %>% 
    dplyr::filter(.data[[IEATools::template_cols$quantity]] %in% c(IEATools::template_cols$e_dot_machine, IEATools::template_cols$e_dot_machine_perc)) %>% 
    nrow() %>% 
    expect_equal(0)
  
  # Test that the original is returned if it is already tidy
  tidy2 <- tidy_eta_fu_table(tidy)
  expect_equal(tidy2, tidy)
})


test_that("complete_eta_fu_table works as expected", {
  eta_fu_table <- load_eta_fu_data()
  eta_fu_table_GHA <- eta_fu_table %>% 
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "GHA")
  eta_fu_table_ZAF <- eta_fu_table %>% 
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "ZAF")
  
  fu_allocation_table <- load_fu_allocation_data() %>% 
    tidy_fu_allocation_table()
    
  fu_allocation_table_GHA <- fu_allocation_table %>% 
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "GHA") 
  fu_allocation_table_ZAF <- fu_allocation_table %>% 
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "ZAF")
    
  # Use ZAF as an exemplar for GHA.  
  # Check that the machines are present, initially.
  eta_fu_table_GHA %>% 
    dplyr::filter(.data[[IEATools::template_cols$machine]] == "Automobiles") %>% 
    nrow() %>% 
    expect_equal(4)
  eta_fu_table_GHA %>% 
    dplyr::filter(.data[[IEATools::template_cols$machine]] == "Irons") %>% 
    nrow() %>% 
    expect_equal(4)
  
  # Eliminate 2 machines (Automobiles and Irons) from GHA and see if their efficiencies get picked up from ZAF and World.
  eta_fu_table_GHA_incomplete <- eta_fu_table_GHA %>% 
    dplyr::filter(.data[[IEATools::template_cols$machine]] != "Automobiles", 
                  .data[[IEATools::template_cols$machine]] != "Irons")
  # Ensure that we eliminated the desired rows
  eta_fu_table_GHA_incomplete %>% 
    dplyr::filter(.data[[IEATools::template_cols$machine]] %in% c("Automobiles", "Irons")) %>% 
    nrow() %>% 
    expect_equal(0)

  # Make exemplar tables from ZAF.
  # The first exemplar (ZAF) will have Automobiles but not Irons.
  exemplar_ZAF <- eta_fu_table_ZAF %>% 
    dplyr::filter(.data[[IEATools::template_cols$machine]] != "Irons")
  # Verify we deleted the right rows.
  exemplar_ZAF %>% 
    dplyr::filter(.data[[IEATools::template_cols$machine]] == "Irons") %>% 
    nrow() %>% 
    expect_equal(0)
  
  # The second exemplar (World) will have Irons, but not Automobiles.
  exemplar_World <- eta_fu_table_ZAF %>% 
    dplyr::filter(.data[[IEATools::template_cols$machine]] != "Automobiles") %>% 
    dplyr::mutate(
      "{IEATools::iea_cols$country}" := "World"
    )
  # Verify we deleted the right rows.
  exemplar_World %>% 
    dplyr::filter(.data[[IEATools::template_cols$machine]] == "Automobiles") %>% 
    nrow() %>% 
    expect_equal(0)

  # Try to complete with ONLY ZAF.
  # Should find that a warning is emitted, because we can't find Irons
  expect_warning(complete_fail <- complete_eta_fu_table(eta_fu_table = eta_fu_table_GHA_incomplete,
                                                        exemplar_eta_fu_tables = exemplar_ZAF, 
                                                        fu_allocation_table = fu_allocation_table_GHA), 
                 "Didn't complete eta FU table for GHA. Returning a data frame of machines for which an efficiency wasn't available.")
  # Check that the uncompleted machine is Irons.
  expect_equal(nrow(complete_fail), 4)
  complete_fail %>% 
    dplyr::filter(.data[[IEATools::template_cols$machine]] == "Irons") %>% 
    magrittr::extract2(IEATools::template_cols$machine) %>% 
    unique() %>% 
    expect_equal("Irons")
    
  # Now call the completion function to pick up Automobiles from ZAF and Irons from World
  completed <- complete_eta_fu_table(eta_fu_table = eta_fu_table_GHA_incomplete,
                                     exemplar_eta_fu_tables = list(exemplar_ZAF, exemplar_World), 
                                     fu_allocation_table = fu_allocation_table_GHA)
  
  # Check that we got Automobiles from ZAF
  completed %>% 
    dplyr::filter(.data[[IEATools::template_cols$machine]] == "Automobiles", 
                  .data[[IEATools::template_cols$eta_fu_phi_u_source]] == "ZAF") %>% 
    nrow() %>% 
    expect_equal(4)
  completed %>% 
    dplyr::filter(.data[[IEATools::template_cols$machine]] == "Automobiles", 
                  .data[[IEATools::template_cols$quantity]] == IEATools::template_cols$eta_fu, 
                  .data[[IEATools::iea_cols$year]] == 1971) %>% 
    magrittr::extract2(IEATools::template_cols$.values) %>% 
    expect_equal(0.3)
  completed %>% 
    dplyr::filter(.data[[IEATools::template_cols$machine]] == "Automobiles", 
                  .data[[IEATools::template_cols$quantity]] == IEATools::template_cols$eta_fu, 
                  .data[[IEATools::iea_cols$year]] == 2000) %>% 
    magrittr::extract2(IEATools::template_cols$.values) %>% 
    expect_equal(0.31)
  completed %>% 
    dplyr::filter(.data[[IEATools::template_cols$machine]] == "Automobiles", 
                  .data[[IEATools::template_cols$quantity]] == IEATools::template_cols$phi_u, 
                  .data[[IEATools::iea_cols$year]] == 1971) %>% 
    magrittr::extract2(IEATools::template_cols$.values) %>% 
    expect_equal(1)
  completed %>% 
    dplyr::filter(.data[[IEATools::template_cols$machine]] == "Automobiles", 
                  .data[[IEATools::template_cols$quantity]] == IEATools::template_cols$phi_u, 
                  .data[[IEATools::iea_cols$year]] == 2000) %>% 
    magrittr::extract2(IEATools::template_cols$.values) %>% 
    expect_equal(1)
  
  # Check that we got Irons from World
  completed %>% 
    dplyr::filter(.data[[IEATools::template_cols$machine]] == "Irons", 
                  .data[[IEATools::template_cols$eta_fu_phi_u_source]] == "World") %>% 
    nrow() %>% 
    expect_equal(4)
  completed %>% 
    dplyr::filter(.data[[IEATools::template_cols$machine]] == "Irons", 
                  .data[[IEATools::template_cols$quantity]] == IEATools::template_cols$eta_fu, 
                  .data[[IEATools::iea_cols$year]] == 1971) %>% 
    magrittr::extract2(IEATools::template_cols$.values) %>% 
    expect_equal(0.3)
  completed %>% 
    dplyr::filter(.data[[IEATools::template_cols$machine]] == "Irons", 
                  .data[[IEATools::template_cols$quantity]] == IEATools::template_cols$eta_fu, 
                  .data[[IEATools::iea_cols$year]] == 2000) %>% 
    magrittr::extract2(IEATools::template_cols$.values) %>% 
    expect_equal(0.35)
  completed %>% 
    dplyr::filter(.data[[IEATools::template_cols$machine]] == "Irons", 
                  .data[[IEATools::template_cols$quantity]] == IEATools::template_cols$phi_u, 
                  .data[[IEATools::iea_cols$year]] == 1971) %>% 
    magrittr::extract2(IEATools::template_cols$.values) %>% 
    expect_equal(0.3698615661)
  completed %>% 
    dplyr::filter(.data[[IEATools::template_cols$machine]] == "Irons", 
                  .data[[IEATools::template_cols$quantity]] == IEATools::template_cols$phi_u, 
                  .data[[IEATools::iea_cols$year]] == 2000) %>% 
    magrittr::extract2(IEATools::template_cols$.values) %>% 
    expect_equal(0.3698615661)
  
  # Try using two eta_fu_table_ZAF's as the exemplars. 
  # It should get everything it needs from the first one, thereafter hitting the break statement
  completed2 <- complete_eta_fu_table(eta_fu_table = eta_fu_table_GHA_incomplete,
                                      exemplar_eta_fu_tables = list(eta_fu_table_ZAF, eta_fu_table_ZAF), 
                                      fu_allocation_table = fu_allocation_table_GHA, 
                                      which_quantity = "eta.fu")
  # Check that completed2 has obtained both Automobiles and Irons from ZAF.
  completed2 %>% 
    dplyr::filter(.data[[IEATools::template_cols$eta_fu_phi_u_source]] == "ZAF", 
                  .data[[IEATools::template_cols$quantity]] == IEATools::template_cols$eta_fu) %>% 
    nrow() %>% 
    expect_equal(4)
  completed2 %>% 
    dplyr::filter(.data[[IEATools::template_cols$eta_fu_phi_u_source]] == "ZAF", 
                  .data[[IEATools::template_cols$quantity]] == IEATools::template_cols$eta_fu) %>% 
    magrittr::extract2(IEATools::template_cols$machine) %>% 
    expect_equal(c("Automobiles", "Automobiles", "Irons", "Irons"))
    
})


test_that("eta_fu_table_completed() works as expected", {
  fu_allocations <- load_fu_allocation_data()
  fu_efficiencies <- load_eta_fu_data()
  expect_true(eta_fu_table_completed(fu_efficiencies, fu_allocations))
  
  # Remove a couple rows to get a FALSE return value.
  expect_false(eta_fu_table_completed(fu_efficiencies[-3, ], fu_allocations))
  
  # Try with a tall allocations data frame
  expect_true(eta_fu_table_completed(fu_efficiencies, 
                                     fu_allocations %>% tidyr::pivot_longer(cols = year_cols(fu_allocations), 
                                                                            names_to = IEATools::iea_cols$year, 
                                                                            values_to = IEATools::template_cols$.values)))
  # Try with a tall efficiencies data frame
  expect_true(eta_fu_table_completed(fu_efficiencies %>% tidyr::pivot_longer(cols = year_cols(fu_efficiencies), 
                                                                             names_to = IEATools::iea_cols$year, 
                                                                             values_to = IEATools::template_cols$.values), 
                                     fu_allocations))
  
  # Try with a NULL fu_efficiencies
  expect_false(eta_fu_table_completed(fu_allocation_table = fu_allocations))
  expect_true(!is.null(eta_fu_table_completed(fu_allocation_table = fu_allocations) %>% 
                         attr("unallocated_rows")))
})

