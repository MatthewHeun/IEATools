###########################################################
context("Completion functions")
###########################################################


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
    dplyr::filter(!(Flow.aggregation.point == IEATools::tfc_flows$other & 
                      Ef.product == IEATools::biofuels_and_waste_products$primary_solid_biofuels & 
                      Destination == IEATools::other_flows$residential))
  # Verify that those PSB rows have been deleted.
  fu_table_GHA %>% 
    dplyr::filter(Flow.aggregation.point == IEATools::tfc_flows$other, 
                  Ef.product == IEATools::biofuels_and_waste_products$primary_solid_biofuels,
                  Destination == IEATools::other_flows$residential) %>% 
    nrow() %>% 
    expect_equal(0)
  
  fu_table_ZAF <- fu_table %>% 
    dplyr::filter(Country == "ZAF")
  # Verify that the ZAF table has rows for Residential consumption of PSBs.
  fu_table_ZAF %>% 
    dplyr::filter(Flow.aggregation.point == IEATools::tfc_flows$other,
                  Ef.product == IEATools::biofuels_and_waste_products$primary_solid_biofuels,
                  Destination == IEATools::other_flows$residential) %>% 
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
    dplyr::filter(C_source == "ZAF") %>% 
    nrow() %>% 
    expect_equal(2)
  # We should have two Ghana rows for PSBs in Residential
  completed %>% 
    dplyr::filter(Country == "GHA", 
                  Flow.aggregation.point == IEATools::tfc_flows$other, 
                  Ef.product == IEATools::biofuels_and_waste_products$primary_solid_biofuels, 
                  Destination == IEATools::other_flows$residential) %>% 
    nrow() %>% 
    expect_equal(2)
  # But their source should not be Ghana.
  completed %>% 
    dplyr::filter(C_source == "GHA", 
                  Flow.aggregation.point == IEATools::tfc_flows$other, 
                  Ef.product == IEATools::biofuels_and_waste_products$primary_solid_biofuels, 
                  Destination == IEATools::other_flows$residential) %>% 
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


test_that("complete_fu_allocation_table works as expected with 2 exemplars", {
  # In this test, we set up an allocation table that has two missing pieces.
  # The two missing pieces are obtained from 2 exemplars, one missing piece from each.
  
  fu_table <- load_fu_allocation_data()
  fu_table_GHA <- fu_table %>% 
    dplyr::filter(Country == "GHA") %>% 
    # Delete rows from Ghana's table for Residential consumption of PSBs
    dplyr::filter(!(Flow.aggregation.point == IEATools::tfc_flows$other & 
                      Ef.product == IEATools::biofuels_and_waste_products$primary_solid_biofuels & 
                      Destination == IEATools::other_flows$residential)) %>% 
    dplyr::filter(!(Flow.aggregation.point == IEATools::tfc_compare_flows$energy_industry_own_use &
                      Ef.product == IEATools::electricity_products$electricity &
                      Destination == IEATools::transformation_processes$main_activity_producer_electricity_plants))
  # Ensure that we removed the correct rows from Ghana.
  fu_table_GHA %>% 
    dplyr::filter(Flow.aggregation.point == IEATools::tfc_flows$other & 
                    Ef.product == IEATools::biofuels_and_waste_products$primary_solid_biofuels & 
                    Destination == IEATools::other_flows$residential) %>% 
    nrow() %>% 
    expect_equal(0)
  fu_table_GHA %>% 
    dplyr::filter(Flow.aggregation.point == IEATools::tfc_compare_flows$energy_industry_own_use &
                    Ef.product == IEATools::electricity_products$electricity &
                    Destination == IEATools::transformation_processes$main_activity_producer_electricity_plants) %>% 
    nrow() %>% 
    expect_equal(0)
  
  # Now make two exemplars from South Africa, one with country ZAF, the other whose country is renamed to "World".
  # South Africa has EIOU Electricity consumed by Main activity producer electricity plants but NO Residential PSB consumption.
  # World has Residential PSB consumption but NO EIOU Electricity consumed by Main activity producer electricity plants.
  # Ghana will pick up EIOU Electricity consumed by Main activity producer electricity plants from South Africa.
  # Ghana will pick up Residential PSB consumption from World.
  fu_table_ZAF <- fu_table %>% 
    dplyr::filter(Country == "ZAF") %>% 
    dplyr::filter(!(Flow.aggregation.point == IEATools::tfc_flows$other & 
                      Ef.product == IEATools::biofuels_and_waste_products$primary_solid_biofuels & 
                      Destination == IEATools::other_flows$residential))
  # Ensure we deleted correct rows from this table.
  fu_table_ZAF %>% 
    dplyr::filter(Flow.aggregation.point == IEATools::tfc_flows$other,
                  Ef.product == IEATools::biofuels_and_waste_products$primary_solid_biofuels,
                  Destination == IEATools::other_flows$residential) %>% 
    nrow() %>% 
    expect_equal(0)
  
  # Second exemplar
  fu_table_World <- fu_table %>% 
    dplyr::filter(Country == "ZAF") %>% 
    dplyr::mutate(
      Country = "World"
    ) %>% 
    dplyr::filter(!(Flow.aggregation.point == IEATools::tfc_compare_flows$energy_industry_own_use &
                      Ef.product == IEATools::electricity_products$electricity &
                      Destination == IEATools::transformation_processes$main_activity_producer_electricity_plants))
  # Ensure we removed the correct rows from the World exemplar.
  fu_table_World %>% 
    dplyr::filter(Flow.aggregation.point == IEATools::tfc_compare_flows$energy_industry_own_use,
                  Ef.product == IEATools::electricity_products$electricity,
                  Destination == IEATools::transformation_processes$main_activity_producer_electricity_plants) %>% 
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
    dplyr::filter(Flow.aggregation.point == IEATools::tfc_flows$other, 
                  Ef.product == IEATools::biofuels_and_waste_products$primary_solid_biofuels,
                  Destination == IEATools::other_flows$residential) %>% 
    magrittr::extract2(IEATools::template_cols$c_source) %>% 
    unique() %>% 
    expect_equal("World")
  # Check that Ghana obtained EIOU Electricity consumed by Main activity producer electricity plants from South Africa. 
  completed %>% 
    dplyr::filter(Flow.aggregation.point == IEATools::tfc_compare_flows$energy_industry_own_use,
                  Ef.product == IEATools::electricity_products$electricity,
                  Destination == IEATools::transformation_processes$main_activity_producer_electricity_plants) %>% 
    magrittr::extract2(IEATools::template_cols$c_source) %>% 
    expect_equal("ZAF")
  
  # Try again without enough information to complete the FU Allocation table.
  # Create this situation by dropping the fu_table_World from the list of exemplars.
  # In this situation, we cannot allocate the Residential PSB rows for Ghana.
  # This attempt should emit a warning.
  expect_warning(complete_failure <- complete_fu_allocation_table(fu_allocation_table = fu_table_GHA, 
                                                                  exemplar_fu_allocation_tables = list(fu_table_ZAF), 
                                                                  tidy_specified_iea_data = tidy_specified_iea_data), 
                 "Didn't complete FU Allocation table for GHA. Returning a data frame of final energy that wasn't allocated.")
  expect_equal(complete_failure[[IEATools::template_cols$ef_product]] %>% unique(), IEATools::biofuels_and_waste_products$primary_solid_biofuels)
})


test_that("complete_eta_fu_table works as expected", {
  eta_fu_table <- load_eta_fu_data()
  eta_fu_table_GHA <- eta_fu_table %>% 
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "GHA")
  eta_fu_table_ZAF <- eta_fu_table %>% 
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "ZAF")
  
  fu_allocation_table <- load_fu_allocation_data()
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
  
  # Eliminate 2 machines (Automobiles and Irons) from GHA and see if their efficiencies get picked up from ZAF.
  eta_fu_table_GHA_incomplete <- eta_fu_table_GHA %>% 
    dplyr::filter(.data[[IEATools::template_cols$machine]] != "Automobiles", .data[[IEATools::template_cols$machine]] != "Irons")
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
    dplyr::filter(.data[[IEATools::template_cols$machine]] != "Automobiles")
  # Verify we deleted the right rows.
  exemplar_World %>% 
    dplyr::filter(.data[[IEATools::template_cols$machine]] == "Automobiles") %>% 
    nrow() %>% 
    expect_equal(0)

  # Now call the completion function to pick up Automobiles from ZAF and Irons from World
  completed <- complete_eta_fu_table(eta_fu_table = eta_fu_table_GHA_incomplete,
                                     exemplar_eta_fu_tables = list(exemplar_ZAF, exemplar_World), 
                                     fu_allocation_table = fu_allocation_table_GHA)
  
  # Check that we got Automobiles from ZAF
  completed %>% 
    dplyr::filter(.data[[IEATools::template_cols$machine]] == "Automobiles") %>% 
    nrow() %>% 
    expect_equal(1)
  completed %>% 
    dplyr::filter(.data[[IEATools::template_cols$machine]] == "Irons") %>% 
    nrow() %>% 
    expect_equal(1)
})


