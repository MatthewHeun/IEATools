###########################################################
context("Template functions")
###########################################################

# This function tests final-to-useful allocation templates
# created from the default data set, filled or not.
check_fu_allocation_template <- function(.DF){
  expect_equal(.DF$Flow.aggregation.point[[1]], "Energy industry own use")
  expect_equal(.DF$Ef.product[[1]], "Refinery gas")
  expect_equal(.DF$Destination[[1]], "Oil refineries")
  expect_equal(.DF$Quantity[[1]], "E.dot")
  last_row <- nrow(.DF)
  expect_equal(.DF$Flow.aggregation.point[[last_row]], "Non-energy use")
  expect_equal(.DF$Ef.product[[last_row]], "Paraffin waxes")
  expect_equal(.DF$Destination[[last_row]], "Non-energy use industry/transformation/energy")
  expect_equal(.DF$Quantity[[last_row]], "C_4 [%]")
}


test_that("openxlsx works as expected", {
  # These are just tests for me to understand the openxlsx package.
  Tidy_iea_df <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    sort_iea_df()
  # Get a temporary file in which to write two data frames on different tabs.
  f <- tempfile(fileext = ".xslx")
  # Write the data from both years, each on its own tab.
  openxlsx::write.xlsx(list(y1971 = Tidy_iea_df %>% 
                              dplyr::filter(Year == 1971), 
                            y2k = Tidy_iea_df %>% 
                              dplyr::filter(Year == 2000)),
                       file = f)
  # Read the data back in, one sheet at a time.
  y1971 <- openxlsx::read.xlsx(f, sheet = "y1971")
  y2k <- openxlsx::read.xlsx(f, sheet = "y2k")
  # And recombine into a single tibble
  Rebuild <- dplyr::bind_rows(y1971, y2k) %>% 
    dplyr::as_tibble()
  # And we should get back what we wrote.
  expect_equal(Rebuild, Tidy_iea_df)
  # Clean up
  if (file.exists(f)) {
    file.remove(f)
  }
})


test_that("fu_allocation_template works as expected", {
  Allocation_template <- load_tidy_iea_df() %>% 
    specify_all() %>%
    fu_allocation_template() %>% 
    arrange_iea_fu_allocation_template()

  # Check rows
  check_fu_allocation_template(Allocation_template)

  # Check columns
  expected_colorder <- c("Country", "Method", "Energy.type", "Last.stage", "Ledger.side", "Flow.aggregation.point", "Unit",
                         "Ef.product", "Machine", "Eu.product", "Destination", 
                         "Quantity", "Maximum.values", "1971", "2000")
  expect_equal(names(Allocation_template), expected_colorder)
  expect_true(all(Allocation_template$Ledger.side == "Consumption" | Allocation_template$Flow.aggregation.point == "Energy industry own use"))
})


test_that("write_fu_allocation_template works as expected", {
  FU_allocation_template <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    fu_allocation_template()
  # Get a temporary file in which to write two data frames on different tabs.
  f <- tempfile(fileext = ".xlsx")
  p <- FU_allocation_template %>% 
    write_fu_allocation_template(f)
  expect_equal(p, f)
  # Now read the tabs back in
  Allocations <- openxlsx::read.xlsx(f, sheet = "FU Allocations") %>% 
    dplyr::rename(
      Maximum.values.reread = Maximum.values,
      `1971.reread` = `1971`,
      `2000.reread` = `2000`
    )
  # Check the tabs to make sure they're the same
  Expected_allocations <- FU_allocation_template
  Joined <- dplyr::full_join(Allocations, Expected_allocations, by = c("Country", "Method", "Energy.type", 
                                                                       "Last.stage", "Ledger.side", "Flow.aggregation.point", 
                                                                       "Unit", "Ef.product", "Machine", 
                                                                       "Eu.product", "Destination", "Quantity")) %>% 
    dplyr::mutate(
      Maximum.values.diff = Maximum.values.reread - Maximum.values,
      `1971_diff` = `1971.reread` - `1971`,
      `2000_diff` = `2000.reread` - `2000`, 
      Maximum.values.OK = abs(Maximum.values.diff) < 1e-6,
      `1971_diff_OK` = abs(`1971_diff`) < 1e-6,
      `2000_diff_OK` = abs(`2000_diff`) < 1e-6
    )
  expect_true(all(Joined$Maximum.values.OK == TRUE |  is.na(Joined$Maximum.values.OK)))
  expect_true(all(Joined$`1971_diff_OK` == TRUE |  is.na(Joined$`1971_diff_OK`)))
  expect_true(all(Joined$`2000_diff_OK` == TRUE |  is.na(Joined$`2000_diff_OK`)))
  
  # Now try to write it again.
  expect_true(file.exists(f))
  expect_error(FU_allocation_template %>% write_fu_allocation_template(p), "File already exists!")
  # Clean up
  if (file.exists(f)) {
    file.remove(f)
  }
  
  # Try with a file name that lacks the .xlsx extension.
  f2 <- tempfile() # No extension
  p2 <- FU_allocation_template %>% 
    write_fu_allocation_template(f2)
  # Check that the .xlsx extension was added to p.
  expect_true(endsWith(p2, ".xlsx"))
  expect_true(startsWith(p2, f2))
  # Clean up
  if (file.exists(f2)) {
    file.remove(f2)
  }
})


test_that("load_fu_allocation_data works as expected", {
  # Load default data and check the filled table.
  load_fu_allocation_data() %>% 
    check_fu_allocation_template()
})


test_that("eta_fu_template works as expected for 2018 data", {
  # Try for 2018 data
  # Use the default sorting (by Eu.product)
  Eta_fu_template_2018 <- load_fu_allocation_data(sample_fu_allocation_table_path(2018)) %>% 
    eta_fu_template()
  expect_equal(Eta_fu_template_2018$Machine[[1]], "Automobiles")
  expect_equal(Eta_fu_template_2018$Machine[[nrow(Eta_fu_template_2018)]], "Oil furnaces")
  expect_equal(as.character(Eta_fu_template_2018$Quantity[[1]]), "E.dot_machine")
  expect_equal(as.character(Eta_fu_template_2018$Quantity[[nrow(Eta_fu_template_2018)]]), "phi.u")
  
  eu_products <- Eta_fu_template_2018$Eu.product %>% unique() %>% as.character()
  # Check that the order is as expected.
  expect_equivalent(eu_products, c("MD", "Light", "HTH.600.C", "MTH.200.C", "MTH.100.C", "LTH.20.C"))
  # Check the class of the year columns. They should be numeric.
  expect_true(is.numeric(Eta_fu_template_2018[["1971"]]))
  expect_true(is.numeric(Eta_fu_template_2018[["2000"]]))
  
  # Now try to sort by importance
  Eta_fu_template_2018_2 <- load_fu_allocation_data(sample_fu_allocation_table_path(2018)) %>% 
    eta_fu_template(sort_by = "importance")
  expect_equal(Eta_fu_template_2018_2$Machine[[1]], "Wood stoves")
  expect_equal(Eta_fu_template_2018_2$Machine[[nrow(Eta_fu_template_2018_2)]], "Gas heaters")
  expect_equal(as.character(Eta_fu_template_2018_2$Quantity[[1]]), "E.dot_machine")
  expect_equal(as.character(Eta_fu_template_2018_2$Quantity[[nrow(Eta_fu_template_2018_2)]]), "phi.u")
  
  eu_products2 <- Eta_fu_template_2018_2$Eu.product %>% unique() %>% as.character()
  # Check that the order is as expected.
  expect_equivalent(eu_products2, c("MTH.100.C", "MTH.200.C", "MD", "HTH.600.C", "Light", "LTH.20.C"))
  # Check the class of the year columns. They should be numeric.
  expect_true(is.numeric(Eta_fu_template_2018_2[["1971"]]))
  expect_true(is.numeric(Eta_fu_template_2018_2[["2000"]]))
})


test_that("eta_fu_template works as expected for 2019 data", {
  # Try for 2019 data
  # Use the default sorting (by Eu.product)
  Eta_fu_template_2019 <- load_fu_allocation_data(sample_fu_allocation_table_path(2019)) %>% 
    eta_fu_template()
  expect_equal(Eta_fu_template_2019$Machine[[1]], "Automobiles")
  expect_equal(Eta_fu_template_2019$Machine[[nrow(Eta_fu_template_2019)]], "Non-energy")
  expect_equal(as.character(Eta_fu_template_2019$Quantity[[1]]), "E.dot_machine")
  expect_equal(as.character(Eta_fu_template_2019$Quantity[[nrow(Eta_fu_template_2019)]]), "phi.u")
  
  eu_products <- Eta_fu_template_2019$Eu.product %>% unique() %>% as.character()
  # Check that the order is as expected.
  expect_equivalent(eu_products, c("MD", "Light", "HTH.600.C", "MTH.200.C", "MTH.100.C", 
                                   "Bitumen", "Lubricants", "Other oil products", 
                                   "LTH.20.C", 
                                   "Hard coal (if no detail) [of Coal mines]", 
                                   "Other bituminous coal [of Coal mines]", 
                                   "Paraffin waxes", "White spirit & SBP"))
  # Check the class of the year columns. They should be numeric.
  expect_true(is.numeric(Eta_fu_template_2019[["1971"]]))
  expect_true(is.numeric(Eta_fu_template_2019[["2000"]]))
  
  # Now try to sort by importance
  Eta_fu_template_2019_2 <- load_fu_allocation_data(sample_fu_allocation_table_path(2019)) %>% 
    eta_fu_template(sort_by = "importance")
  expect_equal(Eta_fu_template_2019_2$Machine[[1]], "Wood stoves")
  expect_equal(Eta_fu_template_2019_2$Machine[[nrow(Eta_fu_template_2019_2)]], "Gas heaters")
  expect_equal(as.character(Eta_fu_template_2019_2$Quantity[[1]]), "E.dot_machine")
  expect_equal(as.character(Eta_fu_template_2019_2$Quantity[[nrow(Eta_fu_template_2019_2)]]), "phi.u")
  
  eu_products2 <- Eta_fu_template_2019_2$Eu.product %>% unique() %>% as.character()
  # Check that the order is as expected.
  expect_equivalent(eu_products2, c("MTH.100.C", "MTH.200.C", "MD", "Light", "HTH.600.C", 
                                    "Lubricants", "Other oil products", "Bitumen", 
                                    "LTH.20.C", 
                                    "Other bituminous coal [of Coal mines]", 
                                    "Hard coal (if no detail) [of Coal mines]", 
                                    "White spirit & SBP", "Paraffin waxes"))
  # Check the class of the year columns. They should be numeric.
  expect_true(is.numeric(Eta_fu_template_2019_2[["1971"]]))
  expect_true(is.numeric(Eta_fu_template_2019_2[["2000"]]))
})


test_that("write_eta_fu_template works as expected for 2018 data", {
  # Try with default sort order
  Eta_fu_template_2018 <- load_fu_allocation_data(sample_fu_allocation_table_path(2018)) %>% 
    eta_fu_template()
  # Get a temporary file in which to write the blank template.
  f <- tempfile(fileext = ".xlsx")
  p <- Eta_fu_template_2018 %>% 
    write_eta_fu_template(f, overwrite_file = TRUE, overwrite_fu_eta_tab = TRUE)
  # Read the tab back in.
  Template.reread <- openxlsx::read.xlsx(f, sheet = "FU etas")
  # Check that it was read back correctly.
  # Use expect_equivalent instead of expect_equal to ignore attributes 
  # (in this case levels) that are different after reading back in.
  expect_equivalent(Template.reread, Eta_fu_template_2018)
  expect_equal(Template.reread$Machine[[9]], "Diesel trucks")
  expect_equal(Template.reread$Machine[[261]], "LPG stoves")
  expect_equal(as.character(Template.reread$Quantity[[9]]), "E.dot_machine")
  expect_equal(as.character(Template.reread$Quantity[[262]]), "E.dot_machine [%]")
  
  # Now try to write it again.
  expect_true(file.exists(f))
  expect_error(Eta_fu_template_2018 %>% write_eta_fu_template(p), "File already exists!")
  
  # Clean up
  if (file.exists(f)) {
    file.remove(f)
  }
  
  # Try without .xlsx extension.
  f2 <- tempfile() # No extension.
  p2 <- Eta_fu_template_2018 %>% 
    write_eta_fu_template(f2, overwrite_file = TRUE, overwrite_fu_eta_tab = TRUE)
  # Check that the .xlsx extension was added to p.
  expect_true(endsWith(p2, ".xlsx"))
  expect_true(startsWith(p2, f2))
  # Clean up
  if (file.exists(f2)) {
    file.remove(f2)
  }
  
  
  # Try with "importance" sort order.
  Eta_fu_template_2018_2 <- load_fu_allocation_data(sample_fu_allocation_table_path(2018)) %>% 
    eta_fu_template(sort_by = "importance")
  # Get a temporary file in which to write the blank template.
  f <- tempfile(fileext = ".xlsx")
  p <- Eta_fu_template_2018_2 %>% 
    write_eta_fu_template(f, overwrite_file = TRUE, overwrite_fu_eta_tab = TRUE)
  # Read the tab back in.
  Template_2018.reread2 <- openxlsx::read.xlsx(f, sheet = "FU etas")
  # Check that it was read back correctly.
  # Use expect_equivalent instead of expect_equal to ignore attributes 
  # (in this case levels) that are different after reading back in.
  expect_equivalent(Template_2018.reread2, Eta_fu_template_2018_2)
  expect_equal(Template_2018.reread2$Machine[[9]], "Automobiles")
  expect_equal(Template_2018.reread2$Machine[[261]], "Kerosene stoves")
  expect_equal(as.character(Template_2018.reread2$Quantity[[9]]), "E.dot_machine")
  expect_equal(as.character(Template_2018.reread2$Quantity[[262]]), "E.dot_machine [%]")
  
  # Now try to write it again. 
  expect_true(file.exists(f))
  expect_error(Eta_fu_template_2018_2 %>% write_eta_fu_template(p), "File already exists!")
  # Clean up
  if (file.exists(f)) {
    file.remove(f)
  }
  
  # Try writing to a file that exists, but the tab doesn't exist.
  # Test writing the file when the tab doesn't exist.
  # First, make a file.
  DF <- data.frame(x = c(1, 2, 3))
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "temp")
  openxlsx::writeData(wb, sheet = "temp", x = DF)
  f3 <- tempfile(fileext = ".xlsx")
  openxlsx::saveWorkbook(wb, file = f3, overwrite = TRUE)
  # Now try to add a new tab in this same workbook.
  p3 <- Eta_fu_template_2018 %>% 
    write_eta_fu_template(path = f3, overwrite_file = TRUE)  
  # Verify that the tab was written
  tabnames <- openxlsx::getSheetNames(p3)
  expect_true("FU etas" %in% tabnames)
  # Now try to write the tab again
  p3 <- Eta_fu_template_2018 %>% 
    write_eta_fu_template(path = f3, overwrite_file = TRUE, overwrite_fu_eta_tab = TRUE)  
  expect_error(Eta_fu_template_2018 %>% 
                 write_eta_fu_template(path = f3, overwrite_file = TRUE), "FU etas already exists")
  # Clean up
  if (file.exists(f3)) {
    file.remove(f3)
  }
})


test_that("write_eta_fu_template works as expected for 2019 data", {
  # Try with default sort order
  Eta_fu_template_2019 <- load_fu_allocation_data(sample_fu_allocation_table_path(2019)) %>% 
    eta_fu_template()
  # Get a temporary file in which to write the blank template.
  f <- tempfile(fileext = ".xlsx")
  p <- Eta_fu_template_2019 %>% 
    write_eta_fu_template(f, overwrite_file = TRUE, overwrite_fu_eta_tab = TRUE)
  # Read the tab back in.
  Template.reread <- openxlsx::read.xlsx(f, sheet = "FU etas")
  # Check that it was read back correctly.
  # Use expect_equivalent instead of expect_equal to ignore attributes 
  # (in this case levels) that are different after reading back in.
  expect_equivalent(Template.reread, Eta_fu_template_2019)
  expect_equal(Template.reread$Machine[[9]], "Diesel trucks")
  expect_equal(Template.reread$Machine[[261]], "Wood furnaces")
  expect_equal(as.character(Template.reread$Quantity[[9]]), "E.dot_machine")
  expect_equal(as.character(Template.reread$Quantity[[262]]), "E.dot_machine [%]")
  
  # Clean up
  if (file.exists(f)) {
    file.remove(f)
  }
  
  # Try with "importance" sort order.
  Eta_fu_template_2019_2 <- load_fu_allocation_data(sample_fu_allocation_table_path(2019)) %>% 
    eta_fu_template(sort_by = "importance")
  # Get a temporary file in which to write the blank template.
  f <- tempfile(fileext = ".xlsx")
  p <- Eta_fu_template_2019_2 %>% 
    write_eta_fu_template(f, overwrite_file = TRUE, overwrite_fu_eta_tab = TRUE)
  # Read the tab back in.
  Template_2019.reread2 <- openxlsx::read.xlsx(f, sheet = "FU etas")
  # Check that it was read back correctly.
  # Use expect_equivalent instead of expect_equal to ignore attributes 
  # (in this case levels) that are different after reading back in.
  expect_equivalent(Template_2019.reread2, Eta_fu_template_2019_2)
  expect_equal(Template_2019.reread2$Machine[[9]], "Automobiles")
  expect_equal(Template_2019.reread2$Machine[[261]], "Non-energy")
  expect_equal(as.character(Template_2019.reread2$Quantity[[9]]), "E.dot_machine")
  expect_equal(as.character(Template_2019.reread2$Quantity[[262]]), "E.dot_machine [%]")
  
  # Clean up
  if (file.exists(f)) {
    file.remove(f)
  }
})


test_that("load_eta_fu_data works as expected", {
  # Load eta_fu data from package
  for (year in valid_iea_release_years) {
    Eta_fu <- load_eta_fu_data(path = sample_eta_fu_table_path(year))
    expect_true(length(Eta_fu) > 0)
    expect_true(nrow(Eta_fu) > 0)
  }
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
    magrittr::extract2("Country") %>% 
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
    magrittr::extract2("C_source") %>% 
    unique() %>% 
    expect_equal("World")
  # Check that Ghana obtained EIOU Electricity consumed by Main activity producer electricity plants from South Africa. 
  completed %>% 
    dplyr::filter(Flow.aggregation.point == IEATools::tfc_compare_flows$energy_industry_own_use,
                  Ef.product == IEATools::electricity_products$electricity,
                  Destination == IEATools::transformation_processes$main_activity_producer_electricity_plants) %>% 
    magrittr::extract2("C_source") %>% 
    expect_equal("ZAF")
  
  # Try again without enough information to complete the FU Allocation table.
  # Create this situation by dropping the fu_table_World from the list of exemplars.
  # In this situation, we cannot allocate the EIOU Electricity rows for Ghana.
  # This attempt should fail.
  complete_failure <- complete_fu_allocation_table(fu_allocation_table = fu_table_GHA, 
                                                   exemplar_fu_allocation_tables = list(fu_table_ZAF), 
                                                   tidy_specified_iea_data = tidy_specified_iea_data)
})

