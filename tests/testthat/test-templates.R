# This function tests final-to-useful allocation templates
# created from the default data set, filled or not.
check_fu_allocation_template <- function(.DF){
  expect_equal(.DF$FlowAggregationPoint[[1]], "Energy industry own use")
  expect_equal(.DF$EfProduct[[1]], "Refinery gas")
  expect_equal(.DF$Destination[[1]], "Oil refineries")
  expect_equal(.DF$Quantity[[1]], "Edot")
  last_row <- nrow(.DF)
  expect_equal(.DF$FlowAggregationPoint[[last_row]], "Non-energy use")
  expect_equal(.DF$EfProduct[[last_row]], "Paraffin waxes")
  expect_equal(.DF$Destination[[last_row]], "Non-energy use industry/transformation/energy")
  expect_equal(.DF$Quantity[[last_row]], "C_4 [%]")
}


test_that("openxlsx works as expected", {
  # These are just tests for me to understand the openxlsx package.
  Tidy_iea_df <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    sort_iea_df()
  # Get a temporary file in which to write two data frames on different tabs.
  f <- tempfile(fileext = ".xlsx")
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
    res <- file.remove(f)
  }
})


test_that("fu_allocation_template() works as expected", {
  Allocation_template <- load_tidy_iea_df() %>% 
    specify_all() %>%
    fu_allocation_template() %>% 
    arrange_iea_fu_allocation_template()

  # Check rows
  check_fu_allocation_template(Allocation_template)

  # Check columns
  expected_colorder <- c("Country", "Method", "EnergyType", "LastStage", "LedgerSide", "FlowAggregationPoint", "Unit",
                         "EfProduct", "Machine", "EuProduct", "Destination", 
                         "Quantity", "Maximum.values", "1971", "2000")
  expect_equal(names(Allocation_template), expected_colorder)
  expect_true(all(Allocation_template$LedgerSide == "Consumption" | Allocation_template$FlowAggregationPoint == "Energy industry own use"))
})


test_that("fu_allocation_tempate() gives expected error message for unknown flow aggregation point", {
  # Load the data frame and change the Flow to something unrecognizable.
  # That will generate the error.
  df <- load_tidy_iea_df() %>% 
    specify_all()
  df[[IEATools::iea_cols$flow_aggregation_point]][[1]] <- "bad_aggregation_point"
  
  expect_error(df %>% 
    fu_allocation_template() %>% 
    arrange_iea_fu_allocation_template(), "and can't be sorted:\\n bad_aggregation_point, Industry not elsewhere specified")
})


test_that("fu_allocation_tempate() gives expected error message for unknown product", {
  # Load the data frame and change the Product to something unrecognizable.
  # That will generate the error.
  df <- load_tidy_iea_df() %>% 
    specify_all()
  df[[IEATools::iea_cols$product]][[1]] <- "bad_product"
  
  expect_error(df %>% 
                 fu_allocation_template() %>% 
                 arrange_iea_fu_allocation_template(), "and can't be sorted:\\n bad_product")
})


test_that("write_fu_allocation_template() works as expected", {
  FU_allocation_template <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    fu_allocation_template()
  # Get a temporary file in which to write two data frames on different tabs.
  f <- tempfile(fileext = ".xlsx")
  p <- FU_allocation_template %>% 
    write_fu_allocation_template(f)
  expect_equal(p, f)
  # Now read the tabs back in
  Allocations <- openxlsx::read.xlsx(f, sheet = IEATools::fu_analysis_file_info$fu_allocation_tab_name) %>% 
    dplyr::rename(
      Maximum.values.reread = Maximum.values,
      `1971.reread` = `1971`,
      `2000.reread` = `2000`
    )
  # Check the tabs to make sure they're the same
  Expected_allocations <- FU_allocation_template
  Joined <- dplyr::full_join(Allocations, Expected_allocations, by = c("Country", "Method", "EnergyType", 
                                                                       "LastStage", "LedgerSide", "FlowAggregationPoint", 
                                                                       "Unit", "EfProduct", "Machine", 
                                                                       "EuProduct", "Destination", "Quantity")) %>% 
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
    res <- file.remove(f)
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
    res <- file.remove(f2)
  }
})


test_that("load_fu_allocation_data() works as expected", {
  # Load default data and check the filled table.
  load_fu_allocation_data() %>% 
    check_fu_allocation_template()
})


test_that("eta_fu_template() works as expected for 2021 data", {
  # Try for 2021 data
  # Use the default sorting (by EuProduct)
  Eta_fu_template_2021 <- load_fu_allocation_data(sample_fu_allocation_table_path(2021)) %>% 
    eta_fu_template()
  expect_equal(Eta_fu_template_2021$Machine[[1]], "Automobiles")
  expect_equal(Eta_fu_template_2021$Machine[[nrow(Eta_fu_template_2021)]], "Non-energy consumption")
  expect_equal(as.character(Eta_fu_template_2021$Quantity[[1]]), "Edot_machine")
  expect_equal(as.character(Eta_fu_template_2021$Quantity[[nrow(Eta_fu_template_2021)]]), "phi.u")
  
  eu_products <- Eta_fu_template_2021[[IEATools::template_cols$eu_product]] |> 
    unique() |> 
    as.character()
  # Check that the order is as expected.
  expect_setequal(eu_products, c("MD", "Light", "HTH.600.C", "MTH.200.C", "MTH.100.C", "NEU", "LTH.20.C"))
  # Check the class of the year columns. They should be numeric.
  expect_true(is.numeric(Eta_fu_template_2021[["1971"]]))
  expect_true(is.numeric(Eta_fu_template_2021[["2000"]]))
  
  # Now try to sort by importance
  Eta_fu_template_2021_2 <- load_fu_allocation_data(sample_fu_allocation_table_path(2021)) %>% 
    eta_fu_template(sort_by = "importance")
  expect_equal(Eta_fu_template_2021_2$Machine[[1]], "Wood cookstoves")
  expect_equal(Eta_fu_template_2021_2$Machine[[nrow(Eta_fu_template_2021_2)]], "Gas heaters")
  expect_equal(as.character(Eta_fu_template_2021_2$Quantity[[1]]), "Edot_machine")
  expect_equal(as.character(Eta_fu_template_2021_2$Quantity[[nrow(Eta_fu_template_2021_2)]]), "phi.u")
  
  eu_products2 <- Eta_fu_template_2021_2[[IEATools::template_cols$eu_product]] |> 
    unique() |> 
    as.character()
  # Check that the order is as expected.
  expect_setequal(eu_products2, c("MTH.100.C", "MTH.200.C", "MD", "HTH.600.C", "Light", "LTH.20.C", "NEU"))
  # Check the class of the year columns. They should be numeric.
  expect_true(is.numeric(Eta_fu_template_2021_2[["1971"]]))
  expect_true(is.numeric(Eta_fu_template_2021_2[["2000"]]))
})


test_that("eta_fu_template() works as expected for 2022 data", {
  # Try for 2022 data
  # Use the default sorting (by EuProduct)
  Eta_fu_template_2022 <- load_fu_allocation_data(sample_fu_allocation_table_path(2022)) %>% 
    eta_fu_template()
  expect_equal(Eta_fu_template_2022$Machine[[1]], "Automobiles")
  expect_equal(Eta_fu_template_2022$Machine[[nrow(Eta_fu_template_2022)]], "Non-energy consumption")
  expect_equal(as.character(Eta_fu_template_2022$Quantity[[1]]), "Edot_machine")
  expect_equal(as.character(Eta_fu_template_2022$Quantity[[nrow(Eta_fu_template_2022)]]), "phi.u")
  
  eu_products <- Eta_fu_template_2022[[IEATools::template_cols$eu_product]] |> 
    unique() |> 
    as.character()
  # Check that the order is as expected.
  expect_setequal(eu_products, c("MD", "Light", "HTH.600.C", "MTH.200.C", "MTH.100.C", "NEU", "LTH.20.C"))
  # Check the class of the year columns. They should be numeric.
  expect_true(is.numeric(Eta_fu_template_2022[["1971"]]))
  expect_true(is.numeric(Eta_fu_template_2022[["2000"]]))
  
  # Now try to sort by importance
  Eta_fu_template_2022_2 <- load_fu_allocation_data(sample_fu_allocation_table_path(2022)) %>% 
    eta_fu_template(sort_by = "importance")
  expect_equal(Eta_fu_template_2022_2$Machine[[1]], "Wood cookstoves")
  expect_equal(Eta_fu_template_2022_2$Machine[[nrow(Eta_fu_template_2022_2)]], "Gas heaters")
  expect_equal(as.character(Eta_fu_template_2022_2$Quantity[[1]]), "Edot_machine")
  expect_equal(as.character(Eta_fu_template_2022_2$Quantity[[nrow(Eta_fu_template_2022_2)]]), "phi.u")
  
  eu_products2 <- Eta_fu_template_2022_2[[IEATools::template_cols$eu_product]] |> 
    unique() |> 
    as.character()
  # Check that the order is as expected.
  expect_setequal(eu_products2, c("MTH.100.C", "MTH.200.C", "MD", "Light", "HTH.600.C", "NEU", "LTH.20.C"))
  # Check the class of the year columns. They should be numeric.
  expect_true(is.numeric(Eta_fu_template_2022_2[["1971"]]))
  expect_true(is.numeric(Eta_fu_template_2022_2[["2000"]]))
})


test_that("eta_fu_template() works with tidy fu allocation data for 2021", {
  tidy_specified_iea_data <- load_tidy_iea_df(system.file(file.path("extdata", 
                                                                    "GH-ZA-ktoe-Extended-Energy-Balances-sample-2021.csv"),
                                                          package = "IEATools"), 
                                              apply_fixes = FALSE) %>% 
    specify_all()
  Eta_fu_template_2021 <- load_fu_allocation_data(sample_fu_allocation_table_path(2021)) %>% 
    tidy_fu_allocation_table() %>% 
    eta_fu_template(tidy_specified_iea_data = tidy_specified_iea_data)

  # Now try a couple tests.
  # These tests are same as the tests in the previous test function.
  expect_equal(Eta_fu_template_2021$Machine[[1]], "Automobiles")
  expect_equal(Eta_fu_template_2021$Machine[[nrow(Eta_fu_template_2021)]], "Non-energy consumption")
  expect_equal(as.character(Eta_fu_template_2021$Quantity[[1]]), "Edot_machine")
  expect_equal(as.character(Eta_fu_template_2021$Quantity[[nrow(Eta_fu_template_2021)]]), "phi.u")
  
  eu_products <- Eta_fu_template_2021[[IEATools::template_cols$eu_product]] |> 
    unique() |> 
    as.character()
  # Check that the order is as expected.
  expect_setequal(eu_products, c("MD", "Light", "HTH.600.C", "MTH.200.C", "MTH.100.C", "LTH.20.C", "NEU"))
  # Check the class of the year columns. They should be numeric.
  expect_true(is.numeric(Eta_fu_template_2021[["1971"]]))
  expect_true(is.numeric(Eta_fu_template_2021[["2000"]]))
})


test_that("eta_fu_template() works with tidy data from the default year", {
  tidy_specified_iea_data <- load_tidy_iea_df(apply_fixes = FALSE) %>% 
    specify_all()
  Eta_fu_template <- load_fu_allocation_data(sample_fu_allocation_table_path()) %>% 
    tidy_fu_allocation_table() %>% 
    eta_fu_template(tidy_specified_iea_data = tidy_specified_iea_data)
  
  # Now try a couple tests.
  # These tests are same as the tests in the previous test function.
  expect_equal(Eta_fu_template$Machine[[1]], "Automobiles")
  expect_equal(Eta_fu_template$Machine[[nrow(Eta_fu_template)]], "Non-energy consumption")
  expect_equal(as.character(Eta_fu_template$Quantity[[1]]), "Edot_machine")
  expect_equal(as.character(Eta_fu_template$Quantity[[nrow(Eta_fu_template)]]), "phi.u")
  
  eu_products <- Eta_fu_template[[IEATools::template_cols$eu_product]] |> 
    unique() |> 
    as.character()
  # Check that the order is as expected.
  expect_equal(eu_products, c("MD", "Light", "HTH.600.C", "MTH.200.C", "MTH.100.C", 
                              "NEU", "LTH.20.C"))
  # Check the class of the year columns. They should be numeric.
  expect_true(is.numeric(Eta_fu_template[["1971"]]))
  expect_true(is.numeric(Eta_fu_template[["2000"]]))
})


test_that("write_eta_fu_template() works as expected for 2021 data", {
  # Try with default sort order
  Eta_fu_template_2021 <- load_fu_allocation_data(sample_fu_allocation_table_path(2021)) %>% 
    eta_fu_template()
  # Get a temporary file in which to write the blank template.
  f <- tempfile(fileext = ".xlsx")
  p <- Eta_fu_template_2021 %>% 
    write_eta_fu_template(f, overwrite_file = TRUE, overwrite_fu_eta_tab = TRUE)
  # Read the tab back in.
  Template.reread <- openxlsx::read.xlsx(f, sheet = IEATools::fu_analysis_file_info$eta_fu_tab_name)
  # Check that it was read back correctly.
  # Use expect_equal with ignore_attr = TRUE to ignore attributes 
  # (in this case levels) that are different after reading back in.
  expect_equal(Template.reread, Eta_fu_template_2021, ignore_attr = TRUE)
  expect_equal(Template.reread$Machine[[9]], "Diesel trucks")
  expect_equal(Template.reread$Machine[[261]], "Kerosene stoves")
  expect_equal(as.character(Template.reread$Quantity[[9]]), "Edot_machine")
  expect_equal(as.character(Template.reread$Quantity[[262]]), "Edot_machine [%]")
  
  # Now try to write it again.
  expect_true(file.exists(f))
  expect_error(Eta_fu_template_2021 %>% write_eta_fu_template(p), "File already exists!")
  
  # Clean up
  if (file.exists(f)) {
    res <- file.remove(f)
  }
  
  # Try without .xlsx extension.
  f2 <- tempfile() # No extension.
  p2 <- Eta_fu_template_2021 %>% 
    write_eta_fu_template(f2, overwrite_file = TRUE, overwrite_fu_eta_tab = TRUE)
  # Check that the .xlsx extension was added to p.
  expect_true(endsWith(p2, ".xlsx"))
  expect_true(startsWith(p2, f2))
  # Clean up
  if (file.exists(f2)) {
    res <- file.remove(f2)
  }
  
  
  # Try with "importance" sort order.
  Eta_fu_template_2021_2 <- load_fu_allocation_data(sample_fu_allocation_table_path(2021)) %>% 
    eta_fu_template(sort_by = "importance")
  # Get a temporary file in which to write the blank template.
  f <- tempfile(fileext = ".xlsx")
  p <- Eta_fu_template_2021_2 %>% 
    write_eta_fu_template(f, overwrite_file = TRUE, overwrite_fu_eta_tab = TRUE)
  # Read the tab back in.
  Template_2021.reread2 <- openxlsx::read.xlsx(f, sheet = IEATools::fu_analysis_file_info$eta_fu_tab_name)
  # Check that it was read back correctly.
  # Use expect_equal with ignore_attr = TRUE to ignore attributes 
  # (in this case levels) that are different after reading back in.
  expect_equal(Template_2021.reread2, Eta_fu_template_2021_2, ignore_attr = TRUE)
  expect_equal(Template_2021.reread2$Machine[[9]], "Automobiles")
  expect_equal(Template_2021.reread2$Machine[[261]], "Oil furnaces")
  expect_equal(as.character(Template_2021.reread2$Quantity[[9]]), "Edot_machine")
  expect_equal(as.character(Template_2021.reread2$Quantity[[262]]), "Edot_machine [%]")
  
  # Now try to write it again. 
  expect_true(file.exists(f))
  expect_error(Eta_fu_template_2021_2 %>% write_eta_fu_template(p), "File already exists!")
  # Clean up
  if (file.exists(f)) {
    res <- file.remove(f)
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
  p3 <- Eta_fu_template_2021 %>% 
    write_eta_fu_template(path = f3, overwrite_file = TRUE)  
  # Verify that the tab was written
  tabnames <- openxlsx::getSheetNames(p3)
  expect_true(IEATools::fu_analysis_file_info$eta_fu_tab_name %in% tabnames)
  # Now try to write the tab again
  p3 <- Eta_fu_template_2021 %>% 
    write_eta_fu_template(path = f3, overwrite_file = TRUE, overwrite_fu_eta_tab = TRUE)  
  expect_error(Eta_fu_template_2021 %>% 
                 write_eta_fu_template(path = f3, overwrite_file = TRUE), 
               paste(IEATools::fu_analysis_file_info$eta_fu_tab_name, "already exists"))
  # Clean up
  if (file.exists(f3)) {
    res <- file.remove(f3)
  }
})


test_that("write_eta_fu_template() works as expected for 2022 data", {
  # Try with default sort order
  Eta_fu_template_2022 <- load_fu_allocation_data(sample_fu_allocation_table_path(2022)) %>% 
    eta_fu_template()
  # Get a temporary file in which to write the blank template.
  f <- tempfile(fileext = ".xlsx")
  p <- Eta_fu_template_2022 %>% 
    write_eta_fu_template(f, overwrite_file = TRUE, overwrite_fu_eta_tab = TRUE)
  # Read the tab back in.
  Template.reread <- openxlsx::read.xlsx(f, sheet = IEATools::fu_analysis_file_info$eta_fu_tab_name)
  # Check that it was read back correctly.
  # Use expect_equal with ignore_attr = TRUE to ignore attributes 
  # (in this case levels) that are different after reading back in.
  expect_equal(Template.reread, Eta_fu_template_2022, ignore_attr = TRUE)
  expect_equal(Template.reread$Machine[[9]], "Diesel trucks")
  expect_equal(Template.reread$Machine[[254]], "Wood furnaces")
  expect_equal(as.character(Template.reread$Quantity[[9]]), "Edot_machine")
  expect_equal(as.character(Template.reread$Quantity[[254]]), "Edot_machine [%]")
  
  # Clean up
  if (file.exists(f)) {
    res <- file.remove(f)
  }
  
  # Try with "importance" sort order.
  Eta_fu_template_2022_2 <- load_fu_allocation_data(sample_fu_allocation_table_path(2022)) %>% 
    eta_fu_template(sort_by = "importance")
  # Get a temporary file in which to write the blank template.
  f <- tempfile(fileext = ".xlsx")
  p <- Eta_fu_template_2022_2 %>% 
    write_eta_fu_template(f, overwrite_file = TRUE, overwrite_fu_eta_tab = TRUE)
  # Read the tab back in.
  Template_2022.reread2 <- openxlsx::read.xlsx(f, sheet = IEATools::fu_analysis_file_info$eta_fu_tab_name)
  # Check that it was read back correctly.
  # Use expect_equal with ignore_attr = TRUE to ignore attributes 
  # (in this case levels) that are different after reading back in.
  expect_equal(Template_2022.reread2, Eta_fu_template_2022_2, ignore_attr = TRUE)
  expect_equal(Template_2022.reread2$Machine[[9]], "Automobiles")
  expect_equal(Template_2022.reread2$Machine[[102]], "Non-energy consumption")
  expect_equal(as.character(Template_2022.reread2$Quantity[[9]]), "Edot_machine")
  expect_equal(as.character(Template_2022.reread2$Quantity[[102]]), "Edot_machine [%]")
  
  # Clean up
  if (file.exists(f)) {
    res <- file.remove(f)
  }
})


test_that("write_eta_fu_template() works as expected for default year", {
  # Try with default sort order
  Eta_fu_template_default <- load_fu_allocation_data(sample_fu_allocation_table_path()) %>% 
    eta_fu_template()
  # Get a temporary file in which to write the blank template.
  f <- tempfile(fileext = ".xlsx")
  p <- Eta_fu_template_default %>% 
    write_eta_fu_template(f, overwrite_file = TRUE, overwrite_fu_eta_tab = TRUE)
  # Read the tab back in.
  Template.reread <- openxlsx::read.xlsx(f, sheet = IEATools::fu_analysis_file_info$eta_fu_tab_name)
  # Check that it was read back correctly.
  # Use expect_equal with ignore_attr = TRUE to ignore attributes 
  # (in this case levels) that are different after reading back in.
  expect_equal(Template.reread, Eta_fu_template_default, ignore_attr = TRUE)
  expect_equal(Template.reread$Machine[[9]], "Diesel trucks")
  expect_equal(Template.reread$Machine[[254]], "Wood furnaces")
  expect_equal(as.character(Template.reread$Quantity[[9]]), "Edot_machine")
  expect_equal(as.character(Template.reread$Quantity[[254]]), "Edot_machine [%]")
  
  # Clean up
  if (file.exists(f)) {
    res <- file.remove(f)
  }
  
  # Try with "importance" sort order.
  Eta_fu_template_default_2 <- load_fu_allocation_data(sample_fu_allocation_table_path()) %>% 
    eta_fu_template(sort_by = "importance")
  # Get a temporary file in which to write the blank template.
  f <- tempfile(fileext = ".xlsx")
  p <- Eta_fu_template_default_2 %>% 
    write_eta_fu_template(f, overwrite_file = TRUE, overwrite_fu_eta_tab = TRUE)
  # Read the tab back in.
  Template_default.reread2 <- openxlsx::read.xlsx(f, sheet = IEATools::fu_analysis_file_info$eta_fu_tab_name)
  # Check that it was read back correctly.
  # Use expect_equal with ignore_attr = TRUE to ignore attributes 
  # (in this case levels) that are different after reading back in.
  expect_equal(Template_default.reread2, Eta_fu_template_default_2, ignore_attr = TRUE)
  expect_equal(Template_default.reread2$Machine[[9]], "Automobiles")
  expect_equal(Template_default.reread2$Machine[[102]], "Non-energy consumption")
  expect_equal(as.character(Template_default.reread2$Quantity[[9]]), "Edot_machine")
  expect_equal(as.character(Template_default.reread2$Quantity[[102]]), "Edot_machine [%]")
  
  # Clean up
  if (file.exists(f)) {
    res <- file.remove(f)
  }
})



test_that("load_eta_fu_data() works as expected", {
  # Load eta_fu data from package
  for (year in valid_iea_release_years) {
    Eta_fu <- load_eta_fu_data(path = sample_eta_fu_table_path(year))
    expect_true(length(Eta_fu) > 0)
    expect_true(nrow(Eta_fu) > 0)
  }
})


test_that("check_fu_allocation_data() works as expected", {
  expect_true(load_fu_allocation_data() %>% check_fu_allocation_data())
  
  # Make a bogus fu_allocation data frame that should fail and make sure it fails
  # in the situation where EfProduct and EuProduct are not same when Machine is Non-energy.
  fu_allocation_bad <- tibble::tribble(~Country, ~FlowAggregationPoint, ~EfProduct, ~Machine, ~EuProduct, ~Destination, 
                                       "Wakanda", "Consumption", "Bitumen", "Non-energy", "Bituman", "Road")
  expect_error(check_fu_allocation_data(fu_allocation_bad), 
               "EfProduct and EuProduct must be identical when Machine is Non-energy. The following combinations do not meet that criterion:\nWakanda, Consumption, Bitumen, Non-energy, Bituman, Road. Please check the FU allocation table for typos or misspellings.")
  
  # Make a bogus fu_allocation data frame that should fail and make sure it fails
  # in the situation where .values is not NA.
  fu_allocation_bad2 <- tibble::tribble(~Country, ~Year, ~FlowAggregationPoint, ~EfProduct, ~Machine, ~EuProduct, ~Destination, ~Quantity, ~Value,
                                        "Wakanda", 2020, "Consumption", "Electricity", NA_character_, "MD", "Industry", "C_1 [%]", "25.0")
  expect_error(check_fu_allocation_data(fu_allocation_bad2), "In the FU Allocations tab, EuProduct and Destination must be filled when Quantity is non-zero. The following combinations do not meet that criterion:\nWakanda, 2020, Consumption, Electricity, NA, MD, Industry, C_1")
})

