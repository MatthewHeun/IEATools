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
  expect_equal(.DF$Flow.aggregation.point[[last_row]], "Other")
  expect_equal(.DF$Ef.product[[last_row]], "Electricity")
  expect_equal(.DF$Destination[[last_row]], "Non-specified (other)")
  expect_equal(.DF$Quantity[[last_row]], "C_3 [%]")
}

test_that("openxlsx works as expected", {
  # These are just tests for me to understand the openxlsx package.
  Tidy_iea_df <- load_tidy_iea_df() %>% 
    specify_all()
  # Get a temporary file in which to write two data frames on different tabs.
  f <- tempfile(fileext = ".xslx")
  # Write the data from both countries, each on its own tab.
  openxlsx::write.xlsx(list(GHA = Tidy_iea_df %>% 
                              dplyr::filter(Country == "GHA"), 
                            ZAF = Tidy_iea_df %>% 
                              dplyr::filter(Country == "ZAF")),
                       file = f)
  # Read the data back in, one sheet at a time.
  GHA <- openxlsx::read.xlsx(f, sheet = "GHA")
  ZAF <- openxlsx::read.xlsx(f, sheet = "ZAF")
  # And recombine into a single Tibble
  Rebuild <- dplyr::bind_rows(GHA, ZAF) %>% 
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
    fu_allocation_template()

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

test_that("eta_fu_template works as expected", {
  # Use the default sorting (by Eu.product)
  Eta_fu_template <- load_fu_allocation_data() %>% 
    eta_fu_template()
  expect_equal(Eta_fu_template$Machine[[1]], "Automobiles")
  expect_equal(Eta_fu_template$Machine[[nrow(Eta_fu_template)]], "Oil furnaces")
  expect_equal(as.character(Eta_fu_template$Quantity[[1]]), "E.dot_machine")
  expect_equal(as.character(Eta_fu_template$Quantity[[nrow(Eta_fu_template)]]), "phi.u")
  
  eu_products <- Eta_fu_template$Eu.product %>% unique() %>% as.character()
  # Check that the order is as expected.
  expect_equivalent(eu_products, c("MD", "Light", "HTH.600.C", "MTH.200.C", "MTH.100.C", "LTH.20.C"))
  # Check the class of the year columns. They should be numeric.
  expect_true(is.numeric(Eta_fu_template[["1971"]]))
  expect_true(is.numeric(Eta_fu_template[["2000"]]))
  
  # Now try to sort by importance
  Eta_fu_template2 <- load_fu_allocation_data() %>% 
    eta_fu_template(sort_by = "importance")
  expect_equal(Eta_fu_template2$Machine[[1]], "Wood stoves")
  expect_equal(Eta_fu_template2$Machine[[nrow(Eta_fu_template2)]], "Gas heaters")
  expect_equal(as.character(Eta_fu_template2$Quantity[[1]]), "E.dot_machine")
  expect_equal(as.character(Eta_fu_template2$Quantity[[nrow(Eta_fu_template)]]), "phi.u")
  
  eu_products2 <- Eta_fu_template2$Eu.product %>% unique() %>% as.character()
  # Check that the order is as expected.
  expect_equivalent(eu_products2, c("MTH.100.C", "MTH.200.C", "MD", "HTH.600.C", "Light", "LTH.20.C"))
  # Check the class of the year columns. They should be numeric.
  expect_true(is.numeric(Eta_fu_template2[["1971"]]))
  expect_true(is.numeric(Eta_fu_template2[["2000"]]))
})

test_that("write_eta_fu_template works as expected", {
  # Try with default sort order
  Eta_fu_template <- load_fu_allocation_data() %>% 
    eta_fu_template()
  # Get a temporary file in which to write the blank template.
  f <- tempfile(fileext = ".xlsx")
  p <- Eta_fu_template %>% 
    write_eta_fu_template(f, overwrite_file = TRUE, overwrite_fu_eta_tab = TRUE)
  # Read the tab back in.
  Template.reread <- openxlsx::read.xlsx(f, sheet = "FU etas")
  # Check that it was read back correctly.
  # Use expect_equivalent instead of expect_equal to ignore attributes 
  # (in this case levels) that are different after reading back in.
  expect_equivalent(Template.reread, Eta_fu_template)
  expect_equal(Template.reread$Machine[[9]], "Diesel trucks")
  expect_equal(Template.reread$Machine[[261]], "LPG stoves")
  expect_equal(as.character(Template.reread$Quantity[[9]]), "E.dot_machine")
  expect_equal(as.character(Template.reread$Quantity[[262]]), "E.dot_machine [%]")
  
  # Now try to write it again.
  expect_true(file.exists(f))
  expect_error(Eta_fu_template %>% write_eta_fu_template(p), "File already exists!")
  
  # Clean up
  if (file.exists(f)) {
    file.remove(f)
  }
  
  # Try without .xlsx extension.
  f2 <- tempfile() # No extension.
  p2 <- Eta_fu_template %>% 
    write_eta_fu_template(f2, overwrite_file = TRUE, overwrite_fu_eta_tab = TRUE)
  # Check that the .xlsx extension was added to p.
  expect_true(endsWith(p2, ".xlsx"))
  expect_true(startsWith(p2, f2))
  # Clean up
  if (file.exists(f2)) {
    file.remove(f2)
  }
  
  # Try with "importance" sort order.
  Eta_fu_template2 <- load_fu_allocation_data() %>% 
    eta_fu_template(sort_by = "importance")
  # Get a temporary file in which to write the blank template.
  f <- tempfile(fileext = ".xlsx")
  p <- Eta_fu_template2 %>% 
    write_eta_fu_template(f, overwrite_file = TRUE, overwrite_fu_eta_tab = TRUE)
  # Read the tab back in.
  Template.reread2 <- openxlsx::read.xlsx(f, sheet = "FU etas")
  # Check that it was read back correctly.
  # Use expect_equivalent instead of expect_equal to ignore attributes 
  # (in this case levels) that are different after reading back in.
  expect_equivalent(Template.reread2, Eta_fu_template2)
  expect_equal(Template.reread2$Machine[[9]], "Automobiles")
  expect_equal(Template.reread2$Machine[[261]], "Kerosene stoves")
  expect_equal(as.character(Template.reread2$Quantity[[9]]), "E.dot_machine")
  expect_equal(as.character(Template.reread2$Quantity[[262]]), "E.dot_machine [%]")
  
  # Now try to write it again.
  expect_true(file.exists(f))
  expect_error(Eta_fu_template2 %>% write_eta_fu_template(p), "File already exists!")
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
  p3 <- Eta_fu_template %>% 
    write_eta_fu_template(path = f3, overwrite_file = TRUE)  
  # Verify that the tab was written
  tabnames <- openxlsx::getSheetNames(p3)
  expect_true("FU etas" %in% tabnames)
  # Now try to write the tab again
  p3 <- Eta_fu_template %>% 
    write_eta_fu_template(path = f3, overwrite_file = TRUE, overwrite_fu_eta_tab = TRUE)  
  expect_error(Eta_fu_template %>% 
                 write_eta_fu_template(path = f3, overwrite_file = TRUE), "FU etas already exists")
  # Clean up
  if (file.exists(f3)) {
    file.remove(f3)
  }
})


test_that("load_eta_fu_data works as expected", {
  # Load eta_fu data from package
  Eta_fu <- load_eta_fu_data()
  expect_true(TRUE)
})
  
