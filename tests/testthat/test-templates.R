###########################################################
context("Template functions")
###########################################################

test_that("eiou fu_allocation_template works as expected", {
  EIOU_template <- load_tidy_iea_df() %>% 
    specify_all() %>%
    fu_allocation_template(template_type = "Energy industry own use")
  expected_colorder <- c("Country", "Method", "Energy.type", "Last.stage", "Ledger.side", "Flow.aggregation.point", "Unit",
                         "Ef.product", "Machine", "Eu.product", "Destination", 
                         "Quantity", "Maximum.values", "1971", "2000")
  expect_equal(names(EIOU_template), expected_colorder)
  expect_true(all(EIOU_template$Flow.aggregation.point == "Energy industry own use"))
})

test_that("final consumption fu_allocation_template works as expected", {
  TFC_template <- load_tidy_iea_df() %>% 
    specify_all() %>%
    fu_allocation_template(template_type = "Final consumption")
  expected_colorder <- c("Country", "Method", "Energy.type", "Last.stage", "Ledger.side", "Flow.aggregation.point", "Unit",
                         "Ef.product", "Machine", "Eu.product", "Destination", 
                         "Quantity", "Maximum.values", "1971", "2000")
  expect_equal(names(TFC_template), expected_colorder)
  expect_true(all(TFC_template$Ledger.side == "Consumption"))
})

test_that("write_fu_allocation_templates works as expected", {
  Tidy_iea_df <- load_tidy_iea_df() %>% 
    specify_all()
  # Get a temporary file in which to write two data frames on different tabs.
  f <- tempfile(fileext = ".xslx")
  Tidy_iea_df %>% 
    write_fu_allocation_templates(f)  
  # Now read the tabs back in
  FD <- openxlsx::read.xlsx(f, sheet = "Final.demand.allocations")
  EIOU <- openxlsx::read.xlsx(f, sheet = "EIOU.allocations")
  # Check the tabs to make sure they're the same
  Expected_FD <- Tidy_iea_df %>% 
    fu_allocation_template(template_type = "Final demand")
  expect_true(all(FD == Expected_FD, na.rm = TRUE))
  Expected_EIOU <- Tidy_iea_df %>% 
    fu_allocation_template(template_type = "Energy industry own use")
  expect_true(all(EIOU == Expected_EIOU, na.rm = TRUE))
  # Clean up
  if (file.exists(f)) {
    file.remove(f)
  }
})

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
