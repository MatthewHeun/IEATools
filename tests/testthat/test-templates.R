###########################################################
context("Template functions")
###########################################################

test_that("fu_allocation_template works as expected", {
  Allocation_template <- load_tidy_iea_df() %>% 
    specify_all() %>%
    fu_allocation_template() %>% 
    arrange_iea_fu_allocation_template_cols()
  expected_colorder <- c("Country", "Method", "Energy.type", "Last.stage", "Ledger.side", "Flow.aggregation.point", "Unit",
                         "Ef.product", "Machine", "Eu.product", "Destination", 
                         "Quantity", "Maximum.values", "1971", "2000")
  expect_equal(names(Allocation_template), expected_colorder)
  expect_true(all(Allocation_template$Ledger.side == "Consumption" | Allocation_template$Flow.aggregation.point == "Energy industry own use"))
})

test_that("write_fu_allocation_template works as expected", {
  Tidy_iea_df <- load_tidy_iea_df() %>% 
    specify_all()
  # Get a temporary file in which to write two data frames on different tabs.
  f <- tempfile(fileext = ".xlsx")
  p <- Tidy_iea_df %>% 
    write_fu_allocation_template(f)
  expect_equal(p, f)
  # Now read the tabs back in
  Allocations <- openxlsx::read.xlsx(f, sheet = "Allocations") %>% 
    dplyr::rename(
      Maximum.values.reread = Maximum.values,
      `1971.reread` = `1971`,
      `2000.reread` = `2000`
    )
  # Check the tabs to make sure they're the same
  Expected_allocations <- Tidy_iea_df %>% 
    fu_allocation_template()
  Joined <- dplyr::full_join(Allocations, Expected_allocations, by = c("Country", "Method", "Energy.type", 
                                                                       "Last.stage", "Ledger.side", "Flow.aggregation.point", 
                                                                       "Unit", "Ef.product", "Machine", 
                                                                       "Eu.product", "Destination", "Quantity")) %>% 
    mutate(
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
  expect_error(Tidy_iea_df %>% write_fu_allocation_template(p), "File already exists!")
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
