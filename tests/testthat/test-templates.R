###########################################################
context("Template functions")
###########################################################

test_that("eiou fu_template works as expected", {
  EIOU_template <- load_tidy_iea_df() %>% 
    specify_all() %>%
    fu_template(template_type = "Energy industry own use")
  expected_colorder <- c("Method", "Last.stage", "Country", "Ledger.side", "Flow.aggregation.point", "Energy.type", "Unit",
                         "Ef product", "Machine", "Eu product", "Destination", 
                         "Quantity", "Maximum values", "1971", "2000")
  expect_equal(names(EIOU_template), expected_colorder)
  expect_true(all(EIOU_template$Flow.aggregation.point == "Energy industry own use"))
})

test_that("final consumption fu_template works as expected", {
  TFC_template <- load_tidy_iea_df() %>% 
    specify_all() %>%
    fu_template(template_type = "Final consumption")
  expected_colorder <- c("Method", "Last.stage", "Country", "Ledger.side", "Flow.aggregation.point", "Energy.type", "Unit",
                         "Ef product", "Machine", "Eu product", "Destination", 
                         "Quantity", "Maximum values", "1971", "2000")
  expect_equal(names(TFC_template), expected_colorder)
  expect_true(all(TFC_template$Ledger.side == "Consumption"))
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
