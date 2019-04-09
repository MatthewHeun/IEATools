###########################################################
context("Template functions")
###########################################################

test_that("eiou_fu_template works as expected", {
  load_tidy_iea_df() %>% 
    specify_all() %>%
    eiou_fu_template()
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
  Rebuild <- bind_rows(GHA, ZAF) %>% 
    dplyr::as_tibble()
  # And we should get back what we wrote.
  expect_equal(Rebuild, Tidy_iea_df)
  # Clean up
  if (file.exists(f)) {
    file.remove(f)
  }
})
