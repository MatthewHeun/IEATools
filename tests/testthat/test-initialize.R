###########################################################
context("Initialize package")
###########################################################

test_that("iea_df works", {
  # Test with only 1 line
  expect_error(iea_df(text = "abc"), "couldn't read 2 lines in iea_df")
  # Test with 2 lines but of wrong style.
  expect_error(iea_df(text = "abc\n123"), "In iea_df, input data didn't start with ',,TIME' or second line wasn't 'COUNTRY,FLOW,PRODUCT'")
  # Test with text that is expected to parse correctly.
  expect_equal(iea_df(text = ",,TIME,1960,1961\nCOUNTRY,FLOW,PRODUCT\nWorld,Production,Hard coal,42,43"), 
               data.frame(COUNTRY = "World", FLOW = "Production", PRODUCT = "Hard coal", `1960` = 42, `1961` = 43, 
                          check.names = FALSE, stringsAsFactors = FALSE))
})

test_that("augment_iea_df works", {
  iea_file <- file.path("~/Documents/Calvin stuff/Useful Work/IEA Data/Extended-Energy-Balances-2018/Extended-Energy-Balances-2018-full-ktoe.csv")
  
})
