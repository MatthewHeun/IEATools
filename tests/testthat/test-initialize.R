###########################################################
context("Initialize package")
###########################################################

test_that("fix_header works", {
  expect_error(iea_df(text = "abc"), "couldn't read 2 lines in iea_df")
  expect_error(iea_df(text = "abc\n123"), "In iea_df, input data didn't start with ',,TIME' or second line wasn't 'COUNTRY,FLOW,PRODUCT'")
  
  expect_equal(iea_df(text = ",,TIME,1960,1961\nCOUNTRY,FLOW,PRODUCT\nWorld,Production,Hard coal,42,43"), 
               data.frame(COUNTRY = "World", FLOW = "Production", PRODUCT = "Hard coal", `1960` = 42, `1961` = 43))
  
  
  
  iea_file <- file.path("~/Documents/Calvin stuff/Useful Work/IEA Data/Extended-Energy-Balances-2018/Extended-Energy-Balances-2018-full-ktoe.csv")
  
  
  
  expect_error(fix_header("abc\n123\nxyz"), "unfixed_header should have only 1 or 2 lines: 3 lines were found")
  expect_error(fix_header("abc"))

  # Note: Need to build the package before this file is put in the correct location.
  mock_filename <- system.file("extdata", "example-unfixed-header.csv", package = "IEAData")
  mock_file <- readChar(mock_filename, file.info(mock_filename)$size)
  expect_true(mock_file != "")
  expect_true(fix_header(mock_file) %>% startsWith("COUNTRY,FLOW,PRODUCT,1960"))
})
