###########################################################
context("Initialize package")
###########################################################

test_that("iea_df works", {
  # Test with only 1 line
  expect_error(iea_df(text = "abc"), "couldn't read 2 lines in iea_df")
  # Test with 2 lines but of wrong style.
  expect_error(iea_df(text = "abc\n123"), "In iea_df, input data didn't start with ',,TIME' or second line didn't start with 'COUNTRY,FLOW,PRODUCT'")
  # Test with text that is expected to parse correctly.
  # This is the format of the original .csv files.
  expectedDF <- data.frame(COUNTRY = "World", FLOW = "Production", PRODUCT = "Hard coal", `1960` = 42, `1961` = 43, 
                           check.names = FALSE, stringsAsFactors = FALSE) 
  expect_equal(iea_df(text = ",,TIME,1960,1961\nCOUNTRY,FLOW,PRODUCT\nWorld,Production,Hard coal,42,43"), expectedDF)
  # This is in a format that would arise IF someone opened the .csv file and resaved it.
  expect_equal(iea_df(text = ",,TIME,1960,1961\nCOUNTRY,FLOW,PRODUCT,,\nWorld,Production,Hard coal,42,43"), expectedDF)
  # Test with a full IEA data file in the correct format
  IEADF <- file.path("extdata", "IEA-2Countries.csv") %>% 
    system.file(package = "IEAData") %>% 
    iea_df()
  expect_equal(nrow(IEADF), 14688)
  expect_equal(ncol(IEADF), 61)
  expect_equal(colnames(IEADF)[[61]], "2017")
  # Test with an IEA data file with extra commas on the 2nd line.
  IEADF2 <- file.path("extdata", "IEA-2Countries-full2ndrow.csv") %>% 
    system.file(package = "IEAData") %>% 
    iea_df()
  expect_equal(nrow(IEADF2), 14688)
  expect_equal(ncol(IEADF2), 61)
  expect_equal(colnames(IEADF2)[[61]], "2017")
})

test_that("augment_iea_df works", {
  IEADF <- file.path("extdata", "IEA-2Countries.csv") %>% 
    system.file(package = "IEAData") %>% 
    iea_df()
  
})
