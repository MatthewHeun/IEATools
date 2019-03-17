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
  expectedDF <- data.frame(COUNTRY = "World", FLOW = "Production", PRODUCT = "Hard coal (if no detail)", `1960` = 42, `1961` = 43, 
                           check.names = FALSE, stringsAsFactors = FALSE) 
  expect_equal(iea_df(text = ",,TIME,1960,1961\nCOUNTRY,FLOW,PRODUCT\nWorld,Production,Hard coal (if no detail),42,43"), expectedDF)
  # This is in a format that would arise IF someone opened the .csv file and resaved it.
  # (Extra commas are present on the 2nd line.)
  expect_equal(iea_df(text = ",,TIME,1960,1961\nCOUNTRY,FLOW,PRODUCT,,\nWorld,Production,Hard coal (if no detail),42,43"), expectedDF)
  # Test with a full IEA data file in the correct format
  # IEAfile <- file.path("extdata", "IEA-2Countries-full2ndrow.csv") %>% 
  IEAfile <- file.path("extdata", "GH-ZA-ktoe-Extended-Energy-Balances-sample.csv") %>% 
    system.file(package = "IEATools")
  IEAtext <- readChar(IEAfile, file.info(IEAfile)$size)
  # Eliminate all series of commas at ends of lines
  # The pattern ,*$ means "match any number (*) of commas (,) at the end of a line ($)".
  IEAtext <- IEAtext %>% gsub(pattern = ",*$", replacement = "", IEAtext)
  # Ensure that commas have been removed from the end of a line.
  # The pattern ",$" means "match any commas (,) at the end of a line ($)
  expect_false(grepl(pattern = ",$", x = IEAtext))
  IEADF <- iea_df(text = IEAtext)
  expect_equal(nrow(IEADF), 14688)
  expect_equal(ncol(IEADF), 5)
  expect_equal(colnames(IEADF)[[5]], "2000")
  # Test with an IEA data file with extra commas on the 2nd line.
  IEADF2 <- file.path("extdata", "GH-ZA-ktoe-Extended-Energy-Balances-sample.csv") %>% 
    system.file(package = "IEATools") %>% 
    iea_df()
  expect_equal(nrow(IEADF2), 14688)
  expect_equal(ncol(IEADF2), 5)
  expect_equal(colnames(IEADF2)[[5]], "2000")
})

test_that("iea_df works with .. and x", {
  expectedDF <- data.frame(COUNTRY = "World", FLOW = "Production", PRODUCT = "Hard coal (if no detail)", `1960` = 0, `1961` = 0, 
                           check.names = FALSE, stringsAsFactors = FALSE) 
  expect_equal(iea_df(text = ",,TIME,1960,1961\nCOUNTRY,FLOW,PRODUCT\nWorld,Production,Hard coal (if no detail),..,x"), expectedDF)
})

test_that("rename_iea_df_cols works", {
  renamed <- iea_df(text = ",,TIME,1960,1961\nCOUNTRY,FLOW,PRODUCT\nWorld,Production,Hard coal (if no detail),42,43") %>% 
    rename_iea_df_cols()
  expect_equal(names(renamed), c("Country", "Flow", "Product", "1960", "1961"))
})

test_that("augment_iea_df works", {
  # Try with a bogus set of data without a Losses row or a Total final consumption row.
  expect_error(iea_df(text = ",,TIME,1960,1961\nCOUNTRY,FLOW,PRODUCT\nWorld,Production,Hard coal (if no detail),42,43") %>% 
    rename_iea_df_cols() %>% 
    augment_iea_df(), 
    "Found neither Losses nor Total final consumption in the Flow column.")
  # Try with bogus data WITH a Losses row.
  simple_with_losses_df <- iea_df(text = paste0(",,TIME,1960,1961\n",
                                                "COUNTRY,FLOW,PRODUCT\n",
                                                "World,Production,Hard coal (if no detail),42,43\n",
                                                "World,Losses,Hard coal (if no detail),1,2")) %>% 
    rename_iea_df_cols() %>% 
    augment_iea_df()
  expect_equal(simple_with_losses_df$Ledger.side, c("Supply", "Supply"))
  expect_equal(simple_with_losses_df$Flow.aggregation.point, c("Total primary energy supply", "TFC compare"))
  # Try with bogus data with a Total final consumption row.
  simple_with_tfc_df <- iea_df(text = paste0(",,TIME,1960,1961\n",
                                             "COUNTRY,FLOW,PRODUCT\n",
                                             "World,Production,Hard coal (if no detail),42,43\n",
                                             "World,Total final consumption,Hard coal (if no detail),1,2")) %>% 
    rename_iea_df_cols() %>% 
    augment_iea_df()
  expect_equal(simple_with_tfc_df$Ledger.side, c("Supply", "Consumption"))
  expect_equal(simple_with_tfc_df$Flow.aggregation.point, c("Total primary energy supply", NA_character_))
  
  IEADF_augmented <- file.path("extdata", "GH-ZA-ktoe-Extended-Energy-Balances-sample.csv") %>% 
    system.file(package = "IEATools") %>% 
    iea_df() %>% 
    rename_iea_df_cols() %>% 
    augment_iea_df()
  # Check column types
  clses <- lapply(IEADF_augmented, class)
  expect_equal(clses$Ledger.side, "character")  
  expect_equal(clses$Flow.aggregation.point, "character")  
  expect_equal(clses$Country, "character")  
  expect_equal(clses$Energy.type, "character")
  expect_equal(clses$Unit, "character")
  expect_equal(clses$Flow, "character")  
  expect_equal(clses$Product, "character")  
  clses[c("Ledger.side", "Flow.aggregation.point", "Country", "Energy.type", "Unit", "Flow", "Product")] <- NULL
  expect_true(all(clses == "numeric"))
  # Ensure that there are no remaining .. or x.
  # This test fails if there are any NA items.
  # We know that NA items appear in the Flow.aggregation.point column.
  # So delete that column first.
  expect_false(any(IEADF_augmented %>% dplyr::select(-Flow.aggregation.point) == ".."))
  expect_false(any(IEADF_augmented %>% dplyr::select(-Flow.aggregation.point) == "x"))
})
