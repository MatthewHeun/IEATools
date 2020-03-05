###########################################################
context("Initialize IEA data")
###########################################################

test_that("iea_file_OK works", {
  f <- sample_iea_data_path()
  expect_true(iea_file_OK(f))
  
  # Read the file as text and use the text argument.
  conn <- file(f, open = "rt") # open file connection
  f_text <- conn %>% readLines()
  expect_true(iea_file_OK(text = f_text))
  close(conn)

  # Mess with the file and expect an error, because rows are no longer identical from one country to another.
  f1 <- data.table::fread(file = f, header = TRUE, strip.white = FALSE, sep = ",")
  f2 <- f1
  f2[[1, 3]] <- f1[[2, 3]]
  f2[[2, 3]] <- f1[[1, 3]]
  # Write to a temporary file as a .csv file
  tf <- tempfile(pattern = "iea_file_OK_test", fileext = ".csv")
  write.csv(f2, file = tf)
  expect_false(iea_file_OK(tf))
  # Delete file if it exists
  if (file.exists(tf)) {
    file.remove(tf)
  }
})

test_that("iea_df works", {
  # Test with only 1 line
  expect_error(iea_df(text = "abc"), "couldn't read 2 lines in iea_df")
  # Test with 2 lines but of wrong style.
  expect_error(iea_df(text = "abc\n123"), ".iea_file didn't start with 'COUNTRY,FLOW,PRODUCT' or ',,TIME\nCOUNTRY,FLOW,PRODUCT'.")
  # Test with text that is expected to parse correctly.
  # This is the format of the original .csv files.
  expectedDF <- data.frame(COUNTRY = "World", FLOW = "Production", PRODUCT = "Hard coal (if no detail)", `1960` = 42, `1961` = 43, 
                           check.names = FALSE, stringsAsFactors = FALSE) 
  expect_equal(iea_df(text = ",,TIME,1960,1961\nCOUNTRY,FLOW,PRODUCT\nWorld,Production,Hard coal (if no detail),42,43"), expectedDF)
  # This is in a format that would arise IF someone opened the .csv file and resaved it.
  # (Extra commas are present on the 2nd line.)
  expect_equal(iea_df(text = ",,TIME,1960,1961\nCOUNTRY,FLOW,PRODUCT,,\nWorld,Production,Hard coal (if no detail),42,43"), expectedDF)
  # Test with a full IEA data file in the correct format
  IEAfile <- sample_iea_data_path()
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
  IEADF2 <- sample_iea_data_path() %>% 
    iea_df()
  expect_equal(nrow(IEADF2), 14688)
  expect_equal(ncol(IEADF2), 5)
  expect_equal(colnames(IEADF2)[[5]], "2000")
})

test_that("iea_df works after first checking the file with iea_file_OK", {
  f <- sample_iea_data_path()
  isOK <- iea_file_OK(f)
  DF2 <- iea_df(f)
  # Verify that we got the right types of columns
  expect_true(inherits(DF2$COUNTRY, "character"))
  expect_true(inherits(DF2$FLOW, "character"))
  expect_true(inherits(DF2$PRODUCT, "character"))
  expect_true(inherits(DF2$`1971`, "numeric"))
  expect_true(inherits(DF2$`2000`, "numeric"))
})

test_that("iea_df works with a plain first row", {
  # This is an alternative format that is sometimes obtained from the IEA.
  # (Extra commas are present on the 2nd line.)
  # This is the format of the original .csv files.
  expectedDF <- data.frame(COUNTRY = "World", FLOW = "Production", PRODUCT = "Hard coal (if no detail)", `1960` = 42, `1961` = 43, 
                           check.names = FALSE, stringsAsFactors = FALSE) 
  file_contents <- "COUNTRY,FLOW,PRODUCT,1960,1961\nWorld,Production,Hard coal (if no detail),42,43"
  expect_equal(iea_df(text = file_contents), expectedDF)
  
  # Instead of text, try with a tiny file that is identical to expectedDF.
  tiny_file <- file.path("extdata", "Tiny-Example-File-Simple-Header.csv") %>% 
    system.file(package = "IEATools")
  expect_equal(iea_df(.iea_file = tiny_file), expectedDF)
})

test_that("iea_df strips white space from FLOW columns", {
  # In the IEA's 2019 data, some data are quoted to avoid creating too many columns. 
  # For example, Paper, pulp and printing is quoted in the raw .csv file: "      Paper, pulp and printing".
  # There are leading spaces, but data.table::fread
  # doesn't strip white space from quoted data.
  # So I have to do extra work to strip that white space. 
  # This test verifies that the code is working correctly.
  
  # Set up a text string that exhibits the problem
  text_contents <- ',,TIME,1960,1961\nCOUNTRY,FLOW,PRODUCT\nWorld,"      Paper, pulp and printing",Hard coal (if no detail),42,43'
  df <- iea_df(text = text_contents)
  # Expect that the leading spaces have been stripped
  expect_equal(df[[1, 2]], "Paper, pulp and printing")
  
  # Try with the sample data
  # Load the data without processing it to verify that there are leading spaces in the FLOW column
  # The example file has 272 rows in which FLOW begins with white space, even after using strip.white = TRUE.
  expect_true(data.table::fread(file = sample_iea_data_path(), strip.white = TRUE, header = TRUE, sep = ",") %>% 
                dplyr::filter(startsWith(.data$FLOW, " ")) %>% 
                nrow() == 272)
  # Now run the iea_df function over the same file. 
  # iea_df ought to strip all of that white space, leaving none left.
  expect_true(iea_df(sample_iea_data_path()) %>% 
                dplyr::filter(startsWith(.data$FLOW, " ")) %>% 
                nrow() == 0)
})

test_that("iea_df works with .. and x", {
  expectedDF <- data.frame(COUNTRY = "World", FLOW = "Production", PRODUCT = "Hard coal (if no detail)", `1960` = 0, `1961` = 0, 
                           check.names = FALSE, stringsAsFactors = FALSE) 
  expect_equal(iea_df(text = ",,TIME,1960,1961\nCOUNTRY,FLOW,PRODUCT\nWorld,Production,Hard coal (if no detail),..,x"), expectedDF)
})

test_that("iea_df works with estimated columns sufffixed by 'E'", {
  expectedDF <- data.frame(COUNTRY = "World", FLOW = "Production", PRODUCT = "Hard coal (if no detail)", `1960` = 0, 
                           check.names = FALSE, stringsAsFactors = FALSE) 
  expect_equal(iea_df(text = ",,TIME,1960,1961E\nCOUNTRY,FLOW,PRODUCT\nWorld,Production,Hard coal (if no detail),..,x"), expectedDF)
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
               "Could not find the rows that separate the Supply and Consumption sides of the ledger in augment_iea_df")
  # Try with bogus data WITH a Losses and an Iron and steel row.
  # This attempt will also fail, because the EIOU split will not be found.
  expect_error(iea_df(text = paste0(",,TIME,1960,1961\n",
                                    "COUNTRY,FLOW,PRODUCT\n",
                                    "World,Production,Hard coal (if no detail),42,43\n",
                                    "World,Losses,Hard coal (if no detail),1,2\n",
                                    "World,Iron and steel,Hard coal (if no detail),5,6")) %>% 
                 rename_iea_df_cols() %>% 
                 augment_iea_df(), 
               "Could not find the rows that separate Statistical differences from Main activity producer electricity plants in augment_iea_df")
  # Try with bogus data WITH a Losses and an Iron and steel row and WITH a Statistical differences and a Main activity producer electricity plants row.
  # This attempt will also fail, because the end of the EIOU split will not be found.
  expect_error(iea_df(text = paste0(",,TIME,1960,1961\n",
                                    "COUNTRY,FLOW,PRODUCT\n",
                                    "World,Production,Hard coal (if no detail),42,43\n",
                                    "World,Losses,Hard coal (if no detail),1,2\n",
                                    "World,Iron and steel,Hard coal (if no detail),5,6\n",
                                    "World,Statistical differences,Hard coal (if no detail),7,8\n",
                                    "World,Main activity producer electricity plants,Hard coal (if no detail),9,10")) %>% 
                 rename_iea_df_cols() %>% 
                 augment_iea_df(), 
               "Could not find the rows that separate Non-specified from Coal mines in augment_iea_df")
  
  # Try another attempt that will fail.
  expect_error(iea_df(text = paste0(",,TIME,1960,1961\n",
                                    "COUNTRY,FLOW,PRODUCT\n",
                                    "World,Production,Hard coal (if no detail),42,43\n",
                                    "World,Losses,Hard coal (if no detail),1,2\n",
                                    "World,Iron and steel,Hard coal (if no detail),5,6\n",
                                    "World,Statistical differences,Hard coal (if no detail),7,8\n",
                                    "World,Main activity producer electricity plants,Hard coal (if no detail),9,10\n",
                                    "World,Non-specified,Hard coal (if no detail),11,12\n",
                                    "World,Coal mines,Hard coal (if no detail),13,14")) %>% 
                 rename_iea_df_cols() %>% 
                 augment_iea_df(), 
               "Could not find the rows that separate Non-specified from Losses in augment_iea_df")
  # This one should work!
  simple_with_tfc_df <- iea_df(text = paste0(",,TIME,1960,1961\n",
                                             "COUNTRY,FLOW,PRODUCT\n",
                                             "World,Production,Hard coal (if no detail),42,43\n",
                                             "World,Statistical differences,Hard coal (if no detail),7,8\n",
                                             "World,Main activity producer electricity plants,Hard coal (if no detail),9,10\n",
                                             "World,Non-specified,Hard coal (if no detail),11,12\n",
                                             "World,Coal mines,Hard coal (if no detail),13,14\n",
                                             "World,Non-specified,Hard coal (if no detail),11,12\n",
                                             "World,Losses,Hard coal (if no detail),1,2\n",
                                             "World,Iron and steel,Hard coal (if no detail),5,6\n")) %>% 
    rename_iea_df_cols() %>% 
    augment_iea_df()
  expect_equal(simple_with_tfc_df$Ledger.side %>% unique(), c("Supply", "Consumption"))
  expect_equal(simple_with_tfc_df$Flow.aggregation.point, c("Total primary energy supply",
                                                            "TFC compare", 
                                                            "Transformation processes", 
                                                            "Transformation processes", 
                                                            "Energy industry own use",
                                                            "Energy industry own use",
                                                            "TFC compare",
                                                            "Industry"))
  IEADF_unaugmented <- sample_iea_data_path() %>% 
    iea_df() %>% 
    rename_iea_df_cols()
  IEADF_augmented <- IEADF_unaugmented %>% 
    augment_iea_df()
  # Check column types
  clses <- lapply(IEADF_augmented, class)
  expect_equal(clses$Method, "character")  
  expect_equal(clses$Last.stage, "character")  
  expect_equal(clses$Country, "character")  
  expect_equal(clses$Ledger.side, "character")  
  expect_equal(clses$Flow.aggregation.point, "character")  
  expect_equal(clses$Energy.type, "character")
  expect_equal(clses$Unit, "character")
  expect_equal(clses$Flow, "character")  
  expect_equal(clses$Product, "character")  
  clses[c("Method", "Last.stage", "Ledger.side", "Flow.aggregation.point", "Country", "Energy.type", "Unit", "Flow", "Product")] <- NULL
  expect_true(all(clses == "numeric"))
  # Ensure that there are no remaining .. or x.
  # This test fails if there are any NA items.
  # We know that NA items appear in the Flow.aggregation.point column.
  # So delete that column first.
  expect_false(any(IEADF_augmented %>% dplyr::select(-Flow.aggregation.point) == ".."))
  expect_false(any(IEADF_augmented %>% dplyr::select(-Flow.aggregation.point) == "x"))
  
  # As of 2019, the IEA no longer tags flows with "(transf.)", "(transformation)", or "(energy)".  
  # So these tests must be applied only to 2018 data.
  IEADF_unaugmented_2018 <- sample_iea_data_path(2018) %>% 
    iea_df() %>% 
    rename_iea_df_cols()
  IEADF_augmented_2018 <- IEADF_unaugmented_2018 %>% 
    augment_iea_df()
  
  # Check that the original has flows that end in "(transf.)"
  expect_true(nrow(IEADF_unaugmented_2018 %>% dplyr::filter(endsWith(Flow, "(transf.)"))) > 0)
  # Check that the original has flows that end in "(transformation)"
  expect_true(nrow(IEADF_unaugmented_2018 %>% dplyr::filter(endsWith(Flow, "(transformation)"))) > 0)
  # Check that the original has flows that end in "(energy)"
  expect_true(nrow(IEADF_unaugmented_2018 %>% dplyr::filter(endsWith(Flow, "(energy)"))) > 0)

  # Check that all "(transf.)" have been removed from the Flow column
  expect_equal(nrow(IEADF_augmented_2018 %>% dplyr::filter(endsWith(Flow, "(transf.)"))), 0)
  # Check that all "(transformation)" have been removed from the Flow column
  expect_equal(nrow(IEADF_augmented_2018 %>% dplyr::filter(endsWith(Flow, "(transformation)"))), 0)
  # Check that all "(energy)" have been removed from the Flow column
  expect_equal(nrow(IEADF_augmented_2018 %>% dplyr::filter(endsWith(Flow, "(energy)"))), 0)
  
  
  # Try a bogus data frame with extra spaces before the suffix.
  Cleaned <- data.frame(Flow = c("Nuclear industry      (transf.)", 
                                 "Nuclear industry    (transformation)",
                                 "Nuclear industry        (energy)",
                                 "Losses"), 
                        stringsAsFactors = FALSE) %>% 
    dplyr::mutate(
      Country = "US",
      Product = "Heat",
      E.dot = 200
    ) %>% 
    augment_iea_df()
  for (i in 1:3) {
    expect_equal(Cleaned$Flow[[i]], "Nuclear industry")
  }
})

###########################################################
context("Testing munge_to_tidy")
###########################################################

test_that("remove_agg_memo_flows works as expected", {
  Cleaned <- sample_iea_data_path() %>% 
    iea_df() %>% 
    rename_iea_df_cols() %>% 
    remove_agg_memo_flows()
  # Verify that none of the aggregation flows are present
  n_agg_rows <- Cleaned %>% 
    dplyr::filter(Flow == "Total primary energy supply" |
                    Flow == "Total final consumption" | 
                    Flow == "Transformation processes" |
                    Flow == "Energy industry own use" | 
                    Flow == "Industry" |
                    Flow == "Transport" |
                    Flow == "Other" |
                    Flow == "Non-energy use") %>% 
    nrow()
  expect_equal(n_agg_rows, 0)
  # Verify that none of the memo flows are present
  n_memo_flows <- Cleaned %>% 
    dplyr::filter(startsWith(Flow, "Memo:")) %>% 
    nrow()
  expect_equal(n_memo_flows, 0)
  # Verify that none of the memo products are present
  n_memo_products <- Cleaned %>% 
    dplyr::filter(startsWith(Product, "Memo:")) %>% 
    nrow()
  expect_equal(n_memo_products, 0)

  # Try again with a different approach. 
  # This time, ensure that rows we want to clean are present first.
  IEA_data <- sample_iea_data_path() %>% 
    iea_df() %>%
    rename_iea_df_cols() %>% 
    augment_iea_df()
  # Verify that aggregation flows exist
  agg_flows <- c("Total primary energy supply", "Total final consumption", "Transformation processes", "Energy industry own use", "Industry", "Transport", "Non-energy use")
  expect_true(lapply(agg_flows, 
                     FUN = function(s){
                       expect_true(IEA_data %>% dplyr::filter(Flow == s) %>% nrow() > 0)
                     }) %>% as.logical() %>% all())
  # Verify that flow memos exist
  memo_flow_prefixes <- c("Memo: ", "Electricity output (GWh)", "Heat output")
  expect_true(lapply(memo_flow_prefixes, 
                     FUN = function(s){
                       expect_true(IEA_data %>% dplyr::filter(startsWith(Flow, s)) %>% nrow() > 0)
                     }) %>% as.logical() %>% all())
  # Verify that product memos exist
  memo_product_prefix <- "Memo: "
  expect_true(IEA_data %>% dplyr::filter(startsWith(Product, memo_product_prefix)) %>% nrow() > 0)
  
  # Now clean the aggregation flows and see if they're gone.
  Cleaned <- IEA_data %>% 
    remove_agg_memo_flows()
  # Ensure that aggregation flows were removed.
  expect_true(lapply(agg_flows, 
                     FUN = function(s){
                       expect_true(Cleaned %>% dplyr::filter(Flow == s) %>% nrow() == 0)
                     }) %>% as.logical() %>% all())
  # Ensure that flow memos were removed
  expect_true(lapply(memo_flow_prefixes, 
                     FUN = function(s){
                       expect_true(Cleaned %>% dplyr::filter(startsWith(Flow, s)) %>% nrow() == 0)
                     }) %>% as.logical() %>% all())
  # Ensure that product memos were removed
  expect_true(IEA_data %>% dplyr::filter(startsWith(Product, memo_product_prefix)) %>% nrow() > 0)
})

test_that("use_iso_countries works as expected", {
  iso3 <- sample_iea_data_path() %>% 
    iea_df() %>% 
    rename_iea_df_cols() %>% 
    use_iso_countries()
  expect_false(any(iso3$Country == "South Africa"))
  expect_true(any(iso3$Country == "ZAF"))
  expect_false(any(iso3$Country == "Ghana"))
  expect_true(any(iso3$Country == "GHA"))

  # Try with 2-letter vs. 3-letter abbreviations
  iso2 <- sample_iea_data_path() %>% 
    iea_df() %>% 
    rename_iea_df_cols() %>% 
    use_iso_countries(iso_abbrev_type = 2)
  expect_false(any(iso2$Country == "South Africa"))
  expect_true(any(iso2$Country == "ZA"))
  expect_false(any(iso2$Country == "Ghana"))
  expect_true(any(iso2$Country == "GH"))
  
  # Try with a data frame that contains a World country.
  world <- iea_df(text = paste0(",,TIME,1960,1961\n",
                                "COUNTRY,FLOW,PRODUCT\n",
                                "World,Production,Hard coal (if no detail),42,43\n",
                                "World,Losses,Hard coal (if no detail),1,2")) %>% 
    rename_iea_df_cols() %>% 
    augment_iea_df() %>% 
    use_iso_countries()
  # Ensure that a "World" country is present.
  n_world_rows <- world %>% 
    dplyr::filter(Country == "World") %>% 
    nrow()
  expect_equal(n_world_rows, 2)
})


test_that("load_tidy_iea_df works as expected", {
  iea_tidy_df <- load_tidy_iea_df()
  # Verify column names and order
  expect_equal(names(iea_tidy_df), c("Country", "Method", "Energy.type", "Last.stage", "Year", "Ledger.side", "Flow.aggregation.point", 
                                     "Flow", "Product", "Unit", "E.dot"))
  # This is a energy exclusive data frame
  expect_true(all(iea_tidy_df$Energy.type == "E"))
  # This is a completely ktoe data frame
  expect_true(all(iea_tidy_df$Unit == "ktoe"))
  # Ledger.side can be only Supply or Consumption
  expect_true(all(iea_tidy_df$Ledger.side %in% c("Supply", "Consumption")))
})


test_that("converting year to numeric works as expected", {
  iea_tidy_df <- load_tidy_iea_df()
  expect_true(is.numeric(iea_tidy_df$Year))
})


test_that("trimming white space works", {
  cleaned <- data.frame(Flow = "  a flow   ", Product = "   a product   ", stringsAsFactors = FALSE) %>% 
    clean_iea_whitespace()
  expect_equal(cleaned$Flow[[1]], "a flow")
  expect_equal(cleaned$Product[[1]], "a product")
})


test_that("load_tidy_iea_df works as expected", {
  simple <- load_tidy_iea_df()
  complicated <- sample_iea_data_path() %>% 
    iea_df() %>%
    rename_iea_df_cols() %>% 
    clean_iea_whitespace() %>% 
    remove_agg_memo_flows() %>% 
    use_iso_countries() %>% 
    augment_iea_df() %>% 
    tidy_iea_df()
  expect_equal(simple, complicated)
})


test_that("Ledger.side is added by agugmentation", {
  # Every row in the Ledger.side column should be filled with a non-NA entry.
  # Verify that's indeed the case.
  for (year in valid_iea_release_years) {
    DF <- load_tidy_iea_df(sample_iea_data_path(year))
    expect_false(DF %>% 
                   magrittr::extract2("Ledger.side") %>% 
                   is.na() %>% 
                   any())
  }
})


test_that("every Flow.aggregation.point is filled by augmentation", {
  # Every row in the Flow.aggregation.point column should be filled with a non-NA entry.
  # Verify that's indeed the case.
  for (year in valid_iea_release_years) {
    expect_false(load_tidy_iea_df(sample_iea_data_path(year)) %>% 
                   magrittr::extract2("Flow.aggregation.point") %>% 
                   is.na() %>% 
                   any())
  }
})


test_that("spreading by years works as expected after load_tidy_iea_df()", {
  # This test will fail if things are not specified correctly.
  # Without correct specification, keys will not be unique.
  for (year in valid_iea_release_years) {
    year_spread <- load_tidy_iea_df(sample_iea_data_path(year)) %>% 
      tidyr::spread(key = Year, value = E.dot)
    expect_true("1971" %in% names(year_spread))
    expect_true("2000" %in% names(year_spread))
  }
})


