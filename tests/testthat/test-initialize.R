###########################################################
context("Initialize IEA data")
###########################################################

test_that("use_iso_countries() works as expected", {
  IEAData <- sample_iea_data_path() %>% 
    iea_df() %>%
    rename_iea_df_cols() 
  IEAData %>% 
    use_iso_countries() %>% 
    magrittr::extract2("Country") %>% 
    unique() %>% 
    expect_equal(c("GHA", "ZAF"))
  
  # Now try with the China exception that has been hard-coded.
  IEAData %>% 
    dplyr::mutate(
      Country = dplyr::recode(Country, Ghana = "People's Republic of China")
    ) %>% 
    use_iso_countries() %>% 
    magrittr::extract2("Country") %>% 
    unique() %>% 
    expect_equal(c("CHN", "ZAF"))
})


test_that("iea_file_OK works", {
  # Try from a file
  f <- sample_iea_data_path()
  expect_true(iea_file_OK(f))
  # Try after slurping
  df <- slurp_iea_to_raw_df(f)
  expect_true(iea_file_OK(.slurped_iea_df = df))
  
  # Read the file as text and use the text argument.
  conn <- file(f, open = "rt") # open file connection
  f_text <- conn %>% readLines()
  expect_true(iea_file_OK(text = f_text))
  close(conn)

  # Mess with the file and expect an error, because rows are no longer identical from one country to another.
  # f1 <- data.table::fread(file = f, header = TRUE, strip.white = FALSE, sep = ",")
  f1 <- read.csv(file = f, header = TRUE, strip.white = FALSE, sep = ",")
  f2 <- f1
  # Switch Hard coal and Brown coal in the PRODUCT column.
  f2[[1, 3]] <- f1[[2, 3]]
  f2[[2, 3]] <- f1[[1, 3]]
  # Write the messed-up data to a temporary file as a .csv file
  tf <- tempfile(pattern = "iea_file_OK_test", fileext = ".csv")
  write.csv(f2, file = tf, row.names = FALSE)
  # Read it back to confirm that it is messed up
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
  expect_error(iea_df(text = "abc\n123"), ".iea_file must start with first line: 'COUNTRY,FLOW,PRODUCT', or first line: ',,TIME' and second line: 'COUNTRY,FLOW,PRODUCT'.  Instead, found first line: 'abc', second line: '123'.")
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
  # Test that it works with a slurped df
  slurped <- sample_iea_data_path() %>% 
    slurp_iea_to_raw_df()
  IEADF3 <- iea_df(.slurped_iea_df = slurped)
  expect_equal(nrow(IEADF3), 14688)
  expect_equal(ncol(IEADF3), 5)
  expect_equal(colnames(IEADF3)[[5]], "2000")
})

test_that("iea_df works after first checking the file with iea_file_OK", {
  f <- sample_iea_data_path()
  isOK <- iea_file_OK(f)
  DF2 <- iea_df(f)
  # Verify that we got the right types of columns
  expect_true(is.character(DF2$COUNTRY))
  expect_true(is.character(DF2$FLOW))
  expect_true(is.character(DF2$PRODUCT))
  expect_true(is.numeric(DF2$`1971`))
  expect_true(is.numeric(DF2$`2000`))
})

test_that("iea_df works with a plain first row", {
  # This is an alternative format that is sometimes obtained from the IEA.
  # (Extra commas are present on the 2nd line.)
  # This is the format of the original .csv files.
  expectedDF <- data.frame(COUNTRY = "World", FLOW = "Production", PRODUCT = "Hard coal (if no detail)", `1960` = 42, `1961` = 43, 
                           check.names = FALSE, stringsAsFactors = FALSE) 
  file_contents <- "COUNTRY,FLOW,PRODUCT,1960,1961\nWorld,Production,Hard coal (if no detail),42,43"
  expect_equal(iea_df(text = file_contents), expectedDF)
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
               "Could not find the rows that separate the Supply and Consumption sides of the ledger in find_supply_consumption_split")
  # Try with bogus data WITH a Losses and an Iron and steel row.
  # This attempt will also fail, because the EIOU split will not be found.
  expect_error(iea_df(text = paste0(",,TIME,1960,1961\n",
                                    "COUNTRY,FLOW,PRODUCT\n",
                                    "World,Production,Hard coal (if no detail),42,43\n",
                                    "World,Losses,Hard coal (if no detail),1,2\n",
                                    "World,Iron and steel,Hard coal (if no detail),5,6")) %>% 
                 rename_iea_df_cols() %>% 
                 augment_iea_df(), 
               "Could not find the rows that identify the beginning of transformation processes in find_transformation_start")
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
               "Could not find the rows that identify the end of Transformation Process rows and the beginning of Energy industry own use in find_transformation_end")
  
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
               "Could not find the rows that separate Non-specified from Losses in find_eiou_end")
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
  simple_with_tfc_df_2 <- iea_df(text = paste0(",,TIME,1960,1961\n",
                                             "COUNTRY,FLOW,PRODUCT\n",
                                             "World,Production,Hard coal (if no detail),42,43\n",
                                             "World,Statistical differences,Hard coal (if no detail),7,8\n",
                                             "World,Main activity producer electricity plants      (transf.),Hard coal (if no detail),9,10\n",
                                             "World,Non-specified,Hard coal (if no detail),11,12\n",
                                             "World,Coal mines      (energy),Hard coal (if no detail),13,14\n",
                                             "World,Non-specified,Hard coal (if no detail),11,12\n",
                                             "World,Losses,Hard coal (if no detail),1,2\n",
                                             "World,Iron and steel,Hard coal (if no detail),5,6\n")) %>% 
    rename_iea_df_cols() %>% 
    augment_iea_df()
  expect_equal(simple_with_tfc_df_2$Flow[[3]], "Main activity producer electricity plants")
  expect_equal(simple_with_tfc_df_2$Flow[[5]], "Coal mines")
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


test_that("remove_agg_regions works as expected", {
  tibble::tibble(Year = c(1967, 1995), Country = c("World", "Spain")) %>%
    remove_agg_regions() %>%
    expect_equal(tibble::tibble(Year = 1995, Country = "Spain"))
  
  n_regions <- length(IEATools::aggregation_regions)
  result <- tibble::tibble(Year = 1900 + 1:(n_regions+1), Country = unlist(c("Spain", IEATools::aggregation_regions))) %>%
    remove_agg_regions()
  expect_equal(result$Year, 1901)
  expect_equivalent(result[["Country"]], "Spain")
  
  # Ensure that Greenland, Palestinian Authority, and Uganda are retained.
  tibble::tibble(Year = c(1967, 1990, 1995, 2020), Country = c("Memo: Greenland", "Memo: Palestinian Authority", 
                                                                "Memo: Uganda", "World")) %>% 
    remove_agg_regions() %>% 
    magrittr::extract2("Year") %>% 
    expect_equal(c(1967, 1990, 1995))
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

  # Try with a data frame that contains a World country.
  world <- iea_df(text = paste0(",,TIME,1960,1961\n",
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
    augment_iea_df() %>% 
    use_iso_countries()
  # Ensure that a "World" country is present.
  n_world_rows <- world %>% 
    dplyr::filter(Country == "World") %>% 
    nrow()
  expect_equal(n_world_rows, 8)
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


test_that("Ledger.side is added by augmentation", {
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


test_that("Loading regional concordance matrix works as intended", {
  concordance_matrix <- read_regions_concordance(file_path = "../testdata/concordance_table_testing.csv")

  expect_equal(nrow(concordance_matrix %>% dplyr::filter(IEA_regions == "Angola")), 0) # Testing that NAs are gotten rid of
  expect_equal(nrow(concordance_matrix %>% dplyr::filter(IEA_regions == "Argentina")), 0) # Testing empty celles are gotten rid of

  expect_equal(nrow(concordance_matrix %>%
                 dplyr::filter(IEA_regions == "France" & Destination_regions == "Fr") %>%
                 dplyr::select(Destination_regions)), 1)
})
#--- EAR, 02/09/2020


test_that("Aggregating by countries works as intended", {

  ### 1. First, checking that it works well when net_trade flag is FALSE.
  tidy_GHA_ZAF_df <- load_tidy_iea_df()

  aggregated_regions <- aggregate_regions(tidy_GHA_ZAF_df,
                                          file_path = "../testdata/checking_aggregation_GHA_ZAF.csv",
                                          net_trade = FALSE)

  manual_aggregation <- tidy_GHA_ZAF_df %>%
    dplyr::group_by(Year, Ledger.side, Flow.aggregation.point, Flow, Product) %>%
    dplyr::summarise(E.dot.aggregated = sum(E.dot))

  comparing <- aggregated_regions %>%
    dplyr::full_join(manual_aggregation) %>%
    dplyr::mutate(
      difference = E.dot.aggregated - E.dot
    )

  # Testing that all rows are perfectly equal and that there are the same number of rows

  count_non_null_differences <- comparing %>%
    dplyr::filter(difference != 0) %>%
    nrow()

  expect_equal(count_non_null_differences, 0)
  expect_equal(nrow(aggregated_regions), nrow(manual_aggregation))


  ### 2. Second, now we check that it works well when net_trade is TRUE.

  aggregated_regions <- aggregate_regions(tidy_GHA_ZAF_df,
                                          file_path = "../testdata/checking_aggregation_GHA_ZAF.csv",
                                          net_trade = TRUE)

  manual_aggregation_excl_ie <- tidy_GHA_ZAF_df %>%
    dplyr::filter(! Flow %in% c("Imports", "Exports")) %>%
    dplyr::group_by(Year, Ledger.side, Flow.aggregation.point, Flow, Product) %>%
    dplyr::summarise(E.dot.aggregated = sum(E.dot))

  manual_aggregation_ie <- tidy_GHA_ZAF_df %>%
    dplyr::filter(Flow %in% c("Imports", "Exports")) %>%
    dplyr::group_by(Year, Ledger.side, Flow.aggregation.point, Flow, Product) %>%
    dplyr::summarise(E.dot.aggregated = sum(E.dot)) %>%
    tidyr::pivot_wider(names_from = Flow, values_from = E.dot.aggregated) %>%
    dplyr::mutate(
      Imports = tidyr::replace_na(Imports, 0),
      Exports = tidyr::replace_na(Exports, 0),
      Net_Imports = Imports + Exports
    ) %>%
    dplyr::select(-c("Imports", "Exports")) %>%
    tidyr::pivot_longer(cols = Net_Imports, names_to = "Flow", values_to = "E.dot.aggregated") %>%
    dplyr::mutate(
      Flow = dplyr::case_when(
        E.dot.aggregated > 0 ~ "Imports",
        E.dot.aggregated < 0 ~ "Exports",
        E.dot.aggregated == 0 ~ "Net_Imports"
      )
    ) %>%
    dplyr::filter(E.dot.aggregated != 0)

  manual_aggregation <- dplyr::bind_rows(manual_aggregation_excl_ie, manual_aggregation_ie)

  comparing <- aggregated_regions %>%
    dplyr::full_join(manual_aggregation) %>%
    dplyr::mutate(
      difference = E.dot.aggregated - E.dot
    )

  # Testing that all rows are perfectly equal and that there are the same number of rows

  count_non_null_differences <- comparing %>%
    dplyr::filter(difference != 0) %>%
    nrow()

  expect_equal(count_non_null_differences, 0)
  expect_equal(nrow(aggregated_regions), nrow(manual_aggregation))

})
# --- EAR, 02/09/2020
