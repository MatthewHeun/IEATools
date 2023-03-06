
test_that("encoding works OK", {
  # The country names Cote d'Ivoire and Curacao have diacritical marks in their country names.
  # The "test_country_name_encoding.csv" file is a sample of an IEA data file
  # which 3 rows, one for World, one for Cote d'Ivoire, and one for Curacao.
  # This test reads the example file and ensures that the names are convertible to straight 
  # ASCII as we do in the code itself.
  res <- data.table::fread(file = system.file("testdata", "test_country_name_encoding.csv", 
                                              package = "IEATools"), 
                           header = TRUE, 
                           encoding = "Latin-1") |>
  # Now change it to ascii characters everywhere.
    dplyr::mutate(
      # This hint is from
      # https://stackoverflow.com/questions/39148759/remove-accents-from-a-dataframe-column-in-r
      # COUNTRY = stringi::stri_trans_general(COUNTRY,id = "Latin-ASCII")
      COUNTRY = iconv(COUNTRY, from = "latin1", to = "ASCII//TRANSLIT"), 
      COUNTRY = gsub(COUNTRY, pattern = "\\^o", replacement = "o")
    )
  expect_equal(res$COUNTRY[[2]], "Cote d'Ivoire")
  expect_equal(res$COUNTRY[[3]], "Curacao/Netherlands Antilles")
})


test_that("use_iso_countries() works with override", {
  iea_df <- tibble::tribble(~Country, 
                            "People's Republic of China", 
                            "Hong Kong (China)", 
                            "World marine bunkers",
                            "World aviation bunkers", 
                            "Ghana", 
                            "Former Soviet Union (if no detail)",
                            "South Africa", 
                            "World", 
                            "Former Yugoslavia (if no detail)")
  res <- iea_df |> 
    use_iso_countries()
  expect_equal(res$Country, c("CHNM", "HKG", "WMBK", "WABK", "GHA", "SUN", "ZAF", "WRLD", "YUG"))
})


test_that("use_iso_countries() works with more columns in override", {
  iea_df <- tibble::tribble(~Country, ~`2000`, ~`2001`,
                            "People's Republic of China", 42, 43,
                            "Hong Kong (China)", 44, 45)
  override <- IEATools::override_iso_codes_df |> 
    dplyr::mutate(bogus_col = "bogus information")
  res <- iea_df |> 
    use_iso_countries(override_df = override) |> 
    tidyr::pivot_longer(cols = c("2000", "2001"), names_to = "Year", values_to = "E.dot")
  
  expect_equal(names(res), c("Country", "Year", "E.dot"))
})


test_that("use_iso_countries() works as expected", {
  for (yr in IEATools::valid_iea_release_years) {
    IEAData <- sample_iea_data_path(yr) |> 
      iea_df() |>
      rename_iea_df_cols() 
    IEAData |> 
      use_iso_countries() |> 
      magrittr::extract2("Country") |> 
      unique() |> 
      expect_equal(c("GHA", "ZAF"))
    
    # Now try with the China exception that has been hard-coded.
    IEAData |> 
      dplyr::mutate(
        Country = dplyr::recode(Country, Ghana = "People's Republic of China")
      ) |> 
      use_iso_countries() |> 
      magrittr::extract2("Country") |> 
      unique() |> 
      expect_equal(c("CHNM", "ZAF"))
    
    # Make ZAF into Hong Kong to be sure that it is recoded to HKG.
    IEAData |> 
      dplyr::mutate(
        Country = dplyr::recode(Country, `South Africa` = "Hong Kong (China)")
      ) |> 
      use_iso_countries() |> 
      magrittr::extract2("Country") |> 
      unique() |> 
      expect_equal(c("GHA", "HKG"))
    
    # Now make ZAF into World marine bunkers to be sure it is recoded to WMB.
    IEAData |> 
      dplyr::mutate(
        Country = dplyr::recode(Country, `South Africa` = "World marine bunkers")
      ) |> 
      use_iso_countries() |> 
      magrittr::extract2("Country") |> 
      unique() |> 
      expect_equal(c("GHA", "WMBK"))
    
    # Now make GHA into World aviation bunkers to be sure it is recoded to WAB.
    IEAData |> 
      dplyr::mutate(
        Country = dplyr::recode(Country, `Ghana` = "World aviation bunkers")
      ) |> 
      use_iso_countries() |> 
      magrittr::extract2("Country") |> 
      unique() |> 
      expect_equal(c("WABK", "ZAF"))
  }
})


test_that("iea_file_OK() works", {
  
  for (yr in IEATools::valid_iea_release_years) {
    # Try from a file
    f <- sample_iea_data_path(yr)
    expect_true(iea_file_OK(f))
    # Try after slurping
    df <- slurp_iea_to_raw_df(f)
    expect_true(iea_file_OK(.slurped_iea_df = df))
    
    # Read the file as text and use the text argument.
    conn <- file(f, open = "rt") # open file connection
    f_text <- conn |> readLines()
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
    if (yr == 2018) {
      # Because the headers on the 2018 example file are messy, 
      # an outright failure is obtained.
      expect_error(iea_file_OK(tf))
    } else {
      expect_false(iea_file_OK(tf))
    }
    # Delete file if it exists
    if (file.exists(tf)) {
      file.remove(tf)
    }
  }
})


test_that("reading IEA files from all valid release years works", {
  for (yr in IEATools::valid_iea_release_years) {
    f <- sample_iea_data_path(yr)
    expect_true(iea_file_OK(f))
  }
})


test_that("iea_df() works", {
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
  
  
  for (yr in IEATools::valid_iea_release_years) {
    # Test with a full IEA data file in the correct format
    IEAfile <- sample_iea_data_path(yr)
    IEAtext <- readChar(IEAfile, file.info(IEAfile)$size)
    # Eliminate all series of commas at ends of lines
    # The pattern ,*$ means "match any number (*) of commas (,) at the end of a line ($)".
    IEAtext <- IEAtext |> gsub(pattern = ",*$", replacement = "", IEAtext)
    # Ensure that commas have been removed from the end of a line.
    # The pattern ",$" means "match any commas (,) at the end of a line ($)
    expect_false(grepl(pattern = ",$", x = IEAtext))
    IEADF <- iea_df(text = IEAtext)
    expect_equal(nrow(IEADF), 14688)
    expect_equal(ncol(IEADF), 5)
    expect_equal(colnames(IEADF)[[5]], "2000")
    # Test with an IEA data file with extra commas on the 2nd line.
    IEADF2 <- sample_iea_data_path(yr) |> 
      iea_df()
    expect_equal(nrow(IEADF2), 14688)
    expect_equal(ncol(IEADF2), 5)
    expect_equal(colnames(IEADF2)[[5]], "2000")
    # Test that it works with a slurped df
    slurped <- sample_iea_data_path(yr) |> 
      slurp_iea_to_raw_df()
    IEADF3 <- iea_df(.slurped_iea_df = slurped)
    expect_equal(nrow(IEADF3), 14688)
    expect_equal(ncol(IEADF3), 5)
    expect_equal(colnames(IEADF3)[[5]], "2000")
  }
})


test_that("iea_df() works after first checking the file with iea_file_OK", {
  for (yr in IEATools::valid_iea_release_years) {
    f <- sample_iea_data_path(yr)
    isOK <- iea_file_OK(f)
    DF2 <- iea_df(f)
    # Verify that we got the right types of columns
    expect_true(is.character(DF2$COUNTRY))
    expect_true(is.character(DF2$FLOW))
    expect_true(is.character(DF2$PRODUCT))
    expect_true(is.numeric(DF2$`1971`))
    expect_true(is.numeric(DF2$`2000`))
  }
})


test_that("iea_df() works with a plain first row", {
  # This is an alternative format that is sometimes obtained from the IEA.
  # (Extra commas are present on the 2nd line.)
  # This is the format of the original .csv files.
  expectedDF <- data.frame(COUNTRY = "World", FLOW = "Production", PRODUCT = "Hard coal (if no detail)", `1960` = 42, `1961` = 43, 
                           check.names = FALSE, stringsAsFactors = FALSE) 
  file_contents <- "COUNTRY,FLOW,PRODUCT,1960,1961\nWorld,Production,Hard coal (if no detail),42,43"
  expect_equal(iea_df(text = file_contents), expectedDF)
})


test_that("iea_df() strips white space from FLOW columns", {
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
  
  
  for (yr in IEATools::valid_iea_release_years) {
    # Read the raw data
    raw <- data.table::fread(file = sample_iea_data_path(yr), strip.white = TRUE, header = TRUE, sep = ",")
    # Account for 2018 which has a weird top row.
    if (raw[[1, 2]] == "FLOW") {
      # Set names to the first row. 
      names(raw) <- c("COUNTRY", "FLOW", "PRODUCT", "1971", "2000")
    }
    # How many rows have leading spaces in the FLOW column?
    num_rows_with_spaces <- raw |> 
      dplyr::filter(startsWith(.data[["FLOW"]], " ")) |> 
      nrow()
    if (num_rows_with_spaces > 0) {
      # Strip the spaces off
      supposedly_no_spaces <- iea_df(sample_iea_data_path(yr))
      # Make sure no rows left.
      num_rows_with_spaces <- supposedly_no_spaces |> 
        dplyr::filter(startsWith(.data$FLOW, " ")) |> 
        nrow()
      expect_true(num_rows_with_spaces == 0)
    }
  }
})


test_that("iea_df() works with .. and x", {
  expectedDF <- data.frame(COUNTRY = "World", FLOW = "Production", PRODUCT = "Hard coal (if no detail)", `1960` = 0, `1961` = 0, 
                           check.names = FALSE, stringsAsFactors = FALSE) 
  expect_equal(iea_df(text = ",,TIME,1960,1961\nCOUNTRY,FLOW,PRODUCT\nWorld,Production,Hard coal (if no detail),..,x"), expectedDF)
})


test_that("iea_df() works with estimated columns sufffixed by 'E'", {
  expectedDF <- data.frame(COUNTRY = "World", FLOW = "Production", PRODUCT = "Hard coal (if no detail)", `1960` = 0, 
                           check.names = FALSE, stringsAsFactors = FALSE) 
  expect_equal(iea_df(text = ",,TIME,1960,1961E\nCOUNTRY,FLOW,PRODUCT\nWorld,Production,Hard coal (if no detail),..,x"), expectedDF)
})


test_that("iea_df() works with all valid release years", {
  lapply(IEATools::valid_iea_release_years, function(yr) {
    df <- iea_df(sample_iea_data_path(yr))
    # Make sure we got something in the read. 
    # This is a minimal test to make sure the sample data are included with the package.
    expect_true(nrow(df) > 0)
  })
})


test_that("rename_iea_df_cols() works", {
  renamed <- iea_df(text = ",,TIME,1960,1961\nCOUNTRY,FLOW,PRODUCT\nWorld,Production,Hard coal (if no detail),42,43") |> 
    rename_iea_df_cols()
  expect_equal(names(renamed), c("Country", "Flow", "Product", "1960", "1961"))
})


test_that("augment_iea_df() works", {
  # Try with a bogus set of data without a Losses row or a Total final consumption row.
  expect_error(iea_df(text = ",,TIME,1960,1961\nCOUNTRY,FLOW,PRODUCT\nWorld,Production,Hard coal (if no detail),42,43") |> 
                 rename_iea_df_cols() |> 
                 augment_iea_df(), 
               "Could not find the rows that separate the Supply and Consumption sides of the ledger in find_supply_consumption_split")
  # Try with bogus data WITH a Losses and an Iron and steel row.
  # This attempt will also fail, because the EIOU split will not be found.
  expect_error(iea_df(text = paste0(",,TIME,1960,1961\n",
                                    "COUNTRY,FLOW,PRODUCT\n",
                                    "World,Production,Hard coal (if no detail),42,43\n",
                                    "World,Losses,Hard coal (if no detail),1,2\n",
                                    "World,Iron and steel,Hard coal (if no detail),5,6")) |> 
                 rename_iea_df_cols() |> 
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
                                    "World,Main activity producer electricity plants,Hard coal (if no detail),9,10")) |> 
                 rename_iea_df_cols() |> 
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
                                    "World,Coal mines,Hard coal (if no detail),13,14")) |> 
                 rename_iea_df_cols() |> 
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
                                             "World,Iron and steel,Hard coal (if no detail),5,6\n")) |> 
    rename_iea_df_cols() |> 
    augment_iea_df()
  expect_equal(simple_with_tfc_df$Ledger.side |> unique(), c("Supply", "Consumption"))
  expect_equal(simple_with_tfc_df$Flow.aggregation.point, c("Total primary energy supply",
                                                            "TFC compare", 
                                                            "Transformation processes", 
                                                            "Transformation processes", 
                                                            "Energy industry own use",
                                                            "Energy industry own use",
                                                            "TFC compare",
                                                            "Industry"))
  IEADF_unaugmented <- sample_iea_data_path() |> 
    iea_df() |> 
    rename_iea_df_cols()
  IEADF_augmented <- IEADF_unaugmented |> 
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
  expect_false(any(IEADF_augmented |> dplyr::select(-Flow.aggregation.point) == ".."))
  expect_false(any(IEADF_augmented |> dplyr::select(-Flow.aggregation.point) == "x"))
  
  # As of 2019, the IEA no longer tags flows with "(transf.)", "(transformation)", or "(energy)".  
  # So these tests must be applied only to 2018 data.
  IEADF_unaugmented_2018 <- sample_iea_data_path(2018) |> 
    iea_df() |> 
    rename_iea_df_cols()
  IEADF_augmented_2018 <- IEADF_unaugmented_2018 |> 
    augment_iea_df()
  
  # Check that the original has flows that end in "(transf.)"
  expect_true(nrow(IEADF_unaugmented_2018 |> dplyr::filter(endsWith(Flow, "(transf.)"))) > 0)
  # Check that the original has flows that end in "(transformation)"
  expect_true(nrow(IEADF_unaugmented_2018 |> dplyr::filter(endsWith(Flow, "(transformation)"))) > 0)
  # Check that the original has flows that end in "(energy)"
  expect_true(nrow(IEADF_unaugmented_2018 |> dplyr::filter(endsWith(Flow, "(energy)"))) > 0)

  # Check that all "(transf.)" have been removed from the Flow column
  expect_equal(nrow(IEADF_augmented_2018 |> dplyr::filter(endsWith(Flow, "(transf.)"))), 0)
  # Check that all "(transformation)" have been removed from the Flow column
  expect_equal(nrow(IEADF_augmented_2018 |> dplyr::filter(endsWith(Flow, "(transformation)"))), 0)
  # Check that all "(energy)" have been removed from the Flow column
  expect_equal(nrow(IEADF_augmented_2018 |> dplyr::filter(endsWith(Flow, "(energy)"))), 0)
  
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
                                             "World,Iron and steel,Hard coal (if no detail),5,6\n")) |> 
    rename_iea_df_cols() |> 
    augment_iea_df()
  expect_equal(simple_with_tfc_df_2$Flow[[3]], "Main activity producer electricity plants")
  expect_equal(simple_with_tfc_df_2$Flow[[5]], "Coal mines")
})


test_that("augment_iea_df() works with NEU flows", {
  # On 3 March 2023, I made sure that all of the 
  # Flow.aggregation.point augmentation 
  # worked even if the aggregated and Memo: flows have not yet been removed.
  # This work was in preparation for doing a better job of
  # using detail available in the Memo: Non-energy use in xxxxx flows.
  IEADF_unaugmented <- sample_iea_data_path() |> 
    iea_df() |> 
    rename_iea_df_cols() |> 
    clean_iea_whitespace() |> 
    use_iso_countries()
  IEADF_augmented <- IEADF_unaugmented |> 
    augment_iea_df()
  
  na_fap <- IEADF_augmented |>
    dplyr::filter(is.na(.data[[IEATools::iea_cols$flow_aggregation_point]]))
  
  # Verify that the ONLY NA values in Flow.aggregation.point 
  # are Total final consumption, Electricity output (GWh), or Heat output
  unique_NA_fap_flows <- na_fap |> 
    magrittr::extract2(IEATools::iea_cols$flow) |> 
    unique()
  expect_equal(unique_NA_fap_flows, 
               c("Total final consumption", "Electricity output (GWh)", "Heat output"))
  
  # Verify that "Non-energy use" has Total final consumption as its fap
  IEADF_augmented |> 
    dplyr::filter(.data[[IEATools::iea_cols$flow]] == IEATools::aggregation_flows$non_energy_use) |> 
    magrittr::extract2(IEATools::iea_cols$flow_aggregation_point) |> 
    unique() |> 
    expect_equal(IEATools::aggregation_flows$total_final_consumption)
  # Verify that Memo: Non-energy use in industry has          
  # Non-energy use industry/transformation/energy as its fap
  IEADF_augmented |> 
    dplyr::filter(.data[[IEATools::iea_cols$flow]] == IEATools::memo_non_energy_flows$memo_non_energy_use_in_industry) |> 
    magrittr::extract2(IEATools::iea_cols$flow_aggregation_point) |> 
    unique() |> 
    expect_equal(IEATools::non_energy_flows$non_energy_use_industry_transformation_energy)
  # Verify that Memo: Non-energy use in xxxxxxx has 
  # Non-energy use in industry as its fap
  # Ignore the aggregation flow
  memos_to_use <- setdiff(IEATools::memo_non_energy_flows, IEATools::memo_non_energy_flows$memo_non_energy_use_in_industry)
  IEADF_augmented |> 
    dplyr::filter(.data[[IEATools::iea_cols$flow]] %in% memos_to_use) |> 
    magrittr::extract2(IEATools::iea_cols$flow_aggregation_point) |> 
    unique() |> 
    expect_equal(IEATools::memo_non_energy_flows$memo_non_energy_use_in_industry) 
})


test_that("specify_non_energy_use() works as expected", {
  df <- sample_iea_data_path() |>
    iea_df() |> 
    rename_iea_df_cols() |> 
    clean_iea_whitespace() |> 
    augment_iea_df()
  
  # Check that the original data are balanced.
  df |> 
    remove_agg_memo_flows() |> 
    tidy_iea_df() |> 
    calc_tidy_iea_df_balances(tol = 1e-3) |> 
    tidy_iea_df_balanced() |> 
    expect_true()
  
  # Check that the original data frame has NEU for South Africa
  neu_rows <- df |> 
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "South Africa", 
                  .data[[IEATools::iea_cols$flow]] == "Non-energy use industry/transformation/energy", 
                  .data[[IEATools::iea_cols$product]] %in% c("Hard coal (if no detail)", "Other bituminous coal"))
  
  neu_rows |> 
    dplyr::filter(.data[[IEATools::iea_cols$product]] == "Hard coal (if no detail)") |> 
    magrittr::extract2("1971") |> 
    expect_equal(747.9545)
  neu_rows |> 
    dplyr::filter(.data[[IEATools::iea_cols$product]] == "Other bituminous coal") |> 
    magrittr::extract2("1971") |> 
    expect_equal(0)
  neu_rows |> 
    dplyr::filter(.data[[IEATools::iea_cols$product]] == "Hard coal (if no detail)") |> 
    magrittr::extract2("2000") |> 
    expect_equal(0)
  neu_rows |> 
    dplyr::filter(.data[[IEATools::iea_cols$product]] == "Other bituminous coal") |> 
    magrittr::extract2("2000") |> 
    expect_equal(5284.6856)
  
  # Specify the data and see that it has moved.
  specified <- df |> 
    specify_non_energy_use()

  # Check that the specified data frame is balanced.
  specified |> 
    remove_agg_memo_flows() |> 
    tidy_iea_df() |> 
    calc_tidy_iea_df_balances(tol = 1e-3) |> 
    tidy_iea_df_balanced() |> 
    expect_true()
  
  # Check that the original rows are now 0.
  # These data have been subtracted.
  original_neu_rows_in_specified <- specified |> 
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "South Africa", 
                  .data[[IEATools::iea_cols$flow]] == "Non-energy use industry/transformation/energy", 
                  .data[[IEATools::iea_cols$product]] %in% c("Hard coal (if no detail)", "Other bituminous coal"))
  
  original_neu_rows_in_specified |> 
    dplyr::filter(.data[[IEATools::iea_cols$product]] == "Hard coal (if no detail)") |> 
    magrittr::extract2("1971") |> 
    expect_equal(0)
  original_neu_rows_in_specified |> 
    dplyr::filter(.data[[IEATools::iea_cols$product]] == "Other bituminous coal") |> 
    magrittr::extract2("1971") |> 
    expect_equal(0)
  original_neu_rows_in_specified |> 
    dplyr::filter(.data[[IEATools::iea_cols$product]] == "Hard coal (if no detail)") |> 
    magrittr::extract2("2000") |> 
    expect_equal(0)
  original_neu_rows_in_specified |> 
    dplyr::filter(.data[[IEATools::iea_cols$product]] == "Other bituminous coal") |> 
    magrittr::extract2("2000") |> 
    expect_equal(0)
  
  # Check that the same energy is now found in Non-energy use in chemical/petrochemical.
  specified_neu_rows <- specified |> 
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "South Africa", 
                  .data[[IEATools::iea_cols$flow]] == "Non-energy use in chemical/petrochemical",
                  .data[[IEATools::iea_cols$product]] %in% c("Hard coal (if no detail)", "Other bituminous coal"))
  
  specified_neu_rows |> 
    dplyr::filter(.data[[IEATools::iea_cols$product]] == "Hard coal (if no detail)") |> 
    magrittr::extract2("1971") |> 
    expect_equal(747.9545)
  specified_neu_rows |> 
    dplyr::filter(.data[[IEATools::iea_cols$product]] == "Other bituminous coal") |> 
    magrittr::extract2("1971") |> 
    expect_equal(0)
  specified_neu_rows |> 
    dplyr::filter(.data[[IEATools::iea_cols$product]] == "Hard coal (if no detail)") |> 
    magrittr::extract2("2000") |> 
    expect_equal(0)
  specified_neu_rows |> 
    dplyr::filter(.data[[IEATools::iea_cols$product]] == "Other bituminous coal") |> 
    magrittr::extract2("2000") |> 
    expect_equal(5284.6856)
})


test_that("specify_non_energy_use() gives matrices we expect", {
  # After specifying Non-energy use flows, we expect those flows
  # to appear in the Y matrix.
  # These tests make sure that happens.
  res <- sample_iea_data_path() |>
    iea_df() |> 
    rename_iea_df_cols() |> 
    clean_iea_whitespace() |> 
    use_iso_countries() |> 
    augment_iea_df() |> 
    specify_non_energy_use() |> 
    remove_agg_memo_flows() %>% 
    tidy_iea_df() |> 
    prep_psut()
  
  # Check that the Hard coal from 1971 goes where it belongs.
  res |> 
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "ZAF", 
                  .data[[IEATools::iea_cols$year]] == 1971) |> 
    magrittr::extract2("Y") |> 
    magrittr::extract2(1) |> 
    magrittr::extract("Hard coal (if no detail)", "Non-energy use in chemical/petrochemical") |> 
    expect_equal(747.9545)

  # Check that the Other bituminous coal from 2000 goes where it belongs.
  res |> 
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "ZAF", 
                  .data[[IEATools::iea_cols$year]] == 2000) |> 
    magrittr::extract2("Y") |> 
    magrittr::extract2(1) |> 
    magrittr::extract("Other bituminous coal", "Non-energy use in chemical/petrochemical") |> 
    expect_equal(5284.6856)

  # Check that Non-energy use industry/transformation/energy is NOT present
  res |> 
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "ZAF", 
                  .data[[IEATools::iea_cols$year]] == 1971) |> 
    magrittr::extract2("Y") |> 
    magrittr::extract2(1) |> 
    magrittr::extract("Hard coal (if no detail)", "Non-energy use in industry/transformation/energy") |> 
    expect_error("subscript out of bounds")
  res |> 
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "ZAF", 
                  .data[[IEATools::iea_cols$year]] == 2000) |> 
    magrittr::extract2("Y") |> 
    magrittr::extract2(1) |> 
    magrittr::extract("Other bituminous coal", "Non-energy use in industry/transformation/energy") |> 
    expect_error("subscript out of bounds")
  
  # Check that we have other Non-energy use industry/transformation/energy
  res |> 
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "ZAF", 
                  .data[[IEATools::iea_cols$year]] == 1971) |> 
    magrittr::extract2("Y") |> 
    magrittr::extract2(1) |>
    magrittr::extract("Bitumen", "Non-energy use industry/transformation/energy") |> 
    expect_equal(184.4368)
  res |> 
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "ZAF", 
                  .data[[IEATools::iea_cols$year]] == 2000) |> 
    magrittr::extract2("Y") |> 
    magrittr::extract2(1) |> 
    magrittr::extract("Bitumen", "Non-energy use industry/transformation/energy") |> 
    expect_equal(214.2448)
  res |> 
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "ZAF", 
                  .data[[IEATools::iea_cols$year]] == 2000) |> 
    magrittr::extract2("Y") |> 
    magrittr::extract2(1) |> 
    magrittr::extract("Lubricants", "Non-energy use industry/transformation/energy") |> 
    expect_equal(79.2491)
  res |> 
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "ZAF", 
                  .data[[IEATools::iea_cols$year]] == 2000) |> 
    magrittr::extract2("Y") |> 
    magrittr::extract2(1) |> 
    magrittr::extract("Paraffin waxes", "Non-energy use industry/transformation/energy") |> 
    expect_equal(6.6877)
  res |> 
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "ZAF", 
                  .data[[IEATools::iea_cols$year]] == 2000) |> 
    magrittr::extract2("Y") |> 
    magrittr::extract2(1) |> 
    magrittr::extract("White spirit & SBP", "Non-energy use industry/transformation/energy") |> 
    expect_equal(70.0350)
})


test_that("remove_agg_memo_flows() works as expected", {
  for (yr in IEATools::valid_iea_release_years) {
    Cleaned <- sample_iea_data_path(yr) |> 
      iea_df() |> 
      rename_iea_df_cols() |> 
      remove_agg_memo_flows()
    # Verify that none of the aggregation flows are present
    n_agg_rows <- Cleaned |> 
      dplyr::filter(Flow == "Total primary energy supply" |
                      Flow == "Total energy supply" |
                      Flow == "Total final consumption" | 
                      Flow == "Transformation processes" |
                      Flow == "Energy industry own use" | 
                      Flow == "Industry" |
                      Flow == "Transport" |
                      Flow == "Other" |
                      Flow == "Non-energy use") |> 
      nrow()
    expect_equal(n_agg_rows, 0)
    # Verify that none of the memo flows are present
    n_memo_flows <- Cleaned |> 
      dplyr::filter(startsWith(Flow, "Memo:")) |> 
      nrow()
    expect_equal(n_memo_flows, 0)
    # Verify that none of the memo products are present
    n_memo_products <- Cleaned |> 
      dplyr::filter(startsWith(Product, "Memo:")) |> 
      nrow()
    expect_equal(n_memo_products, 0)
  }
  
  for (yr in IEATools::valid_iea_release_years) {
    # Try again with a different approach. 
    # This time, ensure that rows we want to clean are present first.
    IEA_data <- sample_iea_data_path(yr) |> 
      iea_df() |>
      rename_iea_df_cols() |> 
      augment_iea_df()
    
    # Verify that aggregation flows exist
    if (yr <= 2019) {
      agg_flows <- c("Total primary energy supply", "Total final consumption", "Transformation processes", "Energy industry own use", "Industry", "Transport", "Non-energy use")
    } else {
      agg_flows <- c("Total energy supply", "Total final consumption", "Transformation processes", "Energy industry own use", "Industry", "Transport", "Non-energy use")
    }
    expect_true(lapply(agg_flows, 
                       FUN = function(s){
                         expect_true(IEA_data |> dplyr::filter(Flow == s) |> nrow() > 0)
                       }) |> as.logical() |> all())
    # Verify that flow memos exist
    memo_flow_prefixes <- c("Memo: ", "Electricity output (GWh)", "Heat output")
    expect_true(lapply(memo_flow_prefixes, 
                       FUN = function(s){
                         expect_true(IEA_data |> dplyr::filter(startsWith(Flow, s)) |> nrow() > 0)
                       }) |> as.logical() |> all())
    # Verify that product memos exist
    memo_product_prefix <- "Memo: "
    expect_true(IEA_data |> dplyr::filter(startsWith(Product, memo_product_prefix)) |> nrow() > 0)
    
    # Now clean the aggregation flows and see if they're gone.
    Cleaned <- IEA_data |> 
      remove_agg_memo_flows()
    # Ensure that aggregation flows were removed.
    expect_true(lapply(agg_flows, 
                       FUN = function(s){
                         expect_true(Cleaned |> dplyr::filter(Flow == s) |> nrow() == 0)
                       }) |> as.logical() |> all())
    # Ensure that flow memos were removed
    expect_true(lapply(memo_flow_prefixes, 
                       FUN = function(s){
                         expect_true(Cleaned |> dplyr::filter(startsWith(Flow, s)) |> nrow() == 0)
                       }) |> as.logical() |> all())
    # Ensure that product memos were removed
    expect_true(IEA_data |> dplyr::filter(startsWith(Product, memo_product_prefix)) |> nrow() > 0)
  }
})


test_that("remove_agg_regions() works as expected", {
  tibble::tibble(Year = c(1967, 1995), Country = c("World", "Spain")) |>
    remove_agg_regions() |>
    expect_equal(tibble::tibble(Year = 1995, Country = "Spain"))
  
  n_regions <- length(IEATools::aggregation_regions)
  result <- tibble::tibble(Year = 1900 + 1:(n_regions+1), Country = unlist(c("Spain", IEATools::aggregation_regions))) |>
    remove_agg_regions()
  expect_equal(result$Year, 1901)
  expect_equivalent(result[["Country"]], "Spain")
  
  # Ensure that Greenland and Palestinian Authority are retained.
  tibble::tibble(Year = c(1967, 1990, 2020), Country = c("Memo: Greenland", "Memo: Palestinian Authority", "World")) |> 
    remove_agg_regions() |> 
    magrittr::extract2("Year") |> 
    expect_equal(c(1967, 1990))
})


test_that("use_iso_countries() works as expected", {
  for (yr in IEATools::valid_iea_release_years) {
    iso3 <- sample_iea_data_path(yr) |> 
      iea_df() |> 
      rename_iea_df_cols() |> 
      use_iso_countries()
    expect_false(any(iso3$Country == "South Africa"))
    expect_true(any(iso3$Country == "ZAF"))
    expect_false(any(iso3$Country == "Ghana"))
    expect_true(any(iso3$Country == "GHA"))
  }

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
                                "World,Iron and steel,Hard coal (if no detail),5,6\n")) |> 
    rename_iea_df_cols() |> 
    augment_iea_df() |> 
    use_iso_countries()
  # Ensure that a "World" country is present.
  n_world_rows <- world |> 
    dplyr::filter(Country == "WRLD") |> 
    nrow()
  expect_equal(n_world_rows, 8)
})


test_that("load_tidy_iea_df() works as expected", {
  for (yr in IEATools::valid_iea_release_years) {
    iea_tidy_df <- sample_iea_data_path(yr) |> 
      load_tidy_iea_df()
    # Verify column names and order
    expect_equal(names(iea_tidy_df), c("Country", "Method", "Energy.type", "Last.stage", "Year", "Ledger.side", "Flow.aggregation.point", 
                                       "Flow", "Product", "Unit", "E.dot"))
    # This is a energy exclusive data frame
    expect_true(all(iea_tidy_df$Energy.type == "E"))
    # This is a completely ktoe data frame
    expect_true(all(iea_tidy_df$Unit == "ktoe"))
    # Ledger.side can be only Supply or Consumption
    expect_true(all(iea_tidy_df$Ledger.side %in% c("Supply", "Consumption")))
  }
})


test_that("converting year to numeric works as expected", {
  for (yr in IEATools::valid_iea_release_years) {
    iea_tidy_df <- sample_iea_data_path(yr) |> 
      load_tidy_iea_df()
    expect_true(is.numeric(iea_tidy_df$Year))
  }  
})


test_that("trimming white space works", {
  cleaned <- data.frame(Flow = "  a flow   ", Product = "   a product   ", stringsAsFactors = FALSE) |> 
    clean_iea_whitespace()
  expect_equal(cleaned$Flow[[1]], "a flow")
  expect_equal(cleaned$Product[[1]], "a product")
})


test_that("load_tidy_iea_df() works as expected", {
  # Try for all valid years.
  for (yr in IEATools::valid_iea_release_years) {
    simple <- sample_iea_data_path(yr) |> 
      load_tidy_iea_df()
    complicated <- sample_iea_data_path(yr) |> 
      iea_df() |>
      rename_iea_df_cols() |> 
      clean_iea_whitespace() |> 
      remove_agg_memo_flows() |> 
      use_iso_countries() |> 
      augment_iea_df() |> 
      specify_non_energy_use() |> 
      tidy_iea_df()
    expect_equal(simple, complicated)
  }
})


test_that("load_tidy_iea_df() gives expected values", {
  iea_df <- sample_iea_data_path(version = 2021) |> 
    load_tidy_iea_df()
  # Try some values
  expect_equal(iea_df |> 
                 dplyr::filter(Country == "ZAF", Year == 1971, Product == "Fuel oil", 
                               Flow == "Oil refineries", Flow.aggregation.point == "Transformation processes") |> 
                 magrittr::extract2("E.dot"), 
               4515.6349)
  
  expect_equal(iea_df |> 
                 dplyr::filter(Country == "GHA", Year == 1971, Product == "Crude oil", 
                               Flow == "Imports") |> 
                 magrittr::extract2("E.dot"), 
               916.2081)

  expect_equal(iea_df |> 
                 dplyr::filter(Country == "ZAF", Year == 1971, Product == "Bitumen", Flow == "Transfers") |> 
                 magrittr::extract2("E.dot"), 
               2.7945)
})


test_that("Ledger.side is added by augmentation", {
  # Every row in the Ledger.side column should be filled with a non-NA entry.
  # Verify that's indeed the case.
  for (year in IEATools::valid_iea_release_years) {
    DF <- load_tidy_iea_df(sample_iea_data_path(year))
    expect_false(DF |> 
                   magrittr::extract2("Ledger.side") |> 
                   is.na() |> 
                   any())
  }
})


test_that("load_tidy_iea_df() fills every Flow.aggregation.point", {
  # Every row in the Flow.aggregation.point column should be filled with a non-NA entry.
  # Verify that's indeed the case.
  for (year in IEATools::valid_iea_release_years) {
    load_tidy_iea_df(sample_iea_data_path(year)) |> 
      magrittr::extract2("Flow.aggregation.point") |> 
      is.na() |> 
      any() |> 
      expect_false()
  }
})


test_that("load_tidy_iea_df() OK when spreading by years after", {
  # This test will fail if things are not specified correctly.
  # Without correct specification, keys will not be unique.
  for (year in IEATools::valid_iea_release_years) {
    year_spread <- load_tidy_iea_df(sample_iea_data_path(year)) |> 
      tidyr::spread(key = Year, value = E.dot)
    expect_true("1971" %in% names(year_spread))
    expect_true("2000" %in% names(year_spread))
  }
})


