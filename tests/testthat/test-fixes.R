
test_that("fix_GHA_psb() works as expected", {
  # Load some example IEA data
  tidy_example <- load_tidy_iea_df(apply_fixes = FALSE) |> 
    # Focus on Ghana only
    dplyr::filter(Country == "GHA") |> 
    # Ghana has PSB for 1971 and 2000.  Let's pretend that 1971 is 1991 and 2000 is 1992
    dplyr::mutate(
      Year := dplyr::case_when(
        Year == 1971 ~ 1991,
        Year == 2000 ~ 1992
      )
    )
  
  # Make sure that we have unmodified data for "1991"
  orig_1991_PSB <- tidy_example |> 
    dplyr::filter(Country == "GHA", Year == 1991, Product == "Primary solid biofuels")
  
  # Production
  orig_1991_PSB |>
    dplyr::filter(Flow == "Production") |> 
    magrittr::extract2("E.dot") |> 
    expect_equal(87399.9985)
  # Charcoal plants
  orig_1991_PSB |> 
    dplyr::filter(Flow == "Charcoal production plants") |> 
    magrittr::extract2("E.dot") |> 
    expect_equal(-20000.0003)
  # Industry
  orig_1991_PSB |> 
    dplyr::filter(Flow == "Industry not elsewhere specified") |> 
    magrittr::extract2("E.dot") |> 
    expect_equal(6100.0001)
  # Residential
  orig_1991_PSB |> 
    dplyr::filter(Flow == "Residential") |> 
    magrittr::extract2("E.dot") |> 
    expect_equal(61299.9981)

  # Make sure that we have unmodified data for "1992"
  orig_1992_PSB <- tidy_example |> 
    dplyr::filter(Country == "GHA", Year == 1992, Product == "Primary solid biofuels")
  
  #Production
  orig_1992_PSB |> 
    dplyr::filter(Flow == "Production") |> 
    magrittr::extract2("E.dot") |> 
    expect_equal(162909)
  # Charcoal plants
  orig_1992_PSB |> 
    dplyr::filter(Flow == "Charcoal production plants") |> 
    magrittr::extract2("E.dot") |> 
    expect_equal(-45803.9981)
  # Industry
  orig_1992_PSB |> 
    dplyr::filter(Flow == "Industry not elsewhere specified") |> 
    magrittr::extract2("E.dot") |> 
    expect_equal(28691.0016)
  # Residential
  orig_1992_PSB |> 
    dplyr::filter(Flow == "Residential") |> 
    magrittr::extract2("E.dot") |> 
    expect_equal(84667.002)
  # Commercial and public services
  orig_1992_PSB |> 
    dplyr::filter(Flow == "Commercial and public services") |> 
    magrittr::extract2("E.dot") |> 
    expect_equal(3630.0017)
  # Agriculture/forestry
  orig_1992_PSB |> 
    dplyr::filter(Flow == "Agriculture/forestry") |> 
    magrittr::extract2("E.dot") |> 
    expect_equal(117.0001)
  
  # Call the function that does the replacement
  fixed_tidy <- fix_GHA_psb(tidy_example) |> 
    # Filter rows from years beyond our interest for this test.
    dplyr::filter(Year %in% c(1991, 1992))
  
  # Check that the replacement was done correctly for 1991
  fixed_1991_PSB <- fixed_tidy |> 
    dplyr::filter(Country == "GHA", Year == 1991, Product == "Primary solid biofuels")
  
  # Production
  fixed_1991_PSB |> 
    dplyr::filter(Flow == "Production") |> 
    magrittr::extract2("E.dot") |> 
    expect_equal(168518.7)
  # Charcoal plants
  fixed_1991_PSB |> 
    dplyr::filter(Flow == "Charcoal production plants") |> 
    magrittr::extract2("E.dot") |> 
    expect_equal(-49781.052)
  # Residential
  fixed_1991_PSB |> 
    dplyr::filter(Flow == "Residential") |> 
    magrittr::extract2("E.dot") |> 
    expect_equal(105256.152)
  # Agriculture/forestry (nothing!)
  fixed_1991_PSB |> 
    dplyr::filter(Flow == "Agriculture/forestry") |> 
    magrittr::extract2("E.dot") |> 
    length() |> 
    expect_equal(0)
  # Industry
  fixed_1991_PSB |> 
    dplyr::filter(Flow == "Industry not elsewhere specified") |> 
    magrittr::extract2("E.dot") |> 
    expect_equal(13481.496)
  # Commercial and public services
  fixed_1991_PSB |> 
    dplyr::filter(Flow == "Commercial and public services") |> 
    magrittr::extract2("E.dot") |> 
    length() |> 
    expect_equal(0)
  
  # Check that the replacement was done correctly for 1992
  fixed_1992_PSB <- fixed_tidy |>  
    dplyr::filter(Country == "GHA", Year == 1992, Product == "Primary solid biofuels")
  
  # Production
  fixed_1992_PSB |> 
    dplyr::filter(Flow == "Production") |> 
    magrittr::extract2("E.dot") |> 
    expect_equal(172077.48)
  # Charcoal plants
  fixed_1992_PSB |>
    dplyr::filter(Flow == "Charcoal production plants") |> 
    magrittr::extract2("E.dot") |> 
    expect_equal(-50618.412)
  # Residential
  fixed_1992_PSB |> 
    dplyr::filter(Flow == "Residential") |>
    magrittr::extract2("E.dot") |> 
    expect_equal(106512.192)
  # Agriculture/forestry (These data are actually coming from 2000, and there is no replacement in the 1992 fixed table, so we have something here.)
  fixed_1992_PSB |>
    dplyr::filter(Flow == "Agriculture/forestry") |>
    magrittr::extract2("E.dot") |> 
    expect_equal(117.0001)
  # Industry
  fixed_1992_PSB |>
    dplyr::filter(Flow == "Industry not elsewhere specified") |> 
    magrittr::extract2("E.dot") |>
    expect_equal(14946.876)
  # Commercial and public services (These data are actually coming from 2000, and there is no replacement in the 1992 fixed table, so we have something here.)
  fixed_1992_PSB |>
    dplyr::filter(Flow == "Commercial and public services") |>
    magrittr::extract2("E.dot") |> 
    expect_equal(3630.0017)

  # Ensure that we haven't added any rows to the data frame
  expect_equal(nrow(fixed_tidy), nrow(tidy_example))
})


test_that("IEA data lacking Ghana is not fixed for PSBs", {
  # Load some example IEA data
  tidy_example <- load_tidy_iea_df(apply_fixes = FALSE) |> 
    # Exclude Ghana
    dplyr::filter(Country != "GHA") |> 
    # Ghana has PSB for 1971 and 2000.  Let's pretend that 1971 is 1991 and 2000 is 1992
    dplyr::mutate(
      Year := dplyr::case_when(
        Year == 1971 ~ 1991,
        Year == 2000 ~ 1992
      )
    )
  nrows_orig <- nrow(tidy_example)
  
  # Now fix the "Ghana" data
  fixed <- fix_GHA_psb(tidy_example)
  nrows_fixed <- nrow(fixed)
  # We should not have added any rows.
  expect_equal(nrows_fixed, nrows_orig)
})


test_that("Fixing GHA Industry Electricity works as expected", {
  tidy_example <- load_tidy_iea_df(apply_fixes = FALSE) |> 
    # Focus on Ghana only
    dplyr::filter(Country == "GHA")
    
  # Verify values for unfixed data.
  expect_equal(tidy_example |> 
                 dplyr::filter(!!as.name(iea_cols$flow) == "Industry not elsewhere specified", 
                               !!as.name(iea_cols$product) == "Electricity") |> 
                 magrittr::extract2(iea_cols$e_dot), c(918.002, 15501.6019))
  # Verify that there are no Mining and quarrying, Non-ferrous metals, or Textiles and leather values for 2000
  expect_equal(tidy_example |> 
                dplyr::filter(!!as.name(iea_cols$flow) %in% c("Mining and quarrying",
                                                              "Non-ferrous metals",
                                                              "Textile and leather")) |> 
                 magrittr::extract2(iea_cols$e_dot), c(698.4001, 7102.8015, 61.1985))
  
  # Fix and check
  # 
  # Check that the unspecified amount of Industry Electricity has gone down to a smaller level.
  # fixed_tidy_example <- fix_GHA_industry_electricity(tidy_example)
  # expect_equal(fixed_tidy_example |> 
  #                dplyr::filter(!!as.name(iea_cols$flow) == "Industry not elsewhere specified",
  #                              !!as.name(iea_cols$product) == "Electricity") |> 
  #                # The first number (21.9261) doesn't change, because it is from 1971 and doesn't need to be fixed.
  #                # However, the second number (from 2000) does change and is smaller,
  #                # because we are now specifying much more of Industry Electricity consumption.
  #                # See that we have picked up its new value.
  #                magrittr::extract2(iea_cols$e_dot), c(918.002, 4342.377677))
  
  # Check that Mining and quarrying now has a value for the year 2000.
  # expect_equal(fixed_tidy_example |> 
  #                dplyr::filter(!!as.name(iea_cols$flow) == "Mining and quarrying") |> 
  #                magrittr::extract2(iea_cols$e_dot), 
  #              c(698.400108, 1902.13380014))
  
  # Check that Non-ferrous metals has a value for the year 2000.
  # expect_equal(fixed_tidy_example |> 
  #                dplyr::filter(!!as.name(iea_cols$flow) == "Non-ferrous metals") |> 
  #                magrittr::extract2(iea_cols$e_dot), 
  #              c(7102.80153, 9162.00000041))
  
  # Check that Textiles and leather has a value for the year 2000.
  # expect_equal(fixed_tidy_example |> 
  #                dplyr::filter(!!as.name(iea_cols$flow) == "Textile and leather") |> 
  #                magrittr::extract2(iea_cols$e_dot), 
  #              c(61.1985, 95.09040001))
  
  # Ensure that data are still in balance
  orig <- tidy_example |> 
    dplyr::filter(Year == 2000, 
           Product == "Electricity", 
           Flow == "Industry not elsewhere specified") |> 
    dplyr::select(E.dot) |> 
    unlist() |> 
    as.numeric()
  # new <- fixed_tidy_example |> 
  #   dplyr::filter(Year == 2000, 
  #          Product == "Electricity", 
  #          Flow %in% c("Mining and quarrying",
  #                      "Non-ferrous metals",
  #                      "Textile and leather", 
  #                      "Industry not elsewhere specified")) |> 
  #   dplyr::select(E.dot) |> 
  #   sum()
  # expect_equal(new, orig)
})


test_that("fix_OAMR_cpp() works as expected", {
  example_tidy_iea_df <- load_tidy_iea_df() |> 
    dplyr::filter(Country == "GHA") |> 
    dplyr::mutate(
      # Pretend that GHA is Other non-OECD Americas.
      Country = "OAMR"
    )
  # Check original values
  orig <- example_tidy_iea_df |> 
    dplyr::filter(Flow %in% c("Production",
                              "Charcoal production plants"), 
                  Product %in% c("Charcoal", "Primary solid biofuels")) |> 
    dplyr::select("Year", "Flow", "Product", "E.dot", "Unit")
  orig |> 
    dplyr::filter(Year == 1971, Flow == "Production") |> 
    purrr::pluck("E.dot", 1) |> 
    expect_equal(87400, tolerance = 0.01)
  orig |> 
    dplyr::filter(Year == 2000, Flow == "Production") |> 
    purrr::pluck("E.dot", 1) |> 
    expect_equal(162909)
  orig |> 
    dplyr::filter(Year == 1971, Flow == "Charcoal production plants", Product == "Primary solid biofuels") |> 
    purrr::pluck("E.dot", 1) |> 
    expect_equal(-20000, tolerance = 0.001)
  orig |> 
    dplyr::filter(Year == 2000, Flow == "Charcoal production plants", Product == "Primary solid biofuels") |> 
    purrr::pluck("E.dot", 1) |> 
    expect_equal(-45804, tolerance = 0.001)
  orig |> 
    dplyr::filter(Year == 1971, Flow == "Charcoal production plants", Product == "Charcoal") |> 
    purrr::pluck("E.dot", 1) |> 
    expect_equal(4990, tolerance = 0.001)
  orig |> 
    dplyr::filter(Year == 2000, Flow == "Charcoal production plants", Product == "Charcoal") |> 
    purrr::pluck("E.dot", 1) |> 
    expect_equal(21683, tolerance = 0.001)
  
  # Check fixed values
  fixed <- example_tidy_iea_df |> 
    fix_OAMR_cpp() |> 
    dplyr::filter(Flow %in% c("Production",
                              "Charcoal production plants"), 
                  Product %in% c("Charcoal", "Primary solid biofuels")) |> 
    dplyr::select("Year", "Flow", "Product", "E.dot", "Unit")
  
  fixed |> 
    dplyr::filter(Year == 1971, Flow == "Production") |> 
    purrr::pluck("E.dot", 1) |> 
    expect_equal(11843.81818)
  fixed |> 
    dplyr::filter(Year == 2000, Flow == "Production") |> 
    purrr::pluck("E.dot", 1) |> 
    expect_equal(10193.63636)
  fixed |> 
    dplyr::filter(Year == 1971, Flow == "Charcoal production plants", Product == "Primary solid biofuels") |> 
    purrr::pluck("E.dot", 1) |> 
    expect_equal(-236.8181818, tolerance = 0.001)
  fixed |> 
    dplyr::filter(Year == 2000, Flow == "Charcoal production plants", Product == "Primary solid biofuels") |> 
    purrr::pluck("E.dot", 1) |> 
    expect_equal(-994.6363636, tolerance = 0.001)
  fixed |> 
    dplyr::filter(Year == 1971, Flow == "Charcoal production plants", Product == "Charcoal") |> 
    purrr::pluck("E.dot", 1) |> 
    expect_equal(154, tolerance = 0.001)
  fixed |> 
    dplyr::filter(Year == 2000, Flow == "Charcoal production plants", Product == "Charcoal") |> 
    purrr::pluck("E.dot", 1) |> 
    expect_equal(646.8, tolerance = 0.001)
})


test_that("fix_OAMR_gw() works as expected", {
  example_tidy_iea_df <- load_tidy_iea_df() |> 
    dplyr::filter(Country == "ZAF", Year == 1971) |> 
    dplyr::mutate(
      # Pretend that ZAF is Other non-OECD Americas.
      Country = "OAMR"
    )
  # Check original values
  orig <- example_tidy_iea_df |> 
    dplyr::filter(Flow %in% c("Production",
                              "Gas works"), 
                  Product %in% c("Natural gas", "Gas works gas")) |> 
    dplyr::select("Year", "Flow", "Product", "E.dot", "Unit")
  orig |> 
    dplyr::filter(Year == 1971, Flow == "Production") |> 
    purrr::pluck("E.dot", 1) |> 
    # There is no Production here.
    expect_null()
  orig |> 
    dplyr::filter(Flow == "Gas works", Product == "Gas works gas") |> 
    purrr::pluck("E.dot", 1) |> 
    expect_equal(5797, tolerance = 0.001)
  orig |> 
    dplyr::filter(Flow == "Gas works", Product == "Natural gas") |> 
    purrr::pluck("E.dot", 1) |> 
    # There is no consumption of Natural gas by Gas works.
    expect_null()

  # Check fixed values
  fixed <- example_tidy_iea_df |> 
    fix_OAMR_gw() |> 
    dplyr::filter(Flow %in% c("Production",
                              "Gas works"), 
                  Product %in% c("Gas works gas", "Natural gas")) |> 
    dplyr::select("Year", "Flow", "Product", "E.dot", "Unit")
  
  fixed |> 
    dplyr::filter(Flow == "Production") |> 
    purrr::pluck("E.dot", 1) |> 
    expect_equal(210.3834233)
  fixed |> 
    dplyr::filter(Flow == "Gas works", Product == "Natural gas") |> 
    purrr::pluck("E.dot", 1) |> 
    expect_equal(-105.0834233)
  fixed |> 
    dplyr::filter(Flow == "Gas works", Product == "Gas works gas") |> 
    purrr::pluck("E.dot", 1) |> 
    expect_equal(82.8)
})


test_that("fix_AUS_bfg() works as expected", {
  # Try when the country doesn't match. 
  # No changes should occur.
  fixed_but_no_fix <- load_tidy_iea_df() |>
    dplyr::filter(Country == "ZAF", Year == 1971) |> 
    fix_AUS_bfg()
  
  fixed_but_no_fix |> 
    dplyr::filter(Product == "Blast furnace gas", 
                  FlowAggregationPoint == "Energy industry own use") |> 
    nrow() |> 
    expect_equal(0)
  fixed_but_no_fix |> 
    dplyr::filter(Product == "Blast furnace gas", 
                  FlowAggregationPoint == "Transformation processes") |> 
    magrittr::extract2("E.dot") |> 
    expect_equal(34575)
  fixed_but_no_fix |> 
    dplyr::filter(Product == "Blast furnace gas", 
                  FlowAggregationPoint == "Industry") |> 
    magrittr::extract2("E.dot") |> 
    expect_equal(34575)
  
  # Now try when changes should occur
  example_tidy_iea_df <- load_tidy_iea_df() |>
    dplyr::filter(Country == "ZAF", Year == 1971) |> 
    dplyr::mutate(
      # Pretend that ZAF is Australia.
      Country = "AUS", 
      # And that 1971 is 2013
      Year = 2013
    )
  # Pass through fix_AUS_bfg() to make sure the fix is applied
  fixed <- example_tidy_iea_df |> 
    fix_AUS_bfg()
  fixed |> 
    dplyr::filter(Product == "Blast furnace gas", 
                  FlowAggregationPoint == "Energy industry own use") |> 
    magrittr::extract2("E.dot") |> 
    expect_equal(-11324.99922)
  
  fixed |> 
    dplyr::filter(Product == "Blast furnace gas", 
                  Flow == "Iron and steel") |> 
    magrittr::extract2("E.dot") |> 
    expect_equal(7549.99948)
})


test_that("fix_RUSEST_heat() works as expected", {
  # First, run through the loading code, just to make sure nothing breaks.
  # Try when the country doesn't match. 
  # No changes should occur.
  iea_data <- load_tidy_iea_df()
  fixed_but_no_fix <- iea_data |>
    fix_RUSEST_heat()
  expect_equal(iea_data, fixed_but_no_fix)
  
  # Try a made-up dataset to see that it works.
  heat_df <- data.frame(Country = c("RUS", "RUS", "RUS", "EST"), 
                        Method = "PCM",
                        EnergyType = "E", LastStage = "Final", 
                        Year = 1990, LedgerSide = "Consumption", 
                        FlowAggregationPoint = c("Industry",
                                                   "Industry", 
                                                   "Other", 
                                                   "Industry"),
                        Flow = c("Industry not elsewhere specified", 
                                 "Industry not elsewhere specified", 
                                 "Final consumption not elsewhere specified",
                                 "Industry not elsewhere specified"), 
                        Product = c("Heat", "Nothing", "Heat", "Heat"), 
                        Unit = "TJ", 
                        E.dot = c(100, 300, 400, 500))
  res <- heat_df |> 
    fix_RUSEST_heat()
  # The "Nothing" row should be unchanged
  res |> 
    dplyr::filter(Product == "Nothing") |> 
    magrittr::extract2("E.dot") |> 
    expect_equal(300)
  # The Not elsewhere specified portion of Heat should now be included.
  res |> 
    dplyr::filter(Country == "RUS", 
                  Flow == "Industry not elsewhere specified",
                  Product == "Heat") |> 
    magrittr::extract2("E.dot") |> 
    expect_equal(136140.9526)
  # Textile and leather should appear, along with several others.
  res |> 
    dplyr::filter(Country == "RUS", 
                  Flow == "Textile and leather",
                  Product == "Heat") |> 
    magrittr::extract2("E.dot") |> 
    expect_equal(143499.5473)
  # Machinery appears
  res |> 
    dplyr::filter(Country == "RUS", 
                  Flow == "Machinery", 
                  Product == "Heat") |> 
    magrittr::extract2("E.dot") |> 
    expect_equal(951348.5567)
  # Test one more: Iron and steel
  res |> 
    dplyr::filter(Country == "RUS", 
                  Flow == "Iron and steel",
                  Product == "Heat") |> 
    magrittr::extract2("E.dot") |> 
    expect_equal(398609.0816)
  
  # Test that RUS works for Final consumption not elsewhere specified
  # This should go away
  res |> 
    dplyr::filter(Country == "RUS", 
                  Flow == "Final consumption not elsewhere specified",
                  Product == "Heat") |> 
    magrittr::extract2("E.dot") |> 
    expect_equal(0)
  # And be replaced by several other categories
  res |> 
    dplyr::filter(Country == "RUS", 
                  Flow == "Agriculture/forestry",
                  Product == "Heat") |> 
    magrittr::extract2("E.dot") |> 
    expect_equal(327003.749)
  res |> 
    dplyr::filter(Country == "RUS", 
                  Flow == "Residential",
                  Product == "Heat") |> 
    magrittr::extract2("E.dot") |> 
    expect_equal(3536023.082)
  
  # Also test that Estonia works  
  res |> 
    dplyr::filter(Country == "EST", 
                  Flow == "Paper, pulp and printing",
                  Product == "Heat") |> 
    magrittr::extract2("E.dot") |> 
    expect_equal(185.6134258)
})


test_that("load_tidy_iea_df(apply_fixes = TRUE) works as expected", {
  # Try without fixes first
  unfixed <- load_tidy_iea_df(apply_fixes = FALSE)
  # Ensure no changes occur.
  # There is no COL data in unfixed.
  unfixed_COL <- unfixed |>
    fix_COL_WRLD_electricity()
  expect_equal(unfixed_COL, unfixed)
  
  # Try same with GHA. This should fix the year 2000 only.
  # fixed_GHA <- unfixed |> 
  #   fix_GHA_psb() |> 
  #   fix_GHA_industry_electricity()
  # Check Industry Electricity
  # fixed_GHA |>
  #   dplyr::filter(Country == "GHA", Year == 2000, Flow == "Mining and quarrying") |> 
  #   dplyr::pull("E.dot") |> 
  #   expect_equal(1902.1338)
  # fixed_GHA |>
  #   dplyr::filter(Country == "GHA", Year == 2000, Flow == "Non-ferrous metals") |> 
  #   dplyr::pull("E.dot") |> 
  #   expect_equal(9162.000002)
  # fixed_GHA |>
  #   dplyr::filter(Country == "GHA", Year == 2000, Flow == "Textile and leather") |> 
  #   dplyr::pull("E.dot") |> 
  #   expect_equal(95.09040001)
  # fixed_GHA |>
  #   dplyr::filter(Country == "GHA", Year == 2000, Flow == "Industry not elsewhere specified", 
  #                 Product == "Electricity") |> 
  #   dplyr::pull("E.dot") |> 
  #   expect_equal(4342.377677)
  # Check PSB production.  No changes should occur, because 
  # all GHA PSB fixes occur in years 1991 -- 1999.
  unfixed |>
    dplyr::filter(Country == "GHA", Year == 2000, Flow == "Production", 
                  Product == "Primary solid biofuels") |> 
    dplyr::pull("E.dot") |> 
    expect_equal(162909)
  # fixed_GHA |>
  #   dplyr::filter(Country == "GHA", Year == 2000, Flow == "Production", 
  #                 Product == "Primary solid biofuels") |> 
  #   dplyr::pull("E.dot") |> 
  #   expect_equal(162909)
})


test_that("do_fix() works with wide-by-year data", {
  unfixed <- load_tidy_iea_df(apply_fixes = FALSE) |> 
    dplyr::filter(Country == "GHA", EnergyType == "E", LastStage == "Final")
  # Check that GHA industry electricity is not fixed, i.e., that there is 
  # no Mining and quarrying in 2000.
  unfixed |> 
    dplyr::filter(Country == "GHA", Year == 2000, Flow == "Mining and quarrying") |> 
    nrow() |> 
    expect_equal(0)
  
  # Make the data frame wide.
  unfixed_wide <- unfixed |> 
    tidyr::pivot_wider(names_from = "Year", values_from = "E.dot")
  # Send to the fix function
  # fixed_wide <- fix_GHA_industry_electricity(unfixed_wide)
  # We should get back a wide data frame with year columns.
  # expect_equal(year_cols(fixed_wide, return_names = TRUE), c("1971", "2000"))
  # Ensure we have the expected values, i.e., there is Electricity consumption
  # by Mining and quarrying.
  # fixed_long <- fixed_wide |> 
  #   tidyr::pivot_longer(cols = dplyr::all_of(year_cols(fixed_wide)), 
  #                       names_to = "Year", values_to = "E.dot")
  # fixed_long |> 
  #   dplyr::filter(Country == "GHA", Year == 2000, Flow == "Mining and quarrying") |> 
  #   dplyr::pull("E.dot") |> 
  #   expect_equal(1902.1338)
})


test_that("Energy remains balanced after fixes are applied", {
  unfixed <- load_tidy_iea_df(apply_fixes = FALSE) |> 
    dplyr::filter(Country == "GHA", EnergyType == "E", LastStage == "Final")
  unfixed |> 
    calc_tidy_iea_df_balances(tol = 0.01) |> 
    tidy_iea_df_balanced() |> 
    expect_true()

  fixed <- load_tidy_iea_df(apply_fixes = TRUE) |> 
    dplyr::filter(Country == "GHA", EnergyType == "E", LastStage == "Final")
  expect_true(nrow(fixed) == nrow(unfixed))
  fixed |> 
    calc_tidy_iea_df_balances(tol = 0.01) |> 
    tidy_iea_df_balanced() |> 
    expect_true()
})


test_that("applying fixes results in balanced energy flows", {
  # Don't run this test on CRAN or in continuous integration workflows.
  skip_on_cran()
  skip_on_ci()
  skip_on_os("windows")
  skip_on_os("linux")
  for (yr in IEATools::valid_iea_release_years) {
    # All IEA data  
    # iea <- "~/Dropbox/Fellowship 1960-2015 PFU database/IEA extended energy balance data/IEA 2022 energy balance data/IEA Extended Energy Balances 2022 (TJ).csv" |>
    # Only WRLD (faster!)
    iea <- paste0("~/Dropbox/Fellowship 1960-2015 PFU database/IEA extended energy balance data/IEA ",
                  yr, " energy balance data/IEA Extended Energy Balances ",
                  yr, " (TJ) World.csv") |>
      IEATools::load_tidy_iea_df(apply_fixes = TRUE) |> 
      # Ignore unbalanced "countries"
      dplyr::filter(Year <= yr - 2, !(.data[[IEATools::iea_cols$country]] %in% 
                                        c("Memo: Americas (UN)", 
                                          "Memo: IEA and Accession/Association countries", 
                                          "Memo: OECD Total", 
                                          "OECD Americas")))
    iea |> 
      calc_tidy_iea_df_balances(tol = 6) |> 
      tidy_iea_df_balanced() |> 
      expect_true()
  }
})
