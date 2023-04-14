
test_that("fix_GHA_psb() works as expected", {
  # Load some example IEA data
  tidy_example <- load_tidy_iea_df() |> 
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
  tidy_example <- load_tidy_iea_df() |> 
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
  tidy_example <- load_tidy_iea_df() |> 
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
  fixed_tidy_example <- fix_GHA_industry_electricity(tidy_example)
  expect_equal(fixed_tidy_example |> 
                 dplyr::filter(!!as.name(iea_cols$flow) == "Industry not elsewhere specified",
                               !!as.name(iea_cols$product) == "Electricity") |> 
                 # The first number (21.9261) doesn't change, because it is from 1971 and doesn't need to be fixed.
                 # However, the second number (from 2000) does change and is smaller,
                 # because we are now specifying much more of Industry Electricity consumption.
                 # See that we have picked up its new value.
                 magrittr::extract2(iea_cols$e_dot), c(918.002, 4342.377677))
  
  # Check that Mining and quarrying now has a value for the year 2000.
  expect_equal(fixed_tidy_example |> 
                 dplyr::filter(!!as.name(iea_cols$flow) == "Mining and quarrying") |> 
                 magrittr::extract2(iea_cols$e_dot), 
               c(698.400108, 1902.13380014))
  
  # Check that Non-ferrous metals has a value for the year 2000.
  expect_equal(fixed_tidy_example |> 
                 dplyr::filter(!!as.name(iea_cols$flow) == "Non-ferrous metals") |> 
                 magrittr::extract2(iea_cols$e_dot), 
               c(7102.80153, 9162.00000041))
  
  # Check that Textiles and leather has a value for the year 2000.
  expect_equal(fixed_tidy_example |> 
                 dplyr::filter(!!as.name(iea_cols$flow) == "Textile and leather") |> 
                 magrittr::extract2(iea_cols$e_dot), 
               c(61.1985, 95.09040001))
  
  # Ensure that data are still in balance
  orig <- tidy_example |> 
    dplyr::filter(Year == 2000, 
           Product == "Electricity", 
           Flow == "Industry not elsewhere specified") |> 
    dplyr::select(E.dot) |> 
    unlist() |> 
    as.numeric()
  new <- fixed_tidy_example |> 
    dplyr::filter(Year == 2000, 
           Product == "Electricity", 
           Flow %in% c("Mining and quarrying",
                       "Non-ferrous metals",
                       "Textile and leather", 
                       "Industry not elsewhere specified")) |> 
    dplyr::select(E.dot) |> 
    sum()
  expect_equal(new, orig)
})


test_that("load_tidy_iea_df(apply_fixes = TRUE) works as expected", {
  # Try without fixes first
  unfixed <- load_tidy_iea_df(apply_fixes = FALSE)
  # Ensure no changes occur.
  # There is no COL data in unfixed.
  unfixed_COL <- unfixed |>
    fix_COL_electricity_generation()
  expect_equal(unfixed_COL, unfixed)
  
  # Try same with GHA. This should fix the year 2000 only.
  fixed_GHA <- unfixed |> 
    fix_GHA_psb() |> 
    fix_GHA_industry_electricity()
  # Check Industry Electricity
  fixed_GHA |>
    dplyr::filter(Country == "GHA", Year == 2000, Flow == "Mining and quarrying") |> 
    dplyr::pull("E.dot") |> 
    expect_equal(1902.1338)
  fixed_GHA |>
    dplyr::filter(Country == "GHA", Year == 2000, Flow == "Non-ferrous metals") |> 
    dplyr::pull("E.dot") |> 
    expect_equal(9162.000002)
  fixed_GHA |>
    dplyr::filter(Country == "GHA", Year == 2000, Flow == "Textile and leather") |> 
    dplyr::pull("E.dot") |> 
    expect_equal(95.09040001)
  fixed_GHA |>
    dplyr::filter(Country == "GHA", Year == 2000, Flow == "Industry not elsewhere specified", 
                  Product == "Electricity") |> 
    dplyr::pull("E.dot") |> 
    expect_equal(4342.377677)
  # Check PSB production.  No changes should occur, because 
  # all GHA PSB fixes occur in years 1991 -- 1999.
  unfixed |>
    dplyr::filter(Country == "GHA", Year == 2000, Flow == "Production", 
                  Product == "Primary solid biofuels") |> 
    dplyr::pull("E.dot") |> 
    expect_equal(162909)
  fixed_GHA |>
    dplyr::filter(Country == "GHA", Year == 2000, Flow == "Production", 
                  Product == "Primary solid biofuels") |> 
    dplyr::pull("E.dot") |> 
    expect_equal(162909)
})
