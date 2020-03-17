###########################################################
context("Fixing functions")
###########################################################

test_that("Fixing GHA PSB works as expected", {
  # Load some example IEA data
  tidy_example <- load_tidy_iea_df() %>% 
    # Focus on Ghana only
    dplyr::filter(Country == "GHA") %>% 
    # Ghana has PSB for 1971 and 2000.  Let's pretend that 1971 is 1991 and 2000 is 1992
    dplyr::mutate(
      Year := dplyr::case_when(
        Year == 1971 ~ 1991,
        Year == 2000 ~ 1992
      )
    )
  
  # Make sure that we have unmodified data for "1991"
  orig_1991_PSB <- tidy_example %>% 
    dplyr::filter(Country == "GHA", Year == 1991, Product == "Primary solid biofuels")
  
  # Production
  orig_1991_PSB %>% dplyr::filter(Flow == "Production") %>% magrittr::extract2("E.dot") %>% expect_equal(2087.5131)
  # Charcoal plants
  orig_1991_PSB %>% dplyr::filter(Flow == "Charcoal production plants") %>% magrittr::extract2("E.dot") %>% expect_equal(-477.6918)
  # Industry
  orig_1991_PSB %>% dplyr::filter(Flow == "Industry not elsewhere specified") %>% magrittr::extract2("E.dot") %>% expect_equal(145.6960)
  # Residential
  orig_1991_PSB %>% dplyr::filter(Flow == "Residential") %>% magrittr::extract2("E.dot") %>% expect_equal(1464.1253)

  # Make sure that we have unmodified data for "1992"
  orig_1992_PSB <- tidy_example %>% 
    dplyr::filter(Country == "GHA", Year == 1992, Product == "Primary solid biofuels")
  
  #Production
  orig_1992_PSB %>% dplyr::filter(Flow == "Production") %>% magrittr::extract2("E.dot") %>% expect_equal(3891.0146)
  # Charcoal plants
  orig_1992_PSB %>% dplyr::filter(Flow == "Charcoal production plants") %>% magrittr::extract2("E.dot") %>% expect_equal(-1094.0097)
  # Industry
  orig_1992_PSB %>% dplyr::filter(Flow == "Industry not elsewhere specified") %>% magrittr::extract2("E.dot") %>% expect_equal(685.2728)
  # Residential
  orig_1992_PSB %>% dplyr::filter(Flow == "Residential") %>% magrittr::extract2("E.dot") %>% expect_equal(2022.2366)
  # Commercial and public services
   orig_1992_PSB %>% dplyr::filter(Flow == "Commercial and public services") %>% magrittr::extract2("E.dot") %>% expect_equal(86.7011)
  # Agriculture/forestry
  orig_1992_PSB %>% dplyr::filter(Flow == "Agriculture/forestry") %>% magrittr::extract2("E.dot") %>% expect_equal(2.7945)
  
  # Call the function that does the replacement
  fixed_tidy <- fix_GHA_psb(tidy_example) %>% 
    # Filter rows from years beyond our interest for this test.
    dplyr::filter(Year %in% c(1991, 1992))
  
  # Check that the replacement was done correctly for 1991
  fixed_1991_PSB <- fixed_tidy %>% 
    dplyr::filter(Country == "GHA", Year == 1991, Product == "Primary solid biofuels")
  
  # Production
  fixed_1991_PSB %>% dplyr::filter(Flow == "Production") %>% magrittr::extract2("E.dot") %>% expect_equal(4025)
  # Charcoal plants
  fixed_1991_PSB %>% dplyr::filter(Flow == "Charcoal production plants") %>% magrittr::extract2("E.dot") %>% expect_equal(-1189)
  # Residential
  fixed_1991_PSB %>% dplyr::filter(Flow == "Residential") %>% magrittr::extract2("E.dot") %>% expect_equal(2514)
  # Agriculture/forestry (nothing!)
  fixed_1991_PSB %>% dplyr::filter(Flow == "Agriculture/forestry") %>% magrittr::extract2("E.dot") %>% length() %>% expect_equal(0)
  # Industry
  fixed_1991_PSB %>% dplyr::filter(Flow == "Industry not elsewhere specified") %>% magrittr::extract2("E.dot") %>% expect_equal(322)
  # Commercial and public services
  fixed_1991_PSB %>% dplyr::filter(Flow == "Commercial and public services") %>% magrittr::extract2("E.dot") %>% length() %>% expect_equal(0)
  
  # Check that the replacement was done correctly for 1992
  fixed_1992_PSB <- fixed_tidy %>% 
    dplyr::filter(Country == "GHA", Year == 1992, Product == "Primary solid biofuels")
  
  # Production
  fixed_1992_PSB %>% dplyr::filter(Flow == "Production") %>% magrittr::extract2("E.dot") %>% expect_equal(4110)
  # Charcoal plants
  fixed_1992_PSB %>% dplyr::filter(Flow == "Charcoal production plants") %>% magrittr::extract2("E.dot") %>% expect_equal(-1209)
  # Residential
  fixed_1992_PSB %>% dplyr::filter(Flow == "Residential") %>% magrittr::extract2("E.dot") %>% expect_equal(2544)
  # Agriculture/forestry (These data are actually coming from 2000, and there is no replacement in the 1992 fixed table, so we have something here.)
  fixed_1992_PSB %>% dplyr::filter(Flow == "Agriculture/forestry") %>% magrittr::extract2("E.dot") %>% expect_equal(2.7945)
  # Industry
  fixed_1992_PSB %>% dplyr::filter(Flow == "Industry not elsewhere specified") %>% magrittr::extract2("E.dot") %>% expect_equal(357)
  # Commercial and public services (These data are actually coming from 2000, and there is no replacement in the 1992 fixed table, so we have something here.)
  fixed_1992_PSB %>% dplyr::filter(Flow == "Commercial and public services") %>% magrittr::extract2("E.dot") %>% expect_equal(86.7011)

  # Ensure that we haven't added any rows to the data frame
  expect_equal(nrow(fixed_tidy), nrow(tidy_example))
})


test_that("IEA data lacking Ghana is not fixed for PSBs", {
  # Load some example IEA data
  tidy_example <- load_tidy_iea_df() %>% 
    # Exclude Ghana
    dplyr::filter(Country != "GHA") %>% 
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
  tidy_example <- load_tidy_iea_df() %>% 
    # Focus on Ghana only
    dplyr::filter(Country == "GHA")
    
  # Verify values for unfixed data.
  expect_equal(tidy_example %>% 
                 dplyr::filter(!!as.name(iea_cols$flow) == "Industry not elsewhere specified", 
                               !!as.name(iea_cols$product) == "Electricity") %>% 
                 magrittr::extract2(iea_cols$e_dot), c(21.9261, 370.2494))
  # Verify that there are no Mining and quarrying, Non-ferrous metals, or Textiles and leather values for 2000
  expect_equal(tidy_example %>% 
                dplyr::filter(!!as.name(iea_cols$flow) %in% c("Mining and quarrying",
                                                              "Non-ferrous metals",
                                                              "Textile and leather")) %>% 
                 magrittr::extract2(iea_cols$e_dot), c(16.681, 169.6475, 1.4617))
  
  # Fix and check
  # 
  # Check that the unspecified amount of Industry Electricity has gone down to a smaller level.
  fixed_tidy_example <- fix_GHA_industry_electricity(tidy_example)
  expect_equal(fixed_tidy_example %>% 
                 dplyr::filter(!!as.name(iea_cols$flow) == "Industry not elsewhere specified",
                               !!as.name(iea_cols$product) == "Electricity") %>% 
                 # The first number (21.9261) doesn't change, because it is from 1971 and doesn't need to be fixed.
                 # However, the second number (from 2000) does change and is smaller,
                 # because we are now specifying much more of Industry Electricity consumption.
                 # See that we have picked up its new value.
                 magrittr::extract2(iea_cols$e_dot), c(21.9261, 103.715909))
  
  # Check that Mining and quarrying now has a value for the year 2000.
  expect_equal(fixed_tidy_example %>% 
                 dplyr::filter(!!as.name(iea_cols$flow) == "Mining and quarrying") %>% 
                 magrittr::extract2(iea_cols$e_dot), 
               c(16.681, 45.4316853))
  
  # Check that Non-ferrous metals has a value for the year 2000.
  expect_equal(fixed_tidy_example %>% 
                 dplyr::filter(!!as.name(iea_cols$flow) == "Non-ferrous metals") %>% 
                 magrittr::extract2(iea_cols$e_dot), 
               c(169.6475, 218.8306105))
  
  # Check that Textiles and leather has a value for the year 2000.
  expect_equal(fixed_tidy_example %>% 
                 dplyr::filter(!!as.name(iea_cols$flow) == "Textile and leather") %>% 
                 magrittr::extract2(iea_cols$e_dot), 
               c(1.4617, 2.271195185))
  
  # Ensure that data are still in balance
  orig <- tidy_example %>% 
    dplyr::filter(Year == 2000, 
           Product == "Electricity", 
           Flow == "Industry not elsewhere specified") %>% 
    dplyr::select(E.dot) %>% 
    unlist() %>% 
    as.numeric()
  new <- fixed_tidy_example %>% 
    dplyr::filter(Year == 2000, 
           Product == "Electricity", 
           Flow %in% c("Mining and quarrying",
                       "Non-ferrous metals",
                       "Textile and leather", 
                       "Industry not elsewhere specified")) %>% 
    dplyr::select(E.dot) %>% 
    sum()
  expect_equal(new, orig)
})
