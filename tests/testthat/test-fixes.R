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
  orig_1991_PSB %>% 
    dplyr::filter(Flow == "Production") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(2087.5131)
  # Charcoal plants
  orig_1991_PSB %>% 
    dplyr::filter(Flow == "Charcoal production plants") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(-477.6918)
  # Industry
  orig_1991_PSB %>% 
    dplyr::filter(Flow == "Industry not elsewhere specified") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(145.6960)
  # Residential
  orig_1991_PSB %>% 
    dplyr::filter(Flow == "Residential") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(1464.1253)

  # Make sure that we have unmodified data for "1992"
  orig_1992_PSB <- tidy_example %>% 
    dplyr::filter(Country == "GHA", Year == 1992, Product == "Primary solid biofuels")
  # Production
  orig_1992_PSB %>% 
    dplyr::filter(Flow == "Production") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(3891.0146)
  # Charcoal plants
  orig_1992_PSB %>% 
    dplyr::filter(Flow == "Charcoal production plants") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(-1094.0097)
  # Industry
  orig_1992_PSB %>% 
    dplyr::filter(Flow == "Industry not elsewhere specified") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(685.2728)
  # Residential
  orig_1992_PSB %>% 
    dplyr::filter(Flow == "Residential") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(2022.2366)
  # Commercial and public services
   orig_1992_PSB %>% 
    dplyr::filter(Flow == "Commercial and public services") %>% 
    magrittr::extract2("E.dot") %>% 
     expect_equal(86.7011)
  # Agriculture/forestry
  orig_1992_PSB %>% 
    dplyr::filter(Flow == "Agriculture/forestry") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(2.7945)
  
  # Call the function that does the replacement
  fixed_tidy <- fix_GHA_psb(tidy_example)
  
  # Check that the replacement was done correctly
  fixed_1991_PSB <- fixed_tidy %>% 
    dplyr::filter(Country == "GHA", Year == 1991, Product == "Primary solid biofuels")
  # Production
  fixed_1991_PSB %>% 
    dplyr::filter(Flow == "Production") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(4025)
  
  
})


