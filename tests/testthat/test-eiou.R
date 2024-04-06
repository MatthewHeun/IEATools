
test_that("specify_tp_eiou() works as expected for Own use in electricity, CHP and heat plants", {
  # Make a bogus data frame
  EIOU <- data.frame(Country = c("US", "US"), 
                     Method = c("PCM", "PCM"),
                     Energy.type = c("E", "E"),
                     Last.stage = c("Final", "Final"),
                     Year = c(2000, 2000),
                     Flow.aggregation.point = c("Energy industry own use", "nothing"),
                     Flow = c("Own use in electricity, CHP and heat plants", "Own use in electricity, CHP and heat plants"), 
                     Ledger.side = c("Supply", "Supply"),
                     Unit = c("TJ", "TJ"),
                     Product = c("Brown coal", "Crude oil"),
                     E.dot = c(-10, -10),
                     stringsAsFactors = FALSE)
  
  EIOU_fixed <- specify_tp_eiou(EIOU)
  # The first row is expected to change, because its Product is "Electricity"
  expect_equal(EIOU_fixed$Flow[[1]], "Main activity producer electricity plants")
  # The second row will not change, because its Product is "Nothing"
  expect_equal(EIOU_fixed$Flow[[2]], "Own use in electricity, CHP and heat plants")
})


test_that("specify_tp_eiou() works as expected for pumped storage plants", {
  # Make a bogus data frame
  EIOU <- data.frame(Country = c("US", "US"), 
                     Method = c("PCM", "PCM"),
                     Energy.type = c("E", "E"),
                     Last.stage = c("Final", "Final"),
                     Year = c(2000, 2000),
                     Flow.aggregation.point = c("Energy industry own use", "Nothing"),
                     Flow = c("Pumped storage plants", "Pumped storage plants"), 
                     Ledger.side = c("Supply", "Supply"),
                     Product = c("Brown coal", "Crude oil"),
                     Unit = c("TJ", "TJ"),
                     E.dot = c(-20, -20),
                     stringsAsFactors = FALSE)
  EIOU_fixed <- specify_tp_eiou(EIOU)
  # The first row is expected to change, because its Product is "Electricity"
  expect_equal(EIOU_fixed$Flow[[1]], "Main activity producer electricity plants")
  # The second row will not change, because its Product is "Nothing"
  expect_equal(EIOU_fixed$Flow[[2]], "Pumped storage plants")
})


test_that("route_pumped_storage() no longer discriminates +/-", {
  # EIOU by Pumped storage plants should always be negative.
  # Weirdly, Japan has a few years where EIOU by Pumped storage plants is negative.
  # In the absence of better information, we convert all positive values to negative values.
  # Verify that negating happens.
  # Make a bogus data frame.
  EIOU <- data.frame(Country = c("JPN", "JPN", "JPN", "JPN"), 
                     Method = c("PCM", "PCM", "PCM", "PCM"),
                     Energy.type = c("E", "E", "E", "E"),
                     Last.stage = c("Final", "Final", "Final", "Final"),
                     Year = c(2000, 2000, 2001, 2001),
                     Flow.aggregation.point = c("Energy industry own use",  
                                                "Energy industry own use",  
                                                "Energy industry own use", 
                                                "Energy industry own use"),
                     Flow = c("Main activity producer electricity plants", "Pumped storage plants",
                              "Main activity producer electricity plants", "Pumped storage plants"), 
                     Ledger.side = c("Supply", "Supply", "Supply", "Supply"),
                     Product = c("Electricity", "Electricity", "Electricity", "Electricity"),
                     Unit = c("TJ", "TJ"),
                     E.dot = c(-20, -2, -21, 3),
                     stringsAsFactors = FALSE)
  # Call the function that should negate the energy flows.
  routed <- route_pumped_storage(EIOU)
  expect_equal(nrow(routed), 2)
  expect_equal(routed$Flow.aggregation.point, c("Energy industry own use", "Energy industry own use"))
  expect_equal(routed$Flow, c("Main activity producer electricity plants", "Main activity producer electricity plants"))
  expect_equal(routed$E.dot, c(-22, -18))
})


test_that("specify_tp_eiou() works for sample data", {
  # This test is failing, because the (energy) suffix is still present in the Flow for Own use in electricity, CHP and heat plants
  # and the function assumes it has been stripped away. 
  # Solution: strip away "(energy)" and "(transf.)" during processing of these data.
  # Also, Flow.aggregation.point is not found. Need to do more to the data frame before calling specify_tp_eiou().
  specified <- load_tidy_iea_df() %>% 
    specify_tp_eiou() %>% 
    dplyr::filter(Flow.aggregation.point == "Energy industry own use", Flow == "Main activity producer electricity plants")
  expect_equal(nrow(specified), 4)
})


# Testing the function that gathers "Main activity producer" and "Autoproducer" flows/industries.
test_that("gather_producer_autoproducer() works", {
  
  # First, with AB data
  A_B_path <- system.file("extdata/A_B_data_full_2018_format_testing.csv", package = "IEATools")
  
  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df()
  
  test <- AB_data %>%
    IEATools::specify_primary_production() %>%
    IEATools::specify_production_to_resources() %>%
    gather_producer_autoproducer()
  
  
  main_activity_flows <- test %>%
    dplyr::filter(stringr::str_detect(Flow, "Main activity"))
  
  expect_equal(length(main_activity_flows[["E.dot"]]), 26)
  
  expect_equal(main_activity_flows %>%
                 dplyr::filter(
                   Country == "A",
                   Flow.aggregation.point == "Transformation processes",
                   Flow == "Main activity producer CHP plants",
                   Product == "Brown coal (if no detail)"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -90)
  
  expect_equal(main_activity_flows %>%
                 dplyr::filter(
                   Country == "A",
                   Flow.aggregation.point == "Transformation processes",
                   Flow == "Main activity producer CHP plants",
                   Product == "Hard coal (if no detail)"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               160)
  
  expect_equal(main_activity_flows %>%
                 dplyr::filter(
                   Country == "A",
                   Flow.aggregation.point == "Transformation processes",
                   Flow == "Main activity producer electricity plants",
                   Product == "Hard coal (if no detail)"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               100)
  
  expect_equal(main_activity_flows %>%
                 dplyr::filter(
                   Country == "B",
                   Flow.aggregation.point == "Transformation processes",
                   Flow == "Main activity producer heat plants",
                   Product == "Brown coal (if no detail)"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -847)
  
  # Second, with default iea data
  res <- load_tidy_iea_df() %>% 
    IEATools::specify_primary_production() %>%
    IEATools::specify_production_to_resources() %>%
    gather_producer_autoproducer()
  
  expect_equal(
    res %>% 
      dplyr::filter(stringr::str_detect(Flow, "Main activity producer") |
                      stringr::str_detect(Flow, "Autoproducer")
                    ) %>% 
      dplyr::select(Flow) %>% 
      dplyr::distinct() %>% 
      nrow(),
    1)
})



# Testing the function that routes the "Pumped storage" EIOU flow to "Main activity producer electricity plants."
test_that("route_pumped_storage() works", {
  
  # First with AB data
  A_B_path <- system.file("extdata/A_B_data_full_2018_format_testing.csv", package = "IEATools")
  
  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df()
  
  test <- AB_data %>%
    IEATools::specify_primary_production() %>%
    IEATools::specify_production_to_resources() %>%
    gather_producer_autoproducer() %>%
    route_pumped_storage()
  
  remaining_number_storage_plants <- test %>%
    dplyr::filter(Flow == "Pumped storage plants") %>%
    dplyr::pull()
  
  expect_equal(length(remaining_number_storage_plants), 0)
  
  former_pumped_storage <- test %>%
    dplyr::filter(Flow == "Main activity producer electricity plants",
                  stringr::str_detect(Product, "Hard coal"),
                  Flow.aggregation.point == "Energy industry own use",
                  Country == "A")
  
  expect_equal(former_pumped_storage %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -299)
  
  # Second with tidy default data
  res <- IEATools::load_tidy_iea_df() %>% 
    IEATools::specify_primary_production() %>%
    IEATools::specify_production_to_resources() %>%
    gather_producer_autoproducer() %>%
    route_pumped_storage()
  
  expect_equal(res %>% dplyr::filter(Flow == "Pumped storage plants") %>% nrow(), 0)
  
  # Third, with AB data but adding hydro and specifying renewable energy industries
  # Adding renewable energy flows
  AB_data_renewable_flows <- AB_data |> 
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Flow.aggregation.point = "Transformation processes", 
      Flow = "Main activity producer electricity plants", Product = IEATools::renewable_products$hydro, Unit = "TJ", E.dot = -1000) |> 
    tibble::add_row(
      Country = "B", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Flow.aggregation.point = "Energy industry own use", 
      Flow = "Pumped storage plants", Product = "Electricity", Unit = "TJ", E.dot = -150)
    
  res2 <- AB_data_renewable_flows |> 
    IEATools::specify_primary_production() %>%
    IEATools::specify_production_to_resources() %>%
    gather_producer_autoproducer() %>%
    route_pumped_storage(specify_renewable_plants = TRUE)
  
  res2 |> 
    dplyr::filter(Country == "A", Flow.aggregation.point == "Energy industry own use", Flow == IEATools::renewable_industries$hydro_plants, Product == "Hard coal (if no detail)") |> 
    magrittr::extract2("E.dot") |> 
    testthat::expect_equal(-299)
  
  res2 |> 
    dplyr::filter(Country == "B", Flow.aggregation.point == "Energy industry own use", Flow == IEATools::main_act_plants$main_act_prod_elect_plants, Product == "Electricity") |> 
    magrittr::extract2("E.dot") |> 
    testthat::expect_equal(-150)
})



test_that("split_oil_gas_extraction_eiou() works", {
  
  # Test with GHA/ZAF data
  tidy_GHA_ZAF_df <- load_tidy_iea_df() |>  
    specify_all()
  
  expect_equal(
    tidy_GHA_ZAF_df %>% 
      dplyr::filter(Flow.aggregation.point == "Energy industry own use") %>% 
      dplyr::filter(Flow == "Oil extraction") %>% 
      magrittr::extract2("E.dot"),
    -1.450736,
    tolerance = 0.01
  )
  
  expect_equal(
    tidy_GHA_ZAF_df %>% 
      dplyr::filter(Flow.aggregation.point == "Energy industry own use") %>% 
      dplyr::filter(Flow == "Natural gas extraction") %>% 
      magrittr::extract2("E.dot"),
    -2.149264,
    tolerance = 0.01
  )
  
  # Testing for AB data
  A_B_path <- system.file("extdata/A_B_data_full_2018_format_testing.csv", package = "IEATools")
  
  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df()
  
  test <- AB_data %>% 
    specify_primary_production() %>% 
    specify_production_to_resources() %>% 
    gather_producer_autoproducer() %>% 
    route_pumped_storage() %>% 
    split_oil_gas_extraction_eiou()
  
  # Testing oil extraction
  expect_equal(
    test %>% 
      dplyr::filter(Flow.aggregation.point == "Energy industry own use",
                    Flow == "Oil extraction",
                    Product == "Electricity") %>% 
      magrittr::extract2("E.dot"),
    -136
  )
  expect_equal(
    test %>% 
      dplyr::filter(Flow.aggregation.point == "Energy industry own use",
                    Flow == "Oil extraction",
                    Product == "Heat") %>% 
      magrittr::extract2("E.dot"),
    -68
  )
  expect_equal(
    test %>% 
      dplyr::filter(Flow.aggregation.point == "Energy industry own use",
                    Flow == "Oil extraction",
                    Product == "Motor gasoline excl. biofuels") %>% 
      magrittr::extract2("E.dot"),
    -34
  )
  
  # Testing nat gas extraction
  expect_equal(
    test %>% 
      dplyr::filter(Flow.aggregation.point == "Energy industry own use",
                    Flow == "Natural gas extraction",
                    Product == "Electricity") %>% 
      magrittr::extract2("E.dot"),
    -64
  )
  expect_equal(
    test %>% 
      dplyr::filter(Flow.aggregation.point == "Energy industry own use",
                    Flow == "Natural gas extraction",
                    Product == "Heat") %>% 
      magrittr::extract2("E.dot"),
    -32
  )
  expect_equal(
    test %>% 
      dplyr::filter(Flow.aggregation.point == "Energy industry own use",
                    Flow == "Natural gas extraction",
                    Product == "Motor gasoline excl. biofuels") %>% 
      magrittr::extract2("E.dot"),
    -16
  )
  
  # Now, what happens if we add to the dataset values for 2001. Does the grouping work as it should?
  AB_data_half_specified <- AB_data %>% 
    specify_primary_production() %>% 
    specify_production_to_resources() %>% 
    gather_producer_autoproducer() %>% 
    route_pumped_storage()
  
  relevant_flows <- AB_data_half_specified %>% 
    dplyr::filter(Flow == "Oil extraction" | Flow == "Natural gas extraction" | Flow == "Oil and gas extraction")
  
  change_year_to_2019 <- relevant_flows %>% 
    dplyr::mutate(
      Year = 2019,
      Flow = dplyr::case_when(
        Flow == "Natural gas extraction" ~ "Oil extraction",
        Flow == "Oil extraction" ~ "Natural gas extraction",
        TRUE ~ Flow
      )
    )
  
  AB_data_added_2019 <- AB_data_half_specified %>% 
    dplyr::bind_rows(change_year_to_2019)
  
  res <- AB_data_added_2019 %>% 
    split_oil_gas_extraction_eiou()
  
  # Testing for 2018, same as before
  # Testing oil extraction
  expect_equal(
    res %>% 
      dplyr::filter(Flow.aggregation.point == "Energy industry own use",
                    Year == 2018,
                    Flow == "Oil extraction",
                    Product == "Electricity") %>% 
      magrittr::extract2("E.dot"),
    -136
  )
  expect_equal(
    res %>% 
      dplyr::filter(Flow.aggregation.point == "Energy industry own use",
                    Year == 2018,
                    Flow == "Oil extraction",
                    Product == "Heat") %>% 
      magrittr::extract2("E.dot"),
    -68
  )
  expect_equal(
    res %>% 
      dplyr::filter(Flow.aggregation.point == "Energy industry own use",
                    Year == 2018,
                    Flow == "Oil extraction",
                    Product == "Motor gasoline excl. biofuels") %>% 
      magrittr::extract2("E.dot"),
    -34
  )
  
  # Testing nat gas extraction
  expect_equal(
    res %>% 
      dplyr::filter(Flow.aggregation.point == "Energy industry own use",
                    Year == 2018,
                    Flow == "Natural gas extraction",
                    Product == "Electricity") %>% 
      magrittr::extract2("E.dot"),
    -64
  )
  expect_equal(
    res %>% 
      dplyr::filter(Flow.aggregation.point == "Energy industry own use",
                    Year == 2018,
                    Flow == "Natural gas extraction",
                    Product == "Heat") %>% 
      magrittr::extract2("E.dot"),
    -32
  )
  expect_equal(
    res %>% 
      dplyr::filter(Flow.aggregation.point == "Energy industry own use",
                    Year == 2018,
                    Flow == "Natural gas extraction",
                    Product == "Motor gasoline excl. biofuels") %>% 
      magrittr::extract2("E.dot"),
    -16
  )

  # Testing for 2019, inverted results
  expect_equal(
    res %>% 
      dplyr::filter(Flow.aggregation.point == "Energy industry own use",
                    Year == 2019,
                    Flow == "Natural gas extraction",
                    Product == "Electricity") %>% 
      magrittr::extract2("E.dot"),
    -136
  )
  expect_equal(
    res %>% 
      dplyr::filter(Flow.aggregation.point == "Energy industry own use",
                    Year == 2019,
                    Flow == "Natural gas extraction",
                    Product == "Heat") %>% 
      magrittr::extract2("E.dot"),
    -68
  )
  expect_equal(
    res %>% 
      dplyr::filter(Flow.aggregation.point == "Energy industry own use",
                    Year == 2019,
                    Flow == "Natural gas extraction",
                    Product == "Motor gasoline excl. biofuels") %>% 
      magrittr::extract2("E.dot"),
    -34
  )
  
  # Testing nat gas extraction
  expect_equal(
    res %>% 
      dplyr::filter(Flow.aggregation.point == "Energy industry own use",
                    Year == 2019,
                    Flow == "Oil extraction",
                    Product == "Electricity") %>% 
      magrittr::extract2("E.dot"),
    -64
  )
  expect_equal(
    res %>% 
      dplyr::filter(Flow.aggregation.point == "Energy industry own use",
                    Year == 2019,
                    Flow == "Oil extraction",
                    Product == "Heat") %>% 
      magrittr::extract2("E.dot"),
    -32
  )
  expect_equal(
    res %>% 
      dplyr::filter(Flow.aggregation.point == "Energy industry own use",
                    Year == 2019,
                    Flow == "Oil extraction",
                    Product == "Motor gasoline excl. biofuels") %>% 
      magrittr::extract2("E.dot"),
    -16
  )
  
  
  # Last, what happens if there IS an EIOU flow, but neither Oil production nor Nat gas production produce anything. 
  # Test that by adding 2021 data.
  relevant_flows <- AB_data_half_specified %>% 
    dplyr::filter(Flow == "Oil and gas extraction")
  
  change_year_to_2021 <- relevant_flows %>% 
    dplyr::mutate(
      Year = 2021
    )
  
  AB_data_added_2021 <- AB_data_half_specified %>% 
    dplyr::bind_rows(change_year_to_2021)
  
  res <- AB_data_added_2021 %>% 
    split_oil_gas_extraction_eiou()
  
  
  # testing
  expect_equal(
    res %>% 
      dplyr::filter(Year == 2021,
                    Flow == "Oil and gas extraction",
                    Product == "Electricity") %>% 
      magrittr::extract2("E.dot"),
    -200
    )
  expect_equal(
    res %>% 
      dplyr::filter(Year == 2021,
                    Flow == "Oil and gas extraction",
                    Product == "Heat") %>% 
      magrittr::extract2("E.dot"),
    -100
  )
  expect_equal(
    res %>% 
      dplyr::filter(Year == 2021,
                    Flow == "Oil and gas extraction",
                    Product == "Motor gasoline excl. biofuels") %>% 
      magrittr::extract2("E.dot"),
    -50
  )
})


# Testing the function that splits the "Own use in electricity, CHP and heat plants" EIOU flow into the three main producer activities (electricity, CHP and heat)
test_that("route_own_use_elect_chp_heat() works", {
  
  A_B_path <- system.file("extdata/A_B_data_full_2018_format_testing.csv", package = "IEATools")
  
  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df()
  
  test <- AB_data %>%
    IEATools::specify_primary_production() %>%
    IEATools::specify_production_to_resources() %>%
    gather_producer_autoproducer() %>%
    route_pumped_storage() %>%
    route_own_use_elect_chp_heat(split_using_shares_of = "output")
  
  main_activity_eiou <- test %>%
    dplyr::filter(stringr::str_detect(Flow, "Main activity"), Country == "A", Flow.aggregation.point == "Energy industry own use")
  
  expect_equal(length(main_activity_eiou$Country), 9)
  
  expect_equal(main_activity_eiou %>%
                 dplyr::filter(
                   Country == "A",
                   Flow.aggregation.point == "Energy industry own use",
                   Flow == "Main activity producer CHP plants",
                   Product == "Anthracite") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -5.82,
               tolerance = 0.0001)
  
  expect_equal(main_activity_eiou %>%
                 dplyr::filter(
                   Country == "A",
                   Flow.aggregation.point == "Energy industry own use",
                   Flow == "Main activity producer CHP plants",
                   Product == "Electricity") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -3.492,
               tolerance = 0.0001)
  
  expect_equal(main_activity_eiou %>%
                 dplyr::filter(
                   Country == "A",
                   Flow.aggregation.point == "Energy industry own use",
                   Flow == "Main activity producer electricity plants",
                   Product == "Electricity") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -55.5555,
               tolerance = 0.0001)
  
  expect_equal(main_activity_eiou %>%
                 dplyr::filter(
                   Country == "A",
                   Flow.aggregation.point == "Energy industry own use",
                   Flow == "Main activity producer electricity plants",
                   Product == "Hard coal (if no detail)") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -947.1481,
               tolerance = 0.0001)
  
  expect_equal(main_activity_eiou %>%
                 dplyr::filter(
                   Country == "A",
                   Flow.aggregation.point == "Energy industry own use",
                   Flow == "Main activity producer heat plants",
                   Product == "Hard coal (if no detail)") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -11.1111,
               tolerance = 0.0001)
  
  second_test <- AB_data %>%
    IEATools::specify_primary_production() %>%
    IEATools::specify_production_to_resources() %>%
    gather_producer_autoproducer() %>%
    route_pumped_storage() %>%
    dplyr::filter(! Flow %in% c("Main activity producer electricity plants", "Main activity producer CHP plants", "Main activity producer heat plants")) %>%
    route_own_use_elect_chp_heat(split_using_shares_of = "output")
  
  expect_true("Main activity producer electricity plants" %in% second_test$Flow)
  expect_false("Own use in electricity, CHP and heat plants" %in% second_test$Flow)
  
  
  
  third_test <- AB_data %>%
    IEATools::specify_primary_production() %>%
    IEATools::specify_production_to_resources() %>%
    gather_producer_autoproducer() %>%
    route_pumped_storage() %>%
    route_own_use_elect_chp_heat(split_using_shares_of = "output")
  
  expect_equal(
    third_test %>% 
      dplyr::filter(Flow == "Own use in electricity, CHP and heat plants") %>% 
      nrow(),
    0
  )
  
  
  # Now use the input shares for calculations
  fourth_test <- AB_data %>%
    IEATools::specify_primary_production() %>%
    IEATools::specify_production_to_resources() %>%
    gather_producer_autoproducer() %>%
    route_pumped_storage() %>%
    route_own_use_elect_chp_heat()
  
  
  fourth_test %>% 
    dplyr::filter(Country == "B",
                  Product == "Electricity",
                  Flow.aggregation.point == "Energy industry own use",
                  Flow == "Main activity producer heat plants") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(-15.65137,
                 tolerance = 0.001)
  
  fourth_test %>% 
    dplyr::filter(Country == "B",
                  Product == "Natural gas",
                  Flow.aggregation.point == "Energy industry own use",
                  Flow == "Main activity producer heat plants") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(-52.17123,
                 tolerance = 0.001)
  
  fourth_test %>% 
    dplyr::filter(Country == "B",
                  Product == "Blast furnace gas",
                  Flow.aggregation.point == "Energy industry own use",
                  Flow == "Main activity producer CHP plants") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(-6.159532,
                 tolerance = 0.001)
  
  
  fourth_test %>% 
    dplyr::filter(Country == "A",
                  Product == "Electricity",
                  Flow.aggregation.point == "Energy industry own use",
                  Flow == "Main activity producer CHP plants") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(-1.648352,
                 tolerance = 0.001)
  
  fourth_test %>% 
    dplyr::filter(Country == "A",
                  Product == "Electricity",
                  Flow.aggregation.point == "Energy industry own use",
                  Flow == "Main activity producer heat plants") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(-1.318681,
                 tolerance = 0.001)
  
  
  # Now, we test with the tidy default iea data
  res <- load_tidy_iea_df() %>% 
    IEATools::specify_primary_production() %>%
    IEATools::specify_production_to_resources() %>%
    gather_producer_autoproducer() %>%
    route_pumped_storage() %>%
    route_own_use_elect_chp_heat()
  
  expect_equal(
    res %>% 
      dplyr::filter(Flow == "Own use in electricity, CHP and heat plants") %>% 
      nrow(),
    0
  )
  
  res <- load_tidy_iea_df() %>% 
    IEATools::specify_primary_production() %>%
    IEATools::specify_production_to_resources() %>%
    gather_producer_autoproducer() %>%
    route_pumped_storage() %>%
    route_own_use_elect_chp_heat(split_using_shares_of = "output")
  
  expect_equal(
    res %>% 
      dplyr::filter(Flow == "Own use in electricity, CHP and heat plants") %>% 
      nrow(),
    0
  )
})



# Testing the function that adds a nuclear industry to the PSUT by adding some Transformation processes flows,
# and modifying the Main activity producer electricity and CHP plants
test_that("add_nuclear_industry() works", {
  
  # First with AB data
  A_B_path <- system.file("extdata/A_B_data_full_2018_format_testing.csv", package = "IEATools")
  
  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df()
  
  test <- AB_data %>%
    IEATools::specify_primary_production() %>%
    IEATools::specify_production_to_resources() %>%
    gather_producer_autoproducer() %>%
    route_pumped_storage() %>%
    route_own_use_elect_chp_heat(split_using_shares_of = "output") %>%
    add_nuclear_industry()
  
  # One test on total length
  expect_equal(length(test[["E.dot"]]), 140)
  
  # One test on length of nuclear industry flows
  expect_equal(test %>%
                 dplyr::filter(Flow == "Nuclear industry") %>%
                 dplyr::pull() %>%
                 length(),
               2)
  
  expect_equal(test %>%
                 dplyr::filter(Product == "Nuclear") %>%
                 dplyr::pull() %>%
                 length(),
               1)
  
  # Some tests on actual values of elec & CHP plants
  expect_equal(test %>%
                 dplyr::filter(Country == "A",
                               Flow.aggregation.point == "Transformation processes",
                               Flow == "Main activity producer electricity plants",
                               Product == "Electricity") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               3163.7)
  
  expect_equal(test %>%
                 dplyr::filter(Country == "A",
                               Flow.aggregation.point == "Transformation processes",
                               Flow == "Main activity producer CHP plants",
                               Product == "Electricity") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               56.7)
  
  # Some tests on actual values of nuclear industry flows
  expect_equal(test %>%
                 dplyr::filter(Country == "A",
                               Flow.aggregation.point == "Transformation processes",
                               Flow == "Nuclear industry",
                               Product == "Electricity") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               39.6)
  
  expect_equal(test %>%
                 dplyr::filter(Country == "A",
                               Flow.aggregation.point == "Transformation processes",
                               Flow == "Nuclear industry",
                               Product == "Nuclear") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -120)
  
  # Now, new example adding heat output for CHP, country A
  second_test <- AB_data %>%
    tibble::add_row(Country = "A",
                    Method = "PCM",
                    Energy.type = "E",
                    Last.stage = "Final",
                    Year = 2018,
                    Ledger.side = "Supply",
                    Flow.aggregation.point = "Transformation processes",
                    Flow = "Main activity producer CHP plants",
                    Product = "Heat",
                    Unit = "TJ",
                    E.dot = 30) %>%
    IEATools::specify_primary_production() %>%
    IEATools::specify_production_to_resources() %>%
    gather_producer_autoproducer() %>%
    route_pumped_storage() %>%
    route_own_use_elect_chp_heat(split_using_shares_of = "output") %>%
    add_nuclear_industry()
  
  # One test on total length
  expect_equal(length(second_test[["E.dot"]]), 142)
  
  # One test on length of nuclear industry flows
  expect_equal(second_test %>%
                 dplyr::filter(Flow == "Nuclear industry") %>%
                 dplyr::pull() %>%
                 length(),
               3)
  
  expect_equal(second_test %>%
                 dplyr::filter(Product == "Nuclear") %>%
                 dplyr::pull() %>%
                 length(),
               1)
  
  # Some tests on actual values of elec & CHP plants
  expect_equal(second_test %>%
                 dplyr::filter(Country == "A",
                               Flow.aggregation.point == "Transformation processes",
                               Flow == "Main activity producer electricity plants",
                               Product == "Electricity") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               3163.7)
  
  expect_equal(second_test %>%
                 dplyr::filter(Country == "A",
                               Flow.aggregation.point == "Transformation processes",
                               Flow == "Main activity producer CHP plants",
                               Product == "Electricity") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               57.8)
  
  expect_equal(second_test %>%
                 dplyr::filter(Country == "A",
                               Flow.aggregation.point == "Transformation processes",
                               Flow == "Main activity producer CHP plants",
                               Product == "Heat") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               28.9)
  
  # Some tests on actual values of nuclear industry flows
  expect_equal(second_test %>%
                 dplyr::filter(Country == "A",
                               Flow.aggregation.point == "Transformation processes",
                               Flow == "Nuclear industry",
                               Product == "Electricity") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               38.5)
  
  expect_equal(test %>%
                 dplyr::filter(Country == "A",
                               Flow.aggregation.point == "Transformation processes",
                               Flow == "Nuclear industry",
                               Product == "Nuclear") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -120)
  
  expect_equal(second_test %>%
                 dplyr::filter(Country == "A",
                               Flow.aggregation.point == "Transformation processes",
                               Flow == "Nuclear industry",
                               Product == "Heat") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               1.1)
  
  # Now with default IEA data
  res <- load_tidy_iea_df() %>% 
    IEATools::specify_primary_production() %>%
    IEATools::specify_production_to_resources() %>%
    gather_producer_autoproducer() %>%
    route_pumped_storage() %>%
    route_own_use_elect_chp_heat() %>%
    add_nuclear_industry()
  
  res %>% 
    dplyr::filter(stringr::str_detect(Flow, "Nuclear")) %>% 
    nrow() %>% 
    expect_equal(5)
})


test_that("route_non_specified_eiou() works", {
  
  # First AB data
  A_B_path <- system.file("extdata/A_B_data_full_2018_format_testing.csv", package = "IEATools")
  
  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df()
  
  test <- AB_data %>%
    IEATools::specify_primary_production() %>%
    IEATools::specify_production_to_resources() %>%
    gather_producer_autoproducer() %>%
    route_pumped_storage() %>%
    route_own_use_elect_chp_heat(split_using_shares_of = "output") %>%
    add_nuclear_industry() %>%
    route_non_specified_eiou()
  
  
  # Now testing a few of the new A flows. Non-specified only had a "hard coal" product
  
  expect_equal(test %>%
                 dplyr::filter(Country == "A",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Blast furnaces",
                               Product == "Hard coal (if no detail)") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -3.3299,
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(Country == "A",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Coal mines",
                               Product == "Hard coal (if no detail)") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -4.7571,
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(Country == "A",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Main activity producer electricity plants",
                               Product == "Hard coal (if no detail)") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               (-947.148148 - 104.2088),
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(Country == "A",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Main activity producer CHP plants",
                               Product == "Hard coal (if no detail)") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               (-40.740741 - 4.7621),
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(Country == "A",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Oil refineries",
                               Product == "Hard coal (if no detail)") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -28.54263,
               tolerance = 0.001)
  
  
  # Now, a few tests for country B.
  # There are non-specified flows both of hard and brown coal.
  
  expect_equal(test %>%
                 dplyr::filter(Country == "B",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Blast furnaces",
                               Product == "Hard coal (if no detail)") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -23.9795,
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(Country == "B",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Blast furnaces",
                               Product == "Brown coal (if no detail)") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -24.0816,
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(Country == "B",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Coke ovens",
                               Product == "Hard coal (if no detail)") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -53.95407,
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(Country == "B",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Main activity producer electricity plants",
                               Product == "Hard coal (if no detail)") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               (-50.4712 - 259.846827),
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(Country == "B",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Main activity producer heat plants",
                               Product == "Hard coal (if no detail)") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               (-18.75406 - 96.553611),
               tolerance = 0.001)
  
  # Checking what happens when no EIOU other that non-specified
  second_test <- AB_data %>%
    IEATools::specify_primary_production() %>%
    IEATools::specify_production_to_resources() %>%
    gather_producer_autoproducer() %>%
    route_pumped_storage() %>%
    route_own_use_elect_chp_heat(split_using_shares_of = "output") %>%
    add_nuclear_industry() %>%
    dplyr::filter(! (Flow.aggregation.point == "Energy industry own use" & Flow != "Non-specified")) %>%
    route_non_specified_eiou()
  
  expect_equal(second_test %>%
                 dplyr::filter(Flow.aggregation.point == "Energy industry own use") %>%
                 dplyr::select(Flow) %>%
                 dplyr::pull(),
               c("Non-specified", "Non-specified", "Non-specified"))
  
  
  # Now default data
  res <- load_tidy_iea_df() %>% 
    IEATools::specify_primary_production() %>%
    IEATools::specify_production_to_resources() %>%
    gather_producer_autoproducer() %>%
    route_pumped_storage() %>%
    route_own_use_elect_chp_heat() %>%
    add_nuclear_industry() %>%
    route_non_specified_eiou()
  
  res %>% 
    dplyr::filter(stringr::str_detect(Flow, "Non-specified") & Flow.aggregation.point == "Ennergy industry own use") %>% 
    nrow() %>% 
    expect_equal(0)
})


test_that("route_non_specified_tp() works", {
  
  A_B_path <- system.file("extdata/A_B_data_full_2018_format_testing.csv", package = "IEATools")
  
  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df()
  
  test <- AB_data %>%
    IEATools::specify_primary_production() %>%
    IEATools::specify_production_to_resources() %>%
    gather_producer_autoproducer() %>%
    route_pumped_storage() %>%
    route_own_use_elect_chp_heat(split_using_shares_of = "output") %>%
    add_nuclear_industry() %>%
    route_non_specified_eiou() %>%
    tibble::add_row(
      Country = "B",
      Method = "PCM",
      Energy.type = "E",
      Last.stage = "Final",
      Year = 2018,
      Ledger.side = "Supply",
      Flow.aggregation.point = "Transformation processes",
      Flow = "Oil refineries",
      Product = "Brown coal (if no detail)",
      Unit = "TJ",
      E.dot = 67
    ) %>%
    route_non_specified_tp()
  
  
  # Country A, Hard coal:
  expect_equal(test %>%
                 dplyr::filter(
                   Country == "A",
                   Flow.aggregation.point == "Transformation processes",
                   Flow == "Main activity producer CHP plants",
                   Product == "Hard coal (if no detail)"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               217.1429,
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(
                   Country == "A",
                   Flow.aggregation.point == "Transformation processes",
                   Flow == "Main activity producer electricity plants",
                   Product == "Hard coal (if no detail)"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               135.7143,
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(
                   Country == "A",
                   Flow.aggregation.point == "Transformation processes",
                   Flow == "Main activity producer heat plants",
                   Product == "Hard coal (if no detail)"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               27.14286,
               tolerance = 0.001)
  
  # Country B, hard coal:
  expect_equal(test %>%
                 dplyr::filter(
                   Country == "B",
                   Flow.aggregation.point == "Transformation processes",
                   Flow == "Main activity producer heat plants",
                   Product == "Hard coal (if no detail)"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               668,
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(
                   Country == "B",
                   Flow.aggregation.point == "Transformation processes",
                   Flow == "Main activity producer electricity plants",
                   Product == "Hard coal (if no detail)"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -330,
               tolerance = 0.001)
  
  
  # Country A, brown coal:
  expect_equal(test %>%
                 dplyr::filter(
                   Country == "A",
                   Flow.aggregation.point == "Transformation processes",
                   Flow == "Main activity producer CHP plants",
                   Product == "Brown coal (if no detail)"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -218.5714,
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(
                   Country == "A",
                   Flow.aggregation.point == "Transformation processes",
                   Flow == "Main activity producer electricity plants",
                   Product == "Brown coal (if no detail)"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -121.4286,
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(
                   Country == "A",
                   Flow.aggregation.point == "Transformation processes",
                   Flow == "Main activity producer heat plants",
                   Product == "Brown coal (if no detail)"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               40,
               tolerance = 0.001)
  
  
  # Country B, brown coal:
  expect_equal(test %>%
                 dplyr::filter(
                   Country == "B",
                   Flow.aggregation.point == "Transformation processes",
                   Flow == "Main activity producer CHP plants",
                   Product == "Brown coal (if no detail)"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -400,
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(
                   Country == "B",
                   Flow.aggregation.point == "Transformation processes",
                   Flow == "Main activity producer electricity plants",
                   Product == "Brown coal (if no detail)"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               620.6897,
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(
                   Country == "B",
                   Flow.aggregation.point == "Transformation processes",
                   Flow == "Main activity producer heat plants",
                   Product == "Brown coal (if no detail)"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -847,
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(
                   Country == "B",
                   Flow.aggregation.point == "Transformation processes",
                   Flow == "Oil refineries",
                   Product == "Brown coal (if no detail)"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               (623-620.6897+67),
               tolerance = 0.001)
  
  # Checking what happens when no TP besides non-specified exist
  second_test <- AB_data %>%
    IEATools::specify_primary_production() %>%
    IEATools::specify_production_to_resources() %>%
    gather_producer_autoproducer() %>%
    route_pumped_storage() %>%
    route_own_use_elect_chp_heat(split_using_shares_of = "output") %>%
    add_nuclear_industry() %>%
    route_non_specified_eiou() %>%
    dplyr::filter(! (Flow.aggregation.point == "Transformation processes" & Flow != "Non-specified")) %>%
    route_non_specified_tp()
  
  expect_equal(second_test %>%
                 dplyr::filter(Flow.aggregation.point == "Transformation processes") %>%
                 dplyr::select(Flow) %>%
                 dplyr::pull(),
               c("Non-specified", "Non-specified", "Non-specified", "Non-specified"))
  
  # Now default data
  res <- load_tidy_iea_df() %>% 
    IEATools::specify_primary_production() %>%
    IEATools::specify_production_to_resources() %>%
    gather_producer_autoproducer() %>%
    route_pumped_storage() %>%
    route_own_use_elect_chp_heat() %>%
    add_nuclear_industry() %>%
    route_non_specified_eiou() %>% 
    route_non_specified_tp()
  
  res %>% 
    dplyr::filter(stringr::str_detect(Flow, "Non-specified") & Flow.aggregation.point == "Transformation processes") %>% 
    nrow() %>% 
    expect_equal(0)
})


test_that("route_non_specified_flows() works", {
  
  A_B_path <- system.file("extdata/A_B_data_full_2018_format_testing.csv", package = "IEATools")
  
  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df(unit_val = "ktoe")
  
  test <- AB_data %>%
    IEATools::specify_primary_production() %>%
    IEATools::specify_production_to_resources() %>%
    gather_producer_autoproducer() %>%
    route_pumped_storage() %>%
    route_own_use_elect_chp_heat(split_using_shares_of = "output") %>%
    add_nuclear_industry() %>%
    tibble::add_row(
      Country = "B",
      Method = "PCM",
      Energy.type = "E",
      Last.stage = "Final",
      Year = 2018,
      Ledger.side = "Supply",
      Flow.aggregation.point = "Transformation processes",
      Flow = "Oil refineries",
      Product = "Brown coal (if no detail)",
      Unit = "ktoe",
      E.dot = 67
    ) %>%
    route_non_specified_flows()
  
  
  # Country A
  expect_equal(test %>%
                 dplyr::filter(Country == "A",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Blast furnaces",
                               Product == "Hard coal (if no detail)") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -3.3299,
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(Country == "A",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Coal mines",
                               Product == "Hard coal (if no detail)") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -4.7571,
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(Country == "A",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Main activity producer electricity plants",
                               Product == "Hard coal (if no detail)") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               (-947.148148 - 104.2088),
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(Country == "A",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Main activity producer CHP plants",
                               Product == "Hard coal (if no detail)") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               (-40.740741 - 4.7621),
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(Country == "A",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Oil refineries",
                               Product == "Hard coal (if no detail)") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -28.54263,
               tolerance = 0.001)
  
  
  # Country B
  expect_equal(test %>%
                 dplyr::filter(Country == "B",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Blast furnaces",
                               Product == "Hard coal (if no detail)") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -23.9795,
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(Country == "B",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Blast furnaces",
                               Product == "Brown coal (if no detail)") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -24.0816,
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(Country == "B",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Coke ovens",
                               Product == "Hard coal (if no detail)") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -53.95407,
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(Country == "B",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Main activity producer electricity plants",
                               Product == "Hard coal (if no detail)") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               (-50.4712 - 259.846827),
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(Country == "B",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Main activity producer heat plants",
                               Product == "Hard coal (if no detail)") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               (-18.75406 - 96.553611),
               tolerance = 0.001)
  
  
  # Second, test that routing TPs flows tests also pass.
  # Country A, Hard coal:
  expect_equal(test %>%
                 dplyr::filter(
                   Country == "A",
                   Flow.aggregation.point == "Transformation processes",
                   Flow == "Main activity producer CHP plants",
                   Product == "Hard coal (if no detail)"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               217.1429,
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(
                   Country == "A",
                   Flow.aggregation.point == "Transformation processes",
                   Flow == "Main activity producer electricity plants",
                   Product == "Hard coal (if no detail)"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               135.7143,
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(
                   Country == "A",
                   Flow.aggregation.point == "Transformation processes",
                   Flow == "Main activity producer heat plants",
                   Product == "Hard coal (if no detail)"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               27.14286,
               tolerance = 0.001)
  
  # Country B, hard coal:
  expect_equal(test %>%
                 dplyr::filter(
                   Country == "B",
                   Flow.aggregation.point == "Transformation processes",
                   Flow == "Main activity producer heat plants",
                   Product == "Hard coal (if no detail)"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               668,
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(
                   Country == "B",
                   Flow.aggregation.point == "Transformation processes",
                   Flow == "Main activity producer electricity plants",
                   Product == "Hard coal (if no detail)"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -330,
               tolerance = 0.001)
  
  
  # Country A, brown coal:
  expect_equal(test %>%
                 dplyr::filter(
                   Country == "A",
                   Flow.aggregation.point == "Transformation processes",
                   Flow == "Main activity producer CHP plants",
                   Product == "Brown coal (if no detail)"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -218.5714,
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(
                   Country == "A",
                   Flow.aggregation.point == "Transformation processes",
                   Flow == "Main activity producer electricity plants",
                   Product == "Brown coal (if no detail)"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -121.4286,
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(
                   Country == "A",
                   Flow.aggregation.point == "Transformation processes",
                   Flow == "Main activity producer heat plants",
                   Product == "Brown coal (if no detail)"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               40,
               tolerance = 0.001)
  
  
  # Country B, brown coal:
  expect_equal(test %>%
                 dplyr::filter(
                   Country == "B",
                   Flow.aggregation.point == "Transformation processes",
                   Flow == "Main activity producer CHP plants",
                   Product == "Brown coal (if no detail)"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -400,
               tolerance = 0.001)
  
  test %>%
    dplyr::filter(
      Country == "B",
      Flow.aggregation.point == "Transformation processes",
      Flow == "Main activity producer electricity plants",
      Product == "Brown coal (if no detail)"
    ) %>%
    dplyr::select(E.dot) %>%
    dplyr::pull() |> 
    expect_equal(620.6897, tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(
                   Country == "B",
                   Flow.aggregation.point == "Transformation processes",
                   Flow == "Main activity producer heat plants",
                   Product == "Brown coal (if no detail)"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -847,
               tolerance = 0.001)
  
  test %>%
    dplyr::filter(
      Country == "B",
      Flow.aggregation.point == "Transformation processes",
      Flow == "Oil refineries",
      Product == "Brown coal (if no detail)"
    ) %>%
    dplyr::select(E.dot) %>%
    dplyr::pull() |> 
    expect_equal(623-620.6897+67, tolerance = 0.001)
  
  first_version_test <- AB_data %>%
    IEATools::specify_primary_production() %>%
    IEATools::specify_production_to_resources() %>%
    gather_producer_autoproducer() %>%
    route_pumped_storage() %>%
    route_own_use_elect_chp_heat(
      split_using_shares_of = "output"
      ) %>%
    add_nuclear_industry() %>%
    route_non_specified_flows(
      route_non_specified_eiou = FALSE,
      route_non_specified_tp = FALSE
    )
  
  second_version_test <- AB_data %>%
    IEATools::specify_primary_production() %>%
    IEATools::specify_production_to_resources() %>%
    gather_producer_autoproducer() %>%
    route_pumped_storage() %>%
    route_own_use_elect_chp_heat(
      split_using_shares_of = "output"
    ) %>%
    add_nuclear_industry()
  
  expect_true(all(first_version_test == second_version_test))
  
  
  # Now with default iea data
  res <- load_tidy_iea_df() %>% 
    IEATools::specify_primary_production() %>%
    IEATools::specify_production_to_resources() %>%
    gather_producer_autoproducer() %>%
    route_pumped_storage() %>%
    route_own_use_elect_chp_heat() %>%
    add_nuclear_industry() %>%
    route_non_specified_eiou() %>% 
    route_non_specified_tp()
  
  res %>% 
    dplyr::filter(stringr::str_detect(Flow, "Non-specified") & (Flow.aggregation.point %in% c("Transformation processes", "Energy industry own use"))) %>% 
    nrow() %>% 
    expect_equal(0)
})


test_that("specify_all() function keeps balance",{
  
  # First with default dataset
  dummy_dataset <- load_tidy_iea_df() %>% 
    specify_all()
  
  expect_true(
    dummy_dataset %>% 
      tidy_iea_df_balanced()
  )
  
  dummy_dataset <- load_tidy_iea_df() %>% 
    IEATools::specify_primary_production() %>%
    IEATools::specify_production_to_resources() %>%
    gather_producer_autoproducer() %>%
    route_pumped_storage() %>%
    route_own_use_elect_chp_heat(split_using_shares_of = "output") %>%
    add_nuclear_industry() %>% 
    route_non_specified_flows()
  
  expect_true(
    dummy_dataset %>% 
      tidy_iea_df_balanced()
  )
  
  # Now with A-B country example.
  A_B_path <- system.file("extdata/A_B_data_full_2018_format_testing.csv", package = "IEATools")
  
  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df()
  
  AB_data %>% 
    tidy_iea_df_balanced()
  
  AB_data_specified <- AB_data %>% 
    specify_all()
  
  expect_true(AB_data_specified %>% 
                tidy_iea_df_balanced())
})


test_that("specify_all() can also not split the non-specified flows", {
  
  A_B_path <- system.file("extdata/A_B_data_full_2018_format_testing.csv", package = "IEATools")
  
  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df()
  
  first_test <- AB_data %>%
    IEATools::specify_primary_production() %>%
    IEATools::specify_production_to_resources() %>%
    gather_producer_autoproducer() %>%
    route_pumped_storage() %>%
    route_own_use_elect_chp_heat() %>%
    add_nuclear_industry()
  
  second_test <- AB_data %>%
    IEATools::specify_primary_production() %>%
    IEATools::specify_production_to_resources() %>%
    gather_producer_autoproducer() %>%
    route_pumped_storage() %>%
    route_own_use_elect_chp_heat() %>%
    add_nuclear_industry() %>% 
    route_non_specified_flows(
      route_non_specified_eiou = FALSE,
      route_non_specified_tp = FALSE
    )
  
  expect_true(
    all(first_test == second_test)
  )
})



test_that("specify_renewable_plants() works", {

  # Now with A-B country example.
  A_B_path <- system.file("extdata/A_B_data_full_2018_format_testing.csv", package = "IEATools")
  
  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df()
  
  AB_data %>% 
    tidy_iea_df_balanced()
  
  # Adding renewable energy flows
  AB_data_renewable_flows <- AB_data |> 
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Flow.aggregation.point = "Transformation processes", 
      Flow = "Main activity producer electricity plants", Product = IEATools::renewable_products$solar_photovoltaics, Unit = "TJ", E.dot = -10) |> 
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Flow.aggregation.point = "Transformation processes", 
      Flow = "Main activity producer electricity plants", Product = IEATools::renewable_products$wind, Unit = "TJ", E.dot = -15) |> 
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Flow.aggregation.point = "Transformation processes", 
      Flow = "Autoproducer electricity plants", Product = IEATools::renewable_products$tide_wave_and_ocean, Unit = "TJ", E.dot = -2) |> 
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Flow.aggregation.point = "Transformation processes", 
      Flow = "Autoproducer electricity plants", Product = IEATools::renewable_products$hydro, Unit = "TJ", E.dot = -20) |> 
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Flow.aggregation.point = "Transformation processes", 
      Flow = "Main activity producer electricity plants", Product = IEATools::renewable_products$geothermal, Unit = "TJ", E.dot = -20) |> 
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Flow.aggregation.point = "Transformation processes", 
      Flow = "Main activity producer electricity plants", Product = IEATools::renewable_products$solar_thermal, Unit = "TJ", E.dot = -20) |> 
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Flow.aggregation.point = "Transformation processes", 
      Flow = "Main activity producer CHP plants", Product = IEATools::renewable_products$geothermal, Unit = "TJ", E.dot = -10) |> 
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Flow.aggregation.point = "Transformation processes", 
      Flow = "Autoproducer CHP plants", Product = IEATools::renewable_products$solar_thermal, Unit = "TJ", E.dot = -5) |> 
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Flow.aggregation.point = "Transformation processes", 
      Flow = "Main activity producer heat plants", Product = IEATools::renewable_products$geothermal, Unit = "TJ", E.dot = -8) |> 
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Flow.aggregation.point = "Transformation processes", 
      Flow = "Autoproducer heat plants", Product = IEATools::renewable_products$solar_thermal, Unit = "TJ", E.dot = -10) |> 
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Flow.aggregation.point = "Transformation processes", 
      Flow = "Main activity producer heat plants", Product = "Heat", Unit = "TJ", E.dot = 100) |> 
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Flow.aggregation.point = "Transformation processes", 
      Flow = "Autoproducer CHP plants", Product = "Heat", Unit = "TJ", E.dot = 80)
  
  
  # First, test that by default nothing gets specified
  AB_data_specified_default <- AB_data_renewable_flows %>% 
    specify_all()
  
  AB_data_specified_default |> dplyr::filter(Flow %in% IEATools::renewable_industries) |> nrow() |> 
    testthat::expect_equal(0)
  
  # Second, specify renewable energy flows
  AB_data_specified_renewables <- AB_data_renewable_flows %>% 
    specify_all(specify_renewable_plants = TRUE)
  
  AB_data_prespecified <- AB_data_renewable_flows %>% 
    dplyr::filter(Product != "Nuclear") |> 
    specify_primary_production() |> 
    gather_producer_autoproducer() %>% 
    route_pumped_storage() %>% 
    split_oil_gas_extraction_eiou() %>% 
    route_own_use_elect_chp_heat() %>% 
    add_nuclear_industry()
  
  AB_data_specified_renewables <- AB_data_prespecified |> 
    specify_renewable_plants(specify_renewable_plants = TRUE)
    

  # (1) Checking renewable energy plants
  # (1.a) Geothermal
  AB_data_specified_renewables |> dplyr::filter(Flow == IEATools::renewable_industries$geothermal_plants, Product == IEATools::renewable_products$geothermal) |> 
    magrittr::extract2("E.dot") |> 
    testthat::expect_equal(-38)
  AB_data_specified_renewables |> dplyr::filter(Flow == IEATools::renewable_industries$geothermal_plants, Product == IEATools::electricity_products$electricity) |> 
    magrittr::extract2("E.dot") |> 
    testthat::expect_equal(2.789474, tolerance = 1e-4)
  AB_data_specified_renewables |> dplyr::filter(Flow == IEATools::renewable_industries$geothermal_plants, Product == "Heat") |> 
    magrittr::extract2("E.dot") |> 
    testthat::expect_equal(5.052632, tolerance = 1e-4)
  
  # (1.b) Solar thermal
  AB_data_specified_renewables |> dplyr::filter(Flow == IEATools::renewable_industries$solar_th_plants, Product == IEATools::renewable_products$solar_thermal) |> 
    magrittr::extract2("E.dot") |> 
    testthat::expect_equal(-35)
  AB_data_specified_renewables |> dplyr::filter(Flow == IEATools::renewable_industries$solar_th_plants, Product == IEATools::electricity_products$electricity) |> 
    magrittr::extract2("E.dot") |> 
    testthat::expect_equal(7.745834, tolerance = 1e-4)
  AB_data_specified_renewables |> dplyr::filter(Flow == IEATools::renewable_industries$solar_th_plants, Product == "Heat") |> 
    magrittr::extract2("E.dot") |> 
    testthat::expect_equal(11.52778, tolerance = 1e-4)
  
  # (1.c) Hydro
  AB_data_specified_renewables |> dplyr::filter(Flow == IEATools::renewable_industries$hydro_plants, Product == IEATools::renewable_products$hydro) |> 
    magrittr::extract2("E.dot") |> 
    testthat::expect_equal(-20)
  AB_data_specified_renewables |> dplyr::filter(Flow == IEATools::renewable_industries$hydro_plants, Product == IEATools::electricity_products$electricity) |> 
    magrittr::extract2("E.dot") |> 
    testthat::expect_equal(20)
  
  # (1.d) Wind
  AB_data_specified_renewables |> dplyr::filter(Flow == IEATools::renewable_industries$wind_power_plants, Product == IEATools::renewable_products$wind) |> 
    magrittr::extract2("E.dot") |> 
    testthat::expect_equal(-15)
  AB_data_specified_renewables |> dplyr::filter(Flow == IEATools::renewable_industries$wind_power_plants, Product == IEATools::electricity_products$electricity) |> 
    magrittr::extract2("E.dot") |> 
    testthat::expect_equal(15)
  
  # (1.e) Solar PV
  AB_data_specified_renewables |> dplyr::filter(Flow == IEATools::renewable_industries$solar_pv_plants, Product == IEATools::renewable_products$solar_photovoltaics) |> 
    magrittr::extract2("E.dot") |> 
    testthat::expect_equal(-10)
  AB_data_specified_renewables |> dplyr::filter(Flow == IEATools::renewable_industries$solar_pv_plants, Product == IEATools::electricity_products$electricity) |> 
    magrittr::extract2("E.dot") |> 
    testthat::expect_equal(10)
  
  # (1.f) Oceanic
  AB_data_specified_renewables |> dplyr::filter(Flow == IEATools::renewable_industries$oceanic_plants, Product == IEATools::renewable_products$tide_wave_and_ocean) |> 
    magrittr::extract2("E.dot") |> 
    testthat::expect_equal(-2)
  AB_data_specified_renewables |> dplyr::filter(Flow == IEATools::renewable_industries$oceanic_plants, Product == IEATools::electricity_products$electricity) |> 
    magrittr::extract2("E.dot") |> 
    testthat::expect_equal(2)
  
    
  # Checking Main activity electricity producer plants
  AB_data_specified_renewables |> dplyr::filter(Country == "A", Flow == IEATools::main_act_plants$main_act_prod_elect_plants, Product == IEATools::electricity_products$electricity, 
                                                Flow.aggregation.point == IEATools::tfc_compare_flows$transformation_processes) |> 
    magrittr::extract2("E.dot") |> 
    testthat::expect_equal(3200-(20+15+10+2+2+20*0.33))
  
  # Checking Main activity CHP producer plants
  AB_data_specified_renewables |> dplyr::filter(Flow == IEATools::main_act_plants$main_act_prod_chp_plants, Product == IEATools::electricity_products$electricity,
                                                Flow.aggregation.point == IEATools::tfc_compare_flows$transformation_processes) |> 
    magrittr::extract2("E.dot") |> 
    testthat::expect_equal(60-(1.145834+0.789474), tolerance = 1e-4)
  AB_data_specified_renewables |> dplyr::filter(Flow == IEATools::main_act_plants$main_act_prod_chp_plants, Product == "Heat") |> 
    magrittr::extract2("E.dot") |> 
    testthat::expect_equal(80-(1.52778+1.052632), tolerance = 1e-4)
  
  # Checking Main activity heat producer plants
  AB_data_specified_renewables |> dplyr::filter(Flow == IEATools::main_act_plants$main_act_prod_heat_plants, Product == "Heat") |> 
    magrittr::extract2("E.dot") |> 
    testthat::expect_equal(100-(4+10))
  
  # Checking no autoproducers lefts
  AB_data_specified_renewables |> dplyr::filter(Flow %in% c(IEATools::main_act_plants$autoprod_chp_plants, IEATools::main_act_plants$autoprod_heat_plants, IEATools::main_act_plants$autoprod_elect_plants)) |> 
    nrow() |> testthat::expect_equal(0)
})


test_that("specify_electricity_grid() works", {

  # Now with A-B country example.
  A_B_path <- system.file("extdata/A_B_data_full_2018_format_testing.csv", package = "IEATools")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df()

  AB_data %>%
    tidy_iea_df_balanced()

  # Adding renewable energy flows
  # AB_expanded <- AB_data |> 
  #   tibble::add_row(
  #     Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Flow.aggregation.point = "Transformation processes", 
  #     Flow = "Main activity producer electricity plants", Product = IEATools::renewable_products$solar_photovoltaics, Unit = "TJ", E.dot = -10) |> 
  #   tibble::add_row(
  #     Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Flow.aggregation.point = "Transformation processes", 
  #     Flow = "Main activity producer electricity plants", Product = IEATools::renewable_products$wind, Unit = "TJ", E.dot = -15) |> 
  #   tibble::add_row(
  #     Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Flow.aggregation.point = "Transformation processes", 
  #     Flow = "Autoproducer electricity plants", Product = IEATools::renewable_products$tide_wave_and_ocean, Unit = "TJ", E.dot = -2) |> 
  #   tibble::add_row(
  #     Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Flow.aggregation.point = "Transformation processes", 
  #     Flow = "Autoproducer electricity plants", Product = IEATools::renewable_products$hydro, Unit = "TJ", E.dot = -20) |> 
  #   tibble::add_row(
  #     Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Flow.aggregation.point = "Transformation processes", 
  #     Flow = "Main activity producer electricity plants", Product = IEATools::renewable_products$geothermal, Unit = "TJ", E.dot = -20) |> 
  #   tibble::add_row(
  #     Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Flow.aggregation.point = "Transformation processes", 
  #     Flow = "Main activity producer electricity plants", Product = IEATools::renewable_products$solar_thermal, Unit = "TJ", E.dot = -20) |> 
  #   tibble::add_row(
  #     Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Flow.aggregation.point = "Transformation processes", 
  #     Flow = "Main activity producer CHP plants", Product = IEATools::renewable_products$geothermal, Unit = "TJ", E.dot = -10) |> 
  #   tibble::add_row(
  #     Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Flow.aggregation.point = "Transformation processes", 
  #     Flow = "Autoproducer CHP plants", Product = IEATools::renewable_products$solar_thermal, Unit = "TJ", E.dot = -5) |> 
  #   tibble::add_row(
  #     Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Flow.aggregation.point = "Transformation processes", 
  #     Flow = "Main activity producer heat plants", Product = IEATools::renewable_products$geothermal, Unit = "TJ", E.dot = -8)
    

  # First, test that by default nothing gets specified
  AB_data_specified_default <- AB_expanded %>%
    specify_all()

  AB_data_specified_default |> dplyr::filter(Flow %in% IEATools::grid_industries) |> nrow() |>
    testthat::expect_equal(0)


  # Second, test specification of electricity grid
  AB_data_prespecified <- AB_expanded %>% 
    dplyr::filter(Product != "Nuclear") |> #??
    specify_primary_production() |> 
    gather_producer_autoproducer() %>% 
    route_pumped_storage() %>% 
    split_oil_gas_extraction_eiou() %>% 
    route_own_use_elect_chp_heat() %>% 
    add_nuclear_industry()
  
  AB_data_specified_grid <- AB_data_prespecified |> 
    specify_electricity_grid(specify_grid = TRUE)
  
  # Testing this
  
})





