library(dplyr)
library(magrittr)

###########################################################
context("Specify EIOU")
###########################################################

test_that("specify_tp_eiou works as expected for Own use in electricity, CHP and heat plants", {
  # Make a bogus data frame
  EIOU <- data.frame(Country = c("US", "US"), 
                     Method = c("PCM", "PCM"),
                     Energy.type = c("E", "E"),
                     Last.stage = c("Final", "Final"),
                     Year = c(2000, 2000),
                     Flow.aggregation.point = c("Energy industry own use", "nothing"),
                     Flow = c("Own use in electricity, CHP and heat plants", "Own use in electricity, CHP and heat plants"), 
                     Ledger.side = c("Supply", "Supply"),
                     Unit = c("ktoe", "ktoe"),
                     Product = c("Brown coal", "Crude oil"),
                     E.dot = c(-10, -10),
                     stringsAsFactors = FALSE)
  
  EIOU_fixed <- specify_tp_eiou(EIOU)
  # The first row is expected to change, because its Product is "Electricity"
  expect_equal(EIOU_fixed$Flow[[1]], "Main activity producer electricity plants")
  # The second row will not change, because its Product is "Nothing"
  expect_equal(EIOU_fixed$Flow[[2]], "Own use in electricity, CHP and heat plants")
})

test_that("specify_tp_eiou works as expected for pumped storage plants", {
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
                     Unit = c("ktoe", "ktoe"),
                     E.dot = c(-20, -20),
                     stringsAsFactors = FALSE)
  EIOU_fixed <- specify_tp_eiou(EIOU)
  # The first row is expected to change, because its Product is "Electricity"
  expect_equal(EIOU_fixed$Flow[[1]], "Main activity producer electricity plants")
  # The second row will not change, because its Product is "Nothing"
  expect_equal(EIOU_fixed$Flow[[2]], "Pumped storage plants")
})


test_that("specify_tp_eiou works for sample data", {
  # This test is failing, because the (energy) suffix is still present in the Flow for Own use in electricity, CHP and heat plants
  # and the function assumes it has been stripped away. 
  # Solution: strip away "(energy)" and "(transf.)" during processing of these data.
  # Also, Flow.aggregation.point is not found. Need to do more to the data frame before calling specify_tp_eiou().
  specified <- load_tidy_iea_df() %>% 
    specify_tp_eiou() %>% 
    filter(Flow.aggregation.point == "Energy industry own use" & 
             Flow == "Main activity producer electricity plants")
  expect_equal(nrow(specified), 4)
})



# Testing the function that gathers "Main activity producer" and "Autoproducer" flows/industries.
test_that("gather_producer_autoproducer works", {
  
  # First, with AB data
  A_B_path <- system.file("A_B_data_full_2018_format_testing.csv", package = "IEATools")
  
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
                   Product == "Brown coal (if no detail) [of Coal mines]"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -90)
  
  expect_equal(main_activity_flows %>%
                 dplyr::filter(
                   Country == "A",
                   Flow.aggregation.point == "Transformation processes",
                   Flow == "Main activity producer CHP plants",
                   Product == "Hard coal (if no detail) [of Coal mines]"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               160)
  
  expect_equal(main_activity_flows %>%
                 dplyr::filter(
                   Country == "A",
                   Flow.aggregation.point == "Transformation processes",
                   Flow == "Main activity producer electricity plants",
                   Product == "Hard coal (if no detail) [of Coal mines]"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               100)
  
  expect_equal(main_activity_flows %>%
                 dplyr::filter(
                   Country == "B",
                   Flow.aggregation.point == "Transformation processes",
                   Flow == "Main activity producer heat plants",
                   Product == "Brown coal (if no detail) [of Coal mines]"
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
    2)
})



# Testing the function that routes the "Pumped storage" EIOU flow to "Main activity producer electricity plants."
test_that("route_pumped_storage works", {
  
  # First with AB data
  A_B_path <- system.file("A_B_data_full_2018_format_testing.csv", package = "IEATools")
  
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
  
  expect_equal(
    res %>% 
      dplyr::filter(Flow == "Pumped storage plants") %>% nrow(),
    0)
    
})



# Testing the function that splits the "Own use in electricity, CHP and heat plants" EIOU flow into the three main producer activities (electricity, CHP and heat)
test_that("route_own_use_elect_chp_heat works", {
  
  A_B_path <- system.file("A_B_data_full_2018_format_testing.csv", package = "IEATools")
  
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
                   Product == "Anthracite [of Coal mines]") %>%
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
                   Product == "Hard coal (if no detail) [of Coal mines]") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -947.1481,
               tolerance = 0.0001)
  
  expect_equal(main_activity_eiou %>%
                 dplyr::filter(
                   Country == "A",
                   Flow.aggregation.point == "Energy industry own use",
                   Flow == "Main activity producer heat plants",
                   Product == "Hard coal (if no detail) [of Coal mines]") %>%
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
  
  # here add more...
  
  
  
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
test_that("add_nuclear_industry works", {
  
  # First with AB data
  A_B_path <- system.file("A_B_data_full_2018_format_testing.csv", package = "IEATools")
  
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
                    Unit = "ktoe",
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
    expect_equal(3)
})





test_that("route_non_specified_eiou works", {
  
  # First AB data
  A_B_path <- system.file("A_B_data_full_2018_format_testing.csv", package = "IEATools")
  
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
                               Product == "Hard coal (if no detail) [of Coal mines]") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -3.3299,
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(Country == "A",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Coal mines",
                               Product == "Hard coal (if no detail) [of Coal mines]") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -4.7571,
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(Country == "A",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Main activity producer electricity plants",
                               Product == "Hard coal (if no detail) [of Coal mines]") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               (-947.148148 - 104.2088),
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(Country == "A",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Main activity producer CHP plants",
                               Product == "Hard coal (if no detail) [of Coal mines]") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               (-40.740741 - 4.7621),
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(Country == "A",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Oil refineries",
                               Product == "Hard coal (if no detail) [of Coal mines]") %>%
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
                               Product == "Hard coal (if no detail) [of Coal mines]") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -23.9795,
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(Country == "B",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Blast furnaces",
                               Product == "Brown coal (if no detail) [of Coal mines]") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -24.0816,
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(Country == "B",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Coke ovens",
                               Product == "Hard coal (if no detail) [of Coal mines]") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -53.95407,
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(Country == "B",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Main activity producer electricity plants",
                               Product == "Hard coal (if no detail) [of Coal mines]") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               (-50.4712 - 259.846827),
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(Country == "B",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Main activity producer heat plants",
                               Product == "Hard coal (if no detail) [of Coal mines]") %>%
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


test_that("route_non_specified_tp works", {
  
  A_B_path <- system.file("A_B_data_full_2018_format_testing.csv", package = "IEATools")
  
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
      Product = "Brown coal (if no detail) [of Coal mines]",
      Unit = "ktoe",
      E.dot = 67
    ) %>%
    route_non_specified_tp()
  
  
  # Country A, Hard coal:
  expect_equal(test %>%
                 dplyr::filter(
                   Country == "A",
                   Flow.aggregation.point == "Transformation processes",
                   Flow == "Main activity producer CHP plants",
                   Product == "Hard coal (if no detail) [of Coal mines]"
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
                   Product == "Hard coal (if no detail) [of Coal mines]"
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
                   Product == "Hard coal (if no detail) [of Coal mines]"
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
                   Product == "Hard coal (if no detail) [of Coal mines]"
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
                   Product == "Hard coal (if no detail) [of Coal mines]"
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
                   Product == "Brown coal (if no detail) [of Coal mines]"
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
                   Product == "Brown coal (if no detail) [of Coal mines]"
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
                   Product == "Brown coal (if no detail) [of Coal mines]"
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
                   Product == "Brown coal (if no detail) [of Coal mines]"
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
                   Product == "Brown coal (if no detail) [of Coal mines]"
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
                   Product == "Brown coal (if no detail) [of Coal mines]"
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
                   Product == "Brown coal (if no detail) [of Coal mines]"
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



test_that("route_non_specified_flows works", {
  
  A_B_path <- system.file("A_B_data_full_2018_format_testing.csv", package = "IEATools")
  
  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df()
  
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
      Product = "Brown coal (if no detail) [of Coal mines]",
      Unit = "ktoe",
      E.dot = 67
    ) %>%
    route_non_specified_flows()
  
  
  # Country A
  expect_equal(test %>%
                 dplyr::filter(Country == "A",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Blast furnaces",
                               Product == "Hard coal (if no detail) [of Coal mines]") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -3.3299,
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(Country == "A",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Coal mines",
                               Product == "Hard coal (if no detail) [of Coal mines]") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -4.7571,
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(Country == "A",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Main activity producer electricity plants",
                               Product == "Hard coal (if no detail) [of Coal mines]") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               (-947.148148 - 104.2088),
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(Country == "A",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Main activity producer CHP plants",
                               Product == "Hard coal (if no detail) [of Coal mines]") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               (-40.740741 - 4.7621),
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(Country == "A",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Oil refineries",
                               Product == "Hard coal (if no detail) [of Coal mines]") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -28.54263,
               tolerance = 0.001)
  
  
  # Country B
  expect_equal(test %>%
                 dplyr::filter(Country == "B",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Blast furnaces",
                               Product == "Hard coal (if no detail) [of Coal mines]") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -23.9795,
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(Country == "B",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Blast furnaces",
                               Product == "Brown coal (if no detail) [of Coal mines]") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -24.0816,
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(Country == "B",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Coke ovens",
                               Product == "Hard coal (if no detail) [of Coal mines]") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -53.95407,
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(Country == "B",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Main activity producer electricity plants",
                               Product == "Hard coal (if no detail) [of Coal mines]") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               (-50.4712 - 259.846827),
               tolerance = 0.001)
  
  expect_equal(test %>%
                 dplyr::filter(Country == "B",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Main activity producer heat plants",
                               Product == "Hard coal (if no detail) [of Coal mines]") %>%
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
                   Product == "Hard coal (if no detail) [of Coal mines]"
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
                   Product == "Hard coal (if no detail) [of Coal mines]"
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
                   Product == "Hard coal (if no detail) [of Coal mines]"
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
                   Product == "Hard coal (if no detail) [of Coal mines]"
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
                   Product == "Hard coal (if no detail) [of Coal mines]"
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
                   Product == "Brown coal (if no detail) [of Coal mines]"
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
                   Product == "Brown coal (if no detail) [of Coal mines]"
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
                   Product == "Brown coal (if no detail) [of Coal mines]"
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
                   Product == "Brown coal (if no detail) [of Coal mines]"
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
                   Product == "Brown coal (if no detail) [of Coal mines]"
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
                   Product == "Brown coal (if no detail) [of Coal mines]"
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
                   Product == "Brown coal (if no detail) [of Coal mines]"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               (623-620.6897+67),
               tolerance = 0.001)
  
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
  A_B_path <- system.file("A_B_data_full_2018_format_testing.csv", package = "IEATools")
  
  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df()
  
  AB_data %>% 
    tidy_iea_df_balanced()
  
  AB_data_specified <- AB_data %>% 
    specify_all()
  
  expect_true(AB_data_specified %>% 
                tidy_iea_df_balanced())
})
