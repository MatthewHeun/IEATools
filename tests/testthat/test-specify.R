library(dplyr)
library(magrittr)

###########################################################
context("Specify flows")
###########################################################

test_that("production is converted to resources correctly", {
  Specific_production <- load_tidy_iea_df() %>% 
    specify_production_to_resources()
  # There should be no "Production" flows remaining.
  expect_false(Specific_production %>% 
                 magrittr::extract2("Flow") %>% 
                 magrittr::equals("Production") %>% 
                 any())
  # Now try with an EIOU flow of "Liquefaction (LNG) / regasification plants"
  # First, make a bogus data frame.
  DF <- tibble::tibble(
    Flow.aggregation.point = c("Energy industry own use"),
    Flow = c("Liquefaction (LNG) / regasification plants"), 
    Product = c("Natural gas"),
    E.dot = c(-42)
  ) %>% 
    specify_primary_production()
  # Expect that Flow has been reassigned.
  expect_equal(DF$Flow[[1]], "Oil and gas extraction")
})

test_that("renamed products are also consumed", {
  Specific_production <- load_tidy_iea_df() %>% 
    # Look at only 1 product to make things simpler
    dplyr::filter((startsWith(Product, "Hard coal") | Flow == "Coal mines"), Year == 1971)
  Renamed_primary <- Specific_production %>% 
    specify_primary_production()
  expect_equal(Renamed_primary %>% dplyr::filter(Flow == paste0("Resources", specify_notation$resources_open, "Coal", specify_notation$resources_close)) %>% nrow(), 1)
  expect_equal(Renamed_primary %>% dplyr::filter(Product == "Electricity") %>% nrow(), 1)
  expect_equal(Renamed_primary %>% dplyr::filter(Product == paste0("Hard coal (if no detail)", specify_notation$resources_open, "Coal mines", specify_notation$resources_close)) %>% nrow(), 18)
})

test_that("interface industries are correctly specified", {
  specified <- load_tidy_iea_df() %>% 
    specify_interface_industries()
  # We should have no more Imports, Exports, International aviation bunkers, International marine bunkers, or Stock changes.
  # Rather, everything should be specified as X (Product).
  for (i in interface_industries) {
    # Ensure that there are no interface_industries remaining
    expect_equal(nrow(specified %>% dplyr::filter(Flow == i)), 0)
    # Ensure that every interface_industry ends with ")", indicating that it has been specified.
    expect_true(specified %>% dplyr::filter(startsWith(Flow, i) & endsWith(Flow, specify_notation$interface_ind_close)) %>% nrow() > 0)
  }
})

test_that("eiou is replaced correctly", {
  Specific_production <- load_tidy_iea_df() %>% 
    specify_primary_production()
  Prod_coal_oilng <- Specific_production %>% 
    dplyr::filter(Flow == "Production" & starts_with_any_of(Product, coal_and_coal_products))
  # There should be no "Production" remaining, only "Resources (Coal)"
  expect_equal(nrow(Prod_coal_oilng), 0)
  Res_coal_oilng <- Specific_production %>% 
    dplyr::filter(startsWith(Flow, "Resources") & starts_with_any_of(Product, c(coal_and_coal_products, oil_and_oil_products, "Natural gas")))
  expect_equal(nrow(Res_coal_oilng), 6)
  expect_true(all(Res_coal_oilng$Flow.aggregation.point == "Total primary energy supply"))
  # There are none of these flows for Ghana (GHA)
  expect_true(all(Res_coal_oilng$Country == "ZAF"))
  # Check for new rows of Coal mines
  Mines <- Specific_production %>% 
    dplyr::filter(Flow == "Coal mines")
  expect_equal(nrow(Mines), 8)
  # Check that EIOU flows correctly remove the "(energy)" suffix.
  eiou <- Specific_production %>% 
    dplyr::filter(Flow.aggregation.point == "Energy industry own use") %>% 
    magrittr::extract2("Flow") %>% 
    unique()
  expect_false(eiou %>% endsWith("(energy)") %>% any())
  
  # Try a bogus data frame with an EIOU Flow of "Nuclear industry". 
  # Make sure it is converted to "Main activity producer electricity plants".
  unspecified <- data.frame(Country = c("HU", "HU"), 
                            Flow.aggregation.point = c("Energy industry own use", "Energy industry own use"),
                            Flow = c("Nuclear industry", "Nuclear industry"), 
                            E.dot = c(-10, -10),
                            stringsAsFactors = FALSE)
  specified <- unspecified %>% 
    specify_tp_eiou()
  expect_equal(specified$Flow, "Main activity producer electricity plants")
  expect_equal(specified$E.dot, -20)
})

test_that("specify_all works as expected", {
  Simple <- load_tidy_iea_df() %>% 
    specify_all()
  Complicated <- load_tidy_iea_df() %>% 
    specify_primary_production() %>% 
    specify_production_to_resources() %>% 
    specify_tp_eiou() %>% 
    specify_interface_industries() %>% 
    tp_sinks_to_nonenergy()
  expect_equal(Simple, Complicated)
})


test_that("despecify_col work as expected", {
  specified <- load_tidy_iea_df() %>% 
    specify_all()
  despecified <- specified %>% 
    despecify_col(col = iea_cols$flow, despecified_col = "clean_Flow")
  despecified %>% 
    dplyr::select(clean_Flow) %>% 
    unlist() %>% 
    endsWith(specify_notation$close) %>% 
    any() %>% 
    expect_false()
  despecified %>% 
    dplyr::select(clean_Flow) %>% 
    unlist() %>% 
    startsWith(tpes_flows$resources) %>% 
    any() %>% 
    expect_false()
})


###########################################################
context("Transformation sinks and sources")
###########################################################

test_that("tp_sinks_sources() works as expected", {
  # Check when type is neither "sinks" nor "sources"
  expect_error(load_tidy_iea_df() %>% 
                 specify_all() %>% 
                 tp_sinks_sources(type = "bogus"), 
               msg = "'arg' should be one of ")
  # Try to send an ungrouped data frame into the function. Should give 0 rows.
  expect_equal(load_tidy_iea_df() %>% 
                 specify_primary_production() %>% 
                 specify_production_to_resources() %>% 
                 specify_tp_eiou() %>% 
                 specify_interface_industries() %>% 
                 tp_sinks_sources() %>%
                 nrow(), 
               0)
  # Try with the built-in data set in which there are no transformation sinks.
  sink_industries <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    tp_sinks_sources()
  expect_equal(nrow(sink_industries), 0)
  # Try with a simple, made-up data set
  Tidy <- data.frame(Flow.aggregation.point = c("Transformation processes", "Transformation processes", "Transformation processes"), 
                     Flow = c("Automobiles", "Automobiles", "Furnaces"),
                     E.dot = c(-1, 1, -2), 
                     stringsAsFactors = FALSE) %>% 
    dplyr::mutate(
      Country = "Bogus",
      Product = "Petrol"
    )
  # Automobiles are fine, but Furnaces don't make anything and are, therefore, a transformation sink.
  # expect_equal(Tidy %>% tp_sinks_sources(grouping_vars = "Country"), 
  expect_equal(Tidy %>% tp_sinks_sources(), 
               tibble::tibble(Country = "Bogus", Flow = "Furnaces"))
})

test_that('tp_sinks_sources(type = "sources") works as expected', {
  # Try to send an ungrouped data frame into the function. Should give 0 rows.
  expect_equal(load_tidy_iea_df() %>% 
                 specify_all() %>% 
                 tp_sinks_sources(type = "sources") %>% 
                 nrow(), 0)
  # Try with the built-in data set in which there are no transformation sources.
  source_industries <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    tp_sinks_sources(type = "sources")
  expect_equal(nrow(source_industries), 0)
  # Try with a simple, made-up data set
  Tidy <- data.frame(Flow.aggregation.point = c("Transformation processes", "Transformation processes", "Transformation processes"), 
                     Flow = c("Automobiles", "Automobiles", "Furnaces"),
                     E.dot = c(-1, 1, 2), 
                     stringsAsFactors = FALSE) %>% 
    dplyr::mutate(
      Country = "Bogus",
      Product = "Petrol"
    )
  # Automobiles are fine, but Furnaces make Petrol without consuming any energy, and are, therefore, a transformation source.
  expect_equal(Tidy %>% tp_sinks_sources(type = "sources"), 
               tibble::tibble(Country = "Bogus", Flow = "Furnaces"))
})

test_that("tp_sinks_to_nonenergy works as expected", {
  # Make a simple data frame to test this function.
  Tidy <- data.frame(
    Ledger.side = c("Supply", "Supply", "Supply", "Consumption"),
    Flow.aggregation.point = c("Transformation processes", "Transformation processes", "Transformation processes", "Non-energy use"), 
    Flow = c("Automobiles", "Automobiles", "Furnaces", "Non-energy use industry/transformation/energy"),
    Product = c("Petrol", "MD", "Coal", "Coal"),
    E.dot = c(-1, 1, -2, 8), 
    stringsAsFactors = FALSE) %>% 
    dplyr::mutate(
      Method = "PCM", 
      Last.stage = "Final",
      Energy.type = "E",
      Country = "Bogus",
      Year = 1971
    )
  Result <- Tidy %>% 
    tp_sinks_to_nonenergy()
  # We expect that the original 4 rows are now down to 3.
  expect_equal(nrow(Result), 3)
  # Check that the sink energy was correctly added to existing Non-energy use.
  expect_equal(Result %>% dplyr::filter(Flow.aggregation.point == "Non-energy use") %>% extract2("E.dot"), 10)
  # Check that the original rows are unchanged
  expect_equal(Result %>% dplyr::filter(Flow == "Automobiles", Product == "Petrol") %>% extract2("E.dot"), -1)
  expect_equal(Result %>% dplyr::filter(Flow == "Automobiles", Product == "MD") %>% extract2("E.dot"), 1)
})

test_that("spreading by years works as expected at each step of specify_all()", {
  # It should be possible to spread by years after any of these function calls.
  # If we can't do so, it means there are duplicated rows
  # in the data frame.
  Tidy <- load_tidy_iea_df()
  Year_spread_1 <- Tidy %>% 
    specify_primary_production() %>% 
    tidyr::spread(key = Year, value = E.dot)
  expect_true("1971" %in% names(Year_spread_1))
  expect_true("2000" %in% names(Year_spread_1))
  
  Year_spread_2 <- Tidy %>% 
    specify_primary_production() %>% 
    specify_production_to_resources() %>% 
    tidyr::spread(key = Year, value = E.dot)
  expect_true("1971" %in% names(Year_spread_2))
  expect_true("2000" %in% names(Year_spread_2))
  
  Year_spread_3 <- Tidy %>% 
    specify_primary_production() %>% 
    specify_production_to_resources() %>% 
    specify_tp_eiou() %>% 
    tidyr::spread(key = Year, value = E.dot)
  expect_true("1971" %in% names(Year_spread_3))
  expect_true("2000" %in% names(Year_spread_3))
  
  Year_spread_4 <- Tidy %>% 
    specify_primary_production() %>% 
    specify_production_to_resources() %>% 
    specify_tp_eiou() %>% 
    specify_interface_industries() %>% 
    tidyr::spread(key = Year, value = E.dot)
  expect_true("1971" %in% names(Year_spread_4))
  expect_true("2000" %in% names(Year_spread_4))
  
  Year_spread_5 <- Tidy %>% 
    specify_primary_production() %>% 
    specify_production_to_resources() %>% 
    specify_tp_eiou() %>% 
    specify_interface_industries() %>% 
    tp_sinks_to_nonenergy() %>% 
    tidyr::spread(key = Year, value = E.dot)
  expect_true("1971" %in% names(Year_spread_5))
  expect_true("2000" %in% names(Year_spread_5))
})

