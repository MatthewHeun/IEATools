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
                 extract2("Flow") %>% 
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

test_that("interface industries are correctly specified", {
  specified <- load_tidy_iea_df() %>% 
    specify_interface_industries()
  # We should have no more Imports, Exports, International aviation bunkers, International marine bunkers, or Stock changes.
  # Rather, everything should be specified as X (Product).
  for (i in interface_industries) {
    # Ensure that there are no interface_industries remaining
    expect_equal(nrow(specified %>% filter(Flow == i)), 0)
    # Ensure that every interface_industry ends with ")", indicating that it has been specified.
    expect_true(specified %>% filter(startsWith(Flow, i) & endsWith(Flow, ")")) %>% nrow() > 0)
  }
})

test_that("eiou is replaced correctly", {
  Specific_production <- load_tidy_iea_df() %>% 
    specify_primary_production()
  Prod_coal_oilng <- Specific_production %>% 
    filter(Flow == "Production" & Product %in% coal_and_coal_products)
  expect_equal(nrow(Prod_coal_oilng), 0)
  Res_coal_oilng <- Specific_production %>% 
    filter(startsWith(Flow, "Resources") & Product %in% c(coal_and_coal_products, oil_and_oil_products, "Natural gas"))
  expect_equal(nrow(Res_coal_oilng), 6)
  expect_true(all(Res_coal_oilng$Flow.aggregation.point == "Total primary energy supply"))
  # There are none of these flows for Ghana (GHA)
  expect_true(all(Res_coal_oilng$Country == "ZAF"))
  # Check for new rows of Coal mines
  Mines <- Specific_production %>% 
    filter(Flow == "Coal mines")
  expect_equal(nrow(Mines), 8)
  # Check that EIOU flows correctly remove the "(energy)" suffix.
  eiou <- Specific_production %>% 
    filter(Flow.aggregation.point == "Energy industry own use") %>% 
    extract2("Flow") %>% 
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

###########################################################
context("Transformation sinks and sources")
###########################################################

test_that("tp_sinks_sources() works as expected", {
  # Check when type is neither "sinks" nor "sources"
  expect_error(load_tidy_iea_df() %>% 
                 specify_all() %>% 
                 tp_sinks_sources(type = "bogus"))
  # Try to send an ungrouped data frame into the function. Should give 0 rows.
  expect_equal(load_tidy_iea_df() %>% 
                 specify_primary_production() %>% 
                 specify_production_to_resources() %>% 
                 specify_tp_eiou() %>% 
                 specify_interface_industries() %>% 
                 tp_sinks_sources(grouping_vars = NULL) %>% 
                 nrow(), 
               0)
  # Try to group on Flow.aggregation.point. Should fail.
  expect_error(load_tidy_iea_df() %>% 
                 specify_all() %>% 
                 tp_sinks_sources(grouping_vars = "Flow.aggregation.point"), "Flow.aggregation.point cannot be a grouping variable of .tidy_iea_df in tp_sinks_sources()")
  # Try to group on Flow. Should fail.
  expect_error(load_tidy_iea_df() %>% 
                 specify_all() %>% 
                 tp_sinks_sources(grouping_vars = "Flow"), "Flow cannot be a grouping variable of .tidy_iea_df in tp_sinks_sources()")
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
  expect_equal(Tidy %>% tp_sinks_sources(grouping_vars = "Country"), 
               data.frame(Country = "Bogus", Flow = "Furnaces", stringsAsFactors = FALSE))
})

test_that('tp_sinks_sources(type = "sources") works as expected', {
  # Try to send an ungrouped data frame into the function. Should give 0 rows.
  expect_equal(load_tidy_iea_df() %>% 
                 specify_all() %>% 
                 tp_sinks_sources(type = "sources", grouping_vars = NULL) %>% 
                 nrow(), 0)
  # Try to group on Flow.aggregation.point. Should fail.
  expect_error(load_tidy_iea_df() %>% 
                 specify_all() %>% 
                 tp_sinks_sources(type = "sources", grouping_vars = "Flow.aggregation.point"), "Flow.aggregation.point cannot be a grouping variable of .tidy_iea_df in tp_sinks_sources()")
  # Try to group on Flow. Should fail.
  expect_error(load_tidy_iea_df() %>% 
                 specify_all() %>% 
                 tp_sinks_sources(type = "sources", grouping_vars = "Flow"), "Flow cannot be a grouping variable of .tidy_iea_df in tp_sinks_sources()")
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
  expect_equal(Tidy %>% tp_sinks_sources(type = "sources", grouping_vars = "Country"), 
               data.frame(Country = "Bogus", Flow = "Furnaces", stringsAsFactors = FALSE))
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
  expect_equal(Result %>% filter(Flow.aggregation.point == "Non-energy use") %>% extract2("E.dot"), 10)
  # Check that the original rows are unchanged
  expect_equal(Result %>% filter(Flow == "Automobiles", Product == "Petrol") %>% extract2("E.dot"), -1)
  expect_equal(Result %>% filter(Flow == "Automobiles", Product == "MD") %>% extract2("E.dot"), 1)
})