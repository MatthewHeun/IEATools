
test_that("production is converted to resources correctly", {
  Specific_production <- load_tidy_iea_df() %>% 
    specify_production_to_resources()
  # There should be no "Production" flows remaining.
  expect_false(Specific_production %>% 
                 magrittr::extract2("Flow") %>% 
                 magrittr::equals("Production") %>% 
                 any())
  
  Specific_production %>% 
    magrittr::extract2("Flow") %>% 
    dplyr::first() %>% 
    expect_equal("Resources [from Primary solid biofuels]")
  
  # Now try with an EIOU flow of "Liquefaction (LNG) / regasification plants"
  # First, make a bogus data frame.
  DF <- tibble::tibble(
    FlowAggregationPoint = c("Energy industry own use"),
    Flow = c("Liquefaction (LNG) / regasification plants"), 
    Product = c("Natural gas"),
    E.dot = c(-42)
  ) %>% 
    specify_primary_production()
  # Expect that Flow has been reassigned.
  expect_equal(DF$Flow[[1]], "Natural gas extraction")
})


test_that("renamed products are also consumed", {
  Specific_production <- load_tidy_iea_df() %>% 
    # Look at only 1 product to make things simpler
    dplyr::filter((startsWith(Product, "Hard coal") | Flow == "Coal mines"), Year == 1971)
  
  Renamed_primary <- Specific_production %>% 
    specify_primary_production()
  
  expect_equal(Renamed_primary %>% 
                 dplyr::filter(Flow == RCLabels::paste_pref_suff(pref = "Resources", suff = "Hard coal (if no detail)", notation = RCLabels::of_notation)) %>% 
                 nrow(),
               1)
  
  expect_equal(Renamed_primary %>% dplyr::filter(Product == "Electricity") %>% nrow(), 1)
  
  expect_equal(Renamed_primary %>% 
                 dplyr::filter(
                   Product == RCLabels::paste_pref_suff(pref = "Hard coal (if no detail)", suff = "Resources", notation = RCLabels::from_notation)
                   ) %>% 
                 nrow(), 
               2)
})


test_that("interface industries are correctly specified", {
  
  int_inds_wout_bunker_exports <- setdiff(interface_industries, c(IEATools::interface_industries$exports_to_world_aviation_bunkers, 
                                                                  IEATools::interface_industries$exports_to_world_marine_bunkers))

    specified <- load_tidy_iea_df() %>% 
    specify_interface_industries()
  # We should have no more Imports, Exports, International aviation bunkers, International marine bunkers, or Stock changes.
  # Rather, everything should be specified as X (Product).
  for (i in int_inds_wout_bunker_exports) {
    # Ensure that there are no interface_industries remaining
    expect_equal(nrow(specified %>% dplyr::filter(Flow == i)), 0)
    # Ensure that every interface_industry ends with "]", indicating that it has been specified.
    expect_true(specified %>% dplyr::filter(startsWith(Flow, i) & endsWith(Flow, RCLabels::of_notation[["suff_end"]])) %>% nrow() > 0)
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
  expect_true(all(Res_coal_oilng$FlowAggregationPoint == "Total primary energy supply"))
  # There are none of these flows for Ghana (GHA)
  expect_true(all(Res_coal_oilng$Country == "ZAF"))
  # Check for new rows of Coal mines
  Mines <- Specific_production %>% 
    dplyr::filter(Flow == "Coal mines")
  expect_equal(nrow(Mines), 8)
  # Check that EIOU flows correctly remove the "(energy)" suffix.
  eiou <- Specific_production %>% 
    dplyr::filter(FlowAggregationPoint == "Energy industry own use") %>% 
    magrittr::extract2("Flow") %>% 
    unique()
  expect_false(eiou %>% endsWith("(energy)") %>% any())
})


test_that("specify_all works as expected", {
  Simple <- load_tidy_iea_df() %>% 
    specify_all()
  Complicated <- load_tidy_iea_df() %>% 
    specify_primary_production() %>% 
    specify_production_to_resources() %>% 
    specify_tp_eiou() %>% 
    specify_bunkers() %>%
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
    endsWith(RCLabels::of_notation[["suff_end"]]) %>% 
    any() %>% 
    expect_false()
  despecified %>% 
    dplyr::select(clean_Flow) %>% 
    unlist() %>% 
    startsWith(tpes_flows$resources) %>% 
    any() %>% 
    expect_false()
})


test_that("tp_sinks_sources() works as expected", {
  # Check when type is neither "sinks" nor "sources"
  load_tidy_iea_df() %>% 
    specify_all() %>% 
    tp_sinks_sources(type = "bogus") |> 
    expect_error()
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
  Tidy <- data.frame(FlowAggregationPoint = c("Transformation processes", "Transformation processes", "Transformation processes"), 
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
  Tidy <- data.frame(FlowAggregationPoint = c("Transformation processes", "Transformation processes", "Transformation processes"), 
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
    LedgerSide = c("Supply", "Supply", "Supply", "Consumption"),
    FlowAggregationPoint = c("Transformation processes", "Transformation processes", "Transformation processes", "Non-energy use"), 
    Flow = c("Automobiles", "Automobiles", "Furnaces", "Non-energy use industry/transformation/energy"),
    Product = c("Petrol", "MD", "Coal", "Coal"),
    E.dot = c(-1, 1, -2, 8), 
    stringsAsFactors = FALSE) %>% 
    dplyr::mutate(
      Method = "PCM", 
      LastStage = "Final",
      EnergyType = "E",
      Country = "Bogus",
      Year = 1971
    )
  Result <- Tidy %>% 
    tp_sinks_to_nonenergy()
  # We expect that the original 4 rows are now down to 3.
  expect_equal(nrow(Result), 3)
  # Check that the sink energy was correctly added to existing Non-energy use.
  expect_equal(Result %>% dplyr::filter(FlowAggregationPoint == "Non-energy use") %>% magrittr::extract2("E.dot"), 10)
  # Check that the original rows are unchanged
  expect_equal(Result %>% dplyr::filter(Flow == "Automobiles", Product == "Petrol") %>% magrittr::extract2("E.dot"), -1)
  expect_equal(Result %>% dplyr::filter(Flow == "Automobiles", Product == "MD") %>% magrittr::extract2("E.dot"), 1)
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


test_that("remove_suffix_specifications() works as expected", {
  cleaned <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    remove_suffix_specifications(col = "Flow", unsuffixed_col = "clean_Flow") %>% 
    dplyr::select(Flow, Product, E.dot, clean_Flow) %>% 
    dplyr::filter(endsWith(Flow, RCLabels::bracket_notation[["suff_end"]]))
  
  tested <- cleaned %>% 
    dplyr::mutate(
      ok = dplyr::case_when(
        endsWith(Flow, RCLabels::bracket_notation[["suff_end"]]) & 
          ! endsWith(clean_Flow, RCLabels::bracket_notation[["suff_end"]]) ~ TRUE, 
      TRUE ~ FALSE
      )
    )
  expect_true(all(tested$ok))
  
  # Try with column replacement
  cleaned_2 <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    remove_suffix_specifications(col = "Flow", unsuffixed_col = "Flow") %>% 
    dplyr::select(Flow, Product, E.dot) %>%
    dplyr::filter(endsWith(Flow, RCLabels::bracket_notation[["suff_end"]])) %>%
    nrow() %>%
    # We should have no rows remaining that end with the bracket notation suffix.
    expect_equal(0)
})


test_that("new tests for specify_interface_industries() work as expected", {
  
  # First, check that specification specifies Resources and Manufacture flows
  tidy_GHA_ZAF_df <- load_tidy_iea_df() %>%
    specify_all()
  
  res <- tidy_GHA_ZAF_df %>%
    remove_suffix_specifications(col = IEATools::iea_cols$flow, unsuffixed_col = IEATools::iea_cols$flow) %>%
    specify_interface_industries()
  
  res %>% 
    dplyr::filter(Flow == "Manufacture") %>% 
    nrow() %>% 
    expect_equal(0)
  
  res %>% 
    dplyr::filter(Flow == "Manufacture [of Hydro]") %>% 
    nrow() %>% 
    expect_equal(8)
  
  res %>% 
    dplyr::filter(Flow == "Resources") %>% 
    nrow() %>% 
    expect_equal(0)
  
  res %>% 
    dplyr::filter(Flow == "Resources [of Hydro]") %>% 
    nrow() %>% 
    expect_equal(4)
  
  # Second, check that Industry not elsewhere specified does not 1) get specified, and 2) end up as part of the supply
  
  # Checking it does not get specified
  res %>% 
    dplyr::filter(stringr::str_detect(Flow, "Industry not elsewhere specified")) %>% 
    dplyr::filter(Flow != "Industry not elsewhere specified") %>% 
    nrow() %>% 
    expect_equal(0)
  
  # Checking it does not end up as part of the supply
  res %>% 
    dplyr::filter(LedgerSide == "Supply" & stringr::str_detect(Flow, "Industry not elsewhere specified")) %>% 
    nrow() %>% 
    expect_equal(0)
  
  res %>% 
    dplyr::filter(FlowAggregationPoint == "Total primary energy supply" & stringr::str_detect(Flow, "Industry not elsewhere specified")) %>% 
    nrow() %>% 
    expect_equal(0)
})


test_that("specify_all() avoids truncating at '.'", {
  tidy_GHA_ZAF_df <- load_tidy_iea_df() %>%
    specify_all()

  # On 16 Mar 2023, this test was returning 0 rows, because
  # specify_all() was cutting off at the ".", yielding 
  # "Stock changes [of Gas/diesel oil excl]".
  # This test verifies that the problem was fixed.
  tidy_GHA_ZAF_df |> 
    dplyr::filter(.data[[IEATools::iea_cols$flow]] == "Stock changes [of Gas/diesel oil excl. biofuels]") |> 
    nrow() |> 
    expect_equal(1)

  # Also check Imports
  tidy_GHA_ZAF_df |> 
    dplyr::filter(.data[[IEATools::iea_cols$flow]] == "Imports [of Gas/diesel oil excl. biofuels]") |> 
    nrow() |> 
    expect_equal(2)

  # Also check Exports
  tidy_GHA_ZAF_df |> 
    dplyr::filter(.data[[IEATools::iea_cols$flow]] == "Exports [of Gas/diesel oil excl. biofuels]") |> 
    nrow() |> 
    expect_equal(2)
})
