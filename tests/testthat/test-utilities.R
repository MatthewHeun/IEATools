
test_that("starts_with_any_of() works properly", {
  expect_true(starts_with_any_of(x = "prefix - suffix", target = c("a", "b", "prefix")))
  expect_false(starts_with_any_of(x = "prefix - suffix", target = c("a", "b", "c")))
  expect_false(starts_with_any_of(x = "prefix - suffix", target = "suffix"))
  expect_equal(starts_with_any_of(x = c("Production - Crude", "Production - NG", "Exports - Oil", "Exports - Crude"),
                                  target = c("Production", "Imports")),
               c(TRUE, TRUE, FALSE, FALSE))
  # Does it also work when x is a list?
  expect_equal(starts_with_any_of(x = list("Production - Crude", "Production - NG", "Exports - Oil", "Exports - Crude"),
                                  target = c("Production", "Imports")),
               c(TRUE, TRUE, FALSE, FALSE))
  # Does it work when target is also a list? 
  expect_equal(starts_with_any_of(x = list("Production - Crude", "Production - NG", "Exports - Oil", "Exports - Crude"),
                                  target = list("Production", "Imports")),
               c(TRUE, TRUE, FALSE, FALSE))
})


test_that("year_cols() works as expected", {
  DF <- data.frame(a = c(1, 2), `1967` = c(3, 4), `-42` = c(5, 6), check.names = FALSE)
  expect_equal(DF %>% year_cols(), c(2, 3))
  expect_equal(DF %>% year_cols(return_names = TRUE), c("1967", "-42"))
  
  DF2 <- data.frame(a = c(1, 2), Year = c(1967, 2020))
  expect_equal(year_cols(DF2, return_names = TRUE), "Year")
  expect_equal(year_cols(DF2, year = NULL, return_names = TRUE), c("a")[-1])
  expect_equal(year_cols(DF2), 2)
  expect_equal(year_cols(DF2, year = 2), 2)
  expect_equal(year_cols(DF2, year = 1), 1)
  # You can shoot yourself in the foot
  expect_equal(year_cols(DF2, year = 1, return_names = TRUE), "a")
})


test_that("meta_cols() works as expected", {
  DF <- data.frame(E.dot = -42, a = c(1, 2), `1967` = c(3, 4), `-42` = c(5, 6), check.names = FALSE)
  expect_equal(meta_cols(DF, return_names = TRUE), "a")  # Because E.dot is excluded by default.
  
  expect_equal(meta_cols(DF), 2)
  expect_equal(meta_cols(DF, not_meta = "a"), 1)
  expect_equal(meta_cols(DF, not_meta = 2), 1)
  expect_equal(meta_cols(DF, not_meta = NULL), c(1, 2))
  expect_equal(meta_cols(DF, not_meta = "E.dot"), 2)
  expect_equal(meta_cols(DF, not_meta = "E.dot", return_names = TRUE), "a")
  expect_equal(meta_cols(DF, not_meta = "a"), 1)
  expect_equal(meta_cols(DF, not_meta = c("E.dot", "a")), c(0)[-1]) # Returns an empty vector
  expect_equal(meta_cols(DF, not_meta = c(1,2)), c(0)[-1]) # Returns an empty vector
  expect_equal(meta_cols(DF, not_meta = c(1,2), return_names = TRUE), c("bogus")[-1]) # Returns an empty vector
  
  expect_equal(meta_cols(DF, years_to_keep = 1967), c(2, 3))
  expect_equal(meta_cols(DF, years_to_keep = 1967, return_names = TRUE), c("a", "1967"))

  expect_equal(meta_cols(DF, not_meta = "a", years_to_keep = 1967, return_names = TRUE), c("E.dot", "1967"))
})


test_that("insert_after() works as expected", {
  l <- list("a", "b", "c", "d", "c")
  expect_equal(insert_after(l, after = "c", values = "1"), list("a", "b", "c", "1", "d", "c", "1"))
  expect_equal(l %>% insert_after(after = "c", values = "1"), list("a", "b", "c", "1", "d", "c", "1"))
  # Try with some special cases
  expect_equal(l %>% insert_after(after = "c", values = "1", .after_all = FALSE), list("a", "b", "c", "1", "d", "c"))
  expect_equal(l %>% insert_after(after = "c", values = "1", .equals_function = magrittr::equals), list("a", "b", "c", "1", "d", "c", "1"))
  expect_equal(l %>% insert_after(after = NULL, "1"), list("a", "b", "c", "d", "c", "1"))
  expect_equal(l %>% insert_after(values = "1"), list("a", "b", "c", "d", "c", "1"))
})


test_that("extract_TK() works as expected", {
  heats1 <- c("HTH.600.C", "MTH.200.C", "MTH.100.C", "LTH.20.C", "LTH.-20.C")
  expect_equal(extract_TK(heats1), c(600, 200, 100, 20, -20) + 273.15)
  
  expect_equal(extract_TK("MTH.500.K"), 500)
  expect_true(is.na(extract_TK("LMH.20.C")))
  expect_equal(extract_TK(c("MMH.20.C", "HTH.600.C")), c(NA_real_, 600 + 273.15))
  expect_true(is.na(extract_TK("HTH.600.P"))) # unknown unit
  expect_true(is.na(extract_TK("MTH.100.J")))
  expect_true(is.na(extract_TK("MTH.XXX.K"))) # bogus temperature numbers
  expect_true(is.na(extract_TK("MTH.ccc.C")))
  
  expect_true(is.na(extract_TK("")))
  expect_true(is.na(extract_TK("HTH")))
  
  expect_equal(extract_TK("HTH.600.F"), 588.70555556)
  expect_equal(extract_TK("STH.600.F"), 588.70555556)
  expect_equal(extract_TK("6TH.600.F"), 588.70555556)
  expect_equal(extract_TK("$TH.600.F"), 588.70555556)
  expect_equal(extract_TK("LTH.-104.75.F"), 197.177777778)
  expect_equal(extract_TK("LTH.55.R"), 30.555555555)
  expect_equal(extract_TK("LTH.-79.2.C"), 193.95)
  expect_equal(extract_TK("LTH.1089.15.K"), 1089.15)
  expect_equal(extract_TK("LTH.-40.F"), extract_TK("LTH.-40.C"))
  
  # Build a vector that has well-formed and mal-formed strings.
  weird_temps <- c("HTH.-40.0.F", "MTH.70.K", "ZHH.70.K")
  kelvins <- extract_TK(weird_temps)
  expect_equal(kelvins[[1]], -40.0 + 273.15)
  expect_equal(kelvins[[2]], 70)
  expect_true(is.na(kelvins[[3]]))
  
  # Try with malformed unit string
  expect_true(is.na(extract_TK("HTH.600.CC")))
  
  # Try with LTC.15.C.  I.e., we are now allowing cooling.
  expect_equal(extract_TK("LTC.15.C"), 15 + 273.15)
  expect_true(is.na(extract_TK("LTW.15.C")))
  expect_equal(extract_TK("HTC.-110.C"), -110 + 273.15)
})


test_that("carnot_efficiency() works as expected", {
  expect_equal(carnot_efficiency("HTH.298.15.K"), 0)

  heats1 <- c("HTH.600.C", "MTH.200.C", "MTH.100.C", "LTH.20.C", "LTH.-20.C")
  expect_equal(carnot_efficiency(heats1), c(0.65853519, 0.36986157, 0.20099156, 0.016770082, 0.15093074))
  
  expect_equal(carnot_efficiency("HTH.600.C", T_0 = 500), 1 - 500 / (600 + 273.15))
  expect_equal(carnot_efficiency(c("HTH.600.C", "LTH.600.C"), T_0 = c(298.15, 273.15)), c(1 - 298.15 / (600 + 273.15), 1 - 273.15 / (600 + 273.15)))
  
  expect_true(is.na(carnot_efficiency("$$H.50.C")))
})


test_that("tp_products() works as expected", {
  # Try with an invalid value for type
  expect_error(IEATools::tp_products(type = "bogus"))
  
  # Try with default stage (Production) and side (Consumption).
  # Should get nothing, because "Production" doesn't use anything.
  consumption_prod <- IEATools::prod_tp_eiou_energy_carriers()
  expect_equal(length(consumption_prod), 0)
  expect_equal(length(names(consumption_prod)), 0)

  # Use the default value of "type," namely "Consumption" and the Transformation process stage.
  consumption_tp <- IEATools::prod_tp_eiou_energy_carriers(stage = "Transformation processes")
  expect_equal(consumption_tp[["Charcoal production plants"]], "Primary solid biofuels")
  expect_equal(consumption_tp[["Coal liquefaction plants"]], c("Hard coal (if no detail)", "Other bituminous coal"))
  expect_equal(consumption_tp[["Gas works"]], c("Hard coal (if no detail)", "Other bituminous coal"))
  expect_equal(consumption_tp[["Gas-to-liquids (GTL) plants"]], "Natural gas")
  expect_equal(consumption_tp[["Coke ovens"]], c("Hard coal (if no detail)", "Coking coal"))
  expect_equal(consumption_tp[["Oil refineries"]], c("Crude oil", "Natural gas liquids"))
  # expect_equal(consumption_tp[["Main activity producer electricity plants"]], 
  #              c("Gas/diesel oil excl. biofuels", "Hydro", "Hard coal (if no detail)", "Crude oil", "Other bituminous coal", "Nuclear"))
  expect_equal(consumption_tp[["Main activity producer electricity plants"]], 
               c("Crude oil", "Gas/diesel oil excl. biofuels", "Hydro", "Hard coal (if no detail)", "Other bituminous coal", "Nuclear"))
  
  # Use the default value of "type" (Consumption) and EIOU.
  consumption_eiou <- IEATools::prod_tp_eiou_energy_carriers(stage = "Energy industry own use")
  expect_equal(consumption_eiou[["Coal mines"]], "Electricity")
  # expect_equal(consumption_eiou[["Oil refineries"]], c("Refinery gas", "Fuel oil", "Gas works gas", "Electricity"))
  expect_equal(consumption_eiou[["Oil refineries"]], c("Refinery gas", "Gas works gas", "Fuel oil", "Electricity"))
  expect_equal(consumption_eiou[["Own use in electricity, CHP and heat plants"]], "Electricity")
  expect_equal(consumption_eiou[["Pumped storage plants"]], "Electricity")
  
  # Now try with the "type" equal to "Supply" at the Production stage
  supply_prod <- IEATools::prod_tp_eiou_energy_carriers(stage = "Production", side = "Supply")
  expect_equal(supply_prod[["Production"]], 
               c("Primary solid biofuels", "Hydro", "Hard coal (if no detail)", "Coking coal",
                 "Other bituminous coal", "Natural gas", "Crude oil", "Natural gas liquids", "Nuclear"))
  
  # Now try with the "type" equal to "Supply" at the Transformation processes stage
  supply_tp <- IEATools::prod_tp_eiou_energy_carriers(stage = "Transformation processes", side = "Supply")
  expect_equal(supply_tp[["Autoproducer electricity plants"]], "Electricity")
  expect_equal(supply_tp[["Blast furnaces"]], "Blast furnace gas")
  expect_equal(supply_tp[["Charcoal production plants"]], "Charcoal")
  expect_equal(supply_tp[["Coal liquefaction plants"]], "Other hydrocarbons")
  expect_equal(supply_tp[["Coke ovens"]], c("Coke oven coke", "Coke oven gas"))
  expect_equal(supply_tp[["Gas works"]], "Gas works gas")
  expect_equal(supply_tp[["Gas-to-liquids (GTL) plants"]], "Other hydrocarbons")
  # expect_equal(supply_tp[["Oil refineries"]], 
  #              c("Refinery gas", "Liquefied petroleum gases (LPG)", "Motor gasoline excl. biofuels",
  #                "Kerosene type jet fuel excl. biofuels", "Other kerosene", "Gas/diesel oil excl. biofuels", 
  #                "Fuel oil", "Aviation gasoline", "Naphtha", "Bitumen", "Other oil products", 
  #                "White spirit & SBP", "Lubricants", "Paraffin waxes"))
  expect_equal(supply_tp[["Oil refineries"]], 
               c("Refinery gas", "Liquefied petroleum gases (LPG)", "Motor gasoline excl. biofuels",
                 "Kerosene type jet fuel excl. biofuels", "Other kerosene", "Gas/diesel oil excl. biofuels", 
                 "Fuel oil", "Aviation gasoline", "Naphtha", "White spirit & SBP", "Lubricants", 
                 "Bitumen", "Paraffin waxes", "Other oil products"))
  
  # Now try with the "type" equal to "Supply" at the EIOU stage.
  # We should get nothing, because EIOU, by definition, does not produce any energy.
  supply_eiou <- IEATools::prod_tp_eiou_energy_carriers(stage = "Energy industry own use", side = "Supply")
  expect_equal(length(supply_eiou), 0)
  expect_equal(length(names(supply_eiou)), 0)
})


test_that("sample_iea_file_path() works correctly", {
  expect_true(endsWith(sample_iea_data_path(), "GH-ZA-TJ-Extended-Energy-Balances-sample-2022.csv"))
  expect_true(endsWith(sample_iea_data_path(2022), "GH-ZA-TJ-Extended-Energy-Balances-sample-2022.csv"))
  expect_true(endsWith(sample_iea_data_path(2021), "GH-ZA-TJ-Extended-Energy-Balances-sample-2021.csv"))
  expect_error(sample_iea_data_path(2020), "Only versions 2021 and later are supported in sample_iea_data_path()")
})


test_that("sample_fu_allocation_table_path() works correctly", {
  expect_true(sample_fu_allocation_table_path() %>% endsWith("GH-ZA-Allocation-sample-2022.xlsx"))
  expect_true(endsWith(sample_fu_allocation_table_path(2022), "GH-ZA-Allocation-sample-2022.xlsx"))
  expect_true(endsWith(sample_fu_allocation_table_path(2021), "GH-ZA-Allocation-sample-2021.xlsx"))
  expect_error(sample_fu_allocation_table_path(2020), "Only versions 2021 and later are supported in sample_fu_allocation_table_path()")
})


test_that("sample_eta_fu_table_path() works correctly", {
  expect_true(sample_eta_fu_table_path() %>% endsWith("GH-ZA-Efficiency-sample-2022.xlsx"))
  expect_true(endsWith(sample_eta_fu_table_path(2022), "GH-ZA-Efficiency-sample-2022.xlsx"))
  expect_true(endsWith(sample_eta_fu_table_path(2021), "GH-ZA-Efficiency-sample-2021.xlsx"))
  expect_error(sample_eta_fu_table_path(2020), "Only versions 2021 and later are supported in sample_eta_fu_table_path()")
})


test_that("adjacent_rownums() works as expected", {
  DF <- data.frame(C1 = c("A", "B", "C"), stringsAsFactors = FALSE)
  expect_equal(adjacent_rownums(DF, col_name = "C1", entries = c("A", "B")), c(1, 2))
  expect_equal(adjacent_rownums(DF, col_name = "C1", entries = c("B", "C")), c(2, 3))
  # When we can't find adjacent entries, get NULL back.
  expect_true(is.null(adjacent_rownums(DF, col_name = "C1", entries = c("A", "Z"))))
  # Try when there are multiple matches.
  DF2 <- data.frame(C1 = c("A", "B", "A", "B"), stringsAsFactors = FALSE)
  expect_error(adjacent_rownums(DF2, col_name = "C1", entries = c("A", "B")), "multiple instances of adjacent entries in adjacent_rownums")
})


test_that("sorting a tidy IEA data frame works as expected", {
  tidy <- load_tidy_iea_df() %>% 
    # load_tidy_iea_df() no longer preserves original order, 
    # due to the switch from tidyr::gather() to tidyr::pivot_longer().
    # So sort the data frame here, before testing.
    sort_iea_df()
  # Should get the same thing back if we sort now, because tidy is already sorted!
  expect_equal(sort_iea_df(tidy), tidy)
  num_rows <- nrow(tidy)
  # Look at the first row
  expect_equal(tidy$Country[[1]], "GHA")
  expect_equal(tidy$Product[[1]], "Primary solid biofuels")
  # Look at the last row
  expect_equal(tidy$Country[[num_rows]], "ZAF")
  expect_equal(tidy$Product[[num_rows]], "Paraffin waxes")
  # Move the first row to the bottom to put everything out of order
  unsorted <- tidy[-1, ] %>% 
    dplyr::bind_rows(tidy[1, ])
  # Check that the moved successfully to the last row
  expect_equal(unsorted$Country[[num_rows]], "GHA")
  expect_equal(unsorted$Product[[num_rows]], "Primary solid biofuels")
  # Now sort it
  sorted <- sort_iea_df(unsorted)
  # Bug: The Last.stage column has NA values
  # Make sure Last.stage has no NA values in it.
  expect_false(any(is.na(sorted$LastStage)))
  # Look at the first row
  expect_equal(sorted$Country[[1]], "GHA")
  expect_equal(sorted$Product[[1]], "Primary solid biofuels")
  # Look at the last row
  expect_equal(sorted$Country[[num_rows]], "ZAF")
  expect_equal(sorted$Product[[num_rows]], "Paraffin waxes")
  
  # Try with a wide data frame, one that spreads years to the right.
  unsorted_wide <- tidy %>% 
    tidyr::pivot_wider(names_from = Year, values_from = E.dot)
  # The wide data frame is not sorted correctly. Sort it.
  sorted_wide <- sort_iea_df(unsorted_wide)
  # Test that we got a good result.
  # First row should have Ghana first
  expect_equal(sorted_wide$Country[[1]], "GHA")
  expect_equal(sorted_wide$LedgerSide[[1]], "Supply")
  expect_equal(sorted_wide$Product[[1]], "Primary solid biofuels")
  # Last row is South Africa
  num_rows <- nrow(sorted_wide)
  expect_equal(sorted_wide$Country[[num_rows]], "ZAF")
  expect_equal(sorted_wide$FlowAggregationPoint[[num_rows]], "Non-energy use")
  expect_equal(sorted_wide$Product[[num_rows]], "Paraffin waxes")
})


test_that("sorting an IEA DF does the right thing with Non-energy flows", {
  tidy <- load_tidy_iea_df() %>%  # Unsorted
    sort_iea_df()
  tidy1971 <- tidy %>% 
    dplyr::filter(Year == 1971)
  expect_equal(sort_iea_df(tidy1971), tidy1971)
  expect_equal(sort_iea_df(tidy), tidy)
})


test_that("sort_iea_df() works on a specified IEA data frame", {
  # Make sure that the initially-loaded data frame has sorting as expected.
  loaded <- load_tidy_iea_df()
  expect_equal(loaded$Flow[[1]], IEATools::tpes_flows$production)
  expect_equal(loaded$Flow[[nrow(loaded)]], IEATools::non_energy_flows$non_energy_use_industry_transformation_energy)
  
  # Sort the data frame and make sure everything is still in the right place.
  sorted_loaded <- sort_iea_df(loaded)
  expect_equal(sorted_loaded$Flow[[1]], IEATools::tpes_flows$production)
  expect_equal(sorted_loaded$Flow[[nrow(sorted_loaded)]], IEATools::non_energy_flows$non_energy_use_industry_transformation_energy)
  
  # Now specify the data frame and make sure sorting still works.
  sorted_specified <- loaded %>% 
    specify_all() %>% 
    sort_iea_df()
  
  expect_equal(sorted_specified$Flow[[1]], 
               RCLabels::paste_pref_suff(pref = "Resources", suff = biofuels_and_waste_products$primary_solid_biofuels, notation = RCLabels::of_notation))
  
  expect_equal(sorted_specified$Flow[[1]], 
               RCLabels::paste_pref_suff(pref = "Resources", suff = biofuels_and_waste_products$primary_solid_biofuels, notation = RCLabels::of_notation))
})


test_that("replace_join() works as expected", {
  DFA <- tibble::tribble(~x, ~y, 
                         1, "A", 
                         2, "B")
  DFB <- tibble::tribble(~x, ~y, 
                         2, "C", 
                         3, "D")
                         
  # Try with incorrect by argument. 
  # replace_col is in by argument
  expect_error(replace_join(DFA, DFB, replace_col = "y", by = c("x", "y")),
               "replace_col must not be in the by argument to replace_join")
  
  # But the default by argument ensures that replace_col is not in the by argument.
  expect_equal(replace_join(DFA, DFB, replace_col = "y"),
               tibble::tribble(~x, ~y, 
                               1, "A", 
                               2, "C", 
                               3, "D"))
               
  expect_equal(replace_join(DFB, DFA, replace_col = "y"),
               tibble::tribble(~x, ~y, 
                               2, "B", 
                               3, "D", 
                               1, "A"))

  # Try with 2 columns in replace_col.
  expect_error(replace_join(DFA, DFB, replace_col = c("x", "y")), 
               "length\\(replace_col\\) is 2 in replace_join. Must be 1.")
  
  DFC <- data.frame(x = c(2, 3), y = c("C", "D"), z = c("E", "F"), stringsAsFactors = FALSE)
  DFC <- tibble::tribble(~x, ~y, ~z, 
                         2, "C", "E", 
                         3, "D", "F")
  expect_equal(replace_join(DFA, DFC, replace_col = "y"), 
               tibble::tribble(~x, ~y, 
                               1, "A", 
                               2, "C", 
                               3, "D"))
  
  # Try when replace_col is in by.  That isn't allowed.
  expect_error(replace_join(DFA, DFB, replace_col = "y", by = c("x", "y")), 
               "replace_col must not be in the by argument to replace_join")
  
  # Try with multiple matching rows
  DFD <- tibble::tribble(~x, ~y,
                         2, "M", 
                         2, "N")
  expect_equal(replace_join(DFA, DFD, replace_col = "y", multiple = "all"), 
               tibble::tribble(~x, ~y, 
                               1, "A", 
                               2, "M", 
                               2, "N"))
  expect_equal(replace_join(DFD, DFA, replace_col = "y"), 
               tibble::tribble(~x, ~y, 
                               2, "B", 
                               2, "B", 
                               1, "A"))

  # Try when one of the columns in x or y contains factors.  
  DFE <- tibble::tribble(~x, ~y, 
                         1, "A", 
                         2, "B") %>% dplyr::mutate(x = factor(x))
  expect_error(replace_join(DFE, DFB, replace_col = "y"), 
               "Columns should not contain factors in arguments to replace_join")
  
  # Try with multiple columns in the by argument
  DFF <- tibble::tribble(~x, ~y, ~z,
                         1, "A", 1, 
                         2, "B", 2)
  DFG <- tibble::tribble(~x, ~y, ~z, 
                         2, "B", 11, 
                         3, "C", 12)
  expect_equal(replace_join(DFF, DFG, replace_col = "z"), 
               tibble::tribble(~x, ~y, ~z, 
                               1, "A", 1,
                               2, "B", 11,
                               3, "C", 12))

  # Try with columns that aren't used.
  # In this case, the z column in DFE just comes along for the ride, yielding an NA value.
  expect_equal(replace_join(DFF, DFB, replace_col = "y"), 
               tibble::tribble(~x, ~z, ~y, 
                               1, 1, "A", 
                               2, 2, "C",
                               3, NA_integer_, "D"))

  # Try switching.  In this case, the z column in DFE is lost, as expected.
  expect_equal(replace_join(DFB, DFF, replace_col = "y"),
               tibble::tribble(~x, ~y, 
                               2, "B", 
                               3, "D", 
                               1, "A"))
  
  # Test with y missing replace_col
  DFH <- tibble::tribble(~a, ~b, 
                         1, 1, 
                         2, 2)
  expect_equal(replace_join(DFA, DFH, replace_col = "y"), DFA)
  
  # Test with x missing replace_col
  expect_equal(replace_join(DFH, DFA, replace_col = "y"), DFH)
})


# Testing default_aggregation_region_table_path() function
test_that("default_aggregation_region_table_path() works correctly", {
  expect_true(default_aggregation_region_table_path() %>% endsWith("aggregation_table_iea_exiobase_2019.xlsx"))
  expect_true(default_aggregation_region_table_path(2020) %>% endsWith("aggregation_table_iea_exiobase_2020.xlsx"))
  expect_error(default_aggregation_region_table_path(2018), "Only 2019, and 2020 are supported in default_aggregation_region_table_path()")
})
# EAR - 29/09/2020
