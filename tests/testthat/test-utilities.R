###########################################################
context("Testing utilities")
###########################################################

test_that("starts_with_any_of() works properly", {
  expect_true(starts_with_any_of(x = "prefix - suffix", target = c("a", "b", "prefix")))
  expect_false(starts_with_any_of(x = "prefix - suffix", target = c("a", "b", "c")))
  expect_false(starts_with_any_of(x = "prefix - suffix", target = "suffix"))
  expect_equal(starts_with_any_of(x = c("Production - Crude", "Production - NG", "Exports - Oil", "Exports - Crude"),
                                  target = c("Production", "Imports")),
               c(TRUE, TRUE, FALSE, FALSE))
  # Does it also work with lists?
  expect_equal(starts_with_any_of(x = list("Production - Crude", "Production - NG", "Exports - Oil", "Exports - Crude"),
                                  target = c("Production", "Imports")),
               c(TRUE, TRUE, FALSE, FALSE))
})

test_that("year_cols() works as expected", {
  DF <- data.frame(a = c(1, 2), `1967` = c(3, 4), `-10` = c(5, 6), check.names = FALSE)
  expect_equal(DF %>% year_cols(), c(2, 3))
  expect_equal(DF %>% year_cols(return_names = TRUE), c("1967", "-10"))
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
})

test_that("carnot_efficiency works as expected", {
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
  expect_equal(consumption_tp[["Main activity producer electricity plants"]], 
               c("Gas/diesel oil excl. biofuels", "Hydro", "Hard coal (if no detail)", "Crude oil", "Other bituminous coal", "Nuclear"))
  
  # Use the default value of "type" (Consumption) and EIOU.
  consumption_eiou <- IEATools::prod_tp_eiou_energy_carriers(stage = "Energy industry own use")
  expect_equal(consumption_eiou[["Coal mines"]], "Electricity")
  expect_equal(consumption_eiou[["Oil refineries"]], c("Refinery gas", "Fuel oil", "Gas works gas", "Electricity"))
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
  expect_equal(supply_tp[["Oil refineries"]], 
               c("Refinery gas", "Liquefied petroleum gases (LPG)", "Motor gasoline excl. biofuels",
                 "Kerosene type jet fuel excl. biofuels", "Other kerosene", "Gas/diesel oil excl. biofuels", 
                 "Fuel oil", "Aviation gasoline", "Naphtha", "Bitumen", "Other oil products", 
                 "White spirit & SBP", "Lubricants", "Paraffin waxes"))
  
  # Now try with the "type" equal to "Supply" at the EIOU stage.
  # We should get nothing, because EIOU, by definition, does not produce any energy.
  supply_eiou <- IEATools::prod_tp_eiou_energy_carriers(stage = "Energy industry own use", side = "Supply")
  expect_equal(length(supply_eiou), 0)
  expect_equal(length(names(supply_eiou)), 0)
})


test_that("sample_iea_file_path works correctly", {
  expect_true(sample_iea_data_path() %>% endsWith("GH-ZA-ktoe-Extended-Energy-Balances-sample-2019.csv"))
  expect_true(endsWith(sample_iea_data_path(2018), "GH-ZA-ktoe-Extended-Energy-Balances-sample-2018.csv"))
  expect_error(sample_iea_data_path(2017), "Only 2018 and 2019 are supported in sample_iea_data_path()")
})


test_that("sample_fu_allocation_table_path works correctly", {
  expect_true(sample_fu_allocation_table_path() %>% endsWith("GH-ZA-Allocation-sample-2019.xlsx"))
  expect_true(endsWith(sample_fu_allocation_table_path(2018), "GH-ZA-Allocation-sample-2018.xlsx"))
  expect_error(sample_fu_allocation_table_path(2017), "Only 2018 and 2019 are supported in sample_fu_allocation_table_path")
})


test_that("sample_eta_fu_table_path works correctly", {
  expect_true(sample_eta_fu_table_path() %>% endsWith("GH-ZA-Efficiency-sample-2019.xlsx"))
  expect_true(endsWith(sample_eta_fu_table_path(2018), "GH-ZA-Efficiency-sample-2018.xlsx"))
  expect_error(sample_eta_fu_table_path(2017), "Only 2018 and 2019 are supported in sample_eta_fu_table_path")
})


test_that("adjacent_rownums works as expected", {
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
  tidy <- load_tidy_iea_df()
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
  sorted <- sort_tidy_iea_df(unsorted)
  # Look at the first row
  expect_equal(sorted$Country[[1]], "GHA")
  expect_equal(sorted$Product[[1]], "Primary solid biofuels")
  # Look at the last row
  expect_equal(sorted$Country[[num_rows]], "ZAF")
  expect_equal(sorted$Product[[num_rows]], "Paraffin waxes")
})
