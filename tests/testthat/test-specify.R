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
  # There are none of these flows for Ghana (GH)
  expect_true(all(Res_coal_oilng$Country == "ZA"))
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
                            stringsAsFactors = FALSE)
  specified <- unspecified %>% 
    specify_tp_eiou()
  expect_equal(specified$Flow, c("Main activity producer electricity plants", "Main activity producer electricity plants"))
})

test_that("specify_all works as expected", {
  Simple <- load_tidy_iea_df() %>% 
    specify_all()
  Complicated <- load_tidy_iea_df() %>% 
    specify_primary_production() %>% 
    specify_production_to_resources() %>% 
    specify_tp_eiou() %>% 
    specify_interface_industries()
  expect_equal(Simple, Complicated)
})

test_that("transformation_sinks works as expected", {
  # Try to send an ungrouped data frame into the function. Should give 0 rows.
  expect_equal(load_tidy_iea_df() %>% 
                 specify_all() %>% 
                 transformation_sinks(grouping_vars = NULL) %>% 
                 nrow(), 0)
  # Try to group on Flow.aggregation.point. Should fail.
  expect_error(load_tidy_iea_df() %>% 
                 specify_all() %>% 
                 group_by(Flow.aggregation.point) %>% 
                 transformation_sinks(grouping_vars = "Flow.aggregation.point"), "Flow.aggregation.point cannot be a grouping variable of .tidy_iea_df in transformation_sinks()")
  # Try to group on Flow.aggregation.point. Should fail.
  expect_error(load_tidy_iea_df() %>% 
                 specify_all() %>% 
                 group_by(Flow) %>% 
                 transformation_sinks(grouping_vars = "Flow"), "Flow cannot be a grouping variable of .tidy_iea_df in transformation_sinks()")
  # Try with the built-in data set in which there are no transformation sinks.
  sink_industries <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    group_by(Method, Last.stage, Country, Year, Energy.type) %>% 
    transformation_sinks()
  expect_equal(nrow(sink_industries), 0)
  # Try with a simple, made-up data set
  Tidy <- data.frame(Flow.aggregation.point = c("Transformation processes", "Transformation processes", "Transformation processes"), 
                     Flow = c("Automobiles", "Automobiles", "Furnaces"),
                     E.dot = c(-1, 1, -2), 
                     stringsAsFactors = FALSE) %>% 
    mutate(
      Country = "Bogus",
      Product = "Petrol"
    ) %>% 
    group_by(Country) %>% 
    specify_all()
  # Automobiles are fine, but Furnaces don't make anything and are, therefore, a transformation sink.
  expect_equal(Tidy %>% transformation_sinks(grouping_vars = "Country"), 
               data.frame(Country = "Bogus", Flow = "Furnaces", stringsAsFactors = FALSE))
})


###########################################################
context("Trying skip")
###########################################################

test_that("transformation_sinks works for all IEA data",{
  
  skip("Skipping transformation_sinks() test for all IEA data")
  
  iea_path <- "~/Documents/Calvin stuff/Useful Work/IEA Data/Extended-Energy-Balances-2018/Extended-Energy-Balances-2018-full-ktoe.csv"
  
  Transformation_sinks <- load_tidy_iea_df(iea_path) %>% 
    specify_all() %>% 
    group_by()
    transformation_sinks()

  Manual_transformation_sinks <- iea_df(iea_path) %>% 
    rename_iea_df_cols() %>% 
    use_iso_countries() %>% 
    group_by(Country) %>% 
    gather(key = "Year", value = "E.dot", -Country, -Flow, -Product) %>% 
    filter(E.dot != 0, Product != "Total") %>% 
    filter(!startsWith(Product, "Memo:")) %>% 
    filter(endsWith(Flow, "(transf.)") | endsWith(Flow, "(transformation)") | endsWith(Flow, "(energy)")) %>% 
    specify_tp_eiou() %>% 
    specify_interface_industries()
  
    
    
    
  
  # This test will only work when access is provided to an IEA data file.
  expect_true(TRUE)
  print("Before skip")
  skip("Skipping everything after this")
  print("After skip")
  expect_true(FALSE)
  expect_true(FALSE)
})
