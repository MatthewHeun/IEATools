library(dplyr)

###########################################################
context("Testing munge_to_tidy")
###########################################################

test_that("remove_agg_memo_flows works as expected", {
  Cleaned <- file.path("extdata", "GH-ZA-ktoe-Extended-Energy-Balances-sample.csv") %>% 
    system.file(package = "IEATools") %>% 
    iea_df() %>% 
    rename_iea_df_cols() %>% 
    remove_agg_memo_flows()
  # Verify that none of the aggregation flows are present
  n_agg_rows <- Cleaned %>% 
    dplyr::filter(Flow == "Total primary energy supply" |
                    Flow == "Total final consumption" | 
                    Flow == "Transformation processes" |
                    Flow == "Energy industry own use" | 
                    Flow == "Industry" |
                    Flow == "Transport" |
                    Flow == "Other" |
                    Flow == "Non-energy use") %>% 
    nrow()
  expect_equal(n_agg_rows, 0)
  # Verify that none of the memo flows are present
  n_memo_flows <- Cleaned %>% 
    dplyr::filter(startsWith(Flow, "Memo:")) %>% 
    nrow()
  expect_equal(n_memo_flows, 0)
  # Verify that none of the memo products are present
  n_memo_products <- Cleaned %>% 
    dplyr::filter(startsWith(Product, "Memo:")) %>% 
    nrow()
  expect_equal(n_memo_products, 0)

  # Try again with a different approach. 
  # This time, ensure that rows we want to clean are present first.
  IEA_data <- file.path("extdata", "GH-ZA-ktoe-Extended-Energy-Balances-sample.csv") %>% 
    system.file(package = "IEATools") %>% 
    iea_df() %>%
    rename_iea_df_cols() %>% 
    augment_iea_df()
  # Verify that aggregation flows exist
  agg_flows <- c("Total primary energy supply", "Total final consumption", "Transformation processes", "Energy industry own use", "Industry", "Transport", "Other", "Non-energy use")
  expect_true(lapply(agg_flows, 
                     FUN = function(s){
                       expect_true(IEA_data %>% filter(Flow == s) %>% nrow() > 0)
                     }) %>% as.logical() %>% all())
  # Verify that flow memos exist
  memo_flow_prefixes <- c("Memo: ", "Electricity output (GWh)", "Heat output")
  expect_true(lapply(memo_flow_prefixes, 
                     FUN = function(s){
                       expect_true(IEA_data %>% filter(startsWith(Flow, s)) %>% nrow() > 0)
                     }) %>% as.logical() %>% all())
  # Verify that product memos exist
  memo_product_prefix <- "Memo: "
  expect_true(IEA_data %>% filter(startsWith(Product, memo_product_prefix)) %>% nrow() > 0)
  
  # Now clean the aggregation flows and see if they're gone.
  Cleaned <- IEA_data %>% 
    remove_agg_memo_flows()
  # Ensure that aggregation flows were removed.
  expect_true(lapply(agg_flows, 
                     FUN = function(s){
                       expect_true(Cleaned %>% filter(Flow == s) %>% nrow() == 0)
                     }) %>% as.logical() %>% all())
  # Ensure that flow memos were removed
  expect_true(lapply(memo_flow_prefixes, 
                     FUN = function(s){
                       expect_true(Cleaned %>% filter(startsWith(Flow, s)) %>% nrow() == 0)
                     }) %>% as.logical() %>% all())
  # Ensure that product memos were removed
  expect_true(IEA_data %>% filter(startsWith(Product, memo_product_prefix)) %>% nrow() > 0)
})

test_that("use_iso_countries works as expected", {
  iso <- file.path("extdata", "GH-ZA-ktoe-Extended-Energy-Balances-sample.csv") %>% 
    system.file(package = "IEATools") %>% 
    iea_df() %>% 
    rename_iea_df_cols() %>% 
    use_iso_countries()
  expect_false(any(iso$Country == "South Africa"))
  expect_true(any(iso$Country == "ZA"))
  expect_false(any(iso$Country == "Ghana"))
  expect_true(any(iso$Country == "GH"))
  
  # Try with a data frame that contains a World country.
  world <- iea_df(text = paste0(",,TIME,1960,1961\n",
                                "COUNTRY,FLOW,PRODUCT\n",
                                "World,Production,Hard coal (if no detail),42,43\n",
                                "World,Losses,Hard coal (if no detail),1,2")) %>% 
    rename_iea_df_cols() %>% 
    augment_iea_df() %>% 
    use_iso_countries()
  # Ensure that a "World" country is present.
  n_world_rows <- world %>% 
    dplyr::filter(Country == "World") %>% 
    nrow()
  expect_equal(n_world_rows, 2)
})

test_that("tidy_iea works as expected", {
  iea_tidy_df <- file.path("extdata", "GH-ZA-ktoe-Extended-Energy-Balances-sample.csv") %>% 
    system.file(package = "IEATools") %>% 
    iea_df() %>%
    rename_iea_df_cols() %>% 
    remove_agg_memo_flows() %>% 
    augment_iea_df() %>% 
    tidy_iea_df()
  # Verify column names and order
  expect_equal(names(iea_tidy_df), c("Country", "Year", "Ledger.side", "Flow.aggregation.point", 
                                     "Energy.type", "Unit", "Flow", "Product", "E.dot"))
  # This is a energy exclusive data frame
  expect_true(all(iea_tidy_df$Energy.type == "E"))
  # This is a completely ktoe data frame
  expect_true(all(iea_tidy_df$Unit == "ktoe"))
  # Ledger.side can be only Supply or Consumption
  expect_true(all(iea_tidy_df$Ledger.side %in% c("Supply", "Consumption")))
})

test_that("add_production_details works as expected", {
  Prod_details <- file.path("extdata", "GH-ZA-ktoe-Extended-Energy-Balances-sample.csv") %>% 
    system.file(package = "IEATools") %>% 
    iea_df() %>% 
    rename_iea_df_cols() %>% 
    augment_iea_df() %>% 
    remove_agg_memo_flows() %>% 
    tidy_iea_df() %>% 
    add_production_details()
  expect_false(any(Prod_details$Flow == "Production"))
  Production <- Prod_details %>% filter(startsWith(Flow, "Production"))
  expect_true(all(Production$Flow %>% startsWith("Production (")))
  expect_true(all(Production$Flow %>% endsWith(")")))
  # add_production_details should do nothing if it has already been done.
  Prod_details_2 <- Prod_details %>% 
    add_production_details()
  # If the function applied the details twice, there will be some Flows
  # that contain the string ") (".
  # So search for that.
  expect_false(any(Prod_details_2 %>% 
                     filter(startsWith(Flow, "Production")) %>% 
                     extract2("Flow") %>% 
                     grepl(pattern = "\\) \\(")))
})
