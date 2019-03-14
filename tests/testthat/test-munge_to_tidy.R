library(dplyr)

###########################################################
context("Testing munge_to_tidy")
###########################################################

test_that("remove_agg_memo_flows works as expected", {
  cleaned <- file.path("extdata", "GH-ZA-ktoe-Extended-Energy-Balances-sample.csv") %>% 
    system.file(package = "IEAData") %>% 
    iea_df() %>% 
    rename_iea_df_cols() %>% 
    remove_agg_memo_flows()
  # Verify that none of the aggregation flows are present
  n_agg_rows <- cleaned %>% 
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
  n_memo_flows <- cleaned %>% 
    dplyr::filter(startsWith(Flow, "Memo:")) %>% 
    nrow()
  expect_equal(n_memo_flows, 0)
  # Verify that none of the memo products are present
  n_memo_products <- cleaned %>% 
    dplyr::filter(startsWith(Product, "Memo:")) %>% 
    nrow()
  expect_equal(n_memo_products, 0)
})

test_that("munge_aug_iea_to_tidy works as expected", {
  tidy_iea_df <- file.path("extdata", "GH-ZA-ktoe-Extended-Energy-Balances-sample.csv") %>% 
    system.file(package = "IEAData") %>% 
    iea_df() %>%
    rename_iea_df_cols() %>% 
    augment_iea_df() %>% 
    munge_aug_iea_to_tidy()
  
})

test_that("use_iso_countries works as expected", {
  iso <- file.path("extdata", "GH-ZA-ktoe-Extended-Energy-Balances-sample.csv") %>% 
    system.file(package = "IEAData") %>% 
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

test_that("remove_agg_memo_flows works as expected", {
  IEA_data <- file.path("extdata", "GH-ZA-ktoe-Extended-Energy-Balances-sample.csv") %>% 
    system.file(package = "IEAData") %>% 
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
