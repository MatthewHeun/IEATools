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
