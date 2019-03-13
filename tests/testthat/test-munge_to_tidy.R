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
  agg_rows <- cleaned %>% 
    dplyr::filter(Flow == "Total primary energy supply" |
                    Flow == "Total final consumption" | 
                    Flow == "Transformation processes" |
                    Flow == "Energy industry own use" | 
                    Flow == "Industry" |
                    Flow == "Transport" |
                    Flow == "Other" |
                    Flow == "Non-energy use") %>% 
    nrow()
  expect_equal(agg_rows, 0)
})

test_that("munge_aug_iea_to_tidy works as expected", {
  tidy_iea_df <- file.path("extdata", "GH-ZA-ktoe-Extended-Energy-Balances-sample.csv") %>% 
    system.file(package = "IEAData") %>% 
    iea_df() %>%
    rename_iea_df_cols() %>% 
    augment_iea_df() %>% 
    munge_aug_iea_to_tidy()
  
})
