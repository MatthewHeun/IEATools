###########################################################
context("IEA energy balance")
###########################################################

test_that("fix_IEA_df_energy_balance works correctly", {
  
})


test_that("verify_IEA_df_energy_balance works correctly", {
  # "Out of the box", these data are not perfeclty balanced.
  # So we expect an error here.
  unbalanced <- file.path("extdata", "GH-ZA-ktoe-Extended-Energy-Balances-sample.csv") %>% 
    system.file(package = "IEAData") %>% 
    iea_df() %>%
    rename_iea_df_cols() %>% 
    remove_agg_memo_flows() %>% 
    augment_iea_df() %>% 
    tidy_iea_df() %>% 
    group_by(Country, Year, Energy.type, Units, Product)
  expect_error(verify_IEA_df_energy_balance(unbalanced), "Energy not balanced in verify_IEA_df_energy_balance. Check return value for non-zero .err column.") 
    
  balanced <- unbalanced %>% 
    fix_IEA_df_energy_balance()
  
  
  
  
  # # Make sure that it works.
  # expect_silent(
  #   UKEnergy2000tidy %>%
  #     group_by(Country, Year, Energy.type, Last.stage) %>%
  #     verify_IEATable_energy_balance(energy = "EX.ktoe")
  # )
  # 
  # # Introduce something to make the energy balance fail.
  # Unbalanced <- UKEnergy2000tidy
  # # Change from 5e4 to 1e4
  # Unbalanced$EX.ktoe[[1]] <- 1e4
  # # Now try energy balance. It should fail.
  # expect_error(Unbalanced %>%
  #                group_by(Country, Year, Energy.type, Last.stage) %>%
  #                verify_IEATable_energy_balance(energy = "EX.ktoe"),
  #              "Energy not balanced in verify_IEATable_energy_balance.")
  
})
