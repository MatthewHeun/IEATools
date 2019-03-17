###########################################################
context("PSUT functions")
###########################################################

test_that("S_units_from_tidy works as expected", {
  S_units <- file.path("extdata", "GH-ZA-ktoe-Extended-Energy-Balances-sample.csv") %>% 
    system.file(package = "IEATools") %>% 
    iea_df() %>%
    rename_iea_df_cols() %>% 
    use_iso_countries() %>% 
    remove_agg_memo_flows() %>% 
    augment_iea_df() %>% 
    tidy_iea_df() %>%
    group_by(Country, Year, Energy.type) %>% 
    extract_S_units_from_tidy()

  for (i in nrow(S_units)) {
    su <- S_units$S_units[[i]]
    expect_true(all(su[ , "ktoe"] == 1))
  }
})

test_that("add_psut_matnames works as expected", {
  WithMatnames <- file.path("extdata", "GH-ZA-ktoe-Extended-Energy-Balances-sample.csv") %>% 
    system.file(package = "IEATools") %>% 
    iea_df() %>%
    rename_iea_df_cols() %>% 
    use_iso_countries() %>% 
    remove_agg_memo_flows() %>% 
    augment_iea_df() %>% 
    tidy_iea_df() %>% 
    filter(Country == "GH", Year == 1971) %>% 
    group_by(Country, Year, Energy.type) %>% 
    add_psut_matnames()
})