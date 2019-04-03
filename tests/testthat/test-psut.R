###########################################################
context("PSUT functions")
###########################################################

test_that("S_units_from_tidy works as expected", {
  S_units <- load_tidy_iea_df() %>% 
    extract_S_units_from_tidy()

  for (i in nrow(S_units)) {
    su <- S_units$S_units[[i]]
    expect_true(all(su[ , "ktoe"] == 1))
  }
})

test_that("add_psut_matnames works as expected", {
  WithMatnames <- load_tidy_iea_df() %>% 
    filter(Country == "GHA", Year == 1971) %>% 
    add_psut_matnames()
})
