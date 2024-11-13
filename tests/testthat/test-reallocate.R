test_that("reallocate_industry_nes() works as expected", {
  psut <- load_tidy_iea_df() |> 
    specify_all() |>  
    prep_psut() |> 
    dplyr::slice_head(n = 1)
  
  psut_reallocated <- psut |> 
    reallocate_industry_nes()
  
  expect_true(!is.null(psut_reallocated))
  
  
})


test_that("reallocate_statistical_differences() works as expected", {
  psut <- load_tidy_iea_df() |> 
    specify_all() |>  
    prep_psut() |> 
    dplyr::slice_head(n = 1)
  
  psut_reallocated <- psut |> 
    reallocate_statistical_differences()
  
  expect_true(!is.null(psut_reallocated))
  
})