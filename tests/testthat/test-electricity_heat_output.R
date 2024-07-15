test_that("electricity_heat_output() works as expected", {
  foo <- sample_iea_data_path() |> 
    electricity_heat_output() |> 
    tibble::as_tibble()
})
