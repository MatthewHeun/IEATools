test_that("electricity_heat_output() works as expected", {
  elec_heat_data <- sample_iea_data_path() |> 
    electricity_heat_output() |> 
    tibble::as_tibble()
  # Ensure that input product and output product columns are present
  expect_true(c(IEATools::elec_heat_output$input_product, 
                IEATools::elec_heat_output$output_product) %in% colnames(elec_heat_data) |> 
                all())

  # Ensure that all rows start with electricity output or heat output
  elec_heat_data |> 
    dplyr::mutate(
      right_col = startsWith(.data[[IEATools::iea_cols$flow]], IEATools::elec_heat_output$electricity_output_prefix) | 
        startsWith(.data[[IEATools::iea_cols$flow]], IEATools::elec_heat_output$heat_output_prefix)
    )
  # Ensure that "Total" and "Memo: Renewables" rows have been removed
  elec_heat_data |> 
    dplyr::filter((.data[[IEATools::iea_cols$product]] ==IEATools::memo_aggregation_product_prefixes$total |
                     startsWith(.data[[IEATools::iea_cols$product]], IEATools::memo_aggregation_flow_prefixes$memo))) |> 
    nrow() |> 
    expect_equal(0)
})
