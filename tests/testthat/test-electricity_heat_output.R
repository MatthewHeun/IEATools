test_that("electricity_heat_output() works as expected", {
  elec_heat_data <- sample_iea_data_path() |> 
    load_electricity_heat_output() |> 
    tibble::as_tibble()
  
  # Check some original (GWhr) values.
  
  # Ensure that input product and output product columns are present
  expect_true(c(IEATools::elec_heat_output$input_product, 
                IEATools::elec_heat_output$output_product, 
                IEATools::iea_cols$unit) %in% colnames(elec_heat_data) |> 
                all())

  # Ensure that all rows have Electricity or Heat
  elec_heat_data |> 
    dplyr::mutate(
      right_col = .data[[IEATools::elec_heat_output$output_product]] %in% c(IEATools::electricity_products$electricity, 
                                                                            IEATools::heat_products$heat)
    ) |> 
    magrittr::extract2("right_col") |> 
    all() |> 
    expect_true()
  
  # Ensure that "Total" and "Memo: Renewables" rows have been removed
  elec_heat_data |> 
    dplyr::filter((startsWith(.data[[IEATools::elec_heat_output$output_product]], IEATools::memo_aggregation_product_prefixes$total) |
                     startsWith(.data[[IEATools::elec_heat_output$output_product]], IEATools::memo_aggregation_flow_prefixes$memo))) |> 
    nrow() |> 
    expect_equal(0)
  
  # Check some TJ values to ensure that unit conversions were done correctly.
  
})
