test_that("electricity_heat_output() works as expected", {
  # Check an original (GWhr) values.
  GWhr_check <- 2909
  sample_iea_data_path() |> 
    iea_df() |> 
    tibble::as_tibble() |> 
    rename_iea_df_cols() |> 
    clean_iea_whitespace() |> 
    use_iso_countries() |> 
    tidyr::pivot_longer(cols = c(`1971`, `2000`),
                        names_to = IEATools::iea_cols$year,
                        values_to = IEATools::iea_cols$e_dot) |> 
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "GHA", 
                  .data[[IEATools::iea_cols$year]] == 1971,
                  startsWith(.data[[IEATools::iea_cols$flow]], 
                             IEATools::elec_heat_output$electricity_output_prefix), 
                  .data[[IEATools::iea_cols$product]] == "Hydro", 
                  .data[[IEATools::iea_cols$e_dot]] != 0) |> 
    magrittr::extract2(IEATools::iea_cols$e_dot) |> 
    magrittr::extract2(1) |> 
    expect_equal(GWhr_check)

  # Get the electricity and heat data
  elec_heat_data <- sample_iea_data_path() |> 
    load_electricity_heat_output() |> 
    tibble::as_tibble()
  
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
  elec_heat_data |> 
    dplyr::filter(.data[[IEATools::iea_cols$country]] == "GHA", 
                  .data[[IEATools::iea_cols$year]] == 1971,
                  .data[[IEATools::elec_heat_output$input_product]] == "Hydro", 
                  .data[[IEATools::elec_heat_output$output_product]] == "Electricity") |> 
    magrittr::extract2(IEATools::iea_cols$e_dot) |> 
    magrittr::extract2(1) |> 
    expect_equal(GWhr_check * 3.6)
})
