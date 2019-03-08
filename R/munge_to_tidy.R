munge_aug_iea_to_tidy <- function(.aug_iea_df, 
                                  country = "Country", year = "Year", ledger_side = "Ledger.side", 
                                  flow_aggregation_point = "Flow.aggregation.point", 
                                  flow = "Flow", product = "Product",
                                  energy = "E.ktoe"){
  # Load country code information
  CountryInfo <- countrycode::codelist %>% 
    dplyr::select(country.name.en, iso2c) %>% 
    dplyr::rename(
      Country = country.name.en
    )
  # There are some "Countries" in the IEA data set that do not have corresponding
  # iso2c abbreviations in the countrycode database.  
  # None of these countries are of interest to us now (March 2018),
  # so we will not try any corrections at this time.
  # Later, we can add additional rows to the CountryInfo data frame to pick up ISO abbreviations
  # for missing countries.
  # The code might look something like this:
  # bind_rows(
  #   data.frame(Country = c("Former Soviet Union (if no detail)",
  #                          "Former Yugoslavia (if no detail)",
  #                          "Republic of Vietnam",
  #                          "Tanzania",
  #                          "Venezuela",
  #                          "Islamic Republic of Iran",
  #                          "Dem. Republic of the Congo",
  #                          "Dem. People's Rep. of Korea",
  #                          "People's Republic of China",
  #                          "C\x99te d'Iviore"),
  #   iso2c = c("SO", "YU", "VN", "TZ", "VE", "IR", "CD", "KP", "CN", "CI"))
  # )

  out <- .aug_iea_df %>% 
    tidyr::gather(!!as.name(year), !!as.name(energy), -c(country, ledger_side, flow_aggregation_point, flow, product))
  
  
  
  # IEAData2 <- IEAData1 %>%
  #   tidyr::gather(year, energy, -c(Country, Ledger.side, Flow, Flow.aggregation.point, Product))
  
}