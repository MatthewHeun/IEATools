#' Remove aggregation and memo rows from data frames of IEA data
#' 
#' Aggregation and memo rows are included with IEA data.
#' Sometimes, it is convenient to remove those rows. 
#' This function does so, using default identifying strings for aggregations and memos.
#'
#' @param .iea_df a data frame of IEA data
#' @param flow the name of the flow column in `iea_df`. Default is "`Flow`".
#' @param product the name of the product columns in `iea_df`. Default is "`Product`".
#' @param agg_flows a vector of strings identifying `Flow`s that are aggregations.
#' @param memo_flow_prefixes a vector of string prefixes for flow memo rows in `.iea_df`
#' @param memo_product_prefix a string prefix for product memo rows in `.iea_df`.
#'
#' @return `.iea_df` less aggregation rows
#' 
#' @export
#'
#' @examples
#' file.path("extdata", "GH-ZA-ktoe-Extended-Energy-Balances-sample.csv") %>% 
#'   system.file(package = "IEAData") %>% 
#'   iea_df() %>%
#'   rename_iea_df_cols() %>% 
#'   remove_agg_memo_flows()
remove_agg_memo_flows <- function(.iea_df,
                                  flow = "Flow",
                                  product = "Product",
                                  agg_flows = c(
                                    "Total primary energy supply",
                                    "Total final consumption", 
                                    "Transformation processes", 
                                    "Energy industry own use",
                                    "Industry",
                                    "Transport",
                                    "Other",
                                    "Non-energy use"),
                                  memo_flow_prefixes = c("Memo: ", "Electricity output (GWh)", "Heat output"), 
                                  memo_product_prefix = "Memo: "){
  .iea_df %>% 
    # Remove Flow aggregations
    dplyr::filter(!startsWith(!!as.name(flow), agg_flows)) %>% 
    # Remove Product aggregations
    dplyr::filter(!!as.name(product) %in% agg_flows) %>% 
    # Remove memo flows
    dplyr::filter(!startsWith(!!as.name(product)), memo_product_prefix)
}


#' Replace country names with 2-letter ISO abbreviations
#' 
#' The IEA uses full country names, but it is more concise to use the 2-letter ISO abbreviations.
#' This function replaces the full country names with ISO abbreviations where possible.
#'
#' @param .iea_df a data frame containing a `country` column
#' @param country the name of the country column in `.iea_df`. Default is "`Country`".
#'
#' @return `.iea_df` with 2-letter ISO country abbreviations
#' 
#' @export
#'
#' @examples
use_iso_countries <- function(.iea_df, 
                              country = "Country"){
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
  
}



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
    # Eliminate aggregation rows.  We'll do our own aggregation if we need it.  
    
    # Gather into a tidy data frame.
    tidyr::gather(!!as.name(year), !!as.name(energy), -c(country, ledger_side, flow_aggregation_point, flow, product))
  
  
  
  
  
  # IEAData2 <- IEAData1 %>%
  #   tidyr::gather(year, energy, -c(Country, Ledger.side, Flow, Flow.aggregation.point, Product))
  
}