#' Remove aggregation and memo rows from data frames of IEA data
#' 
#' Aggregation and memo rows are included with IEA data.
#' Sometimes, it is convenient to remove those rows. 
#' This function does so, using default identifying strings for aggregations and memos.
#' 
#' Note that the IEA data somtimes includes a variable number of spaces 
#' before the "Memo: " string. 
#' This function ignores all leading spaces in the `Flow` and `Product` columns
#' before searching for `Flow` and `Product` prefixes.
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
                                  memo_product_prefixes = c("Memo: ", "Total")){
  .iea_df %>% 
    dplyr::mutate(
      # These regular expression patterns match any number (+) of spaces ( ) at the beginning of the strings (^).
      !!as.name(flow) := gsub(pattern = "^ +", replacement = "", x = !!as.name(flow)),
      !!as.name(product) := gsub(pattern = "^ +", replacement = "", x = !!as.name(product))
    ) %>% 
    # Remove Flow aggregations
    dplyr::filter(!(!!as.name(flow) %in% agg_flows)) %>%
    # Remove Flow memos
    dplyr::filter(!starts_with_any_of(!!as.name(flow), memo_flow_prefixes)) %>% 
    # Remove Product memos
    dplyr::filter(!starts_with_any_of(!!as.name(product), memo_product_prefixes))
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
#' file.path("extdata", "GH-ZA-ktoe-Extended-Energy-Balances-sample.csv") %>% 
#'   system.file(package = "IEAData") %>% 
#'   iea_df() %>%
#'   rename_iea_df_cols() %>% 
#'   use_iso_countries()
use_iso_countries <- function(.iea_df, 
                              country = "Country"){
  # Load country code information
  CountryInfo <- countrycode::codelist %>% 
    dplyr::select(country.name.en, iso2c) %>% 
    dplyr::rename(
      !!as.name(country) := country.name.en
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
  .iea_df %>%
    dplyr::left_join(CountryInfo, by = c("Country")) %>% # left_join preserves all rows of IEA data
    dplyr::mutate(
      iso2c := case_when(
        # Add "World" to the Country column to preserve world data, if present.
        !!as.name(country) == "World" ~ "World", 
        TRUE ~ iso2c
      )
    ) %>% 
    dplyr::select(-!!as.name(country)) %>% 
    dplyr::rename(!!as.name(country) := iso2c) %>% 
    dplyr::select(!!as.name(country), everything()) %>% 
    # The effect of the next line is to eliminate non-countries from the data set.
    # For example, OECD, IEA, etc. are not Countries, so they are dropped here.
    filter(!is.na(Country))
}



#' Creates a tidy IEA data frame
#' 
#' Data from the IEA have years in columns, 
#' but the [tidy data format](https://doi.org/10.18637/jss.v059.i10)
#' requires one row for each datum.
#' This function uses [tidyr::gather()] to 
#' make an IEA data frame tidy.
#' 
#' Default argument values assume that [rename_iea_df_cols()] has been called on `.iea_df`.
#'
#' @param .iea_df a IEA data frame whose columns have been renamed by [rename_iea_df_cols()]
#' @param year the name of the year column created in `.iea_df` by this function. (Default is "`Year`".)
#' @param year the name of the energy/exergy value column createdin `.iea_df` by this function. (Default is "`EX`".)
#' @param country the name of the country column in `.iea_df`. (Default is "`Country`".)
#' @param ledger_side the name of the ledger side in `.iea_df`. (Default is "`Ledger.side`".)
#' @param flow_aggregation_point the name of the flow aggregation point column in `.iea_df`. (Default is "`Flow.aggregation.point`".)
#' @param energy_type the name of the energy type column in `.iea_df`. (Default is "`Energy.type`".)
#' @param units the name of the units column in `.iea_df`. (Default is "`Units`".)
#' @param flow the name of the flow column in `.iea_df`. (Default is "`Flow`".)
#' @param product the name of the product column in `.iea_df`. (Default is "`Product`".)
#' @param remove_zeroes a logical indicating whether data points with the value `0` are to be removed from the output. (Default is `TRUE`.)
#'
#' @return a tidy version of `.iea_df` containing new columns `year` and `ex` and, optionally, `0` values removed
#' 
#' @export
#'
#' @examples
#' file.path("extdata", "GH-ZA-ktoe-Extended-Energy-Balances-sample.csv") %>% 
#'   system.file(package = "IEAData") %>% 
#'   iea_df() %>%
#'   rename_iea_df_cols() %>% 
#'   remove_agg_memo_flows() %>% 
#'   use_iso_countries() %>% 
#'   tidy_iea()
tidy_iea <- function(.iea_df, 
                     year = "Year", ex = "EX", 
                     country = "Country", ledger_side = "Ledger.side", 
                     flow_aggregation_point = "Flow.aggregation.point", 
                     energy_type = "Energy.type", units = "Units", 
                     flow = "Flow", product = "Product",
                     remove_zeroes = TRUE){
  out <- .iea_df %>% 
    # Gather into a tidy data frame.
    tidyr::gather(key = !!as.name(year), value = !!as.name(ex), -c(country, ledger_side, flow_aggregation_point, flow, product,
                                                                   energy_type, units)) %>% 
    # Set the column order to something rational
    dplyr::select(country, year, ledger_side, flow_aggregation_point, energy_type, units, flow, product, ex)
  if (remove_zeroes) {
    out <- out %>% 
      filter(!(!!as.name(ex) == 0))
  }
  return(out)
}