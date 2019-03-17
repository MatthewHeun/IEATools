#' Rename columns of an IEA data frame
#' 
#' The IEA data has columns named `COUNTRY`, `FLOW`, and `PRODUCT`.
#' This function turns off the shouting, 
#' renaming the columms (by default) to `Country`, `Flow`, and `Product`.
#'
#' @param .iea_df a data frame produced by [iea_df()]
#' @param country the original name for the country column. (Default is `COUNTRY`.)
#' @param new_country the new name for the country column. (Default is `Country`.)
#' @param flow the original name for the flow column. (Default is `FLOW`.)
#' @param new_flow the new name for the flow column. (Default is `Flow`.)
#' @param product the original name for the product column. (Default is `PRODUCT`.)
#' @param new_product the new name for the product column. (Default is `Product`.)
#'
#' @return `.iea_df` with renamed columns
#' 
#' @export
#'
#' @examples
#' iea_df(text = ",,TIME,1960,1961\nCOUNTRY,FLOW,PRODUCT\nWorld,Production,Hard coal,42,43") %>% 
#'   rename_iea_df_cols()
rename_iea_df_cols <- function(.iea_df, 
                               country = "COUNTRY", new_country = "Country", 
                               flow = "FLOW", new_flow = "Flow", 
                               product = "PRODUCT", new_product = "Product"){
  .iea_df %>% 
    dplyr::rename(!!new_country := !!country,
                  !!new_flow := flow,
                  !!new_product := product)
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
#'   system.file(package = "IEATools") %>% 
#'   iea_df() %>%
#'   rename_iea_df_cols() %>% 
#'   use_iso_countries()
use_iso_countries <- function(.iea_df, 
                              country = "Country"){
  # Load country code information
  country.name.en <- as.name("country.name.en") # Eliminates a warning.
  iso2c <- as.name("iso2c")
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
      iso2c := dplyr::case_when(
        # Add "World" to the Country column to preserve world data, if present.
        !!as.name(country) == "World" ~ "World", 
        TRUE ~ iso2c
      )
    ) %>% 
    dplyr::select(-!!as.name(country)) %>% 
    dplyr::rename(!!as.name(country) := iso2c) %>% 
    dplyr::select(!!as.name(country), dplyr::everything()) %>% 
    # The effect of the next line is to eliminate non-countries from the data set.
    # For example, OECD, IEA, etc. are not Countries, so they are dropped here.
    dplyr::filter(!is.na(country))
}


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
#' @param memo_product_prefixes a string prefix for product memo rows in `.iea_df`.
#'
#' @return `.iea_df` without its aggregation rows
#' 
#' @export
#'
#' @examples
#' file.path("extdata", "GH-ZA-ktoe-Extended-Energy-Balances-sample.csv") %>% 
#'   system.file(package = "IEATools") %>% 
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


#' Augment an IEA data frame
#' 
#' This function prepares an IEA data frame created by [iea_df()] for use in R.
#' 
#' This function solves several problems.
#' The first problem is that metadata in the `COUNTRY`, `FLOW`, and `PRODUCT`
#' columns of an IEA data table are not unique.
#' To solve this problem, two additional columns are added: `Ledger.side` and `Flow.aggregation.point`.
#' `Ledger.side` can be one of "`Supply`" or "`Consumption`", corresponding to the top or bottom of the IEA's tables, respectively.
#' `Flow.aggregation.point` indicates the next level of aggregation for these data. 
#' `Flow.aggregation.point` can be one of 
#' "`Total primary energy supply`", "`Transformation processes`", "`Energy industry own use`", or "`TFC compare`"
#' on the `Supply` side of the ledger.
#' On the `Consumption` side of the ledger, `Flow.aggregation.point` can be one of 
#' "`Industry`", "`Transport`", "`Other`", or "`Non-energy use`".
#' 
#' The second problem this function solves is that energy type and units are not specified in IEA data.
#' An `Energy.type` column is added with the value of `energy_type_val`. 
#' (Default is `E`, for energy, as opposed to `X`, which would be exergy.)
#' A `Unit` column is added with the value of `unit_val`.
#' (Default is `ktoe`, although any string can be specified in `unit_val`.)
#' 
#' Note that this function decides where to divide `Supply` from `Consumption`. 
#' To do so, it first looks for rows in which `Flow` is "`Losses`".
#' The last "`Losses`" row is the last row of the `Supply` side of the ledger. 
#' If "`Losses`" rows are not found, the function looks for rows in which `Flow` is "`Total final consumption`".
#' The first "`Total final consumption`" row is the first row of the `Consumption` side of the ledger.
#' If neither "`Losses`" nor "`Total final consumption`" `Flow`s are present, 
#' an error is generated.
#'
#' @param .iea_df a data frame produced by the [iea_df()] function
#' @param country the name of the country column in `.iea_df`. Default is "`Country`".
#' @param ledger_side the name of the ledger side column to be added to `.iea_df`. Default is "`Ledger.side`".
#' @param flow_aggregation_point the name of the flow aggregation point column to be added to `.iea_df`. Default is "`Flow.aggregation.point`".
#' @param flow the name of the flow column in `.iea_df`.  Default is "`Flow`".
#' @param energy_type the name of the energy type column to be added to `.iea_df`. Default is "`Energy.type`.
#' @param energy_type_val the value to put in the `energy_type` column. Default is "`E`".
#' @param unit the name of the unit column to be added to `.iea_df`. Default is "`Unit`".
#' @param unit_val the value to put in the `unit` column. Default is "`ktoe`" for kilotons of oil equivalent.
#' @param supply the string that identifies supply `Ledger.side`. Default is "`Supply`".
#' @param consumption the string that identifies consumption `Ledger.side`. Default is "`Consumption`".
#' @param tpes the string that identifies total primary energy supply `Flow.aggregation.point`. Default is "`Total primary energy supply`.
#' @param tpes_flows a vector of strings that give flows that are aggregated to `Total primary energy supply`. 
#' @param tfc_compare a string that identifies the `TFC compare` flow aggregation point. Default is `TFC compare`.
#' @param tfc_compare_flows a vector of strings that give `Flow`s that are aggregated to `TFC compare`.
#' @param losses the string that indicates losses in the `Flow` column. Default is "`Losses`".
#' @param transformation_processes the string that indicates transformation processes in the `Flow` column. Default is "`Transformation processes`".
#' @param tp_flows_suffix the suffix for transformation processes in the `Flow` column. Default is "`(transf.)`".
#' @param nstp_flows_suffix the suffix for non-specified transformation processes in the `Flow` column. Default is "`(transformation)`".
#' @param eiou the string that identifies energy industry own use in the `Flow` column. Default is "`Energy industry own use`".
#' @param eiou_flows_suffix the suffix for energy industry own use in the `Flow` column. Default is "`(energy)`".
#' @param tfc the string that identifies total final consumption in the `Flow` column. Default is "`Total final consumption`".
#' @param tfc_flows a vector of strings that give total final consumption in the `Flow` column.
#' @param industry a string that names the industry `Flow.aggregation.point`. Default is "`Industry`".
#' @param industry_flows a vector of strings representing `Flow`s to be aggregated in the `Industry` `Flow.aggregation.point`. 
#' @param transport a string that names the transport `Flow.aggregation.point`. Default is "`Transport`".
#' @param transport_flows a vector of strings representing `Flow`s to be aggregated in the `Transport` `Flow.aggregation.point`. 
#' @param other a string that names the other `Flow.aggregation.point`. Default is "`Other`".
#' @param other_flows a vector of strings representing `Flow`s to be aggregated in the `Other` `Flow.aggregation.point`. 
#' @param non_energy a string that names the non-energy `Flow.aggregation.point`. Default is "`Non-energy use`".
#' @param non_energy_prefix a string prefix for `Flow`s to be aggregated in the `Non-energy use` `Flow.aggregation.point`. 
#' @param electricity_output a string that names the electricity output `Flow`. Default is "`Electricity output (GWh)`". 
#' @param electricity_output_flows_prefix a string prefix for `Flow`s to be aggregated in electricity output. Default is "`Electricity output (GWh)-`".
#' @param heat_output a string that names the heat output `Flow`. Default is "`Heat output`". 
#' @param heat_output_flows_prefix a string prefix for `Flow`s to be aggregated in heat output. Default is "`Heat output-`".
#' @param .rownum the name of a column created (and destroyed) internally by this function. 
#'        The `.rownum` column temporarily holds row numbers for internal calculations.
#'        The `.rownum` column is deleted before returning. 
#'
#' @return `.iea_df` with additional columns named `ledger_side`, `flow_aggregation_point`, `energy_type`, and `unit`.
#' 
#' @export
#'
#' @examples
#' iea_df(text = paste0(",,TIME,1960,1961\n",
#'                        "COUNTRY,FLOW,PRODUCT\n",
#'                        "World,Production,Hard coal (if no detail),42,43\n",
#'                        "World,Losses,Hard coal (if no detail),1,2")) %>% 
#'   rename_iea_df_cols() %>% 
#'   augment_iea_df()
augment_iea_df <- function(.iea_df, 
                           country = "Country", 
                           ledger_side = "Ledger.side", flow_aggregation_point = "Flow.aggregation.point", flow = "Flow", 
                           energy_type = "Energy.type", energy_type_val = "E",
                           unit = "Unit", unit_val = "ktoe",
                           supply = "Supply", consumption = "Consumption",
                           tpes = "Total primary energy supply", 
                           tpes_flows = c("Production", "Imports", "Exports", "International marine bunkers", "International aviation bunkers", "Stock changes"),
                           tfc_compare = "TFC compare",
                           tfc_compare_flows = c("Total primary energy supply", "Transfers", "Statistical differences", "Transformation processes", "Energy industry own use", "Losses"),
                           losses = "Losses", 
                           transformation_processes = "Transformation processes",
                           tp_flows_suffix = "(transf.)",
                           nstp_flows_suffix = "(transformation)",
                           eiou = "Energy industry own use",
                           eiou_flows_suffix = "(energy)",
                           tfc = "Total final consumption",
                           tfc_flows = c("Industry", "Transport", "Other", "Non-energy use"),
                           industry = "Industry",
                           industry_flows = c("Iron and steel", "Chemical and petrochemical", "Non-ferrous metals", "Non-metallic minerals", "Transport equipment", "Machinery", "Mining and quarrying", "Food and tobacco", "Paper, pulp and print", "Wood and wood products", "Construction", "Textile and leather", "Non-specified (industry)"), 
                           transport = "Transport",
                           transport_flows = c("World aviation bunkers", "Domestic aviation", "Road", "Rail", "Pipeline transport", "World marine bunkers", "Domestic navigation", "Non-specified (transport)"),
                           other = "Other",
                           other_flows = c("Residential", "Commercial and public services", "Agriculture/forestry", "Fishing", "Non-specified (other)"),
                           non_energy = "Non-energy use",
                           non_energy_prefix = "Non-energy use",
                           electricity_output = "Electricity output (GWh)",
                           electricity_output_flows_prefix = "Electricity output (GWh)-",
                           heat_output = "Heat output",
                           heat_output_flows_prefix = "Heat output-",
                           .rownum = ".rownum"){
  .iea_df %>% 
    # Eliminate rownames, leaving only numbers
    tibble::remove_rownames() %>% 
    dplyr::group_by(!!as.name(country)) %>% 
    # The split between Supply and Consumption ledger sides occurs where Flow == Losses and Flow == Total final consumption.
    # Find this dividing line in .iea_df. 
    # Then create the Ledger.side column. 
    dplyr::group_map(function(ctry_tbl, ctry){
      # At this point, 
      # ctry_tbl is the rows for this country, and
      # ctry is a data frame with one country column and one country row containing the country.
      with_row_nums <- ctry_tbl %>% 
        tibble::rownames_to_column(var = .rownum) %>% 
        dplyr::mutate(
          !!as.name(.rownum) := as.numeric(!!as.name(.rownum))
        )
      # Find the break point between the Supply side and the Consumption side of the ledger.
      # We'll find the break point by identifying the last supply row in the data frame.
      # We'll take two runs at this.
      # One by looking for "Losses" in the Flow column (the end of the supply side) and 
      # the other by looking for "Total final consumption" in the Flow column (the beginning of the consumption side).
      loss_rows <- with_row_nums %>% 
        dplyr::filter(!!as.name(flow) == losses)
      if (nrow(loss_rows) > 0) {
        # First, calculate the last row of Losses. last_loss_row is the last row of the supply side of the ledger.
        last_supply_row <- loss_rows %>% magrittr::extract2(.rownum) %>% max()
      } else {
        # Look for the first row that is Total final consumption and subtract one.
        tfc_rows <- with_row_nums %>% 
          dplyr::filter(!!as.name(flow) == tfc)
        if (nrow(tfc_rows) > 0) {
          last_supply_row <- tfc_rows %>% magrittr::extract2(.rownum) %>% min() - 1
        } else {
          # Everything failed. Throw an error
          stop(paste("Found neither", losses, "nor", tfc, "in the", flow, "column."))
        }
      }
      
      with_row_nums %>% 
        dplyr::mutate(
          !!as.name(ledger_side) := dplyr::case_when(
            !!as.name(.rownum) <= last_supply_row ~ supply,
            TRUE ~ consumption
          )
        )
    }) %>% 
    dplyr::mutate(
      # Now add the Flow.aggregation.point column
      !!as.name(flow_aggregation_point) := dplyr::case_when(
        !!as.name(ledger_side) == supply & !!as.name(flow) %in% tpes_flows ~ tpes, 
        !!as.name(ledger_side) == supply & !!as.name(flow) %in% tfc_compare_flows ~ tfc_compare,
        !!as.name(ledger_side) == supply & endsWith(!!as.name(flow), tp_flows_suffix) ~ transformation_processes,
        !!as.name(ledger_side) == supply & endsWith(!!as.name(flow), nstp_flows_suffix) ~ transformation_processes,
        !!as.name(ledger_side) == supply & endsWith(!!as.name(flow), eiou_flows_suffix) ~ eiou,
        !!as.name(ledger_side) == consumption & !!as.name(flow) == tfc ~ NA_character_,
        !!as.name(ledger_side) == consumption & !!as.name(flow) %in% tfc_flows ~ tfc,
        !!as.name(ledger_side) == consumption & !!as.name(flow) %in% industry_flows ~ industry,
        !!as.name(ledger_side) == consumption & !!as.name(flow) %in% transport_flows ~ transport,
        !!as.name(ledger_side) == consumption & !!as.name(flow) %in% other_flows ~ other,
        !!as.name(ledger_side) == consumption & startsWith(!!as.name(flow), non_energy_prefix) ~ non_energy,
        !!as.name(ledger_side) == consumption & startsWith(!!as.name(flow), electricity_output_flows_prefix) ~ electricity_output,
        !!as.name(ledger_side) == consumption & startsWith(!!as.name(flow), heat_output_flows_prefix) ~ heat_output,
        TRUE ~ NA_character_
      ), 
      # Add energy type column
      !!energy_type := energy_type_val,
      # Add the Unit column
      !!unit := unit_val
    ) %>% 
    # Finally, reorder the columns, remove the .rownum column, and return
    dplyr::select(-.rownum) %>% 
    dplyr::select(country, ledger_side, flow_aggregation_point, energy_type, unit, dplyr::everything()) %>% 
    # Remove the grouping that we created.
    dplyr::ungroup()
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
#' @param e_dot the name of the energy/exergy value column created in `.iea_df` by this function. (Default is "`E.dot`".)
#' @param country the name of the country column in `.iea_df`. (Default is "`Country`".)
#' @param ledger_side the name of the ledger side in `.iea_df`. (Default is "`Ledger.side`".)
#' @param flow_aggregation_point the name of the flow aggregation point column in `.iea_df`. (Default is "`Flow.aggregation.point`".)
#' @param energy_type the name of the energy type column in `.iea_df`. (Default is "`Energy.type`".)
#' @param unit the name of the unit column in `.iea_df`. (Default is "`Units`".)
#' @param flow the name of the flow column in `.iea_df`. (Default is "`Flow`".)
#' @param product the name of the product column in `.iea_df`. (Default is "`Product`".)
#' @param remove_zeroes a logical indicating whether data points with the value `0` are to be removed from the output. (Default is `TRUE`.)
#'
#' @return a tidy version of `.iea_df` containing new columns `year` and `e_dot` and, optionally, `0` values removed
#' 
#' @export
#'
#' @examples
#' file.path("extdata", "GH-ZA-ktoe-Extended-Energy-Balances-sample.csv") %>% 
#'   system.file(package = "IEATools") %>% 
#'   iea_df() %>%
#'   rename_iea_df_cols() %>% 
#'   remove_agg_memo_flows() %>% 
#'   use_iso_countries() %>% 
#'   augment_iea_df() %>% 
#'   tidy_iea_df()
tidy_iea_df <- function(.iea_df, 
                          year = "Year", e_dot = "E.dot", 
                          country = "Country", ledger_side = "Ledger.side", 
                          flow_aggregation_point = "Flow.aggregation.point", 
                          energy_type = "Energy.type", unit = "Unit", 
                          flow = "Flow", product = "Product",
                          remove_zeroes = TRUE){
  out <- .iea_df %>% 
    # Gather into a tidy data frame.
    tidyr::gather(key = !!as.name(year), value = !!as.name(e_dot), -c(country, ledger_side, flow_aggregation_point, flow, product,
                                                                   energy_type, unit)) %>% 
    # Set the column order to something rational
    dplyr::select(country, year, ledger_side, flow_aggregation_point, energy_type, unit, flow, product, e_dot)
  if (remove_zeroes) {
    out <- out %>% 
      dplyr::filter(!(!!as.name(e_dot) == 0))
  }
  return(out)
}