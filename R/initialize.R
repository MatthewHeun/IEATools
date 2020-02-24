#' Load IEA data from an extended enegy balances csv file
#'
#' This function reads an IEA extended energy balances file and
#' converts it to a data frame with appropriately-labeled columns.
#' One of `iea_file` or `text` must be specified, but not both.
#' The first line of `iea_file` or `text`
#' is expected to start with `expected_start_1st_line`, and
#' the second line is expected to start with `expected_2nd_line_start`, and
#' it may have any number of commas appended.
#' (The extra commas might come from opening and re-saving the file in Excel.)
#' Alternatively, the file may start with `expected_simple_start`.
#' If none of these conditions are not met, execution is halted, and
#' an error message is provided.
#' Files should have a return character at the end of their final line.
#' 
#' This function is designed to work even as more years are added
#' in columns at the right of `.iea_file`, 
#' because column names in the output are constructed from the header line(s) of `.iea_file` 
#' (which contain years and country, flow, product information).
#' 
#' The IEA data have indicators for 
#' not applicable values ("`x`") and for
#' unavailable values ("`..`"). 
#' (See "World Energy Balances: Database Documentation (2018 edition)" at
#' <http://wds.iea.org/wds/pdf/worldbal_documentation.pdf>.)
#' `R` has three concepts that could be used for "`x`" and "`..`":
#' `0` would indicate value known to be zero.
#' `NULL` would indicate an undefined value.
#' `NA` would indicate a value that is unavailable.
#' In theory, mapping from the IEA's indicators to `R` should proceed as follows:
#' "`..`" (unavailable) in the IEA data would be converted to `NA` in `R`.
#' "`x`" (not applicable) in the IEA data would be converted to `0` in `R`.
#' "`NULL`" would not be used.
#' However, the IEA are not consistent with their coding. 
#' In some places "`..`" (indicating unavailable) is used for not applicable values, 
#' e.g., World Anthracite supply in 1971. 
#' (World Anthracite supply in 1971 is actually not applicable, because Anthracite was
#' classified under "Hard coal (if no detail)" in 1971.)
#' On the other hand, "`..`" is used for data in the most recent year 
#' when those data have not yet been incorporated into the database. 
#' In the face of IEA’s inconsistencies, 
#' the only rational way to proceed is to convert 
#' both "`x`" and "`..`" in the IEA files to "`0`" in the output data frame
#' from this function.
#' Furthermore, confidential data (coded by the IEA as "`c`") is also interpreted as `0`.
#' (What else can we do?)
#' 
#' The data frame returned from this function is not ready to be used in R, 
#' because rows are not unique.
#' To further prepare the data frame for use, call [augment_iea_df()],
#' passing the output of this function to the `.iea_df` argument of [augment_iea_df()].
#'
#' @param .iea_file a string containing the path to a .csv file of extended energy balances from the IEA.
#'        Default is the path to a sample IEA file provided in this package.
#' @param text a character string that can be parsed as IEA extended energy balances. 
#'        (This argument is useful for testing.)
#' @param expected_1st_line_start the expected start of the first line of `iea_file`. Default is ",,TIME".
#' @param expected_2nd_line_start the expected start of the second line of `iea_file`. Default is "COUNTRY,FLOW,PRODUCT".
#' @param expected_simple_start the expected starting of the first line of `iea_file`. Default is the value of `expected_2nd_line_start`.
#'        Note that `expected_simple_start` is sometimes encountered in data supplied by the IEA.
#'        Furthermore, `expected_simple_start` could be the format of the file when somebody "helpfully" fiddles with 
#'        the raw data from the IEA.
#' @param missing_data a string that identifies missing data. Default is "`..`".
#'        Entries of `missing_data` are coded as `0`` in output.
#' @param not_applicable_data a string that identifies not-applicable data. Default is "x".
#'        Entries of `not_applicable_data` are coded as `0` in output.
#' @param confidential_data a string that identifies confidential data. Default is "c".
#'        Entries of `confidential_data` are coded as `0` in output.
#' @param estimated_year a string that identifies an estimated year. 
#'        Default is "E".
#'        E.g., in "2014E", the "E" indicates that data for 2014 are estimated.
#'        Data from estimated years are removed from output.
#'
#' @return a data frame containing the IEA extended energy balances data
#' 
#' @export
#' 
#' @examples 
#' # Original file format
#' iea_df(text = paste0(",,TIME,1960,1961\n",
#'                      "COUNTRY,FLOW,PRODUCT\n",
#'                      "World,Production,Hard coal (if no detail),42,43"))
#' # With extra commas on the 2nd line
#' iea_df(text = paste0(",,TIME,1960,1961\n",
#'                      "COUNTRY,FLOW,PRODUCT,,,\n",
#'                      "World,Production,Hard coal (if no detail),42,43"))
#' # With a clean first line
#' iea_df(text = paste0("COUNTRY,FLOW,PRODUCT,1960,1961\n",
#'                      "World,Production,Hard coal (if no detail),42,43"))
iea_df <- function(.iea_file = NULL, 
                   text = NULL, 
                   expected_1st_line_start = ",,TIME", expected_2nd_line_start = "COUNTRY,FLOW,PRODUCT", 
                   expected_simple_start = expected_2nd_line_start,
                   missing_data = "..", not_applicable_data = "x", confidential_data = "c", 
                   estimated_year = "E"){
  assertthat::assert_that(xor(is.null(.iea_file), is.null(text)), 
                          msg = "need to supply one but not both of .iea_file and text arguments to iea_df")
  if (!is.null(.iea_file)) {
    conn <- file(.iea_file, open = "rt") # open file connection
  } else {
    # text has been provided, probably for testing purposes.
    conn <- textConnection(text)
  }
  
  # Check if the first line has the simple format
  first_two_lines <- conn %>% readLines(n = 2)
  close(conn)
  assertthat::assert_that(length(first_two_lines) == 2, msg = "couldn't read 2 lines in iea_df")
  first_line <- first_two_lines[[1]]
  second_line <- first_two_lines[[2]]
  # Ensure that we have an expected format for the first line or two in first_two_lines.
  assertthat::assert_that(first_line %>% startsWith(expected_simple_start) | 
                            (first_line %>% startsWith(expected_1st_line_start) & second_line %>% startsWith(expected_2nd_line_start)), 
                          msg = paste0(".iea_file didn't start with '",
                                 expected_simple_start,
                                 "' or '",
                                 expected_1st_line_start,
                                 "\n",
                                 expected_2nd_line_start,
                                 "'."))
  if (first_line %>% startsWith(expected_simple_start)) {
    # We have the simple start to the file, so we can assume a one-line header.
    if (!is.null(.iea_file)) {
      IEAData_withheader <- data.table::fread(file = .iea_file, header = TRUE, sep = ",")
    } else {
      IEAData_withheader <- data.table::fread(text = text, header = TRUE, sep = ",")
    }
    cnames <- colnames(IEAData_withheader)
  } else if (first_line %>% startsWith(expected_1st_line_start) & 
             second_line %>% startsWith(expected_2nd_line_start)) {
    # We have the complicated start to the file, so go through some additional work to apply the proper header
    # to the file.
    if (second_line %>% endsWith(",")) {
      # The file may have been opened in Excel and resaved.
      # When that occurs, many commas are appended to the 2nd line.
      # Strip out these commas before proceeding further.
      # The pattern ,*$ means "match any number (*) of commas (,) at the end of the line ($)".
      second_line <- sub(pattern = ",*$", replacement = "", second_line)
    }
    if (!is.null(.iea_file)) {
      # Slurp the file. This slurping ignores the header, which we know are the first 2 lines.
      # Note that I'm using data.table::fread at the recommendation of
      # https://statcompute.wordpress.com/2014/02/11/efficiency-of-importing-large-csv-files-in-r/
      # which indicates this function is significantly faster than other options.
      IEAData_noheader <- data.table::fread(file = .iea_file, header = FALSE, sep = ",", skip = 2)
    } else {
      IEAData_noheader <- data.table::fread(text = text, header = FALSE, sep = ",", skip = 2)
    }
    # At this point, the IEAData_noheader data frame has default (meaningless) column names, V1, V2, V3, ...
    # Create column names from the header lines that we read previously.
    # The code here should be robust to adding more years through time,
    # because it simply replaces the first 3 items of the first line
    # with appropriate values from the 2nd line.
    cnames <- gsub(pattern = expected_1st_line_start, replacement = expected_2nd_line_start, first_line) %>%
      strsplit(",") %>%
      unlist()
    IEAData_withheader <- IEAData_noheader %>%
      magrittr::set_names(cnames)
  }
  
  # At this point, IEAData_withheader may have some column names that end in estimated_year.
  # We should delete those columns.
  cols_to_delete <- grepl(pattern = paste0(estimated_year, "$"), x = cnames)
  # cols_to_delete has TRUE for each column to eliminate. But we need true for each column to KEEP.
  # With [!cols_to_delete], we get the desired effect.
  IEAData_withheader <- IEAData_withheader[!cols_to_delete]

  # Data tagged as not-applicable in the IEA database should be coded as 0.
  # We still want to allow calculations with these data.
  IEAData_withheader[IEAData_withheader == not_applicable_data] <- 0
  # However, missing data should be tagged as "not available", because calculations
  # with unavailable data should fail.
  # However, there are so many pieces of missing data, we code them as "0" for now.
  IEAData_withheader[IEAData_withheader == missing_data] <- 0
  # Data tagged as confidential are converted to 0.
  # What else can we do?
  IEAData_withheader[IEAData_withheader == confidential_data] <- 0

  # Convert all year columns (columns whose names are all numbers) to numeric,
  # convert into a data frame, and
  # return.
  IEAData_withheader %>%
      dplyr::mutate_at(dplyr::vars(year_cols(IEAData_withheader)), as.numeric) %>%
      as.data.frame()
}


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


#' Clean whitespace from Flow and Product strings
#' 
#' Occasionally, in the IEA extended energy balance data, 
#' extra whitespace characters are found at the beginning or end of `Flow` and `Product` strings.
#' This function removes all leading and trailing whitespece.
#'
#' @param .iea_df a data frame containing `Flow` and `Product` columns
#' @param flow the name of the flow column in `iea_df`. Default is "`Flow`".
#' @param product the name of the product columns in `iea_df`. Default is "`Product`".
#'
#' @return `.iea_df` with leading and trailing whitespace removed from `Flow` and `Product` column strings
#' 
#' @export
#'
#' @examples
#' data.frame(Flow = "  a flow   ", Product = "   a product   ", stringsAsFactors = FALSE) %>% 
#'   clean_iea_whitespace()
clean_iea_whitespace <- function(.iea_df, 
                                 flow = "Flow", 
                                 product = "Product"){
  .iea_df %>% 
    dplyr::mutate(
      # These regular expression patterns match any number (+) of whitespace characters (\\s) at the beginning of the strings (^).
      !!as.name(flow) := gsub(pattern = "^\\s+", replacement = "", x = !!as.name(flow)),
      !!as.name(product) := gsub(pattern = "^\\s+", replacement = "", x = !!as.name(product)),
      # These regular expression patterns match any number (+) of whitespace characters (\\s) at the end of the strings ($).
      !!as.name(flow) := gsub(pattern = "\\s+$", replacement = "", x = !!as.name(flow)),
      !!as.name(product) := gsub(pattern = "\\s+$", replacement = "", x = !!as.name(product))
    )
}


#' Replace country names with 2-letter ISO abbreviations
#' 
#' The IEA uses full country names, but it is more concise to use the 2-letter ISO abbreviations.
#' This function replaces the full country names with ISO abbreviations where possible.
#'
#' @param .iea_df a data frame containing a `country` column
#' @param country the name of the country column in `.iea_df`. Default is "`Country`".
#' @param iso_abbrev_type an integer, either `2` for 2-letter abbreviations or `3` for 3-letter abbreviations. Default is 3.
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
                              country = "Country",
                              iso_abbrev_type = 3){
  # Load country code information
  country.name.en <- "country.name.en" # Eliminates a warning.
  assertthat::assert_that(iso_abbrev_type %in% c(2, 3))
  if (iso_abbrev_type == 2) {
    iso_type = "iso2c"
  } else if (iso_abbrev_type == 3) {
    iso_type = "iso3c"
  } 
  CountryInfo <- countrycode::codelist %>%
    dplyr::select(!!as.name(country.name.en), !!as.name(iso_type)) %>% 
    dplyr::rename(
      !!as.name(country) := country.name.en
    )
  .iea_df %>%
    dplyr::left_join(CountryInfo, by = c("Country")) %>% # left_join preserves all rows of IEA data
    # If there is no ISO abbreviation for the country name, 
    # we set the ios2c column to be the same as the country column.
    # This step preserves all countries, even if they don't have a 2-letter ISO abbreviation.
    dplyr::mutate(
      !!as.name(iso_type) := dplyr::case_when(
        is.na(!!as.name(iso_type)) ~ !!as.name(country), 
        TRUE ~ !!as.name(iso_type)
      )
    ) %>% 
    # Now we can get rid of the country column.
    dplyr::select(-!!as.name(country)) %>% 
    # And rename the iso_type column to be country
    dplyr::rename(!!as.name(country) := iso_type) %>% 
    # And put the country column first.
    dplyr::select(!!as.name(country), dplyr::everything())
}


#' Remove aggregation and memo rows from data frames of IEA data
#' 
#' Aggregation and memo rows are included with IEA data.
#' Sometimes, it is convenient to remove those rows. 
#' This function does so, using default identifying strings for aggregations and memos.
#' 
#' Note that the IEA data somtimes includes a variable number of spaces 
#' before the "Memo: " string. 
#' There are several places where trailing spaces are found, such as "Nuclear industry ".
#' This function strips all leading and trailing spaces in the `Flow` and `Product` columns.
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
                                  agg_flows = IEATools::aggregation_flows,
                                  memo_flow_prefixes = IEATools::memo_aggregation_flow_prefixes, 
                                  memo_product_prefixes = IEATools::memo_aggregation_product_prefixes){
  .iea_df %>% 
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
#' A second problem is that the `FLOW` column contains both industries to which energy is flowing _and_ 
#' the type of flow that is involved.  
#' (E.g., the suffix "`(energy)`" means that the flow is an own use by the energy industry.
#' The "`(transf.)`" suffix means that a flow is involved in a transformation process
#' between primary and final energy. 
#'
#' To solve these problems, two additional columns are added: `Ledger.side` and `Flow.aggregation.point`.
#' `Ledger.side` can be one of "`Supply`" or "`Consumption`", corresponding to the top or bottom of the IEA's tables, respectively.
#' `Flow.aggregation.point` indicates the next level of aggregation for these data. 
#' `Flow.aggregation.point` can be one of 
#' "`Total primary energy supply`", "`Transformation processes`", "`Energy industry own use`", or "`TFC compare`"
#' on the `Supply` side of the ledger.
#' On the `Consumption` side of the ledger, `Flow.aggregation.point` can be one of 
#' "`Industry`", "`Transport`", "`Other`", or "`Non-energy use`".
#' When the `Flow.aggregation.point` column is present, 
#' the need for the "`(energy)`" and "`(transf.)`" suffixes is eliminated,
#' so they are deleted.
#' 
#' The third problem this function solves is that energy type and units are not specified in IEA data.
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
#' @param product the name of the product column in `.iea_df`.  Default is "`Product`".
#' @param energy_type the name of the energy type column to be added to `.iea_df`. Default is "`Energy.type`.
#' @param energy_type_val the value to put in the `energy_type` column. Default is "`E`".
#' @param method the name of the method column to be added to `.iea_df`. Default is "`Method`.
#' @param method_val the value to put in the `method` column. Default is "`PCM`" (Physical Content Method, which is used by the IEA).
#' @param last_stage the name of the last stage column to be added to `.iea_df`. Default is "`Last.stage`.
#' @param last_stage_val the value to put in the `last_stage` column. Default is "`Final`" (which is the last stage supplied by the IEA).
#' @param unit the name of the unit column to be added to `.iea_df`. Default is "`Unit`".
#' @param unit_val the value to put in the `unit` column. Default is "`ktoe`" for kilotons of oil equivalent.
#' @param supply the string that identifies supply `Ledger.side`. Default is "`Supply`".
#' @param consumption the string that identifies consumption `Ledger.side`. Default is "`Consumption`".
#' @param tpes the string that identifies total primary energy supply `Flow.aggregation.point`. Default is "`Total primary energy supply`.
#' @param tpes_flows a vector of strings that give flows that are aggregated to `Total primary energy supply`. 
#' @param tfc_compare a string that identifies the `TFC compare` flow aggregation point. Default is `TFC compare`.
#' @param tfc_compare_flows a vector of strings that give `Flow`s that are aggregated to `TFC compare`.
#' @param transfers = a string that identifies transfers in the flow column. Default is "`Transfers`".
#' @param statistical_differences a string that identifies statistical differences in flow column. Default is "`Statistical differences`".
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
                           ledger_side = "Ledger.side", 
                           flow_aggregation_point = "Flow.aggregation.point", 
                           flow = "Flow", 
                           product = "Product",
                           energy_type = "Energy.type", energy_type_val = "E",
                           method = "Method", method_val = "PCM",
                           last_stage = "Last.stage", last_stage_val = "Final",
                           unit = "Unit", unit_val = "ktoe",
                           supply = "Supply", 
                           consumption = "Consumption",
                           tpes = "Total primary energy supply", 
                           tpes_flows = c("Production", "Imports", "Exports", "International marine bunkers", "International aviation bunkers", "Stock changes"),
                           tfc_compare = "TFC compare",
                           tfc_compare_flows = c("Total primary energy supply", "Transfers", "Statistical differences", "Transformation processes", "Energy industry own use", "Losses"),
                           transfers = "Transfers",
                           statistical_differences = "Statistical differences",
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
    dplyr::group_modify(function(ctry_tbl, ctry){
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
      # Now that Flow.aggregation.point is present, we no longer need the (energy),  (transf.), and (transformation) suffixes, so delete them.
      # The string "\s+" means to match any number (+) of whitespace (\\s) characters.
      !!as.name(flow) := dplyr::case_when(
        # Delete the " (transf.)" suffix
        endsWith(!!as.name(flow), tp_flows_suffix) ~ gsub(pattern = paste0("\\s+", Hmisc::escapeRegex(tp_flows_suffix)), replacement = "", x = !!as.name(flow)), 
        # Delete the " (transformation)" suffix
        endsWith(!!as.name(flow), nstp_flows_suffix) ~ gsub(pattern = paste0("\\s+", Hmisc::escapeRegex(nstp_flows_suffix)), replacement = "", x = !!as.name(flow)), 
        # Delete the " (energy)" suffix
        endsWith(!!as.name(flow), eiou_flows_suffix) ~ gsub(pattern = paste0("\\s+", Hmisc::escapeRegex(eiou_flows_suffix)), replacement = "", x = !!as.name(flow)),
        TRUE ~ !!as.name(flow)
      ),
      # Add method column
      !!method := method_val,
      # Add last stage column
      !!last_stage := last_stage_val,
      # Add energy type column
      !!energy_type := energy_type_val,
      # Add the Unit column
      !!unit := unit_val
    ) %>% 
    # Finally, reorder the columns, remove the .rownum column, and return
    dplyr::select(-.rownum) %>% 
    dplyr::select(country, method, energy_type, last_stage, ledger_side, flow_aggregation_point, flow, product, unit, dplyr::everything()) %>% 
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
#' @param method the name of the method column created in `.iea_df` by this function. (Default is "`Method`".)
#' @param last_stage the name of the last stage column created in `.iea_df` by this function. (Default is "`Last.stage`".)
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
                        method = "Method", 
                        last_stage = "Last.stage",
                        country = "Country", 
                        ledger_side = "Ledger.side", 
                        flow_aggregation_point = "Flow.aggregation.point", 
                        energy_type = "Energy.type", unit = "Unit", 
                        flow = "Flow", product = "Product",
                        remove_zeroes = TRUE){
  out <- .iea_df %>% 
    # Gather into a tidy data frame.
    tidyr::gather(key = !!as.name(year), value = !!as.name(e_dot), -c(method, country, last_stage, ledger_side, 
                                                                      flow_aggregation_point, flow, product, energy_type, unit)) %>% 
    # Set the column order to something rational
    dplyr::select(country, method, energy_type, last_stage, year, ledger_side, flow_aggregation_point, flow, product, unit, e_dot) %>% 
    # Set the year column to be numeric
    dplyr::mutate(
      !!as.name(year) := as.numeric(!!as.name(year))
    )
  if (remove_zeroes) {
    out <- out %>% 
      dplyr::filter(!(!!as.name(e_dot) == 0))
  }
  return(out)
}


#' Load IEA extended energy balance data into tidy format
#' 
#' Loads an IEA extended energy balance data file in `.csv` format from disk and converts to a tidy format.
#' This function bundles several others:
#' 1. [iea_df()], 
#' 2. [rename_iea_df_cols()],  
#' 3. [use_iso_countries()],
#' 4. [remove_agg_memo_flows()],
#' 5. [augment_iea_df()], and 
#' 6. [tidy_iea_df()].
#' 
#' Each bundled function is called in turn using default arguments.
#' See examples for two ways to achieve the same result.
#' 
#' @param file_path the path of the file to be loaded. Default loads example data bundled with the package.
#' @param remove_zeroes a logical indicating whether data points with the value `0` are to be removed from the output. (Default is `TRUE`.)
#'
#' @return a tidy, augmented data frame of IEA extended energy balance data.
#' 
#' @export
#'
#' @examples
#' simple <- load_tidy_iea_df()
#' complicated <- file.path("extdata", "GH-ZA-ktoe-Extended-Energy-Balances-sample.csv") %>% 
#'   system.file(package = "IEATools") %>% 
#'   iea_df() %>%
#'   rename_iea_df_cols() %>% 
#'   clean_iea_whitespace() %>% 
#'   remove_agg_memo_flows() %>% 
#'   use_iso_countries() %>% 
#'   augment_iea_df() %>% 
#'   tidy_iea_df()
#' all(simple == complicated)
load_tidy_iea_df <- function(file_path = file.path("extdata", "GH-ZA-ktoe-Extended-Energy-Balances-sample.csv") %>% 
                               system.file(package = "IEATools"), 
                             remove_zeroes = TRUE){
  file_path %>% 
    iea_df() %>%
    rename_iea_df_cols() %>% 
    clean_iea_whitespace() %>% 
    remove_agg_memo_flows() %>% 
    use_iso_countries() %>% 
    augment_iea_df() %>% 
    tidy_iea_df(remove_zeroes = remove_zeroes)
}

