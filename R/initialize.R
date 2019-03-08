#' Load IEA data from an extended enegy balances csv file
#'
#' This function reads an IEA extended energy balances file and
#' converts it to a data frame with appropriately-labelled columns.
#' One of `iea_file` or `text` must be specified, but not both.
#' The first line of `iea_file` or `text`
#' is expected to start with `expected_start_1st_line`, and
#' the second line is expected to start with `expected_2nd_line_start`, and
#' it may have any number of commas appended.
#' (The extra commas might come from opening and re-saving the file in Excel.)
#' If those conditions are not met, execution is halted, and
#' an error message is given.
#' 
#' This function is designed to work even as more years are added
#' in columns at the right of `.iea_file`, 
#' because column names in the output are constructed from the first two lines of `iea_file` 
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
#' In some places "`..`" is used for not applicable values
#' (e.g., World Anthracite supply in 1971). 
#' World Anthracite supply in 1971 is actually not applicable, because Anthracite was
#' classified under "Hard coal (if no detail)" in 1971.
#' On the other hand, "`..`" is used for data in the most recent year 
#' when those data have not yet been incorporated into the database. 
#' In the face of IEA’s inconsistencies, 
#' the only rational way to proceed is to convert 
#' both "`x`" and "`..`" to "`0`"
#' in this function.
#' 
#' The data frame returned from this function is not ready to be used in R, 
#' because rows are not unique.
#' To further prepre the data frame for use, call `augment_iea_data()`,
#' passing the output of this function in the `.iea_df` argument of `augment_iea_data()`.
#'
#' @param .iea_file a string containing the path to a .csv file of extended energy balances from the IEA
#' @param text a character string that can be parsed as IEA extended energy balances. 
#'        (This argument is useful for testing.)
#' @param expected_1st_line_start the expected start of the first line of `iea_file`. Default is "`,,TIME`".
#' @param expected_2nd_line_start the expected start of the second line of `iea_file`. Default is "`COUNTRY,FLOW,PRODUCT`".
#' @param year_colname_pattern a regex that identifies columns with year titles. 
#'        Default is "`^\\d*$`" which identifies columns whose names are exclusively digits.
#' @param missing_data a string that identifies missing data. Default is "`..`".
#'        Entries of "`missing_data`" are coded as `0`` in output.
#' @param not_applicable_data a string that identifies not-applicable data.
#'        Entries of "`not_applicable_data`" are coded as `0` in output.
#'
#' @return a data frame containing the IEA extended energy balances data
#' 
#' @export
#' 
#' @examples 
#' # Original file format
#' iea_df(text = ",,TIME,1960,1961\nCOUNTRY,FLOW,PRODUCT\nWorld,Production,Hard coal,42,43")
#' # With extra commas on the 2nd line
#' iea_df(text = ",,TIME,1960,1961\nCOUNTRY,FLOW,PRODUCT,,\nWorld,Production,Hard coal,42,43")
iea_df <- function(.iea_file = NULL, text = NULL, 
                   expected_1st_line_start = ",,TIME", expected_2nd_line_start = "COUNTRY,FLOW,PRODUCT", 
                   year_colname_pattern = "^\\d*$", missing_data = "..", not_applicable_data = "x"){
  assertthat::assert_that(xor(is.null(.iea_file), is.null(text)), 
                          msg = "need to supply one but not both of iea_file and text arguments to iea_df")
  if (!is.null(.iea_file)) {
    conn <- file(.iea_file, open = "rt") # open file connection
  } else {
    # text has been provided, probably for testing purposes.
    conn <- textConnection(text)
  }
  header <- conn %>% readLines(n = 2) # read header
  close(conn)
  # Check whether header has the form we expect.
  assertthat::assert_that(length(header) == 2, msg = "couldn't read 2 lines in iea_df")
  if (header[[2]] %>% startsWith(expected_2nd_line_start) & header[[2]] %>% endsWith(",")) {
    # The file may have been opened in Excel and resaved.
    # When that occurs, many commas are appended to the 2nd line.
    # Strip out these commas before proceeding further.
    # The pattern ,*$ means "match any number (*) of commas (,) at the end of the line ($)".
    header[[2]] <- gsub(pattern = ",*$", replacement = "", header[[2]])
  }
  assertthat::assert_that(header[[1]] %>% startsWith(expected_1st_line_start) & header[[2]] == expected_2nd_line_start, 
                          msg = paste0("In iea_df, input data didn't start with '", expected_1st_line_start, 
                                       "' or second line didn't start with '", expected_2nd_line_start, "'")) 
  if (!is.null(.iea_file)) {
    # Slurp the file. This slurping ignores the header, which fread deems to be the first 2 lines.
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
  colnames <- gsub(pattern = expected_1st_line_start, replacement = expected_2nd_line_start, header[[1]]) %>% 
    strsplit(",") %>% 
    unlist()
  IEAData_withheader <- IEAData_noheader %>% 
    magrittr::set_names(colnames)
  # Data tagged as not-applicable in the IEA database should be coded as 0.
  # We still want to allow calculations with these data.
  IEAData_withheader[IEAData_withheader == not_applicable_data] <- 0
  # However, missing data should be tagged as "not available", because calculations 
  # with unavaiable data should fail.
  IEAData_withheader[IEAData_withheader == missing_data] <- 0
  # Convert all year columns (columns whose names are all numbers) to numeric, 
  # convert into a data frame, and 
  # return.
  IEAData_withheader %>% 
    dplyr::mutate_at(dplyr::vars(dplyr::matches(year_colname_pattern)), as.numeric) %>% 
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

#' Augment an IEA data frame
#' 
#' This function prepares an IEA data frame created by [iea_df()] for use in R.
#' 
#' This function solves several problems.
#' The first problem is that metadata in the `COUNTRY`, `FLOW`, and `PRODUCT`
#' collumns of an IEA data table are not unique.
#' To solve this problem, two additional columns are added: `Ledger.side` and `Flow.aggregation.point`.
#' `Ledger.side` can be one of "`Supply`" or "`Consumption`", corresponding to the top or bottom of the IEA's tables, respectively.
#' `Flow.aggregation.point` indicates the next level of aggregation for these data. 
#' `Flow.aggregation.point` can be one of 
#' "`Total primary energy supply`", "`Transformation processes`", "`Energy industry own use`", or "`TFC compare`"
#' on the `Supply` side of the ledger.
#' On the `Consumption` side of the ledger, `Flow.aggregation.point` can be one of 
#' "`Industry`", "`Transport`", "`Other`", or "`Non-energy use`".
#' The second problem is that the countries are given by their (long) full name. 
#' To solve this problem, the country column is filled with 2-letter ISO abbreviations.
#'
#' @param .iea_df a data frame produced by the [iea_df()] function
#' @param ledger_side the name of the ledger side column. Default is "`Ledger.side`".
#' @param flow_aggregation_point the name of the flow aggregation point column. Default is "`Flow.aggregation.point`".
#' @param country the name of the country column in `.iea_df`. Default is "`COUNTRY`".
#' @param flow the name of the flow column in `.iea_df`. Default is "`FLOW`".
#' @param country the name of the country column in `.iea_df`. Default is "`COUNTRY`".
#' @param losses the string that indicates losses in the `flow` column. Default is "`Losses`".
#' @param supply the string that indicates supply in the `ledger_side` column. Default is "`Supply`".
#' @param consumption the string that indicates consumption in the `ledger_side` column. Default is "`Consumption`".
#' @param .rownum the name of a column created (and destroyed) internally by this function. 
#'        The `.rownum` column temporarily holds row numbers for internal calculations.
#'        The `.rownum` column is deleted before returning. 
#'
#' @return `.iea_df` with additional columns named `ledger_side` and `flow_aggregation_point`
#' 
#' @export
#'
#' @examples
#' iea_df(text = ",,TIME,1960,1961\nCOUNTRY,FLOW,PRODUCT\nWorld,Production,Hard coal,42,43") %>% 
#'   rename_iea_df_cols() %>% 
#'   augment_iea_df()
augment_iea_df <- function(.iea_df, country = "Country", 
                           ledger_side = "Ledger.side", flow_aggregation_point = "Flow.aggregation.point", 
                           flow = "Flow", 
                           losses = "Losses", supply = "Supply", consumption = "Consumption",
                           tpes_flows = c("Production", "Imports", "Exports", "International marine bunkers", "International aviation bunkers", "Stock changes"),
                           tpes = "Total primary energy supply", 
                           tfc_compare_flows = c("Total primary energy supply", "Transfers", "Statistical differences", "Transformation processes", "Energy industry own use", "Losses"),
                           tfc_compare = "TFC compare",
                           tp_flows_suffix = "(transf.)",
                           nstp_flows_suffix = "(transformation)",
                           transformation_processes = "Transformation processes",
                           eiou_flows_suffix = "(energy)",
                           eiou = "Energy industry own use",
                           tfc_flows = c("Industry", "Transport", "Other", "Non-energy use"),
                           tfc = "Total final consumption",
                           industry_flows = c("Iron and steel", "Chemical and petrochemical", "Non-ferrous metals", "Non-metallic minerals", "Transport equipment", "Machinery", "Mining and quarrying", "Food and tobacco", "Paper, pulp and print", "Wood and wood products", "Construction", "Textile and leather", "Non-specified (industry)"), 
                           industry = "Industry",
                           transport_flows = c("World aviation bunkers", "Domestic aviation", "Road", "Rail", "Pipeline transport", "World marine bunkers", "Domestic navigation", "Non-specified (transport)"),
                           transport = "Transport",
                           other_flows = c("Residential", "Commercial and public services", "Agriculture/forestry", "Fishing", "Non-specified (other)"),
                           other = "Other",
                           non_energy_prefix = "Non-energy use",
                           non_energy = "Non-energy use",
                           electricity_output_flows_prefix = "Electricity output (GWh)-",
                           electricity_output = "Electricity output (GWh)",
                           heat_output_flows_prefix = "Heat output-",
                           heat_output = "Heat output",
                           .rownum = ".rownum"){
  WithLedgerSide <- .iea_df %>% 
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
      # Calculate the last row of Losses. last_loss_row is the last row of the supply side of the ledger.
      last_loss_row <- with_row_nums %>% 
        dplyr::filter(!!as.name(flow) == losses) %>% 
        magrittr::extract2(.rownum) %>% 
        max()
      with_row_nums %>% 
        dplyr::mutate(
          !!as.name(ledger_side) := dplyr::case_when(
            !!as.name(.rownum) <= last_loss_row ~ supply,
            TRUE ~ consumption
          )
        )
    }) 
  # Now add the Flow.aggregation.point column
  WithFAP <- WithLedgerSide %>% 
    dplyr::mutate(
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
      )
    )
    
  # Finally, reorder the columns, remove the .rownum column, and return
  WithFAP %>% 
    dplyr::select(-.rownum) %>% 
    dplyr::select(country, ledger_side, flow_aggregation_point, dplyr::everything())
}