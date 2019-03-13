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
#' A `Units` column is added with the value of `units_val`.
#' (Default is `ktoe`, although any string can be specified in `units_val`.)
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
#' @param country the name of the country column. Default is "`Country`".
#' @param ledger_side the name of the ledger side column. Default is "`Ledger.side`".
#' @param flow_aggregation_point the name of the flow aggregation point column. Default is "`Flow.aggregation.point`".
#' @param energy_type the name of the energy type column to be added to `.iea_df`. Default is "`Energy.type`.
#' @param energy_type_val the value to put in the `energy_type` column. Default is "`E`".
#' @param units the name of the units column to be added to `.iea_df`. Default is "`Units`".
#' @param units_val the value to put in the `units` column. Default is "`ktoe`" for kilotons of oil equivalent.
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
#' @param eoiu the string that identifies energy industry own use in the `Flow` column. Default is "`Energy industry own use`".
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
#' @param heat_output_prefix a string prefix for `Flow`s to be aggregated in heat output. Default is "`Heat output-`".
#' @param .rownum the name of a column created (and destroyed) internally by this function. 
#'        The `.rownum` column temporarily holds row numbers for internal calculations.
#'        The `.rownum` column is deleted before returning. 
#'
#' @return `.iea_df` with additional columns named `ledger_side` and `flow_aggregation_point`
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
                           units = "Units", units_val = "ktoe",
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
      # Add the Units column
      !!units := units_val
    ) %>% 
    # Finally, reorder the columns, remove the .rownum column, and return
    dplyr::select(-.rownum) %>% 
    dplyr::select(country, ledger_side, flow_aggregation_point, energy_type, units, dplyr::everything())
}