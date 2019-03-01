#' Load IEA data from csv file
#'
#' This function reads an IEA extended energy balances file and
#' converts it to a data frame with appropriately-labelled columns.
#' One of \code{iea_file} or \code{text} must be specified, but not both.
#' The first line of \code{iea_file} or \code{text} 
#' is expected to start with \code{expected_start_1st_line}, and
#' the second line is expected to start with \code{expected_2nd_line_start}, and
#' it may have any number of commas appended.
#' (The extra commas might come from opening and re-saving the file in Excel.)
#' If those conditions are not met, execution is halted, and
#' an error message is given.
#' 
#' Missing data (by default "\code{..}" and "\code{x}") are converted to \code{0}. 
#' 
#' This function is designed to work even as more years are added
#' as columns at the right of \code{iea_file}, 
#' because column names in the output are constructed from the first two lines of \code{iea_file} 
#' (which contain years and country, flow, product information).
#' 
#' The data frame returned from this function is not ready to be used in R, 
#' because rows are not unique.
#' To further prepre the data frame for use, call \code{augment_iea_data()},
#' passing the output of this function in the \code{.iea_df} argument \code{augment_iea_data()}.
#'
#' @param .iea_file a string containing the path to a .csv file of extended energy balances from the IEA
#' @param text a character string that can be parsed as IEA extended energy balances. 
#'        (This argument is useful for testing.)
#' @param expected_1st_line_start the expected start of the first line of \code{iea_file}. Default is "\code{,,TIME}".
#' @param expected_2nd_line_start the expected start of the second line of \code{iea_file}. Default is "\code{COUNTRY,FLOW,PRODUCT}".
#' @param year_colname_pattern a regex that identifies columns with year titles. 
#'        Default is "\code{^\\d*$}" which identifies columns whose names are exclusively digits.
#' @param missing_data a vector containing strings that identify missing data. Default is \code{c("..", "x")}.
#'
#' @return a data frame containing the as-read IEA data
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
                   year_colname_pattern = "^\\d*$", missing_data = c("..", "x")){
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
  IEAData_noheader %>% 
    magrittr::set_names(colnames) %>% 
    # Clean up data in year columns
    dplyr::mutate_at(dplyr::vars(dplyr::matches(year_colname_pattern)),
              function(x){
                replace(x, x %in% missing_data, 0)
              }
    ) %>% 
    # Convert all year columns (columns whose names are all numbers) to numeric
    dplyr::mutate_at(dplyr::vars(dplyr::matches(year_colname_pattern)), as.numeric) %>% 
    # Convert to a data frame.
    as.data.frame()
}



#' Augment IEA data frame
#' 
#' This function prepares an IEA data frame created by \link{iea_df} for use in R.
#' 
#' This function solves several problems.
#' The first problem is that metadata in the \code{COUNTRY}, \code{FLOW}, and \code{PRODUCT}
#' collumns of an IEA data table are not unique.
#' To solve this problem, two additional columns are added: \code{Ledger.side} and \code{Flow.aggregation.point}.
#' \code{Ledger.side} can be one of "\code{Supply}" or "\code{Consumption}", corresponding to the top or bottom of the IEA's tables, respectively.
#' \code{Flow.aggregation.point} indicates the next level of aggregation for these data. 
#' \code{Flow.aggregation.point} can be one of 
#' "\code{Total primary energy supply}", "\code{Transformation processes}", "\code{Energy industry own use}", or "\code{TFC compare}"
#' on the \code{Supply} side of the ledger.
#' On the \code{Consumption} side of the ledger, \code{Flow.aggregation.point} can be one of 
#' "\code{Industry}", "\code{Transport}", "\code{Other}", or "\code{Non-energy use}".
#' The second problem is that missing data are indicated by character string ("\code{..}" or "\code{x}".
#' To solve this problem, missing data are converted to \code{NA}.
#' The third problem is that the countries are given by their (long) full name. 
#' To solve this problem, the country column is filled with 2-letter ISO abbreviations.
#'
#' @param .iea_df a data frame produced by the \link{iea_df} function
#' @param ledger_side the name of the ledger side column. Default is "\code{Ledger.side}".
#' @param flow_aggregation_point the name of the flow aggregation point column. Default is "\code{Flow.aggregation.point}".
#' @param country the name of the country column in \code{.iea_df}. Default is "\code{COUNTRY}".
#' @param flow the name of the flow column in \code{.iea_df}. Default is "\code{FLOW}".
#' @param country the name of the country column in \code{.iea_df}. Default is "\code{COUNTRY}".
#' @param losses the string that indicates losses in the \code{flow} column. Default is "\code{Losses}".
#' @param supply the string that indicates supply in the \code{ledger_side} column. Default is "\code{Supply}".
#' @param consumption the string that indicates consumption in the \code{ledger_side} column. Default is "\code{Consumption}".
#' @param .rownum the name of a column created (and destroyed) internally by this function. 
#'        The \code{.rownum} column temporarily holds row numbers for internal calculations.
#'        The \code{.rownum} column is deleted before returning.#' 
#'
#' @return \code{.ieadf} with additional columns named \code{ledger_side} and \code{flow_aggregation_point}
#' 
#' @export
#'
#' @examples
#' iea_df(text = ",,TIME,1960,1961\nCOUNTRY,FLOW,PRODUCT\nWorld,Production,Hard coal,42,43") %>% 
#'   augment_iea_df()
augment_iea_df <- function(.iea_df, ledger_side = "Ledger.side", flow_aggregation_point = "Flow.aggregation.point", 
                           country = "COUNTRY", flow = "FLOW", 
                           losses = "Losses", supply = "Supply", consumption = "Consumption",
                           .rownum = ".rownum"){
  # The split between Supply and Consumption ledger sides occurs where Flow == Losses and Flow == Total final consumption.
  # Find this dividing line in .iea_df. 
  # Then create the Ledger.side column. 
  temp <- .iea_df %>% 
    # Eliminate rownames, leaving only numbers
    tibble::remove_rownames() %>% 
    dplyr::group_by(!!as.name(country)) %>% 
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
          !!as.name(ledger_side) := case_when(
            !!as.name(.rownum) <= last_loss_row ~ supply,
            TRUE ~ consumption
          )
        )
    }) %>% 
    # Reorder the columns and remove the .rownum column
    dplyr::select(ledger_side,  everything()) %>% 
    dplyr::select(-.rownum)
}