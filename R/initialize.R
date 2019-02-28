#' Load IEA data from csv file
#'
#' This function reads an IEA extended energy balances file and
#' converts it to a data frame with appropriately-labelled columns.
#' One of \code{iea_file} or \code{text} must be specified, but not both.
#' The first line of \code{iea_file} or \code{text} 
#' is expected to start with \code{expected_start_1st_line}, and
#' the second line is expected to be \code{expected_2nd_line}.
#' If those conditions are not met, execution is halted.
#' 
#' This function is expected to work even as more years are added
#' as columns at the right of \code{iea_file}, 
#' because column names are constructed from the first line of \code{iea_file} (which contains years).
#' 
#' The data frame returned from this function is not ready to be used in R, 
#' because rows are not unique, and 
#' several empty cells are filled with ".." or "x".
#' Call \code{augment_iea_data()} to prepare the data frame for use.
#'
#' @param iea_file a string containing the path to a .csv file of extended energy balances from the IEA
#' @param text a character string that can be parsed as IEA extended energy balances
#' @param expected_1st_line_start the expected start of the first line of \code{iea_file}. Default is "\code{,,TIME}".
#' @param expected_2nd_line the expected second line of \code{iea_file}. Default is "\code{COUNTRY,FLOW,PRODUCT}".
#'
#' @return a data frame containing the as-read IEA data
#' 
#' @export
#' 
#' @examples 
#' iea_df(text = ",,TIME,1960,1961\nCOUNTRY,FLOW,PRODUCT\nWorld,Production,Hard coal,42,43")
iea_df <- function(iea_file = NULL, text = NULL, expected_1st_line_start = ",,TIME", expected_2nd_line = "COUNTRY,FLOW,PRODUCT"){
  assertthat::assert_that(xor(is.null(iea_file), is.null(text)), 
                          msg = "need to supply one but not both of iea_file and text arguments to iea_df")
  if (!is.null(iea_file)) {
    conn <- file(iea_file, open = "rt") # open file connection
  } else {
    # text has been provided
    conn <- textConnection(text)
  }
  header <- conn %>% readLines(n = 2) # read in header
  close(conn)
  # Check whether header has the form we expect.
  assertthat::assert_that(length(header) == 2, msg = "couldn't read 2 lines in iea_df")
  assertthat::assert_that(header[[1]] %>% startsWith(expected_1st_line_start) & header[[2]] == expected_2nd_line, 
                          msg = paste0("In iea_df, input data didn't start with '", expected_1st_line_start, "' or second line wasn't '", expected_2nd_line, "'")) 
  if (!is.null(iea_file)) {
    # Slurp the file. This slurping ignores the header, which fread deems to be the first 2 lines.
    # Note that I'm using data.table::fread at the recommendation of
    # https://statcompute.wordpress.com/2014/02/11/efficiency-of-importing-large-csv-files-in-r/
    # which indicates this function is significantly faster than other options.
    IEAData_noheader <- data.table::fread(file = iea_file, header = FALSE, sep = ",")
  } else {
    IEAData_noheader <- data.table::fread(text = text, header = FALSE, sep = ",")
  }
  # At this point, the IEAData_noheader data frame has default (meaningless) column names, V1, V2, V3, ...  
  # Create column names from the first two lines of the file. 
  # The code here should be robust to adding more years through time,
  # because it simply replaces the first 3 items of the first line 
  # with appropriate values from the 2nd line.
  colnames <- gsub(pattern = expected_1st_line_start, replacement = paste0(expected_2nd_line), header[[1]]) %>% 
    strsplit(",") %>% 
    unlist()
  IEAData_noheader %>% 
    magrittr::set_names(colnames) %>% 
    as.data.frame()
}



augment_iea_df <- function(.iea_df){
  
}