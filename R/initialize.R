#' Slurp an IEA extended energy balance data file
#' 
#' This is an internal helper function. 
#' This function reads an IEA extended energy balances .csv file and
#' converts it to a data frame with appropriately-labeled columns.
#' One of `iea_file` or `text` must be specified, but not both.
#' The first line of `iea_file` or `text`
#' is expected to start with `expected_start_1st_line`, and
#' the second line is expected to start with `expected_2nd_line_start`, and
#' it may have any number of commas appended.
#' (The extra commas might come from opening and re-saving the file in Excel.)
#' Alternatively, the file may have a first line of `expected_simple_start`.
#' If none of these conditions are not met, execution is halted, and
#' an error message is provided.
#' Files should have a return character at the end of their final line.
#' 
#' This function is designed to work even as more years are added
#' in columns at the right of `.iea_file`, 
#' because column names in the output are constructed from the header line(s) of `.iea_file` 
#' (which contain years and country, flow, product information).
#'
#' @param .iea_file the path to the raw IEA data file for which quality assurance is desired
#' @param text a string containing text to be parsed as an IEA file.
#' @param expected_1st_line_start the expected start of the first line of `iea_file`. Default is ",,TIME".
#' @param expected_2nd_line_start the expected start of the second line of `iea_file`. Default is "COUNTRY,FLOW,PRODUCT".
#' @param expected_simple_start the expected starting of the first line of `iea_file`. Default is the value of `expected_2nd_line_start`.
#'        Note that `expected_simple_start` is sometimes encountered in data supplied by the IEA.
#'        Furthermore, `expected_simple_start` could be the format of the file when somebody "helpfully" fiddles with 
#'        the raw data from the IEA.
#'
#' @return a raw data frame of IEA extended energy balance data with appropriate column titles
#' 
#' @export
#'
#' @examples
#' # 2018 and earlier file format
#' slurp_iea_to_raw_df(text = paste0(",,TIME,1960,1961\n",
#'                      "COUNTRY,FLOW,PRODUCT\n",
#'                      "World,Production,Hard coal (if no detail),42,43"))
#' # With extra commas on the 2nd line
#' slurp_iea_to_raw_df(text = paste0(",,TIME,1960,1961\n",
#'                      "COUNTRY,FLOW,PRODUCT,,,\n",
#'                      "World,Production,Hard coal (if no detail),42,43"))
#' # With a clean first line (2019 file format)
#' slurp_iea_to_raw_df(text = paste0("COUNTRY,FLOW,PRODUCT,1960,1961\n",
#'                      "World,Production,Hard coal (if no detail),42,43"))
slurp_iea_to_raw_df <- function(.iea_file = NULL, 
                                text = NULL, 
                                expected_1st_line_start = ",,TIME", 
                                expected_2nd_line_start = "COUNTRY,FLOW,PRODUCT", 
                                expected_simple_start = expected_2nd_line_start) {
  assertthat::assert_that(xor(!is.null(.iea_file), !is.null(text)), 
                          msg = "need to supply only one of .iea_file or text arguments to iea_df")
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
  # first_line <- first_two_lines[[1]]
  # second_line <- first_two_lines[[2]]
  # Eliminate any quotes that are present
  first_line <- gsub(pattern = '\\"', replacement = "", x = first_two_lines[[1]])
  second_line <- gsub(pattern = '\\"', replacement = "", x = first_two_lines[[2]])
  # Ensure that we have an expected format for the first line or two in first_two_lines.
  assertthat::assert_that(first_line %>% startsWith(expected_simple_start) | 
                            (first_line %>% startsWith(expected_1st_line_start) & second_line %>% startsWith(expected_2nd_line_start)), 
                          msg = paste0(".iea_file must start with ",
                                       "first line: '", expected_simple_start, "', ",
                                       "or ",
                                       "first line: '", expected_1st_line_start, "' and ",
                                       "second line: '", expected_2nd_line_start, "'.  ",
                                       "Instead, found ",
                                       "first line: '", first_line, "', ",
                                       "second line: '", second_line, "'."))
  if (first_line %>% startsWith(expected_simple_start)) {
    # We have the simple start to the file, so we can assume a one-line header.
    if (!is.null(.iea_file)) {
      IEAData_withheader <- data.table::fread(file = .iea_file, header = TRUE, sep = ",")
    } else {
      IEAData_withheader <- data.table::fread(text = text, header = TRUE, sep = ",")
    }
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
  return(IEAData_withheader)
}


#' Perform quality assurance on a raw IEA data file
#' 
#' When starting to work with an IEA data file, 
#' it is important to verify its integrity.
#' This function performs some validation tests on `.iea_file`.
#' 
#' At this time, the only verification step performed by this function
#' is confirming that every country has the same flow and product rows in the same order.
#' The approach is to add a per-country row number column to the data frame and delete all the data in year columns.
#' Then, the resulting data frame is queried for duplicate row numbers.
#' If none are found, the function returns the data frame read from the file.
#' 
#' Note that `.iea_file` is read internally with [data.table::fread()] *without* stripping white space.
#' 
#' If `.slurped_iea_df` is supplied, arguments `.iea_file` or `text` are ignored. 
#' If `.slurped_iea_df` is absent, 
#' either `.iea_file` or `text` are required, and 
#' the helper function `slurp_iea_to_raw_df()` is called internally 
#' to load a raw data frame of data.

#'
#' @param .iea_file the path to the raw IEA data file for which quality assurance is desired
#' @param text a string containing text to be parsed as an IEA file.
#' @param expected_1st_line_start the expected start of the first line of `iea_file`. Default is ",,TIME".
#' @param expected_2nd_line_start the expected start of the second line of `iea_file`. Default is "COUNTRY,FLOW,PRODUCT".
#' @param expected_simple_start the expected starting of the first line of `iea_file`. Default is the value of `expected_2nd_line_start`.
#'        Note that `expected_simple_start` is sometimes encountered in data supplied by the IEA.
#'        Furthermore, `expected_simple_start` could be the format of the file when somebody "helpfully" fiddles with 
#'        the raw data from the IEA.
#' @param .slurped_iea_df a data frame created by `slurp_iea_to_raw_df()`
#' @param country the name of the country column. Default is "COUNTRY".
#' @param flow the name of the flow column. Default is "FLOW".
#' @param product the name of the product column. Default is "PRODUCT".
#' @param rowid the name of a row number column added internally to `.iea_file` per country. Default is "rowid".
#'
#' @return `TRUE` if `.iea_file` passes all checks. Errors are thrown when a verification step fails.
#' 
#' @export
#'
#' @examples
#' library(magrittr)
#' sample_iea_data_path() %>% 
#'  iea_file_OK()
iea_file_OK <- function(.iea_file = NULL, 
                        text = NULL, 
                        expected_1st_line_start = ",,TIME", 
                        expected_2nd_line_start = "COUNTRY,FLOW,PRODUCT", 
                        expected_simple_start = expected_2nd_line_start,
                        .slurped_iea_df = NULL,
                        country = "COUNTRY",
                        flow = "FLOW", 
                        product = "PRODUCT", 
                        rowid = "rowid") {
  
  if (is.null(.slurped_iea_df)) {
    DF <- slurp_iea_to_raw_df(.iea_file = .iea_file, 
                              text = text, 
                              expected_1st_line_start = expected_1st_line_start, 
                              expected_2nd_line_start = expected_2nd_line_start, 
                              expected_simple_start = expected_simple_start)
  } else {
    DF <- .slurped_iea_df
  }
  
  # Verify that each country has the same order of flows and products
  flow_product <- DF %>% 
    # Group by country so that adding row numbers is done per-country
    dplyr::group_by(!!as.name(country)) %>% 
    # Add row numbers
    dplyr::do(
      tibble::rowid_to_column(.data, var = rowid)
    ) %>% 
    dplyr::ungroup() %>% 
    # Keep only rowid, flow, and product. This removes COUNTRY and years from the data frame
    dplyr::select(rowid, flow, product) %>% 
    # In this context, unique() gives unique combinations of per-country row number, flow, product triples.
    # If all countries have the same order of things, 
    # all countries should have the same row number, flow, product triples, and
    # this call to unique() will give the same number of rows as exist one country.
    unique()
  
  # After having obtained the unique (per-country) row number, flow, product triples,
  # we see if there are any duplicated row numbers.
  # If all countries have the same row number, flow, product triples, 
  # there will be no duplicated row numbers.
  flow_product %>% 
    # Look at the rowid column only
    dplyr::select(rowid) %>% 
    # duplicated() returns TRUE for any duplicated values
    duplicated() %>% 
    # Any tells us if there are any duplicated values (TRUEs).
    # We want all FALSE (no duplicated values).
    any() %>% 
    # If everything is FALSE (what we want), any() will return FALSE.
    # But we want a good result from this function to return TRUE, 
    # so we reverse the logic with not().
    magrittr::not()
}


#' Load IEA data from an extended energy balances .csv file
#' 
#' If `.slurped_iea_df` is supplied, arguments `.iea_file` or `text` are ignored. 
#' If `.slurped_iea_df` is absent, 
#' either `.iea_file` or `text` are required, and 
#' the helper function `slurp_iea_to_raw_df()` is called internally 
#' to load a raw data frame of data.
#' 
#' Next, this function does some cleaning of the data.
#' 
#' In the IEA's data, some entries in the "FLOW" column are quoted to avoid creating too many columns. 
#' For example, "Paper, pulp and printing" is quoted in the raw .csv file: 
#' "      Paper, pulp and printing".
#' Internally, this function uses [data.table::fread()], which, unfortunately, does not
#' strip leading and trailing white space from quoted entries.
#' So the function uses [base::trimws()] to finish the job.
#' 
#' When the IEA includes estimated data for a year, 
#' the column name of the estimated year includes an "E" appended.
#' (E.g., "2017E".) 
#' This function eliminates estimated columns.
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
#' In the face of IEA's inconsistencies, 
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
#' @param .slurped_iea_df a data frame created by `slurp_iea_to_raw_df()`
#' @param flow the name of the flow column, entries of which are stripped of leading and trailing white space. Default is "FLOW".
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
                   expected_1st_line_start = ",,TIME", 
                   expected_2nd_line_start = "COUNTRY,FLOW,PRODUCT", 
                   expected_simple_start = expected_2nd_line_start,
                   .slurped_iea_df = NULL, 
                   flow = "FLOW",
                   missing_data = "..", not_applicable_data = "x", confidential_data = "c", 
                   estimated_year = "E"){

  if (is.null(.slurped_iea_df)) {
    IEAData_withheader <- slurp_iea_to_raw_df(.iea_file = .iea_file, 
                                              text = text, 
                                              expected_1st_line_start = expected_1st_line_start, 
                                              expected_2nd_line_start = expected_2nd_line_start, 
                                              expected_simple_start = expected_simple_start)
  } else {
    IEAData_withheader <- .slurped_iea_df
  }
  
  # At this point, IEAData_withheader may have some column names that end in `estimated_year`.
  # We should delete those columns.
  cnames <- colnames(IEAData_withheader)
  cols_to_delete <- grepl(pattern = paste0(estimated_year, "$"), x = cnames)
  # cols_to_delete has TRUE for each column to eliminate. But we need true for each column to KEEP.
  # With [!cols_to_delete], we get the desired effect.
  IEAData_withheader <- IEAData_withheader[!cols_to_delete]
  
  # The data.table::fread function doesn't trim leading and trailing whitespace from quoted entries.
  # In practice, this means that any FLOW containing a comma may have leading whitespace.
  # Eliminate this whitespace.
  IEAData_withheader[[flow]] <- trimws(IEAData_withheader[[flow]])

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
#' renaming the columns (by default) to `Country`, `Flow`, and `Product`.
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


#' Replace country names with 3-letter ISO abbreviations
#' 
#' The IEA uses full country names, but it is more concise to use the 3-letter ISO abbreviations.
#' This function replaces the full country names with ISO abbreviations where possible.
#' 
#' Special cases are considered where 
#' IEA extended energy balance data country names 
#' differ from 
#' `countrycode` package country names.
#' 
#' |IEA name|`countrycode` name (names)|`use_iso_countries()` decision|
#' |---------|-------------------|------------------------------|
#' |"People's Republic of China"|"China"|"CHN"|
#'
#' @param .iea_df A data frame containing a `country` column
#' @param country The name of the country column in `.iea_df`. Default is "Country".
#' @param iea_china_HK The IEA string for China and Hong Kong. 
#'                     Default is "China (P.R. of China and Hong Kong, China)".
#' @param ieatools_china The 3-letter string that replaces `iea_china_HK`. 
#'                       Default is "CHN".
#'
#' @return `.iea_df` with 3-letter ISO country abbreviations in the `country` column.
#' 
#' @export
#'
#' @examples
#' sample_iea_data_path() %>% 
#'   iea_df() %>%
#'   rename_iea_df_cols() %>% 
#'   use_iso_countries()
use_iso_countries <- function(.iea_df, 
                              country = "Country", 
                              iea_china_HK = "People's Republic of China", 
                              ieatools_china = "CHN"){
  # Eliminates warnings.
  country.name.en <- "country.name.en" 
  iso_type = "iso3c"
  # Load country code information
  CountryInfo <- countrycode::codelist %>%
    dplyr::select(!!as.name(country.name.en), !!as.name(iso_type)) %>% 
    dplyr::rename(
      "{country}" := country.name.en
    )
  .iea_df %>%
    dplyr::left_join(CountryInfo, by = country) %>% # left_join preserves all rows of IEA data
    dplyr::mutate(
      "{iso_type}" := dplyr::case_when(
        #
        # First, deal with some special cases.
        # 
        # The IEA extended energy balance data have the country
        # "China (P.R. of China and Hong Kong, China)", but 
        # the country name database in the countrycode package has separate entries 
        # for China CHN and Hong Kong SAR China HKG.
        # To resolve this issue, we recode
        # `iea_china_HK` as `ieatools_china`.
        .data[[country]] == iea_china_HK ~ ieatools_china,
        #
        # Next, if we get an NA and we haven't dealt with a country as a special case, 
        # just set the iso_type column to the value of the country column.
        is.na(.data[[iso_type]]) ~ .data[[country]],

        # As a default, keep the iso_type value in place.
        TRUE ~ .data[[iso_type]]
      )
    ) %>% 
    # Now we can get rid of the country column.
    dplyr::select(-!!as.name(country)) %>% 
    # And rename the iso_type column to be country
    dplyr::rename(
      "{country}" := iso_type
    ) %>% 
    # And put the country column first.
    dplyr::select(!!as.name(country), dplyr::everything())
}


#' Remove aggregation and memo rows from data frames of IEA data
#' 
#' Aggregation and memo rows are included with IEA data.
#' Sometimes, it is convenient to remove those rows. 
#' This function does so, using default identifying strings for aggregations and memos.
#' 
#' Note that the IEA data sometimes includes a variable number of spaces 
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
#' sample_iea_data_path() %>% 
#'   iea_df() %>%
#'   rename_iea_df_cols() %>% 
#'   remove_agg_memo_flows()
remove_agg_memo_flows <- function(.iea_df,
                                  flow = IEATools::iea_cols$flow,
                                  product = IEATools::iea_cols$product,
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


#' Remove aggregation regions from an IEA data frame
#' 
#' The IEA extended energy balances contain several aggregation regions
#' by default.
#' In some situations, it may be desirable to remove those aggregation regions.
#' This function performs that task.
#'
#' @param .iea_df The IEA data frame from which you want to remove aggregation regions.
#' @param country The name of the Country column in `.iea_df`.
#' @param agg_regions A list of aggregation regions in the `country` column of `.iea_df`.
#'
#' @return A version of `.iea_df` with aggregation regions removed.
#' 
#' @export
#'
#' @examples
#' tibble::tibble(Year = c(1967, 1995), 
#'                Country = c("World", "Spain")) %>%
#'  remove_agg_regions()
remove_agg_regions <- function(.iea_df, 
                               country = IEATools::iea_cols$country,
                               agg_regions = IEATools::aggregation_regions) {
  .iea_df %>%
    dplyr::filter(!.data[[country]] %in% agg_regions)
}


#' Augment an IEA data frame
#' 
#' This function prepares an IEA data frame created by [iea_df()] for use in R.
#' It works on IEA data from the 2018 and 2019 releases
#' of the IEA's extended energy balances.
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
#' @param country the name of the country column in `.iea_df`. Default is "Country".
#' @param ledger_side the name of the ledger side column to be added to `.iea_df`. Default is "Ledger.side".
#' @param flow_aggregation_point the name of the flow aggregation point column to be added to `.iea_df`. Default is "Flow.aggregation.point".
#' @param flow the name of the flow column in `.iea_df`.  Default is "Flow".
#' @param product the name of the product column in `.iea_df`.  Default is "Product".
#' @param energy_type the name of the energy type column to be added to `.iea_df`. Default is "Energy.type.
#' @param energy_type_val the value to put in the `energy_type` column. Default is "E".
#' @param method the name of the method column to be added to `.iea_df`. Default is "Method".
#' @param method_val the value to put in the `method` column. Default is "PCM" (Physical Content Method, which is used by the IEA).
#' @param last_stage the name of the last stage column to be added to `.iea_df`. Default is "Last.stage".
#' @param last_stage_val the value to put in the `last_stage` column. Default is "Final" (which is the last stage supplied by the IEA).
#' @param unit the name of the unit column to be added to `.iea_df`. Default is "Unit".
#' @param unit_val the value to put in the `unit` column. Default is "`ktoe`" for kilotons of oil equivalent.
#' @param supply the string that identifies supply `Ledger.side`. Default is "Supply".
#' @param consumption the string that identifies consumption `Ledger.side`. Default is "Consumption".
#' @param tpes the string that identifies total primary energy supply `Flow.aggregation.point`. Default is "Total primary energy supply".
#' @param tpes_flows a vector of strings that give flows that are aggregated to `Total primary energy supply`. 
#' @param tfc_compare a string that identifies the `TFC compare` flow aggregation point. Default is "TFC compare".
#' @param tfc_compare_flows a vector of strings that give `Flow`s that are aggregated to `TFC compare`.
#' @param transfers = a string that identifies transfers in the flow column. Default is "Transfers".
#' @param statistical_differences a string that identifies statistical differences in flow column. Default is "Statistical differences".
#' @param losses the string that indicates losses in the `Flow` column. Default is "Losses".
#' @param transformation_processes the string that indicates transformation processes in the `Flow` column. Default is "Transformation processes".
#' @param tp_flows_suffix the suffix for transformation processes in the `Flow` column. Default is "(transf.)".
#' @param nstp_flows_suffix the suffix for non-specified transformation processes in the `Flow` column. Default is "(transformation)".
#' @param mapep the string that identifies main activity producer electricity plants in the `Flow` column. Default is "Main activity producer electricity plants".
#' @param eiou the string that identifies energy industry own use in the `Flow` column. Default is "Energy industry own use".
#' @param eiou_flows_suffix the suffix for energy industry own use in the `Flow` column. Default is "(energy)".
#' @param coal_mines the string that identifies coal mines in the `Flow` column. Default is "Coal mines".
#' @param non_specified the string that identifies non-specified flows in the `Flow` column. Default is "Non-specified".
#' @param tfc the string that identifies total final consumption in the `Flow` column. Default is "Total final consumption".
#' @param tfc_flows a vector of strings that give total final consumption in the `Flow` column.
#' @param industry a string that names the industry `Flow.aggregation.point`. Default is "Industry".
#' @param industry_flows a vector of strings representing `Flow`s to be aggregated in the `Industry` `Flow.aggregation.point`. 
#' @param iron_and_steel a string that identifies the iron and steel industry. Default is "Iron and steel".
#' @param mining_and_quarrying a string that identifies the mining and quarrying industry. Default is "Mining and quarrying".
#' @param transport a string that names the transport `Flow.aggregation.point`. Default is "Transport".
#' @param transport_flows a vector of strings representing `Flow`s to be aggregated in the `Transport` `Flow.aggregation.point`. 
#' @param other a string that names the other `Flow.aggregation.point`. Default is "Other".
#' @param other_flows a vector of strings representing `Flow`s to be aggregated in the `Other` `Flow.aggregation.point`. 
#' @param non_energy a string that names the non-energy `Flow.aggregation.point`. Default is "Non-energy use".
#' @param non_energy_prefix a string prefix for `Flow`s to be aggregated in the `Non-energy use` `Flow.aggregation.point`. 
#' @param electricity_output a string that names the electricity output `Flow`. Default is "Electricity output (GWh)". 
#' @param electricity_output_flows_prefix a string prefix for `Flow`s to be aggregated in electricity output. Default is "Electricity output (GWh)-".
#' @param heat_output a string that names the heat output `Flow`. Default is "Heat output". 
#' @param heat_output_flows_prefix a string prefix for `Flow`s to be aggregated in heat output. Default is "Heat output-".
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
#'                      "COUNTRY,FLOW,PRODUCT\n",
#'                      "World,Production,Hard coal (if no detail),42,43\n",
#'                      "World,Statistical differences,Hard coal (if no detail),7,8\n",
#'                      "World,Main activity producer electricity plants,",
#'                        "Hard coal (if no detail),9,10\n",
#'                      "World,Non-specified,Hard coal (if no detail),11,12\n",
#'                      "World,Coal mines,Hard coal (if no detail),13,14\n",
#'                      "World,Non-specified,Hard coal (if no detail),11,12\n",
#'                      "World,Losses,Hard coal (if no detail),1,2\n",
#'                      "World,Iron and steel,Hard coal (if no detail),5,6\n")) %>% 
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
                           tpes_flows = IEATools::tpes_flows,
                           tfc_compare = "TFC compare",
                           tfc_compare_flows = IEATools::tfc_compare_flows,
                           transfers = "Transfers",
                           statistical_differences = "Statistical differences",
                           losses = "Losses", 
                           transformation_processes = "Transformation processes",
                           tp_flows_suffix = "(transf.)",
                           nstp_flows_suffix = "(transformation)",
                           mapep = "Main activity producer electricity plants",
                           eiou = "Energy industry own use",
                           eiou_flows_suffix = "(energy)",
                           coal_mines = "Coal mines",
                           non_specified = "Non-specified",
                           tfc = "Total final consumption",
                           tfc_flows = IEATools::tfc_flows,
                           industry = "Industry",
                           industry_flows = IEATools::industry_flows, 
                           iron_and_steel = "Iron and steel",
                           mining_and_quarrying = "Mining and quarrying",
                           transport = "Transport",
                           transport_flows = IEATools::transport_flows,
                           other = "Other",
                           other_flows = IEATools::other_flows,
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
    dplyr::mutate(
      # The 2018 IEA extended energy balance data used suffixes to identify portions of their data.
      # But in the 2019 edition of the data, those suffixes were removed,
      # so we can no longer rely upon them.
      # Thus, we delete the suffixes if they are present in the flow column of the data frame.
      # The string "\s+" means to match any number (+) of whitespace (\\s) characters.
      "{flow}" := dplyr::case_when(
        # Delete the " (transf.)" suffix
        endsWith(.data[[flow]], tp_flows_suffix) ~ gsub(pattern = paste0("\\s+", Hmisc::escapeRegex(tp_flows_suffix)), replacement = "", x = .data[[flow]]), 
        # Delete the " (transformation)" suffix
        endsWith(.data[[flow]], nstp_flows_suffix) ~ gsub(pattern = paste0("\\s+", Hmisc::escapeRegex(nstp_flows_suffix)), replacement = "", x = .data[[flow]]), 
        # Delete the " (energy)" suffix
        endsWith(.data[[flow]], eiou_flows_suffix) ~ gsub(pattern = paste0("\\s+", Hmisc::escapeRegex(eiou_flows_suffix)), replacement = "", x = .data[[flow]]),
        TRUE ~ .data[[flow]]
      )
    ) %>% 
    dplyr::group_by(.data[[country]]) %>% 
    # Perform the next operations on a per-country basis
    dplyr::group_modify(function(ctry_tbl, ctry){
      # At this point,
      # ctry_tbl contains all rows for this country, and
      # ctry is a 1-cell data frame (one country row and one country column) containing the name of the country we're working on now.

      # Find the split point between the Supply and Consumption sides of the ledger.
      supply_consumption_split <- find_supply_consumption_split(ctry_tbl, flow = flow, losses = losses, 
                                                                iron_and_steel = iron_and_steel, 
                                                                mining_and_quarrying = mining_and_quarrying, 
                                                                tfc = tfc, industry = industry)

      # Start of the Transformation processes section of the IEA data
      transformation_start <- find_transformation_start(ctry_tbl, flow = flow, statistical_differences = statistical_differences,
                                                        transformation_processes = transformation_processes,
                                                        mapep = mapep)
      
      # End of the Transformation processes section of the IEA data
      transformation_end <- find_transformation_end(ctry_tbl, flow = flow, non_specified = non_specified, 
                                                    eiou = eiou, coal_mines = coal_mines)
      # Start of EIOU (energy industry own use) section of the IEA data
      eiou_start <- find_eiou_start(ctry_tbl, flow = flow, non_specified = non_specified, 
                                    eiou = eiou, coal_mines = coal_mines)
      # End of EIOU flows
      eiou_end <- find_eiou_end(ctry_tbl, flow = flow, non_specified = non_specified, losses = losses)

      ctry_tbl %>% 
        # Add a temporary .rownum column
        tibble::rownames_to_column(var = .rownum) %>%
        dplyr::mutate(
          "{.rownum}" := as.numeric(.data[[.rownum]])
        ) %>% 
        dplyr::mutate(
          # Add the Ledger.side column
          !!as.name(ledger_side) := dplyr::case_when(
            !!as.name(.rownum) <= supply_consumption_split[[1]] ~ supply,
            !!as.name(.rownum) >= supply_consumption_split[[2]] ~ consumption,
            TRUE ~ NA_character_), 
          # Add the Flow.aggregation.point column
          !!as.name(flow_aggregation_point) := dplyr::case_when(
            # Supply side flows
            !!as.name(ledger_side) == supply & !!as.name(flow) %in% tpes_flows ~ tpes,
            !!as.name(ledger_side) == supply & !!as.name(flow) %in% tfc_compare_flows ~ tfc_compare,
            !!as.name(.rownum) >= transformation_start[[2]] & !!as.name(.rownum) <= transformation_end[[1]] ~ transformation_processes,
            !!as.name(.rownum) >= eiou_start[[2]] & !!as.name(.rownum) <= eiou_end[[1]] ~ eiou,
            # Consumption side flows
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
    }) %>% 
    # End of the per-country processing
    # Do some processing on the entire data frame.
    dplyr::mutate(
      # Add method column
      !!as.name(method) := method_val,
      # Add last stage column
      !!as.name(last_stage) := last_stage_val,
      # Add energy type column
      !!as.name(energy_type) := energy_type_val,
      # Add the Unit column
      !!as.name(unit) := unit_val
    ) %>%
    # Remove the rownum column
    dplyr::select(-.rownum) %>%
    # Reorder the columns
    dplyr::select(country, method, energy_type, last_stage, ledger_side, flow_aggregation_point, flow, product, unit, dplyr::everything()) %>%
    # Remove the per-country grouping that we created.
    dplyr::ungroup()
}


#' Creates a tidy IEA data frame
#' 
#' Data from the IEA have years in columns, 
#' but the [tidy data format](https://doi.org/10.18637/jss.v059.i10)
#' requires one row for each datum.
#' This function uses `tidyr::gather()` to 
#' make an IEA data frame tidy.
#' 
#' Default argument values assume that `rename_iea_df_cols()` has been called on `.iea_df`.
#'
#' @param .iea_df a IEA data frame whose columns have been renamed by `rename_iea_df_cols()`
#' @param col_names a list of column names in IEA data frames. Default is `IEATools::iea_cols`.
#' @param year the name of the year column created in `.iea_df` by this function. (Default is "Year".)
#' @param method the name of the method column created in `.iea_df` by this function. (Default is "Method".)
#' @param last_stage the name of the last stage column created in `.iea_df` by this function. (Default is "Last.stage".)
#' @param e_dot the name of the energy/exergy value column created in `.iea_df` by this function. (Default is "E.dot".)
#' @param country the name of the country column in `.iea_df`. (Default is "Country".)
#' @param ledger_side the name of the ledger side in `.iea_df`. (Default is "Ledger.side".)
#' @param flow_aggregation_point the name of the flow aggregation point column in `.iea_df`. (Default is "Flow.aggregation.point".)
#' @param energy_type the name of the energy type column in `.iea_df`. (Default is "Energy.type".)
#' @param unit the name of the unit column in `.iea_df`. (Default is "Units".)
#' @param flow the name of the flow column in `.iea_df`. (Default is "Flow".)
#' @param product the name of the product column in `.iea_df`. (Default is "Product".)
#' @param remove_zeroes a logical indicating whether data points with the value `0` are to be removed from the output. (Default is `TRUE`.)
#'
#' @return a tidy version of `.iea_df` containing new columns `year` and `e_dot` and, optionally, `0` values removed
#' 
#' @export
#'
#' @examples
#' sample_iea_data_path() %>% 
#'   iea_df() %>%
#'   rename_iea_df_cols() %>% 
#'   remove_agg_memo_flows() %>% 
#'   use_iso_countries() %>% 
#'   augment_iea_df() %>% 
#'   tidy_iea_df()
tidy_iea_df <- function(.iea_df, 
                        col_names = IEATools::iea_cols,
                        year = col_names$year,
                        e_dot = col_names$e_dot, 
                        method = col_names$method, 
                        last_stage = col_names$last_stage,
                        country = col_names$country, 
                        ledger_side = col_names$ledger_side, 
                        flow_aggregation_point = col_names$flow_aggregation_point, 
                        energy_type = col_names$energy_type,
                        unit = col_names$unit, 
                        flow = col_names$flow, 
                        product = col_names$product,
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
#' @param .iea_file the path of the file to be loaded. Default loads example data bundled with the package via [sample_iea_data_path()].
#' @param remove_zeroes a logical indicating whether data points with the value `0` are to be removed from the output. (Default is `TRUE`.)
#'
#' @return a tidy, augmented data frame of IEA extended energy balance data.
#' 
#' @export
#'
#' @examples
#' # Check the file first
#' iea_file_OK(sample_iea_data_path())
#' # Take a simple approach
#' simple <- load_tidy_iea_df()
#' # Take the complicated approach
#' complicated <- sample_iea_data_path() %>% 
#'   iea_df() %>%
#'   rename_iea_df_cols() %>% 
#'   clean_iea_whitespace() %>% 
#'   remove_agg_memo_flows() %>% 
#'   use_iso_countries() %>% 
#'   augment_iea_df() %>% 
#'   tidy_iea_df()
#' # simple and complicated should be exactly the same
#' all(simple == complicated)
load_tidy_iea_df <- function(.iea_file = sample_iea_data_path(), 
                             remove_zeroes = TRUE){
  .iea_file %>% 
    iea_df() %>%
    rename_iea_df_cols() %>% 
    clean_iea_whitespace() %>% 
    remove_agg_memo_flows() %>% 
    use_iso_countries() %>% 
    augment_iea_df() %>% 
    tidy_iea_df(remove_zeroes = remove_zeroes)
}



#' Loads region aggregation table
#' 
#' This functions loads a user-defined aggregation table that re-routes each IEA region to a user-defined region.
#' By default, the concordance matrix used re-routes IEA regions to Exiobase regions. See details for more information.
#' 
#' The aggregation table must have a column that identifies the IEA regions to be re-routed (default is "IEA_regions"),
#' and a second column that identifies the new regions to IEA regions are re-routed (default is "Destination_regions"). 
#' There is no need to include all IEA regions; 
#' those that are not included will be removed when calling the `aggregate_regions()` function.
#' IEA regions that are rerouted to "NA" or to an empty vakye are aslso removed when calling the `aggregate_regions()` function.
#' 
#' @param file_path The path of the file (xlsx file) to be loaded. The default path leads to an aggregation table converting IEA regions 
#' into Exiobase regions for 2019 IEA data is loaded. Using the `default_aggregation_table_path()` function, the user can
#' select the default IEA regions to Exiobase regions aggregation table for a different year.
#' @param country The name of the country column. 
#' Default is "country".
#' @param iea_regions The name of the column containing IEA regions.
#' Default is "IEA_regions".
#' @param destination_regions The name of the column containing the destination regions.
#' Default is "Destination_regions".
#' 
#' @return A three column concordance table (as a data frame) mapping the `iea_regions` column to a `destination_regions` column,
#' using a `country` column (with iso country IDs) as intermediate, which is added to the loaded aggregation table 
#' within the function. 
#' For those IEA regions that do not match to an ISO code (for instance, "World marine bunkers"), 
#' the full IEA region name is kept in the `country` column.
#' 
#' @export
#' 
#' @examples
#' read_regions_concordance() # Returns the default aggregation table for the year 2019
#' read_regions_concordance(file_path = default_aggregation_table_path(2020)) # Returns the default aggregation table for the year 2020
#' read_regions_concordance(file_path = "extdata/checking_aggregation_GHA_ZAF.xslx") # Returns an aggregation table that aggregates Ghana and South Africa into a new GHAZAF region
read_regions_concordance <- function(file_path = default_aggregation_table_path(2019),
                                     country = IEATools::iea_cols$country,
                                     iea_regions = "IEA_regions",
                                     destination_regions = "Destination_regions"){
  concordance_table <- openxlsx::read.xlsx(file_path) %>%
    dplyr::mutate(
      "{country}" := .data[[iea_regions]]
    ) %>%
    use_iso_countries(country = country) %>%
    dplyr::filter(! (is.na(.data[[destination_regions]]) | .data[[destination_regions]] == "" | is.null(.data[[destination_regions]])))
  return(concordance_table)
}
# --- EAR, 02/09/2020

#' Aggregates IEA regions based on a user-defined concordance matrix
#' 
#' Takes as input a tidy dataframe, the file of a country concordance table, and aggregates flows per regions following the
#' user-defined concordance table. The boolean argument "net_trade" enables to perform the aggregation by keeping only net imports
#' and/or net exports or by keeping gross imports and exports.
#' 
#' @param .tidy_iea_df The `.tidy_iea_df` data frame that needs to be aggregated by regions. The `.tidy_iea_df` is likely
#' to have been obtained with the `load_tidy_iea_df()` function.
#' @param file_path The path of the file to be loaded. By default, a concordance table converting IEA regions into Exiobase regions
#' is loaded.
#' @param net_trade The boolean that defines whether imports and exports by aggregation region should be converted 
#' into net imports / exports or not. Default is FALSE.
#' 
#' @return A `.tidy_iea_df` that contains the data of the input `.tidy_iea_df` aggregated by regions as specified in the user-defined
#' country concordance table provided.
#' 
#' @export
#' 
#' @examples 
aggregate_regions <- function(.tidy_iea_df,
                              aggregation_table = "aggregation_table",
                              net_trade = FALSE, 
                              country = IEATools::iea_cols$country, 
                              destination_regions = "Destination_regions"){
  
  iea_code_regions <- aggregation_table[[country]]
  dest_regions <- as.character(aggregation_table[[destination_regions]])
  
  aggregated_tidy_iea_df <-.tidy_iea_df %>%
    dplyr::filter(
      .data[[country]] %in% iea_code_regions
      ) %>%
    dplyr::inner_join(
      aggregation_table, by = country
      ) %>%
    dplyr::mutate(#ask Matt here
      # Country = Destination_regions
      "{country}" := .data[[destination_regions]]
    ) %>%
    dplyr::group_by(#ask Matt here
      Country, Method, Energy.type, Last.stage, Year, Ledger.side, Flow.aggregation.point, Flow, Product, Unit
      ) %>%
    dplyr::summarise(#ask Matt here
      E.dot = sum(E.dot)
      )
  
  if (net_trade == TRUE){
    aggregated_net_trade <- aggregated_tidy_iea_df %>% 
      dplyr::filter(Flow == "Imports" | Flow == "Exports") %>% 
      tidyr::pivot_wider(names_from = Flow, values_from = E.dot) %>% 
      dplyr::mutate(
        Imports = tidyr::replace_na(Imports, 0),
        Exports = tidyr::replace_na(Exports, 0),
        Net_Imports = Imports + Exports
      ) %>% 
      tidyr::pivot_longer(cols = c("Imports", "Exports", "Net_Imports"), names_to = "Flow", values_to = "E.dot") %>%
      dplyr::filter(Flow == "Net_Imports") %>% 
      dplyr::mutate(
        Flow = dplyr::case_when(
          E.dot > 0 ~ "Imports",
          E.dot < 0 ~ "Exports",
          E.dot == 0 ~ "Net_Imports"
        )
      ) %>% 
      dplyr::filter(E.dot != 0) %>%
      dplyr::arrange(Year, Country, desc(Ledger.side), Flow.aggregation.point, Flow)
    
    aggregated_tidy_iea_df <- aggregated_tidy_iea_df %>% 
      dplyr::filter(! Flow %in% c("Imports", "Exports")) %>%
      dplyr::bind_rows(aggregated_net_trade) %>%
      dplyr::arrange(Year, Country, desc(Ledger.side), Flow.aggregation.point, Flow)
  }
  
  return(aggregated_tidy_iea_df)
}
# --- EAR, 02/09/2020

#Tidy_IEA_df <- load_tidy_iea_df("/home/manolo/Documents/Extended-Energy-Balances-2018/IEA Extended Energy Balances 2019.csv")

