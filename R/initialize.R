#' Slurp an IEA extended energy balance data file
#' 
#' This is the internal helper function that reads IEA data files. 
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
#' This function is designed to work as more years are added
#' in columns at the right of the `.iea_file`, 
#' because column names in the output are constructed from the header line(s) of `.iea_file` 
#' (which contain years and country, flow, product information).
#' 
#' Extended energy balance data can be obtained from the IEA 
#' as a *.ivt file.
#' To export the data for use with the IEATools package, 
#' perform the following actions:
#' 
#' 1. Open the *.ivt file in the Beyond 20/20 browser on a relatively high-powered computer
#'    with lots of memory, because the file is very large.
#' 2. Arrange the columns in the following order: "COUNTRY", "FLOW", "PRODUCT", followed by years.
#' 3. Change to the unit (ktoe or TJ) desired.
#' 4. Save the results in .csv format. (Saving may take a while.)
#' 
#' This function is vectorized over `.iea_file`.
#'
#' @param .iea_file The path to the raw IEA data file for which quality assurance is desired.
#'                  Can be a vector of file paths, in which case
#'                  each file is loaded sequentially and stacked together
#'                  with [dplyr::bind_rows()].
#' @param text A string containing text to be parsed as an IEA file.
#'             Can be a vector of text strings, in which case
#'             each string is processed sequentially and stacked to gether
#'             with [dplyr::bind_rows()].
#' @param expected_1st_line_start The expected start of the first line of `iea_file`. Default is ",,TIME".
#' @param country The name of the country column. 
#'                Default is "COUNTRY".
#' @param expected_2nd_line_start The expected start of the second line of `iea_file`. Default is "COUNTRY,FLOW,PRODUCT".
#' @param expected_simple_start The expected starting of the first line of `iea_file`. 
#'                              Default is the value of `expected_2nd_line_start`.
#'                              Note that `expected_simple_start` is sometimes encountered in data supplied by the IEA.
#'                              Furthermore, `expected_simple_start` could be the format of the file when somebody "helpfully" fiddles with 
#'                              the raw data from the IEA.
#' @param ensure_ascii_countries A boolean that tells whether to convert country names
#'                               to pure ASCII, removing diacritical marks and accents.
#'                               Default is `TRUE`.
#'
#' @return A raw data frame of IEA extended energy balance data with appropriate column titles.
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
                                country = "COUNTRY",
                                expected_2nd_line_start = paste0(country, ",FLOW,PRODUCT"), 
                                expected_simple_start = expected_2nd_line_start, 
                                ensure_ascii_countries = TRUE) {
  assertthat::assert_that(xor(!is.null(.iea_file), !is.null(text)), 
                          msg = "need to supply only one of .iea_file or text arguments to iea_df")
  if (!is.null(.iea_file)) {
    conns <- lapply(.iea_file, FUN = function(this_iea_file) {
      file(this_iea_file, open = "rt") # open file connection
    })
    text <- rep_len("", length(.iea_file))
  } else {
    # text has been provided, probably for testing purposes.
    conns <- list(textConnection(text))
    text <- list(text)
    .iea_file <- list(NULL)
  }
  
  Map(conns, .iea_file, text, f = function(this_conn, this_iea_file, this_text) {
    # Check if the first line has the simple format
    first_two_lines <- this_conn |> 
      readLines(n = 2)
    close(this_conn)
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
      if (!is.null(this_iea_file)) {
        IEAData_withheader <- data.table::fread(file = this_iea_file, header = TRUE, sep = ",")
      } else {
        IEAData_withheader <- data.table::fread(text = this_text, header = TRUE, sep = ",")
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
      if (!is.null(this_iea_file)) {
        # Slurp the file. This slurping ignores the header, which we know are the first 2 lines.
        # Note that I'm using data.table::fread at the recommendation of
        # https://statcompute.wordpress.com/2014/02/11/efficiency-of-importing-large-csv-files-in-r/
        # which indicates this function is significantly faster than other options.
        IEAData_noheader <- data.table::fread(file = this_iea_file, header = FALSE, sep = ",", skip = 2, encoding = "Latin-1")
      } else {
        IEAData_noheader <- data.table::fread(text = this_text, header = FALSE, sep = ",", skip = 2, encoding = "Latin-1")
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
    # Convert the country column to pure ASCII, if desired.
    if (ensure_ascii_countries) {
      IEAData_withheader <- IEAData_withheader %>%
        dplyr::mutate(
          # This hint is from
          # https://stackoverflow.com/questions/39148759/remove-accents-from-a-dataframe-column-in-r
          # COUNTRY = stringi::stri_trans_general(COUNTRY,id = "Latin-ASCII")
          # iconv is much faster than stringi.
          # First, convert from latin1 to ascii. 
          "{country}" := iconv(.data[[country]], from = "latin1", to = "ASCII//TRANSLIT"), 
          # However, this results in "^o" for o with circumflex as in Côte d'Ivoire.
          # So replace those strings with simple "o"
          "{country}" := gsub(.data[[country]], pattern = "\\^o", replacement = "o")
        )
    }
    return(IEAData_withheader)
  }) |> 
    dplyr::bind_rows()
    

  
  # lapply(conns, FUN = function(this_conn) {
  #   # Check if the first line has the simple format
  #   first_two_lines <- this_conn |> 
  #     readLines(n = 2)
  #   close(this_conn)
  #   assertthat::assert_that(length(first_two_lines) == 2, msg = "couldn't read 2 lines in iea_df")
  #   # first_line <- first_two_lines[[1]]
  #   # second_line <- first_two_lines[[2]]
  #   # Eliminate any quotes that are present
  #   first_line <- gsub(pattern = '\\"', replacement = "", x = first_two_lines[[1]])
  #   second_line <- gsub(pattern = '\\"', replacement = "", x = first_two_lines[[2]])
  #   # Ensure that we have an expected format for the first line or two in first_two_lines.
  #   assertthat::assert_that(first_line %>% startsWith(expected_simple_start) | 
  #                             (first_line %>% startsWith(expected_1st_line_start) & second_line %>% startsWith(expected_2nd_line_start)), 
  #                           msg = paste0(".iea_file must start with ",
  #                                        "first line: '", expected_simple_start, "', ",
  #                                        "or ",
  #                                        "first line: '", expected_1st_line_start, "' and ",
  #                                        "second line: '", expected_2nd_line_start, "'.  ",
  #                                        "Instead, found ",
  #                                        "first line: '", first_line, "', ",
  #                                        "second line: '", second_line, "'."))
  #   if (first_line %>% startsWith(expected_simple_start)) {
  #     # We have the simple start to the file, so we can assume a one-line header.
  #     if (!is.null(.iea_file)) {
  #       IEAData_withheader <- data.table::fread(file = .iea_file, header = TRUE, sep = ",")
  #     } else {
  #       IEAData_withheader <- data.table::fread(text = text, header = TRUE, sep = ",")
  #     }
  #   } else if (first_line %>% startsWith(expected_1st_line_start) & 
  #              second_line %>% startsWith(expected_2nd_line_start)) {
  #     # We have the complicated start to the file, so go through some additional work to apply the proper header
  #     # to the file.
  #     if (second_line %>% endsWith(",")) {
  #       # The file may have been opened in Excel and resaved.
  #       # When that occurs, many commas are appended to the 2nd line.
  #       # Strip out these commas before proceeding further.
  #       # The pattern ,*$ means "match any number (*) of commas (,) at the end of the line ($)".
  #       second_line <- sub(pattern = ",*$", replacement = "", second_line)
  #     }
  #     if (!is.null(.iea_file)) {
  #       # Slurp the file. This slurping ignores the header, which we know are the first 2 lines.
  #       # Note that I'm using data.table::fread at the recommendation of
  #       # https://statcompute.wordpress.com/2014/02/11/efficiency-of-importing-large-csv-files-in-r/
  #       # which indicates this function is significantly faster than other options.
  #       IEAData_noheader <- data.table::fread(file = .iea_file, header = FALSE, sep = ",", skip = 2, encoding = "Latin-1")
  #     } else {
  #       IEAData_noheader <- data.table::fread(text = text, header = FALSE, sep = ",", skip = 2, encoding = "Latin-1")
  #     }
  #     # At this point, the IEAData_noheader data frame has default (meaningless) column names, V1, V2, V3, ...
  #     # Create column names from the header lines that we read previously.
  #     # The code here should be robust to adding more years through time,
  #     # because it simply replaces the first 3 items of the first line
  #     # with appropriate values from the 2nd line.
  #     cnames <- gsub(pattern = expected_1st_line_start, replacement = expected_2nd_line_start, first_line) %>%
  #       strsplit(",") %>%
  #       unlist()
  #     IEAData_withheader <- IEAData_noheader %>%
  #       magrittr::set_names(cnames)
  #   }
  #   # Convert the country column to pure ASCII, if desired.
  #   if (ensure_ascii_countries) {
  #     IEAData_withheader <- IEAData_withheader %>%
  #       dplyr::mutate(
  #         # This hint is from
  #         # https://stackoverflow.com/questions/39148759/remove-accents-from-a-dataframe-column-in-r
  #         # COUNTRY = stringi::stri_trans_general(COUNTRY,id = "Latin-ASCII")
  #         # iconv is much faster than stringi.
  #         # First, convert from latin1 to ascii. 
  #         "{country}" := iconv(.data[[country]], from = "latin1", to = "ASCII//TRANSLIT"), 
  #         # However, this results in "^o" for o with circumflex as in Côte d'Ivoire.
  #         # So replace those strings with simple "o"
  #         "{country}" := gsub(.data[[country]], pattern = "\\^o", replacement = "o")
  #       )
  #   }
  #   return(IEAData_withheader)
  # }) |> 
  #   dplyr::bind_rows()
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
#' If `.slurped_iea_df` is `NULL` (the default), 
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
#' @param .slurped_iea_df a data frame created by [slurp_iea_to_raw_df()]
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
    # dplyr::select(rowid, flow, product) %>% 
    dplyr::select(dplyr::all_of(c(rowid, flow, product))) %>% 
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
    # dplyr::select(rowid) %>% 
    dplyr::select(dplyr::all_of(rowid)) %>% 
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
#' @param new_country the new name for the country column. (Default is `IEATools::iea_cols$country`.)
#' @param flow the original name for the flow column. (Default is `FLOW`.)
#' @param new_flow the new name for the flow column. (Default is `IEATools::iea_cols$flow`.)
#' @param product the original name for the product column. (Default is `PRODUCT`.)
#' @param new_product the new name for the product column. (Default is `IEATools::iea_cols$product`.)
#'
#' @return `.iea_df` with renamed columns
#' 
#' @export
#'
#' @examples
#' iea_df(text = ",,TIME,1960,1961\nCOUNTRY,FLOW,PRODUCT\nWorld,Production,Hard coal,42,43") %>% 
#'   rename_iea_df_cols()
rename_iea_df_cols <- function(.iea_df, 
                               country = "COUNTRY", new_country = IEATools::iea_cols$country, 
                               flow = "FLOW", new_flow = IEATools::iea_cols$flow, 
                               product = "PRODUCT", new_product = IEATools::iea_cols$product){
  .iea_df %>% 
    dplyr::rename(
      "{new_country}" := dplyr::all_of(country),
      "{new_flow}" := dplyr::all_of(flow),
      "{new_product}" := dplyr::all_of(product)
    )
}


#' Clean whitespace from Flow and Product strings
#' 
#' Occasionally, in the IEA extended energy balance data, 
#' extra whitespace characters are found at the beginning or end of `Flow` and `Product` strings.
#' This function removes all leading and trailing whitespece.
#'
#' @param .iea_df A data frame containing `Flow` and `Product` columns.
#' @param flow The name of the flow column in `iea_df`. Default is "`Flow`".
#' @param product The name of the product columns in `iea_df`. Default is "`Product`".
#'
#' @return `.iea_df` with leading and trailing whitespace removed from `Flow` and `Product` column strings
#' 
#' @export
#'
#' @examples
#' data.frame(Flow = "  a flow   ", Product = "   a product   ", stringsAsFactors = FALSE) %>% 
#'   clean_iea_whitespace()
clean_iea_whitespace <- function(.iea_df, 
                                 flow = IEATools::iea_cols$flow,
                                 product = IEATools::iea_cols$product){
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
#' Internally, the `countrycode` package is used for ISO codes where possible.
#' Optionally, an `override_df` can be specified.
#' `override_df` should contain columns `pfu_code` (for 3-letter ISO codes) and `iea_name` for country names.
#' 
#' If neither the `countrycode` package nor `override_df` contain a matching `country` name, 
#' the `country` column is left untouched.
#' 
#' By default, special cases are considered via the `override_df` argument.
#' The default value of `override_df` sets codes for China and Hong Kong,
#' as well as World X bunkers.
#'
#' @param .iea_df A data frame containing a `country` column.
#' @param override_df A data frame containing columns named `pfu_code` and `iea_name`.
#'                    Default is `IEATools::overide_iso_codes_df`.
#' @param country The name of the country column in `.iea_df`. 
#'                Default is `IEATools::iea_cols$country`.
#' @param pfu_code,iea_name See `IEATools::countryconcordance_cols`.
#' @param override_suffix A suffix added to override columns. 
#'                        Default is "...override".
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
                              override_df = IEATools::override_iso_codes_df,
                              country = IEATools::iea_cols$country,
                              pfu_code = IEATools::country_concordance_cols$pfu_code, 
                              iea_name = IEATools::country_concordance_cols$iea_name, 
                              override_suffix = "...override"){
  # Eliminates warnings on column names in the countrycode::codelist data frame.
  country.name.en <- "country.name.en" 
  iso_type <- "iso3c"
  # Load country code information from the countrycode package
  CountryCodeInfo <- countrycode::codelist %>%
    dplyr::select(dplyr::all_of(c(country.name.en, iso_type))) %>% 
    dplyr::rename(
      "{country}" := dplyr::all_of(country.name.en), 
      "{pfu_code}" := dplyr::all_of(iso_type)
    )
  # Strategy is to do two left_joins.
  # The first left_join is with the countrycode information.
  # That will fail for the case of China and World X bunkers, because
  # the countrycode data does not have "People's Republic of China" or "World X bunkers".
  # The second left_join will be with the override_df and create a column named pfu_code...override.
  # Then, we discriminate among the options, 
  # lastly, leaving the Country column unchanged, if no code has been found.
  override_col_name <- paste0(pfu_code, override_suffix)
  
  dplyr::left_join(.iea_df, CountryCodeInfo, by = country) %>% 
    # Make sure that the override_df has only two columns:
    # iea_name and pfu_code.
    dplyr::left_join(override_df %>% 
                       dplyr::rename("{country}" := dplyr::all_of(iea_name)) %>% 
                       dplyr::select(dplyr::all_of(c(country, pfu_code))), 
                     by = country, 
                     suffix = c("", override_suffix)) %>% 
    dplyr::mutate(
      "{pfu_code}" := dplyr::case_when(
        # First priority, if we got a match in override_df, use it.
        !is.na(.data[[override_col_name]]) ~ .data[[override_col_name]], 
        # If we got a match from countrycode, use it.
        !is.na(.data[[pfu_code]]) ~ .data[[pfu_code]], 
        # If neither the countrycode information nor the override_df information matches, 
        # leave the existing Country name.
        TRUE ~ .data[[country]],
      )
    ) %>% 
    # Now we can get rid of the override column and the country column.
    dplyr::select(-dplyr::all_of(c(override_col_name, country))) %>% 
    # And rename the iso_type column to be country
    dplyr::rename(
      "{country}" := dplyr::all_of(pfu_code)
    ) %>% 
    # And put the country column first.
    dplyr::select(dplyr::all_of(country), dplyr::everything())
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
#'                Default is `IEATools::iea_cols$country`.
#' @param agg_regions A list of aggregation regions in the `country` column of `.iea_df`.
#'                    Default is `IEATools::aggregation_regions`.
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
#' This function prepares an IEA data frame created by `iea_df()` for use in R.
#' It works on IEA data from the 2018 and 2019 releases
#' of the IEA's extended energy balances.
#' 
#' This function solves several problems.
#' The first problem is that metadata in the `COUNTRY`, `FLOW`, and `PRODUCT`
#' columns of an IEA data table are not unique.
#' A second problem is that the `FLOW` column contains both industries to which energy is flowing _and_ 
#' the type of flow that is involved.  
#' (E.g., the suffix "(energy)" means that the flow is an own use by the energy industry.
#' The "(transf.)" suffix means that a flow is involved in a transformation process
#' between primary and final energy. 
#'
#' To solve these problems, two additional columns are added: `Ledger.side` and `Flow.aggregation.point`.
#' `Ledger.side` can be one of "Supply" or "Consumption", corresponding to the top or bottom of the IEA's tables, respectively.
#' `Flow.aggregation.point` indicates the next level of aggregation for these data. 
#' `Flow.aggregation.point` can be one of 
#' "Total primary energy supply", "Transformation processes", "Energy industry own use", or "TFC compare"
#' on the `Supply` side of the ledger.
#' On the `Consumption` side of the ledger, `Flow.aggregation.point` can be one of 
#' "Industry", "Transport", "Other", "Non-energy use", or "Memo: Non-energy use in industry".
#' When the `Flow.aggregation.point` column is present, 
#' the need for the "(energy)" and "(transf.)" suffixes is eliminated,
#' so they are deleted.
#' 
#' The third problem this function solves is that energy type and units are not specified in IEA data.
#' An `EnergyType` column is added with the value of `energy_type_val`. 
#' (Default is `E`, for energy, as opposed to `X`, which would be exergy.)
#' A `Unit` column is added with the value of `unit_val`.
#' (Default is "TJ", although any string can be specified in `unit_val`.)
#' 
#' Note that this function decides where to divide `Supply` from `Consumption`. 
#' To do so, it first looks for rows in which `Flow` is "Losses".
#' The last "Losses" row is the last row of the "Supply" side of the ledger. 
#' If "Losses" rows are not found, the function looks for rows in which `Flow` is "Total final consumption".
#' The first "Total final consumption" row is the first row of the `Consumption` side of the ledger.
#' If neither "Losses" nor "Total final consumption" `Flow`s are present, 
#' an error is generated.
#'
#' @param .iea_df A data frame produced by the `iea_df()` function.
#' @param country The name of the country column in `.iea_df`. 
#'                Default is `IEATools::iea_cols$country`.
#' @param ledger_side The name of the ledger side column to be added to `.iea_df`. 
#'                    Default is `IEATools::iea_cols$ledger_side`.
#' @param flow_aggregation_point The name of the flow aggregation point column to be added to `.iea_df`. 
#'                               Default is `IEATools::iea_cols$flow_aggregation_point`.
#' @param flow The name of the flow column in `.iea_df`.  
#'             Default is `IEATools::iea_cols$flow`.
#' @param product The name of the product column in `.iea_df`.  
#'                Default is `IEATools::iea_cols$product`.
#' @param energy_type The name of the energy type column to be added to `.iea_df`. 
#'                    Default is `IEATools::iea_cols$energy_type`.
#' @param energy_type_val The value to put in the `energy_type` column. 
#'                        Default is "E".
#' @param method The name of the method column to be added to `.iea_df`. 
#'               Default is `IEATools::iea_cols$method`.
#' @param method_val The value to put in the `method` column. 
#'                   Default is "PCM" (Physical Content Method, which is used by the IEA).
#' @param last_stage The name of the last stage column to be added to `.iea_df`. 
#'                   Default is `IEATools::iea_cols$last_stage`.
#' @param last_stage_val The value to put in the `last_stage` column. 
#'                       Default is "Final" (which is the last stage supplied by the IEA).
#' @param unit The name of the unit column to be added to `.iea_df`. 
#'             Default is `IEATools::iea_cols$unit`.
#' @param unit_val The value to put in the `unit` column. 
#'                 Default is "TJ" for terajoule.
#' @param supply The string that identifies supply ledger side. 
#'               Default is `IEATools::iea_cols$ledger_side`.
#' @param consumption The string that identifies consumption `Ledger.side`. 
#'                    Default is "Consumption".
#' @param tpes The string that identifies total primary energy supply `Flow.aggregation.point`. 
#'             Default is "Total primary energy supply".
#' @param tpes_flows A vector of strings that give flows that are aggregated to `Total primary energy supply`. 
#' @param tfc_compare A string that identifies the `TFC compare` flow aggregation point. 
#'                    Default is "TFC compare".
#' @param tfc_compare_flows A vector of strings that give `Flow`s that are aggregated to `TFC compare`.
#' @param transfers = A string that identifies transfers in the flow column. 
#'                    Default is "Transfers".
#' @param statistical_differences A string that identifies statistical differences in flow column. 
#'                                Default is "Statistical differences".
#' @param losses The string that indicates losses in the `Flow` column. 
#'               Default is "Losses".
#' @param transformation_processes The string that indicates transformation processes in the `Flow` column. 
#'                                 Default is "Transformation processes".
#' @param tp_flows_suffix The suffix for transformation processes in the `Flow` column. 
#'                        Default is "(transf.)".
#' @param nstp_flows_suffix The suffix for non-specified transformation processes in the `Flow` column. 
#'                          Default is "(transformation)".
#' @param mapep The string that identifies main activity producer electricity plants in the `Flow` column. 
#'              Default is "Main activity producer electricity plants".
#' @param eiou The string that identifies energy industry own use in the `Flow` column.
#'             Default is "Energy industry own use".
#' @param eiou_flows_suffix The suffix for energy industry own use in the `Flow` column. 
#'                          Default is "(energy)".
#' @param coal_mines The string that identifies coal mines in the `Flow` column. 
#'                   Default is "Coal mines".
#' @param non_specified The string that identifies non-specified flows in the `Flow` column. 
#'                      Default is "Non-specified".
#' @param tfc The string that identifies total final consumption in the `Flow` column. 
#'            Default is "Total final consumption".
#' @param tfc_flows A vector of strings that give total final consumption in the `Flow` column.
#' @param industry A string that names the industry `Flow.aggregation.point`. 
#'                 Default is "Industry".
#' @param industry_flows A vector of strings representing `Flow`s to be aggregated in the `Industry` `Flow.aggregation.point`. 
#' @param iron_and_steel A string that identifies the iron and steel industry. 
#'                       Default is "Iron and steel".
#' @param mining_and_quarrying A string that identifies the mining and quarrying industry.
#'                             Default is "Mining and quarrying".
#' @param transport A string that names the transport `Flow.aggregation.point`.
#'                  Default is "Transport".
#' @param transport_flows A vector of strings representing `Flow`s to be aggregated in the `Transport` `Flow.aggregation.point`. 
#' @param other A string that names the other `Flow.aggregation.point`. 
#'              Default is "Other".
#' @param other_flows A vector of strings representing `Flow`s to be aggregated in the `Other` `Flow.aggregation.point`. 
#' @param non_energy A string that names the non-energy `Flow.aggregation.point`. 
#'                   Default is "Non-energy use".
#' @param non_energy_flows A list of `Flow`s to be aggregated to the `Non-energy use` `Flow.aggregation.point`. 
#' @param memo_non_energy_flows A list of `Flow`s to be aggregated to "Memo: Non-energy use in industry". 
#'                              Default is `IEATools::memo_non_energy_flows`.
#' @param electricity_output A string that names the electricity output `Flow`. 
#'                           Default is "Electricity output (GWh)". 
#' @param electricity_output_flows_prefix A string prefix for `Flow`s to be aggregated in electricity output. 
#'                                        Default is "Electricity output (GWh)-".
#' @param heat_output A string that names the heat output `Flow`. 
#'                    Default is "Heat output". 
#' @param heat_output_flows_prefix A string prefix for `Flow`s to be aggregated in heat output. 
#'                                 Default is "Heat output-".
#' @param .rownum The name of a column created (and destroyed) internally by this function. 
#'                The `.rownum` column temporarily holds row numbers for internal calculations.
#'                The `.rownum` column is deleted before returning. 
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
                           country = IEATools::iea_cols$country, 
                           ledger_side = IEATools::iea_cols$ledger_side, 
                           flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point, 
                           flow = IEATools::iea_cols$flow, 
                           product = IEATools::iea_cols$product,
                           energy_type = IEATools::iea_cols$energy_type, energy_type_val = "E",
                           method = IEATools::iea_cols$method, method_val = "PCM",
                           last_stage = IEATools::iea_cols$last_stage, last_stage_val = "Final",
                           unit = IEATools::iea_cols$unit, unit_val = "TJ",
                           supply = "Supply", 
                           consumption = "Consumption",
                           tpes = IEATools::tfc_compare_flows$total_primary_energy_supply, 
                           tpes_flows = IEATools::tpes_flows,
                           tfc_compare = "TFC compare",
                           tfc_compare_flows = IEATools::tfc_compare_flows,
                           transfers = "Transfers",
                           statistical_differences = IEATools::tfc_compare_flows$statistical_differences,
                           losses = IEATools::tfc_compare_flows$losses, 
                           transformation_processes = IEATools::tfc_compare_flows$transformation_processes,
                           tp_flows_suffix = "(transf.)",
                           nstp_flows_suffix = "(transformation)",
                           mapep = IEATools::transformation_processes$main_activity_producer_electricity_plants,
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
                           non_energy_flows = IEATools::non_energy_flows,
                           memo_non_energy_flows = IEATools::memo_non_energy_flows,
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
          "{ledger_side}" := dplyr::case_when(
            .data[[.rownum]] <= supply_consumption_split[[1]] ~ supply,
            .data[[.rownum]] >= supply_consumption_split[[2]] ~ consumption,
            TRUE ~ NA_character_), 
          # Add the Flow.aggregation.point column
          "{flow_aggregation_point}" := dplyr::case_when(
            # Supply side flows
            .data[[ledger_side]] == supply & .data[[flow]] %in% tpes_flows ~ tpes,
            .data[[ledger_side]] == supply & .data[[flow]] %in% tfc_compare_flows ~ tfc_compare,
            .data[[.rownum]] >= transformation_start[[2]] & .data[[.rownum]] <= transformation_end[[1]] ~ transformation_processes,
            .data[[.rownum]] >= eiou_start[[2]] & .data[[.rownum]] <= eiou_end[[1]] ~ eiou,
            # Consumption side flows
            .data[[ledger_side]] == consumption & .data[[flow]] == tfc ~ NA_character_,
            .data[[ledger_side]] == consumption & .data[[flow]] %in% tfc_flows ~ tfc,
            .data[[ledger_side]] == consumption & .data[[flow]] %in% industry_flows ~ industry,
            .data[[ledger_side]] == consumption & .data[[flow]] %in% transport_flows ~ transport,
            .data[[ledger_side]] == consumption & .data[[flow]] %in% other_flows ~ other,
            .data[[ledger_side]] == consumption & starts_with_any_of(.data[[flow]], non_energy_flows) ~ non_energy,
            # This first Memo: Non-energy use in industry has Non-energy use in industry/transformation/energy as its aggregation point.
            .data[[ledger_side]] == consumption & .data[[flow]] == memo_non_energy_flows$memo_non_energy_use_in_industry ~ non_energy_flows$non_energy_use_industry_transformation_energy,
            # All other Memo: Non-energy use in xxxxxx flows have Memo: Non-energy use in industry as their aggregation point.
            .data[[ledger_side]] == consumption & .data[[flow]] %in% memo_non_energy_flows ~ memo_non_energy_flows$memo_non_energy_use_in_industry,
            # Electricity output itself does not aggregate to anything.
            .data[[ledger_side]] == consumption & .data[[flow]] == electricity_output ~ NA_character_,
            # But Electricity output (GWh)- aggregates to Electricity output (GWh)
            .data[[ledger_side]] == consumption & startsWith(.data[[flow]], electricity_output_flows_prefix) ~ electricity_output,
            # Heat output itself does not aggregate to anything.
            .data[[ledger_side]] == consumption & .data[[flow]] == heat_output ~ NA_character_,
            # But Heat output- aggregates to Heat output.
            .data[[ledger_side]] == consumption & startsWith(.data[[flow]], heat_output_flows_prefix) ~ heat_output,

            TRUE ~ NA_character_
          )
        )
    }) %>% 
    # End of the per-country processing
    # Do some processing on the entire data frame.
    dplyr::mutate(
      # Add method column
      "{method}" := method_val,
      # Add last stage column
      "{last_stage}" := last_stage_val,
      # Add energy type column
      "{energy_type}" := energy_type_val,
      # Add the Unit column
      "{unit}" := unit_val
    ) %>%
    # Remove the rownum column
    # dplyr::select(-.rownum) %>%
    dplyr::select(-dplyr::any_of(.rownum)) %>%
    # Reorder the columns
    # dplyr::select(country, method, energy_type, last_stage, ledger_side, flow_aggregation_point, flow, product, unit, dplyr::everything()) %>%
    dplyr::select(dplyr::all_of(c(country, method, energy_type, last_stage, ledger_side, flow_aggregation_point, flow, product, unit)), dplyr::everything()) %>%
    # Remove the per-country grouping that we created.
    dplyr::ungroup()
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
#' @param .iea_df A data frame of IEA data.
#' @param flow The name of the flow column in `iea_df`. 
#'             Default is `IEATools::iea_cols$flow`.
#' @param product The name of the product columns in `iea_df`. 
#'                Default is `IEATools::iea_cols$product`.
#' @param agg_flows A vector of strings identifying `Flow`s that are aggregations.
#'                  Default is `IEATools::aggregation_flows`.
#' @param memo_flow_prefixes A vector of string prefixes for flow memo rows in `.iea_df`.
#'                           Default is `IEATools::memo_aggregation_flow_prefixes`.
#' @param memo_product_prefixes A string prefix for product memo rows in `.iea_df`.
#'                              Default is `IEATools::memo_aggregation_product_prefixes`.
#'
#' @return `.iea_df` without its aggregation rows.
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


#' Specify Non-energy use when possible
#' 
#' For some countries and years, Non-energy use details are supplied 
#' by "Memo:" fields. 
#' We use those details if they exist.
#' This function assess the energy balance of
#' replacing "Non-energy use industry/transformation/energy" by
#' "Non-energy use in <<specific industry>>"
#' rows.
#' If there is an imbalance, an attempt is made to adjust the imbalance.
#' 
#' Note that energy balance checks are _not_ performed on the incoming `.iea_df`,
#' but that the specification process itself
#' ensures that the modifications, themselves, are balanced.
#' In other words, specifying Non-energy use flows 
#' will not change the energy balance of the incoming `.iea_df` data frame.
#' 
#' This function does _not_ promise the resulting data frame
#' is internally consistent. 
#' Rather, this function should be considered a step along the path to 
#' creating a coherent data frame of IEA data.
#' Specifically, 
#' this function does not specify aggregation or Memo: flows, so
#' coherence will be achieved again only after
#' `remove_agg_memo_flows()`
#' is called.
#' Note that `load_tidy_iea_df(specify_non_energy_flows = TRUE)` 
#' correctly calls `remove_agg_memo_flows()` internally, so 
#' using `load_tidy_iea_df()` is preferred to calling `specify_non_energy_use()` directly.
#'
#' @param .iea_df A data frame of IEA data, 
#'                created by `augment_iea_df()`.
#' @param country,year,method,energy_type,unit,ledger_side,last_stage,flow_aggregation_point,flow,product See `IEATools::iea_cols`.
#' @param non_energy_use See `IEATools::aggregation_flows`.
#' @param non_energy_flows_industry_transformation_energy See `IEATools::non_energy_flows`.
#' @param memo_non_energy_use_in_industry_not_elsewhere_specified See `IEATools::memo_non_energy_flows`.
#' @param memo A string prefix for memo flows. 
#'             Default is `IEATools::memo_aggregation_flow_prefixes$memo`.
#' @param memo_non_energy_flows_industry See `IEATools::memo_non_energy_flows`.
#' @param non_energy_use_in_industry_not_elsewhere_specified Same as `memo_non_energy_use_in_industry_not_elsewhere_specified` without the `memo` prefix.
#' @param memo_non_energy_use_in A prefix for specific Non-energy use flows.
#'                               Default is "Memo: Non-energy use in ".
#' @param non_energy_use_in Same as `memo_non_energy_use_in` without the `memo` prefix.
#' @param total See `IEATools::memo_aggregation_product_prefixes`.
#' @param .values An internal column name.
#'                Default is `IEATools::template_cols$.values`.
#' @param .values_summarised An internal column name.
#'                           Default is `paste0(.values, "_summarised")`.
#' @param .diff An internal column name. 
#'              Default is ".diff".
#' @param tol The tolerance for differences from `0`.
#'            Default is `1e-6`.
#'
#' @return `.iea_df` with specified `Non-energy use`s, where possible.
#' 
#' @export
#'
#' @examples
#' sample_iea_data_path() %>% 
#'   iea_df() %>%
#'   rename_iea_df_cols() %>% 
#'   clean_iea_whitespace() %>%
#'   augment_iea_df() %>%
#'   specify_non_energy_use()
specify_non_energy_use <- function(.iea_df, 
                                   country = IEATools::iea_cols$country,
                                   year = IEATools::iea_cols$year,
                                   method = IEATools::iea_cols$method,
                                   energy_type = IEATools::iea_cols$energy_type,
                                   unit = IEATools::iea_cols$unit,
                                   ledger_side = IEATools::iea_cols$ledger_side, 
                                   last_stage = IEATools::iea_cols$last_stage,
                                   flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                                   flow = IEATools::iea_cols$flow, 
                                   product = IEATools::iea_cols$product,
                                   non_energy_use = IEATools::aggregation_flows$non_energy_use,
                                   non_energy_flows_industry_transformation_energy = 
                                     IEATools::non_energy_flows[[
                                       "non_energy_use_industry_transformation_energy"]],
                                   memo_non_energy_use_in_industry_not_elsewhere_specified = 
                                     IEATools::memo_non_energy_flows[[
                                       "memo_non_energy_use_in_industry_not_elsewhere_specified"]],
                                   non_energy_use_in_industry_not_elsewhere_specified = sub(pattern = paste0("^", memo), replacement = "", memo_non_energy_use_in_industry_not_elsewhere_specified),
                                   memo = IEATools::memo_aggregation_flow_prefixes$memo,
                                   memo_non_energy_flows_industry = IEATools::memo_non_energy_flows$memo_non_energy_use_in_industry,
                                   memo_non_energy_use_in = "Memo: Non-energy use in ",
                                   non_energy_use_in = sub(pattern = paste0("^", memo),
                                                           replacement = "", 
                                                           x = memo_non_energy_use_in), 
                                   total = IEATools::memo_aggregation_product_prefixes$total,
                                   .values = IEATools::template_cols$.values, 
                                   .values_summarised = paste0(.values, "_summarised"), 
                                   .diff = ".diff",
                                   tol = 1e-6) {
  year_columns <- .iea_df %>% 
    year_cols()
  
  # Gather the "Memo: Non-industry use in <<specific industry>>" rows
  neu_memo_flows <- .iea_df %>% 
    dplyr::filter(startsWith(.data[[flow]], memo_non_energy_use_in), 
                  !(.data[[flow]] == memo_non_energy_flows_industry), 
                  .data[[product]] != total) %>% 
    tidyr::pivot_longer(cols = dplyr::all_of(year_columns), names_to = year, values_to = .values) %>% 
    dplyr::filter(.data[[.values]] != 0) 
  
  # Create a data frame that will later be subtracted from Non-energy use industry/transformation/energy
  neu_memo_flows_summarised <- neu_memo_flows %>% 
    matsindf::group_by_everything_except(flow, .values) %>% 
    # Summarize the rows so that we we get totals.
    dplyr::summarise(
      "{.values_summarised}" := sum(.data[[.values]])
    ) |> 
    dplyr::ungroup() |> 
    dplyr::mutate(
      # Set the Flow.aggregation.point and Flow columns
      # appropriate for later subtracting the summarised energy
      # from the .iea_df.
      "{flow_aggregation_point}" := non_energy_use, 
      "{flow}" := non_energy_flows_industry_transformation_energy
    )
  
  # Figure out how much energy needs to be subtracted
  # from the larger category due to the upcoming specification.
  subtracted <- .iea_df |> 
    tidyr::pivot_longer(cols = dplyr::all_of(year_columns), names_to = year, values_to = .values) |>
    dplyr::left_join(neu_memo_flows_summarised, 
                     by = c(country, method, energy_type, unit, ledger_side, last_stage, flow_aggregation_point, flow, product, year)) |> 
    # When the above join happens, 
    # missing rows will give NA. 
    # Replace NA with 0.
    dplyr::mutate(
      "{.values_summarised}" := dplyr::case_when(
        is.na(.data[[.values_summarised]]) ~ 0, 
        TRUE ~ .data[[.values_summarised]]
      ), 
      # Now do the subtraction
      "{.values}" := .data[[.values]] - .data[[.values_summarised]], 
      # And remove the summarized column
      "{.values_summarised}" := NULL
    )
  
  # Create a data frame of specific Non-energy use in <<specific industry>>.
  # Need to trim the "Memo: " prefix
  to_add <- neu_memo_flows |> 
    dplyr::mutate(
      "{flow_aggregation_point}" := non_energy_use,
      "{flow}" := sub(pattern = paste0("^", memo), replacement = "", x = .data[[flow]])
    )
  
  # Check if we subtracted too much energy, i.e. see if 
  # Non-energy use in <<specific industries>> was greater than 
  # the original amount of 
  # Non-energy use industry/transformation/energy
  subtracted_too_much <- subtracted |> 
    dplyr::filter(.data[[flow]] == non_energy_flows_industry_transformation_energy, .data[[.values]] < 0)
  # If we subtracted too much, we want to eliminate 
  # Non-energy use in industry not elsewhere specified 
  # and add a row for Non-energy use in industry not elsewhere specified.
  if (nrow(subtracted_too_much) > 0) {
    to_add <- to_add |> 
      dplyr::bind_rows(
        # Add a row of Non-energy use in industry not elsewhere specified
        # that has the same magnitude as the subtracted too much row of
        # Non-energy use industry/transformation/energy.
        subtracted_too_much |> 
          dplyr::mutate(
            "{flow}" := non_energy_use_in_industry_not_elsewhere_specified
          )
      ) |> 
      matsindf::group_by_everything_except(.values) |> 
      # This summarise step will reduce Non-energy use in energy not elsewhere specified 
      # by the correct amount.
      dplyr::summarise(
        "{.values}" := sum(.data[[.values]])
      )
    # Check if we have a negative number.
    # assertthat::assert_that(all(to_add[[.values]] >= 0),
    #                         msg = paste("Found a negative value in the correction row",
    #                                     "when specifying Non-energy use in IEATools::specify_non_energy_use().",
    #                                     "This means that we subtracted_too_much",
    #                                     "'Non-energy use in industry not elsewhere specified'",
    #                                     "from 'Non-energy use industry/transformation/energy'.",
    #                                     "Which in turn means that it was likely that there was BOTH",
    #                                     "too much 'Memo: Non-energy use in <<specific industry>>' flows AND",
    #                                     "little or no 'Non-energy use in industry not elsewhere specified' in the original IEA data.",
    #                                     "You put this off, hoping it would not be a problem,",
    #                                     "but it looks like you need to solve it now!",
    #                                     "The 'to_add' data frame looks like this:\n",
    #                                     to_add))
    subtracted <- subtracted |> 
      # Now eliminate the subtracted too much rows.
      dplyr::anti_join(subtracted_too_much, by = names(subtracted))
  }
    
  # Now prepare the outgoing data frame.
  subtracted |> 
    # Eliminate the rows that we'll replace. These are Memo: Non-energy use in <<specific industry>>
    dplyr::anti_join(neu_memo_flows, by = names(subtracted)) |> 
    # Add the replacement rows.
    dplyr::bind_rows(to_add) |> 
    # Finally, pivot wider to return.
    tidyr::pivot_wider(values_from = dplyr::all_of(.values),
                       names_from = dplyr::all_of(year),
                       values_fill = 0)
}


#' Creates a tidy IEA data frame
#' 
#' Data from the IEA have years in columns, 
#' but the [tidy data format](\doi{10.18637/jss.v059.i10})
#' requires one row for each datum.
#' This function uses `tidyr::pivot_longer()` to 
#' make an IEA data frame tidy.
#' 
#' Default argument values assume that `rename_iea_df_cols()` has been called on `.iea_df`.
#'
#' @param .iea_df A IEA data frame whose columns have been renamed by `rename_iea_df_cols()`.
#' @param col_names A list of column names in IEA data frames. 
#'                  Default is `IEATools::iea_cols`.
#' @param year The name of the year column created in `.iea_df` by this function. 
#'             Default is `col_names$year`.
#' @param method The name of the method column created in `.iea_df` by this function. 
#'               Default is `col_names$method`.
#' @param last_stage The name of the last stage column created in `.iea_df` by this function. 
#'                   Default is `col_names$last_stage`.
#' @param e_dot The name of the energy/exergy value column created in `.iea_df` by this function. 
#'              Default is `col_names$e_dot`.
#' @param country The name of the country column in `.iea_df`. 
#'                Default is `col_names$country`.
#' @param ledger_side The name of the ledger side in `.iea_df`. 
#'                    Default is `col_names$ledger_side`.
#' @param flow_aggregation_point The name of the flow aggregation point column in `.iea_df`. 
#'                               Default is `col_names$flow_aggregation_point`.
#' @param energy_type The name of the energy type column in `.iea_df`. 
#'                    Default is `col_names$energy_type`.
#' @param unit The name of the unit column in `.iea_df`. 
#'             Default is `col_names$unit`.
#' @param flow The name of the flow column in `.iea_df`. 
#'             Default is `col_names$flow`.
#' @param product The name of the product column in `.iea_df`. 
#'                Default is `col_names$product`.
#' @param remove_zeroes A logical indicating whether data points with the value `0` are to be removed from the output. 
#'                      Default is `TRUE`.
#'
#' @return A tidy version of `.iea_df` containing new columns `year` and `e_dot` and, optionally, `0` values removed.
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
    # tidyr::gather(key = !!as.name(year), value = !!as.name(e_dot), -c(method, country, last_stage, ledger_side,
    #                                                                   flow_aggregation_point, flow, product, energy_type, unit)) %>%
    # tidyr::pivot_longer(names_to = year, values_to = e_dot, cols = -dplyr::any_of(c(method, country, energy_type, last_stage, ledger_side,
    #                                                                                 flow_aggregation_point, flow, product, unit))) %>%
    tidyr::pivot_longer(names_to = year, values_to = e_dot, cols = year_cols(.iea_df)) %>%
    # Set the column order to something rational
    # dplyr::select(country, method, energy_type, last_stage, year, ledger_side, flow_aggregation_point, flow, product, unit, e_dot) %>% 
    dplyr::select(dplyr::all_of(c(country, method, energy_type, last_stage, year, ledger_side, flow_aggregation_point, flow, product, unit, e_dot))) %>%
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
#' This function bundles several others and relies mostly on default values for arguments:
#' 1. `iea_df()`, 
#' 2. `rename_iea_df_cols()`,  
#' 3. `clean_iea_whitespace()`,
#' 4. `use_iso_countries()`,
#' 5. `augment_iea_df()`,
#' 6. `specify_non_energy_use()` (optionally), 
#' 7. `fix_GHA_industry_electricity() |> fix_GHA_psb() |> fix_COL_electricity_generation()` (optionally),
#' 8. `remove_agg_memo_flows()`, and 
#' 9. `tidy_iea_df()`.
#' 
#' Each bundled function is called in turn using default arguments.
#' See examples for two ways to achieve the same result.
#' 
#' @param .iea_file The path of the file to be loaded. Default loads example data bundled with the package via [sample_iea_data_path()].
#' @param unit_val The units for this file. 
#'                 Default is "TJ".
#' @param remove_zeroes A logical indicating whether data points with the value `0` are to be removed from the output. 
#'                      This argument is passed to `tidy_iea_df()`. 
#'                      Default is `TRUE`.
#' @param specify_non_energy_flows A logical indicating whether "Non-energy use in xxxxx" Flows
#'                                 should be specified by "Memo: Non-energy use in <<specific industry>>"
#'                                 entries in the IEA data. 
#'                                 Default is `FALSE`.
#' @param apply_fixes A logical indicating whether fixes should be applied to IEA data.
#'                    Default is `FALSE`.
#' @param override_df A data frame containing columns `pfu_code` and `iea_name` that provides 3-letter country codes. See `IEATools::use_iso_countries()`.
#'                    Default is `IEATools::override_iso_codes_df`.
#' @param country The name of the country column in the data frames. See `IEATools::iea_cols$country`.
#' @param pfu_code,iea_name Names of columns in the override data frame for 3-letter country codes. 
#'                          These arguments are passed to `use_iso_countries()`.
#'                          Defaults are taken from `IEATools::country_concordance_cols`.
#'
#' @return A tidy, augmented data frame of IEA extended energy balance data.
#' 
#' @export
#'
#' @examples
#' # Check the file first
#' iea_file_OK(sample_iea_data_path())
#' # Take a simple approach
#' simple <- load_tidy_iea_df(specify_non_energy_flows = TRUE)
#' # Take the complicated approach
#' complicated <- sample_iea_data_path() |> 
#'   iea_df() |> 
#'   rename_iea_df_cols() |> 
#'   clean_iea_whitespace() |> 
#'   remove_agg_memo_flows() |>  
#'   use_iso_countries() |> 
#'   augment_iea_df() |> 
#'   specify_non_energy_use() |> 
#'   tidy_iea_df()
#' # simple and complicated should be exactly the same
#' all(simple == complicated)
load_tidy_iea_df <- function(.iea_file = sample_iea_data_path(), 
                             unit_val = "TJ", 
                             remove_zeroes = TRUE, 
                             specify_non_energy_flows = FALSE,
                             apply_fixes = FALSE,
                             override_df = IEATools::override_iso_codes_df,
                             country = IEATools::iea_cols$country, 
                             pfu_code = IEATools::country_concordance_cols$pfu_code,
                             iea_name = IEATools::country_concordance_cols$iea_name){
  out <- .iea_file |> 
    iea_df() |> 
    rename_iea_df_cols() |> 
    clean_iea_whitespace() |> 
    use_iso_countries(override_df = override_df, country = country, pfu_code = pfu_code, iea_name = iea_name) |> 
    augment_iea_df(unit_val = unit_val)
  if (specify_non_energy_flows) {
    out <- out |> 
      specify_non_energy_use()
  }
  if (apply_fixes) {
    out <- out |> 
      # We no longer need to fix GHA's industry electricity, 
      # because details are now available in the IEA's WEEB.
      # fix_GHA_industry_electricity() |> 
      fix_GHA_psb() |> 
      fix_COL_WRLD_electricity() |> 
      fix_OAMR_cpp() |> 
      fix_OAMR_gw() |> 
      fix_AUS_bfg() |> 
      fix_RUSEST_heat()
  }
  out |>  
    remove_agg_memo_flows() |> 
    tidy_iea_df(remove_zeroes = remove_zeroes)
}




