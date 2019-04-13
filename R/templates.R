#' Write blank final-to-useful templates
#' 
#' The analyst often starts with IEA extended energy balance data covering primary and final energy stages.
#' This function writes a blank Excel template file that, when filled,
#' allow the energy conversion chain to be extended to the useful stage.
#' 
#' This function returns a data frame that is the template that needs to be filled. 
#' Use the function `write.csv()` or similar to save to disk.
#' The analyst must supply allocation and efficiency data.
#' Allocations are to be placed in `c` rows and efficiencies are to be placed in `eta` rows.
#'
#' @param .tidy_iea_df a data frame as discussed in Details
#' @param path the file path into which the blank template file will be written. 
#'        Include both folder and file name. 
#'        If not present, the ".xlsx" extension is added.
#' @param allocations_tab_name the name of the tab on which the template will be written. Default is "`Allocations`".
#' @param quantity the name of the quantity column to be created on output. Default is "`Quantity`".
#' @param e_dot the name of the energy flow rate column in `.tidy_iea_df` and the name of the energy flow rate rows to be included in the Excel file that is written by this function.
#'        Default is "`E.dot`".
#' @param e_dot_perc the name of the energy flow rate percentage row to be included in the Excel file that is written by this function.
#'        Default is "`E.dot.perc`".
#' @param energy_row_font_color a hex string representing the font color to be used for `e_dot` and `e_dot_perc` rows in the Excel file that is written by this function.
#'        Default is "`#104273`", a light blue color.
#' @param energy_row_shading_color a hex string representing the font color to be used for `e_dot` and `e_dot_perc` rows in the Excel file that is written by this function.
#'        Default is "`#104273`".
#' @param overwrite a boolean that tells whether an existing file at `path` will be overwritten. Default is "`FALSE`".
#'        If `path` already exists and `overwrite = FALSE`, an error is given.
#' @param .rownum a temporary column created internally. `.rownum` must not exist in `.tidy_iea_df` when `write_fu_allocation_template` is called.
#'        Default is "`.rownum`".
#'
#' @return the value of the `path` argument, regardless of the value of `overwrite`.
#' 
#' @export
#' 
#' @examples
#' load_tidy_iea_df() %>% 
#'   specify_all() %>% 
#'   write_fu_templates()
write_fu_allocation_template <- function(.tidy_iea_df, 
                                         path, 
                                         allocations_tab_name = "Allocations",
                                         quantity = "Quantity", 
                                         e_dot = "E.dot",
                                         e_dot_perc = paste0(e_dot, ".perc"), 
                                         energy_row_font_color = "#104273",
                                         energy_row_shading_color = "#B8D8F5", 
                                         overwrite = FALSE,
                                         .rownum = ".rownum"){
  # Create the template data frame
  Allocation_template <- .tidy_iea_df %>% 
    fu_allocation_template()
  matsindf::verify_cols_missing(Allocation_template, .rownum)
  
  # Create the Excel file and add formatting
  
  # First, figure out which rows are energy rows that need their background color changed
  # e_rows_df gives the row numbers in the data frame, 
  # where 0 is the header row 
  # and 1 is the first data row
  e_rows_df <- Allocation_template %>% 
    # Get rid of rownames so that we have only row numbers
    tibble::remove_rownames() %>% 
    # Move the row numbers into a column
    tibble::rownames_to_column(var = .rownum) %>% 
    # Keep only those rows that have e_dot or e_dot_perc in the Quantity column
    dplyr::filter(!!as.name(quantity) == e_dot | !!as.name(quantity) == e_dot_perc) %>% 
    magrittr::extract2(.rownum) %>% 
    as.numeric()
  # e_rows_excel is the row number in the Excel spreadsheet.
  # In this case, 
  # 1 is the header row and
  # 2 is the first data row.
  # So 1 must be added to e_rows_df
  e_rows_excel <- e_rows_df + 1
  
  # Create the workbook, add the worksheet, and stuff the data into the worksheet
  fu_wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(fu_wb, allocations_tab_name)
  openxlsx::writeData(fu_wb, allocations_tab_name, Allocation_template)
  # Apply formatting style for energy rows
  energy_row_style <- openxlsx::createStyle(fontColour = energy_row_font_color, fgFill = energy_row_shading_color)
  openxlsx::addStyle(fu_wb, allocations_tab_name, style = energy_row_style, rows = e_rows_excel, cols = 1:ncol(Allocation_template), gridExpand = TRUE)
  # Now save it!
  if (!endsWith(path, ".xlsx")) {
    path <- paste0(path, ".xlsx")
  }
  openxlsx::saveWorkbook(fu_wb, path, overwrite = overwrite)
  # And return the path
  return(path)
}


#' Create a template for analysis of final-to-useful transformation processes
#' 
#' This function creates a blank template for final-to-useful energy transformation process analysis.
#' The template is a data frame derived from IEA extended energy balance data (`.tidy_iea_df`),
#' which give both energy industry own use and final demand for final energy carriers.
#' From the IEA's final energy information, templates are created for conversion to 
#' useful energy carriers.
#' Final energy consumed by each final demand sector or transformation process machines
#' is allocated to final-to-useful machines and converted to useful energy at some efficiency.
#' The allocation fractions and efficiencies are to be supplied by the analyst by filling blanks in 
#' the template. 
#'
#' @param .tidy_iea_df a tidy data frame containing IEA extended energy balance data
#' @param template_type one of "`Final demand`" or "`Energy industry own use`" for final consumption or energy industry own use, respectively. 
#'        Default is "`Final demand`".
#' @param energy_type the name of the energy type column. Default is "`Energy.type`".
#' @param energy the string identifier for energy (as opposed to exergy). Default is "`E`".
#' @param last_stage the name of the last stage column. Default is "`Last.stage`".
#' @param final the string identifier for final energy (as `Last.stage`). Default is "`Final`".
#' @param year the name of the year column. Default is "`Year`".
#' @param ledger_side the name of the ledger side column. Default is "`Ldeger.side`".
#' @param consumption the string identifier for the consumption side of the ledger. Default is "`Consumption`".
#' @param flow_aggregation_point the name of the flow aggregation point column. Default is "`Flow.aggregation.point`".
#' @param eiou the string identifier for energy industry own use. Default is "`Energy industry own use`".
#' @param tfc the string identifier for total final consumption. Default is "`Total final consumption`".
#' @param tpes the string identifier for total primary energy supply. Default is "`Total primary energy supply`".
#' @param flow the name of the flow column. Default is "`Flow`".
#' @param product the name of the product column. Default is "`Product`".
#' @param destination the name for the destination column. Default is "`Destination`".
#' @param quantity the name of the quantity column. Default is "`Quantity`".
#' @param e_dot the name of the energy flow rate column. Default is "`E.dot`".
#' @param e_dot_total the string identifier for total energy. Default is "`E.dot.total`".
#' @param e_dot_perc the string identifier for energy percentage. Default is "`E.dot.perc`".
#' @param maximum_values the name for the maximum energy values column. Default is "`Maximum values`".
#' @param year_for_maximum_values an integer for the first year (in which maximum values will be stored before renaming the column to `maximum_values`). 
#'        Default is `0`.
#' @param ef_product the name of the final energy carrier column. Default is "`Ef product`".
#' @param allocation_var the string identifier for the allocation percentage column. Default is "`C_`".
#' @param n_allocation_rows an integer stating how many allocation rows are desired. Default is `3`.
#' @param machine the name of the column of final-to-useful transformation process machines. Default is "`Machine`".
#' @param eu_product the name of the useful energy carrier column. Default is "`Eu product`". 
#' @param .value the name of the value column added to `.tidy_iea_df`. Default is "`.value`".
#'
#' @return a data frame containing the EIOU template
#' 
#' @export
#'
#' @examples
#' # By default, gives a template for Final consumption
#' load_tidy_iea_df() %>% 
#'   specify_all() %>% 
#'   fu_allocation_template()
#' # You can specify if you want a template for Energy industry own use
#' load_tidy_iea_df() %>% 
#'   specify_all() %>% 
#'   fu_allocation_template(template_type = "Energy industry own use")
fu_allocation_template <- function(.tidy_iea_df,
                        energy_type = "Energy.type",
                        energy = "E",
                        last_stage = "Last.stage",
                        final = "Final",
                        year = "Year",
                        ledger_side = "Ledger.side",
                        consumption = "Consumption",
                        flow_aggregation_point = "Flow.aggregation.point", 
                        eiou = "Energy industry own use", 
                        tfc = "Total final consumption",
                        tpes = "Total primary energy supply",
                        flow = "Flow", 
                        product = "Product",
                        destination = "Destination",
                        quantity = "Quantity",
                        e_dot = "E.dot",
                        e_dot_total = paste0(e_dot, ".total"),
                        perc_unit_string = "[%]",
                        e_dot_perc = paste(e_dot, perc_unit_string),
                        maximum_values = "Maximum.values",
                        year_for_maximum_values = 0,
                        ef_product = "Ef.product",
                        allocation_var = "C_",
                        n_allocation_rows = 3,
                        machine = "Machine",
                        eu_product = "Eu.product",
                        .value = ".value"){
  # template_type <- match.arg(template_type)
  # Ensure that the incoming data frame has exclusively "E" as the Energy.type.
  assertthat::assert_that(.tidy_iea_df %>% 
                            magrittr::extract2(energy_type) %>% 
                            magrittr::equals(energy) %>% 
                            all())
  # Ensure that the incoming data frame has exclusively "Final" as the Last.stage.
  assertthat::assert_that(.tidy_iea_df %>% 
                            magrittr::extract2(last_stage) %>% 
                            magrittr::equals(final) %>% 
                            all())
  Filtered <- .tidy_iea_df %>% 
    dplyr::filter(!!as.name(ledger_side) == consumption | !!as.name(flow_aggregation_point) == eiou)
  Totals <- Filtered %>%
    matsindf::group_by_everything_except(ledger_side, flow_aggregation_point, flow, product, e_dot) %>%
    dplyr::summarise(!!as.name(e_dot_total) := sum(!!as.name(e_dot)))
  # Calculate a Tidy data frame with percentages.
  Tidy <- Filtered %>% 
    # Add the totals to the data frame in preparation for calculating percentages
    dplyr::left_join(Totals, by = matsindf::everything_except(.tidy_iea_df, ledger_side, flow_aggregation_point, flow, product, e_dot, .symbols = FALSE)) %>% 
    dplyr::mutate(
      # Calculate percentage of all energy flows for that country and year
      !!as.name(e_dot_perc) := !!as.name(e_dot) / !!as.name(e_dot_total) * 100, 
      !!as.name(e_dot) := abs(!!as.name(e_dot)), 
      # Eliminate the total column: we don't need it any more
      !!as.name(e_dot_total) := NULL 
    ) %>% 
    # Rename a couple columns
    dplyr::rename(
      !!as.name(destination) := !!as.name(flow)
    )
  # Calculate the maximum energy consumption across all years
  Max <- Tidy %>% 
    matsindf::group_by_everything_except(year, e_dot, e_dot_perc) %>%
    dplyr::summarise(
      !!as.name(e_dot) := max(!!as.name(e_dot)), 
      !!as.name(e_dot_perc) := max(!!as.name(e_dot_perc))
    ) %>% 
    tidyr::gather(key = !!as.name(quantity), value = !!as.name(.value), !!as.name(e_dot), !!as.name(e_dot_perc)) %>% 
    dplyr::mutate(
      # Set the year for max values to 0 so that the max values will appear as the earliest year.
      !!as.name(year) := year_for_maximum_values,
      # Need to make this into a character column so that we can add it to "" in the C_ columns later.
      !!as.name(.value) := as.character(!!as.name(.value))
    )
  
  # Create a vector of allocation percentages
  c_cols <- paste0(allocation_var, 1:n_allocation_rows, " ", perc_unit_string)
  # Add allocation columns to the data frame
  for (i in 1:n_allocation_rows) {
    Tidy <- Tidy %>% 
      dplyr::mutate(
        !!as.name(c_cols[[i]]) := ""
      )
  }
  # Reshape the data frame into the format that we want for an Excel spreadsheet
  out <- Tidy %>% 
    # Gather all of the columns that we want to spread across the sheet
    tidyr::gather(key = !!as.name(quantity), value = !!as.name(.value), !!as.name(e_dot), !!as.name(e_dot_perc), !!!lapply(c_cols, as.name)) %>% 
    # Add the Max data frame so that we can include its numbers
    dplyr::bind_rows(Max) %>% 
    # Set levels for the quantity column so that we can get the right order when we spread the years
    dplyr::mutate(
      !!as.name(quantity) := factor(!!as.name(quantity), levels = c(e_dot, e_dot_perc, c_cols))
    ) %>% 
    # Now spread by years across the spreadsheet.
    tidyr::spread(key = year, value = .value) %>% 
    dplyr::rename(
      # Rename the year 0 column
      !!as.name(maximum_values) := !!as.name(year_for_maximum_values), 
      # Rename the product column: it is really a final energy product
      !!as.name(ef_product) := !!as.name(product)
    ) %>% 
    dplyr::mutate(
      # Get rid of NA values in the maximum_values, machine, and eu_product columns.
      !!as.name(maximum_values) := dplyr::case_when(
        is.na(!!as.name(maximum_values)) ~ "",
        TRUE ~ !!as.name(maximum_values)
      ), 
      !!as.name(machine) := "",
      !!as.name(eu_product) := ""
    )
  # Figure out the order for the columns
  colnames <- names(out)
  # Figure out which columns are years, sort them, and save for later.
  year_colname_indices <- which(grepl(pattern = "^\\d+$", colnames))
  year_colnames <- as.numeric(colnames[year_colname_indices]) %>% sort() %>% as.character()
  machine_and_product_columns <- c(ef_product, machine, eu_product, destination, quantity, maximum_values)
  # Figure out the metadata columns
  meta_cols <- setdiff(colnames, year_colnames) %>% 
    setdiff(machine_and_product_columns)
  # Now put the column names together in the desired order
  col_order <- c(meta_cols, machine_and_product_columns, year_colnames)
  out %>% 
    dplyr::select(col_order)
}


#' Final-to-useful efficiency template
#' 
#' Using a final-to-useful allocation table, 
#' this function generates a blank template for final-to-useful machine efficiencies.
#'
#' @param .fd_fu_allocations a data frame containing a completed final-to-useful allocation template for final demand.
#' @param .eiou_fu_allocations a data frame containing a completed final-to-useful allocation template for energy industry own use.
#'
#' @return a data frame containing a blank template for final-to-useful machine efficiencies.
#' 
#' @export
#'
#' @examples
eta_template <- function(.fd_fu_allocations, .eiou_fu_allocations){
  
}
