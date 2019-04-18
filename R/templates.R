#' Write blank final-to-useful templates
#' 
#' The analyst often starts with IEA extended energy balance data covering primary and final energy stages.
#' This function writes a blank Excel template file that, when filled,
#' allow the energy conversion chain to be extended to the useful stage.
#' This function saves an Excel workbook to `path` that contains an analyst's template.
#' 
#' Allocations are to be placed in `C_x` rows of the template.
#' 
#' Formatting is applied to the template:
#' * Gray cells indicate cells that will be ignored when the template is read later.
#'   Analysts may fill those cells, but it is not necessary to do so.
#' * Blue cells indicate final demand energy consumption.
#' * Yellow cells indicate energy industry own use energy consumption.
#' * Rows are written in same order as EIA extended energy balance data.
#' * Columns are written in a reasonable order, namely that left-to-right order
#'   approximates flow through the energy conversion chain.
#'
#' @param .fu_allocation_template a data frame produced by ``fu_allocation_template()`
#' @param path the file path into which the blank template file will be written. 
#'        Include both folder and file name. 
#'        If not present, the ".xlsx" extension is added.
#' @param ledger_side the name of the ledger side column in `.tidy_iea_df`. Default is "`Ledger.side`".
#' @param consumption the string identifier for consumption in the `ledger_side` column.  Default is "`Consumption`".
#' @param flow_aggregation_point the name of the flow aggregation point column in `.tidy_iea_df`. Default is "`Flow.aggregation.point`".
#' @param eiou the string identifier for energy industry own use in the `flow_aggregation_point` column. Default is "`Energy industry own use`".
#' @param allocations_tab_name the name of the tab on which the template will be written. Default is "`Allocations`".
#' @param quantity the name of the quantity column to be created on output. Default is "`Quantity`".
#' @param e_dot the name of the energy flow rate column in `.tidy_iea_df` and the name of the energy flow rate rows to be included in the Excel file that is written by this function.
#'        Default is "`E.dot`".
#' @param e_dot_perc the name of the energy flow rate percentage row to be included in the Excel file that is written by this function.
#'        Default is "`E.dot.perc`".
#' @param maximum_values the name of the maximum values column in output. Default is "`Maximum.values`".
#' @param energy_row_font_color_fd a hex string representing the font color for `e_dot` and `e_dot_perc` final demand rows in the Excel file that is written by this function.
#'        Default is "`#104273`", a dark blue color.
#' @param energy_row_shading_color_fd a hex string representing the shading color for `e_dot` and `e_dot_perc` final demand rows in the Excel file that is written by this function.
#'        Default is "`#104273`", a light blue color.
#' @param energy_row_font_color_eiou a hex string representing the font color for `e_dot` and `e_dot_perc` energy industry own use rows in the Excel file that is written by this function.
#'        Default is "`#104273`", a dark yellow color.
#' @param energy_row_shading_color_eiou a hex string representing the shading color for `e_dot` and `e_dot_perc` energy industry own use rows in the Excel file that is written by this function.
#'        Default is "`#104273`", a light yellow color.
#' @param dont_fill_shading_color a hex string representing the shading color for cells that don't require inputs. 
#'        Default is "`#A8A8A8`", a medim gray color.
#' @param overwrite a boolean that tells whether an existing file at `path` will be overwritten. Default is "`FALSE`".
#'        If `path` already exists and `overwrite = FALSE`, an error is given.
#' @param .rownum a temporary column created internally. `.rownum` must not exist in `.tidy_iea_df` when `write_fu_allocation_template` is called.
#'        Default is "`.rownum`".
#'
#' @return the value of the `path` argument
#' 
#' @export
#' 
#' @examples
#' f <- tempfile(fileext = ".xlsx")
#' load_tidy_iea_df() %>% 
#'   specify_all() %>% 
#'   fu_allocation_template() %>% 
#'   write_fu_allocation_template(f)
#' if (file.exists(f)) {
#'   file.remove(f)
#' }
write_fu_allocation_template <- function(.fu_allocation_template, 
                                         path, 
                                         ledger_side = "Ledger.side",
                                         consumption = "Consumption",
                                         flow_aggregation_point = "Flow.aggregation.point",
                                         eiou = "Energy industry own use",
                                         allocations_tab_name = "Allocations",
                                         machine = "Machine",
                                         eu_product = "Eu.product",
                                         quantity = "Quantity", 
                                         e_dot = "E.dot",
                                         e_dot_perc = paste(e_dot, "[%]"), 
                                         maximum_values = "Maximum.values",
                                         energy_row_font_color_fd = "#104273",
                                         energy_row_shading_color_fd = "#B8D8F5", 
                                         energy_row_font_color_eiou = "#918700",
                                         energy_row_shading_color_eiou = "#FCFCAB", 
                                         dont_fill_shading_color = "#A8A8A8",
                                         overwrite = FALSE,
                                         n_allocation_rows = 3,
                                         .rownum = ".rownum", 
                                         .year = ".year",
                                         .value = ".value"){
  # Create the template data frame
  matsindf::verify_cols_missing(.fu_allocation_template, .rownum)
  
  # Create the workbook, add the worksheet, and stuff the temmplate into the worksheet
  fu_wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(fu_wb, allocations_tab_name)
  openxlsx::writeData(fu_wb, allocations_tab_name, .fu_allocation_template)
  
  # A function to identify some rows of the spreadsheet (e_rows means "energy rows")
  e_rows <- function(which_quantity = c(e_dot, e_dot_perc), which_type = c("fd", "eiou")){
    which_quantity <- match.arg(which_quantity)
    which_type <- match.arg(which_type)
    # Get rid of rownames so that we have only row numbers and put those row numbers into a column
    keep_rows <- .fu_allocation_template %>% 
      tibble::remove_rownames() %>% 
      tibble::rownames_to_column(var = .rownum)
    if (which_type == "fd") {
      keep_rows <- keep_rows %>% 
        dplyr::filter(!!as.name(ledger_side) == consumption)
    } else {
      # which_type is "eiou"
      keep_rows <- keep_rows %>% 
        dplyr::filter(!!as.name(flow_aggregation_point) == eiou)
    }
    keep_rows %>% 
      # Keep only those rows with e_dot in the Quantity column
      dplyr::filter(!!as.name(quantity) == which_quantity) %>% 
      magrittr::extract2(.rownum) %>% 
      # + 1 returns the rows as they appear in the Excel spreadsheet,
      # remembering that row 1 in Excel is the header of the table
      as.numeric() + 1
  }
  
  # First, figure out which some zones of the worksheet
  e_dot_rows_fd <- e_rows(which_quantity = e_dot, which_type = "fd")
  e_dot_perc_rows_fd <- e_rows(which_quantity = e_dot_perc, which_type = "fd")
  e_dot_rows_eiou <- e_rows(which_quantity = e_dot, which_type = "eiou")
  e_dot_perc_rows_eiou <- e_rows(which_quantity = e_dot_perc, which_type = "eiou")
  max_values_col_index <- which(names(.fu_allocation_template) == maximum_values)
  year_cols_indices <- year_cols(.fu_allocation_template)
  year_cols_names <- year_cols(.fu_allocation_template, return_names = TRUE)
  # Note the "1" is for row 1, which we don't want to color gray.
  c_rows_indices <- base::setdiff(1:(nrow(.fu_allocation_template) + 1), c(1, e_dot_rows_fd, e_dot_perc_rows_fd, e_dot_rows_eiou, e_dot_perc_rows_eiou))
  
  # Apply color formatting style for energy and energy percentage rows
  energy_row_style_fd <- openxlsx::createStyle(fontColour = energy_row_font_color_fd, fgFill = energy_row_shading_color_fd)
  energy_row_style_eiou <- openxlsx::createStyle(fontColour = energy_row_font_color_eiou, fgFill = energy_row_shading_color_eiou)
  openxlsx::addStyle(fu_wb, allocations_tab_name, style = energy_row_style_fd, rows = union(e_dot_rows_fd, e_dot_perc_rows_fd), cols = 1:ncol(.fu_allocation_template), gridExpand = TRUE)
  openxlsx::addStyle(fu_wb, allocations_tab_name, style = energy_row_style_eiou, rows = union(e_dot_rows_eiou, e_dot_perc_rows_eiou), cols = 1:ncol(.fu_allocation_template), gridExpand = TRUE)
  
  # Apply shading for cells that don't need to be filled
  # First, tackle the cells in the Maximum.values column.
  dont_fill_style <- openxlsx::createStyle(fgFill = dont_fill_shading_color)
  openxlsx::addStyle(fu_wb, allocations_tab_name, style = dont_fill_style, rows = c_rows_indices, cols = max_values_col_index, gridExpand = TRUE)
  # Now work on the year columns. 
  # Find all the E.dot rows
  for (yr_index in 1:length(year_cols_indices)) {
    col_index <- year_cols_indices[[yr_index]]
    col_name <- year_cols_names[[yr_index]]
    # Find the rows with NA in the e_dot column
    e_dot_NA_rownums_in_Excel <- .fu_allocation_template %>% 
      # Make a column of row numbers
      tibble::remove_rownames() %>% tibble::rownames_to_column(var = .rownum) %>% dplyr::mutate(!!as.name(.rownum) := as.numeric(!!as.name(.rownum))) %>% 
      # Filter to get only e_dot rows
      dplyr::filter(!!as.name(quantity) == e_dot) %>% 
      # Find only those rows that are NA in the year of interest. 
      # These rows represent Flows that are zero in this year. 
      dplyr::filter(is.na(!!as.name(col_name))) %>% 
      # Grab the .rownum column.
      # These are the row indices that are zero in this year.
      magrittr::extract2(.rownum) %>% 
      # At this point, we have the row numbers in the data.frame, but
      # we need the row numbers in the Excel spreadsheet.
      # So we need to add 1 to get the row numbers in the Excel spreadsheet.
      # (The first row in the Excel spreadsheet is the header row, 
      # which is not counted in the data frame.)
      magrittr::add(1)
    # We don't want to gray the e_dot cells. 
    # We want to gray the C (allocation fraction) cells that are beneath the e_dot cells.
    # Beneath the e_dot cells, we have the e_dot_perc cells.
    # Then, we have n_allocation_rows of C rows.
    # So, we need to add to the indices we just found.
    gray_rows_for_year_col_index <- c()
    for (i in 1:n_allocation_rows) {
      # The 1 is to jump across the e_dot_perc row.
      # The i is for the allocation rows.
      gray_rows_for_year_col_index <- c(gray_rows_for_year_col_index, e_dot_NA_rownums_in_Excel + 1 + i)
    }
    # Now make these cells gray.
    openxlsx::addStyle(fu_wb, allocations_tab_name, style = dont_fill_style, 
                       rows = gray_rows_for_year_col_index, cols = col_index, stack = TRUE)
  }
  
  # Set percentage format for numbers in the e_dot_perc rows.
  e_dot_perc_style <- openxlsx::createStyle(numFmt = "PERCENTAGE")
  openxlsx::addStyle(fu_wb, allocations_tab_name, style = e_dot_perc_style, 
                     rows = c(e_dot_perc_rows_fd, e_dot_perc_rows_eiou), cols = c(max_values_col_index, year_cols_indices), gridExpand = TRUE, stack = TRUE)
  
  # Set column widths to something intelligent
  openxlsx::setColWidths(fu_wb, allocations_tab_name, cols = 1:ncol(.fu_allocation_template), widths = "auto")
  
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
#' Non-energy use is removed from `.tidy_iea_df` before creating the template.
#'
#' @param .tidy_iea_df a tidy data frame containing IEA extended energy balance data
#' @param energy_type the name of the energy type column. Default is "`Energy.type`".
#' @param energy the string identifier for energy (as opposed to exergy). Default is "`E`".
#' @param last_stage the name of the last stage column. Default is "`Last.stage`".
#' @param final the string identifier for final energy (as `Last.stage`). Default is "`Final`".
#' @param year the name of the year column. Default is "`Year`".
#' @param ledger_side the name of the ledger side column. Default is "`Ldeger.side`".
#' @param consumption the string identifier for the consumption side of the ledger. Default is "`Consumption`".
#' @param flow_aggregation_point the name of the flow aggregation point column. Default is "`Flow.aggregation.point`".
#' @param eiou the string identifier for energy industry own use in `flow_aggregation_point`. Default is "`Energy industry own use`".
#' @param non_energy_use string identifier for non-energy use in `flow_aggregation_point`. Default is "`Non-energy use`".
#' @param tfc the string identifier for total final consumption. Default is "`Total final consumption`".
#' @param tpes the string identifier for total primary energy supply. Default is "`Total primary energy supply`".
#' @param flow the name of the flow column. Default is "`Flow`".
#' @param product the name of the product column. Default is "`Product`".
#' @param destination the name for the destination column. Default is "`Destination`".
#' @param quantity the name of the quantity column. Default is "`Quantity`".
#' @param e_dot the name of the energy flow rate column. Default is "`E.dot`".
#' @param e_dot_total the string identifier for total energy. Default is "`E.dot.total`".
#' @param perc_unit_string the string used to indicate percentages. Default is "`[%]`".
#' @param e_dot_perc the string identifier for energy percentage. Default is "`E.dot.perc`".
#' @param maximum_values the name for the maximum energy values column. Default is "`Maximum values`".
#' @param year_for_maximum_values an integer for the first year (in which maximum values will be stored before renaming the column to `maximum_values`). 
#'        Default is `0`.
#' @param ef_product the name of the final energy carrier column. Default is "`Ef product`".
#' @param allocation_var the string identifier for the allocation percentage column. Default is "`C_`".
#' @param n_allocation_rows an integer stating how many allocation rows are desired. Default is `3`.
#' @param machine the name of the column of final-to-useful transformation process machines. Default is "`Machine`".
#' @param eu_product the name of the useful energy carrier column. Default is "`Eu product`". 
#' @param arrange a boolean telling whether to arranged the rows and columns 
#'        (using `arrange_iea_fu_allocation_template()`) before returning. 
#'        Default is `TRUE`.
#' @param .value the name of a temproary value column added to `.tidy_iea_df`. 
#'        A `.value` column must not be present in `.tidy_iea_df`. Default is "`.value`".
#'
#' @return a data frame containing the EIOU template
#' 
#' @export
#'
#' @examples
#' load_tidy_iea_df() %>% 
#'   specify_all() %>% 
#'   fu_allocation_template()
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
                        non_energy_use = "Non-energy use",
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
                        arrange = TRUE,
                        .value = ".value"){
  matsindf::verify_cols_missing(.tidy_iea_df, .value)
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
    dplyr::filter(!!as.name(ledger_side) == consumption | !!as.name(flow_aggregation_point) == eiou) %>% 
    dplyr::filter(!!as.name(flow_aggregation_point) != non_energy_use) %>% 
    dplyr::mutate(
      # Ensure that all energy values are positive to calculate totals and percentages accurately.
      !!as.name(e_dot) := abs(!!as.name(e_dot))
    )
  Totals <- Filtered %>%
    matsindf::group_by_everything_except(ledger_side, flow_aggregation_point, flow, product, e_dot) %>%
    dplyr::summarise(!!as.name(e_dot_total) := sum(!!as.name(e_dot)))
  # Calculate a Tidy data frame with percentages.
  Tidy <- Filtered %>% 
    # Add the totals to the data frame in preparation for calculating percentages
    dplyr::left_join(Totals, by = matsindf::everything_except(.tidy_iea_df, ledger_side, flow_aggregation_point, flow, product, e_dot, .symbols = FALSE)) %>% 
    dplyr::mutate(
      # Calculate percentage of all energy flows for that country and year
      # Don't need to multiply by 100 here, because we'll 
      # change to percentage formatting when we write to Excel.
      !!as.name(e_dot_perc) := !!as.name(e_dot) / !!as.name(e_dot_total), 
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
      !!as.name(year) := year_for_maximum_values #,
      # Need to make this into a character column so that we can add it to "" in the C_ columns later.
      # !!as.name(.value) := as.character(!!as.name(.value))
    )
  
  # Create a vector of allocation percentages
  c_cols <- paste0(allocation_var, 1:n_allocation_rows, " ", perc_unit_string)
  # Add allocation columns to the data frame
  for (i in 1:n_allocation_rows) {
    Tidy <- Tidy %>% 
      dplyr::mutate(
        !!as.name(c_cols[[i]]) := NA_real_
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
      # Get rid of the factor that we created earlier.
      !!as.name(quantity) := as.character(!!as.name(quantity)),
      !!as.name(machine) := NA_real_,
      !!as.name(eu_product) := NA_real_
    )
  if (arrange) {
    out <- out %>% 
      arrange_iea_fu_allocation_template()
  }
  return(out)
}


#' Arrange the fows and/or columns of an fu allocation template
#' 
#' It is helpful to sort rows and columns of a final-to-useful allocation template
#' in reasonable ways to assist analysts.
#' This function sorts rows (top-to-bottom) by default in the same order as appears in the IEA extended energy balance data.
#' Other orderings can be specified with the `fap_flow_order` and `ef_product_order` arguments.
#' It sorts columns (left-to-right) in the order of energy flow through the energy conversion chain.
#' 
#' Whe sorting rows, 
#' the `flow_allocation_point`, `flow`, and `product` columns are considered. 
#' Internally, this function figures out which columns are metadata columns
#' and groups on those columns before applying the row ordering.
#' If you want to preserve ordering by metadata, be sure to set factor levels on metadata columns prior to calling this function.
#' 
#' When sorting columns, the order of energy flows through the energy conversion chain is considered. 
#' The column order is:
#' * metadata columns,
#' * final energy product (`Ef.product`). 
#' * Machine (the final-to-useful transformation process),
#' * useful energy product (`Eu.product`),
#' * destination where the useful energy now flows,
#' * years (in columns), and 
#' * allocations (C_x rows).
#'
#' @param .fu_allocation_template the final-to-useful allocation template created by `fu_allocation_template()`
#' @param rowcol one of "`both`", "`row`", or "`col`" to indicate whether rows, columns, or both should be arranged.
#'        Default is "`both`". 
#' @param ledger_side the ledger side column in `.fu_allocation_template`. Default is "`Ledger.side`".
#' @param flow_aggregation_point the flow aggregation point column in `.fu_allocation_template`. Default is "`Flow.aggregation.point`".
#' @param ef_product the name of the final energy column in `.fu_allocation_template`. Default is "`Ef.product`".
#' @param machine the name of the machine columnin `.fu_allocation_template`. Default is "`Machine`".
#' @param eu_product the name of the useful energy product column in `.fu_allocation_template`. Default is "`Eu.product`".
#' @param destination the name of the destination column in `.fu_allocation_template`. Default is "`Destination`".
#' @param unit the name of the unit in `.fu_allocation_template`. Default is "`Unit`".
#' @param fap_dest_order the desired order for the combination of `flow_aggregation_point` and `destination` columns. Default is `IEATools::fap_flow_iea_order`.
#' @param ef_product_order the desired order for final energy products in `.fu_allocation_template`. Default is "`Ef.product`".
#' @param quantity the name of the quantity column in `.fu_allocation_template`. Default is "`Quantity`".
#' @param maximum_values the name of the maximum value column `.fu_allocation_template`. Default is "`Unit`".
#' @param .temp_sort the name of a temporary column to be added to `.fu_allocation_template`. 
#'        Default is "`.fap_flow`".
#'        This column must not be present in `.fu_allocation_template`.
#'
#' @return An row- and/or column-ordered version of `.fu_allocation_template`
#' 
#' @export
#'
#' @examples
#' Template <- load_tidy_iea_df() %>% 
#'   specify_all() %>%
#'   fu_allocation_template()
#' Template
#' Template %>% 
#'   arrange_iea_fu_allocation_template()
arrange_iea_fu_allocation_template <- function(.fu_allocation_template, 
                                               rowcol = c("both", "row", "col"),
                                               ledger_side = "Ledger.side", 
                                               flow_aggregation_point = "Flow.aggregation.point",
                                               ef_product = "Ef.product",
                                               machine = "Machine",
                                               eu_product = "Eu.product",
                                               destination = "Destination",
                                               unit = "Unit",
                                               fap_dest_order = IEATools::fap_flow_iea_order,
                                               ef_product_order = IEATools::product_iea_order, 
                                               quantity = "Quantity",
                                               maximum_values = "Maximum.values", 
                                               .temp_sort = ".fap_flow"){
  rowcol <- match.arg(rowcol)
  out <- .fu_allocation_template
  # Work on row order
  if (rowcol == "both" | rowcol == "row") {
    matsindf::verify_cols_missing(out, .temp_sort)
    # Figure out which columns are metadata columns.
    colnames <- names(out)
    year_colnames <- year_cols(out, return_names = TRUE)
    machine_and_product_columns <- c(ledger_side, flow_aggregation_point, unit, ef_product, machine, eu_product, 
                                     destination, quantity, maximum_values)
    # Columns that are not years and are not machine_and_product_columns are metadata columns.
    # We group by these columns later.
    # meta_cols <- setdiff(colnames, year_colnames) %>% 
    #   setdiff(machine_and_product_columns)
    meta_cols <- out %>% 
      matsindf::everything_except(c(year_colnames, machine_and_product_columns))  
    out <- out %>% 
      tidyr::unite(col = !!as.name(.temp_sort), !!as.name(flow_aggregation_point), !!as.name(destination), sep = "_", remove = FALSE)
    # Ensure that no .fap_flow and no ef_products are NA at this point.
    assertthat::assert_that(!any(is.na(out[[.temp_sort]])))
    assertthat::assert_that(!any(is.na(out[[ef_product]])))
    # Convert .temp_sort and ef_product to factors so that they can be arranged (sorted) later.
    out <- out %>% 
      dplyr::mutate(
        !!as.name(.temp_sort) := factor(!!as.name(.temp_sort), levels = fap_dest_order),
        !!as.name(ef_product) := factor(!!as.name(ef_product), levels = ef_product_order)
      ) 
    # Ensure that we have not accidentally created NA values in the .temp_sort or ef_product columns.
    # NA values in either of these columns will occur when we do not have a complete set of factors 
    # in fap_flow_iea_order or ef_product_order.
    na_temp_sort <- out %>% 
      dplyr::filter(is.na(!!as.name(.temp_sort)))
    na_ef_product <- out %>% 
      dplyr::filter(is.na(!!as.name(ef_product)))
    assertthat::assert_that(nrow(na_temp_sort) == 0)
    assertthat::assert_that(nrow(na_ef_product) == 0)
    out <- out %>% 
      dplyr::group_by(!!!meta_cols) %>% 
      dplyr::arrange(!!as.name(.temp_sort), !!as.name(ef_product), .by_group = TRUE) %>% 
      dplyr::mutate(
        !!as.name(.temp_sort) := NULL, 
        # Undo the factorization of the Ef.product column.
        !!as.name(ef_product) := as.character(!!as.name(ef_product))
      ) %>% 
      # Undo the grouping that we performed above.
      dplyr::ungroup()
  }
  # Work on column order
  if (rowcol == "both" | rowcol == "col") {
    # Figure out the order for the columns
    colnames <- names(out)
    year_colnames <- year_cols(out, return_names = TRUE)
    machine_and_product_columns <- c(ef_product, machine, eu_product, destination, quantity, maximum_values)
    # Figure out the metadata columns.
    # Columns that are not years and are not machine_and_product_columns are metadata columns.
    meta_cols <- out %>% 
      matsindf::everything_except(c(year_colnames, machine_and_product_columns))
    # Now put the column names together in the desired order
    col_order <- c(meta_cols, machine_and_product_columns, year_colnames)
    out <- out %>% 
      dplyr::select(!!!col_order)
  }
  return(out)
}


#' Final-to-useful efficiency template
#' 
#' Using a final-to-useful allocation table, 
#' this function generates a blank template for final-to-useful machine efficiencies.
#'
#' @param .fu_allocation_template a data frame containing a completed final-to-useful allocation template for final demand.
#' @param row_order the desired row order. Default is `IEATools::fap_flow_iea_order`.
#' @param product_order the desired product order. Default is `IEATools::product_iea_order`.
#' @param .temp_sort a temporary column created in `.fu_allocation_template`. 
#'        No column with this name can be present in `.fu_allocation_template`.
#'
#' @return a data frame containing row-ordered blank template for final-to-useful machine efficiencies.
#' 
#' @export
#'
#' @examples
eta_template <- function(.fu_allocation_template){
  
}
