#' Create a template for analysis of final-to-useful transformation processes
#' 
#' This function creates a blank template for final-to-useful energy transformation process analysis.
#' The template is a data frame derived from IEA extended energy balance data (`.tidy_iea_df`),
#' which gives both energy industry own use and final demand for final energy carriers.
#' From the IEA's final energy information, templates are created for conversion to 
#' useful energy carriers.
#' Final energy consumed by each final demand sector or transformation process machine
#' is allocated to final-to-useful machines and converted to useful energy at some efficiency.
#' The allocation fractions are to be supplied by the analyst by filling blanks in 
#' the template. 
#' 
#' Non-energy use is removed from `.tidy_iea_df` before creating the template.
#'
#' @param .tidy_iea_df a tidy data frame containing IEA extended energy balance data
#' @param energy_type the name of the energy type column. Default is "`Energy.type`".
#' @param energy the string identifier for energy (as opposed to exergy) in the `energy_type` column. Default is "`E`".
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
#' @param .value the name of a temporary value column added to `.tidy_iea_df`. 
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
                                   n_allocation_rows = 4,
                                   machine = "Machine",
                                   eu_product = "Eu.product",
                                   arrange = TRUE,
                                   .value = ".value"){
  matsindf::verify_cols_missing(.tidy_iea_df, .value)
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
    # Removing this line, because we are now tracking non-energy use with very low efficiency
    # so that we can swim upstream, if needed.
    # dplyr::filter(!!as.name(flow_aggregation_point) != non_energy_use) %>% 
    dplyr::mutate(
      # Ensure that all energy values are positive to calculate totals and percentages accurately.
      !!as.name(e_dot) := abs(!!as.name(e_dot))
    )
  Totals <- Filtered %>%
    # Group on ledger_side.  Doing so allows the totals to be calculated per ledger_side.
    # The effect of grouping on ledger_side is the EIOU (Supply) and final consumption (Consumption)
    # totals are calculated separately. 
    # That allows the percentages to be calculated independently per ledger_side.
    # Thus, percentages add to 100% for EIOU and for final consumption!
    matsindf::group_by_everything_except(flow_aggregation_point, flow, product, e_dot) %>%
    dplyr::summarise(!!as.name(e_dot_total) := sum(!!as.name(e_dot)))
  # Calculate a Tidy data frame with percentages.
  Tidy <- Filtered %>% 
    # Add the totals to the data frame in preparation for calculating percentages
    dplyr::left_join(Totals, by = matsindf::everything_except(.tidy_iea_df, flow_aggregation_point, flow, product, e_dot, .symbols = FALSE)) %>% 
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
      !!as.name(year) := year_for_maximum_values
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


#' Arrange the rows and/or columns of a final-to-useful allocation template
#' 
#' It is helpful to sort rows and columns of a final-to-useful allocation template
#' in reasonable ways to assist analysts.
#' This function sorts rows (top-to-bottom) by default in the same order as appears in the IEA extended energy balance data.
#' Other orderings can be specified with the `fap_flow_order` and `ef_product_order` arguments.
#' It sorts columns (left-to-right) in the order of energy flow through the energy conversion chain.
#' 
#' When sorting rows, 
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
#' @param rowcol one of "both", "row", or "col" to indicate whether rows, columns, or both should be arranged.
#'        Default is "both". 
#' @param ledger_side the ledger side column in `.fu_allocation_template`. Default is "Ledger.side".
#' @param flow_aggregation_point the flow aggregation point column in `.fu_allocation_template`. Default is "Flow.aggregation.point".
#' @param ef_product the name of the final energy column in `.fu_allocation_template`. Default is "Ef.product".
#' @param machine the name of the machine column in `.fu_allocation_template`. Default is "Machine".
#' @param eu_product the name of the useful energy product column in `.fu_allocation_template`. Default is "Eu.product".
#' @param destination the name of the destination column in `.fu_allocation_template`. Default is "Destination".
#' @param unit the name of the unit in `.fu_allocation_template`. Default is "Unit".
#' @param fap_dest_order the desired order for the combination of `flow_aggregation_point` and `destination` columns. Default is `IEATools::fap_flow_iea_order`.
#' @param ef_product_order the desired order for final energy products in `.fu_allocation_template`. Default is "Ef.product".
#' @param quantity the name of the quantity column in `.fu_allocation_template`. Default is "Quantity".
#' @param maximum_values the name of the maximum value column `.fu_allocation_template`. Default is "Unit".
#' @param .temp_sort the name of a temporary column to be added to `.fu_allocation_template`. 
#'        Default is ".fap_flow".
#'        This column must not be present in `.fu_allocation_template`.
#' @param .clean_ef_product the name of a temporary column to be added to `.fu_allocation_template`. 
#'        Default is ".clean_ef_product".
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
                                               fap_dest_order = IEATools::fap_flows,
                                               ef_product_order = IEATools::products, 
                                               quantity = "Quantity",
                                               maximum_values = "Maximum.values", 
                                               .temp_sort = ".fap_flow", 
                                               .clean_ef_product = ".clean_Ef_product"){
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
    meta_cols <- out %>% 
      matsindf::everything_except(c(year_colnames, machine_and_product_columns, ef_product))
    # Adjust the columns in preparation for sorting.
    out <- out %>% 
      # De-specify the Ef.product column so it can be sorted.
      despecify_col(col = ef_product, despecified_col = .clean_ef_product) %>% 
      # Create a united Flow.aggregation.point_Flow column.
      tidyr::unite(col = !!as.name(.temp_sort), !!as.name(flow_aggregation_point), !!as.name(destination), sep = "_", remove = FALSE)
    # Ensure that no .fap_flow and no ef_products are NA at this point.
    assertthat::assert_that(!any(is.na(out[[.temp_sort]])))
    assertthat::assert_that(!any(is.na(out[[.clean_ef_product]])))
    # Convert .temp_sort and ef_product to factors so that they can be arranged (sorted) later.
    out <- out %>% 
      dplyr::mutate(
        !!as.name(.temp_sort) := factor(!!as.name(.temp_sort), levels = fap_dest_order),
        !!as.name(.clean_ef_product) := factor(!!as.name(.clean_ef_product), levels = ef_product_order)
      ) 
    # Ensure that we have not accidentally created NA values in the .temp_sort or ef_product columns.
    # NA values in either of these columns will occur when we do not have a complete set of factors 
    # in fap_flows or ef_products.
    na_temp_sort <- out %>% 
      dplyr::filter(is.na(!!as.name(.temp_sort)))
    na_ef_product <- out %>% 
      dplyr::filter(is.na(!!as.name(.clean_ef_product)))
    assertthat::assert_that(nrow(na_temp_sort) == 0)
    assertthat::assert_that(nrow(na_ef_product) == 0)
    out <- out %>% 
      dplyr::group_by(!!!meta_cols) %>% 
      dplyr::arrange(!!as.name(.temp_sort), !!as.name(.clean_ef_product), .by_group = TRUE) %>% 
      dplyr::mutate(
        # Eliminate temporary columns
        !!as.name(.temp_sort) := NULL, 
        !!as.name(.clean_ef_product) := NULL
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
#' * Rows are written in same order as IEA extended energy balance data.
#' * Columns are written in a reasonable order, namely that left-to-right order
#'   approximates flow through the energy conversion chain.
#'
#' @param .fu_allocation_template a data frame produced by `fu_allocation_template()`
#' @param path the file path into which the blank template file will be written. 
#'        Include both folder and file name. 
#'        If not present, the ".xlsx" extension is added.
#' @param ledger_side the name of the ledger side column in `.tidy_iea_df`. Default is "`Ledger.side`".
#' @param consumption the string identifier for consumption in the `ledger_side` column.  Default is "`Consumption`".
#' @param flow_aggregation_point the name of the flow aggregation point column in `.tidy_iea_df`. Default is "`Flow.aggregation.point`".
#' @param eiou the string identifier for energy industry own use in the `flow_aggregation_point` column. Default is "`Energy industry own use`".
#' @param fu_allocations_tab_name the name of the tab on which the template will be written. Default is "`FU Allocations`".
#' @param machine the name of the machine column in output. Default is "`Machine`"
#' @param eu_product the name of the useful energy product column in output. Default is "`Eu.product`".
#' @param quantity the name of the quantity column to be created on output. Default is "`Quantity`".
#' @param e_dot the name of the energy flow rate column in `.tidy_iea_df` and the name of the energy flow rate rows to be included in the Excel file that is written by this function.
#'        Default is "`E.dot`".
#' @param e_dot_perc the name of the energy flow rate percentage row to be included in the Excel file that is written by this function.
#'        Default is "`E.dot.perc`".
#' @param maximum_values the name of the maximum values column in output. Default is "`Maximum.values`".
#' @param header_row_font_color a hex string representing the font color for the header row in the Excel file that is written by this function.
#'        Default is "`#FFFFFF`", white.
#' @param header_row_shading_color a hex string representing the shading color for the header row in the Excel file that is written by this function.
#'        Default is "`#5A80B8`", medium blue.
#' @param energy_row_font_color_fd a hex string representing the font color for `e_dot` and `e_dot_perc` final demand rows in the Excel file that is written by this function.
#'        Default is "`#104273`", a dark blue color.
#' @param energy_row_shading_color_fd a hex string representing the shading color for `e_dot` and `e_dot_perc` final demand rows in the Excel file that is written by this function.
#'        Default is "`#104273`", a light blue color.
#' @param energy_row_font_color_eiou a hex string representing the font color for `e_dot` and `e_dot_perc` energy industry own use rows in the Excel file that is written by this function.
#'        Default is "`#104273`", a dark yellow color.
#' @param energy_row_shading_color_eiou a hex string representing the shading color for `e_dot` and `e_dot_perc` energy industry own use rows in the Excel file that is written by this function.
#'        Default is "`#104273`", a light yellow color.
#' @param dont_fill_shading_color a hex string representing the shading color for cells that don't require inputs. 
#'        Default is "`#A8A8A8`", a medium gray color.
#' @param overwrite_file a boolean that tells whether an existing file at `path` will be overwritten. Default is `FALSE`.
#'        If `path` already exists and `overwrite = FALSE`, an error is given.
#' @param n_allocation_rows the number of allocation rows to write for each final energy product. Default is `3`.
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
                                         fu_allocations_tab_name = "FU Allocations",
                                         machine = "Machine",
                                         eu_product = "Eu.product",
                                         quantity = "Quantity", 
                                         e_dot = "E.dot",
                                         e_dot_perc = paste(e_dot, "[%]"), 
                                         maximum_values = "Maximum.values",
                                         header_row_font_color = "#FFFFFF",
                                         header_row_shading_color = "#5A80B8",
                                         energy_row_font_color_fd = "#104273",
                                         energy_row_shading_color_fd = "#B8D8F5", 
                                         energy_row_font_color_eiou = "#918700",
                                         energy_row_shading_color_eiou = "#FCFCAB", 
                                         dont_fill_shading_color = "#A8A8A8",
                                         overwrite_file = FALSE,
                                         n_allocation_rows = 3,
                                         .rownum = ".rownum"){
  matsindf::verify_cols_missing(.fu_allocation_template, .rownum)
  
  # Create the workbook, add the worksheet, and stuff the temmplate into the worksheet
  fu_wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(fu_wb, fu_allocations_tab_name)
  openxlsx::writeData(fu_wb, fu_allocations_tab_name, .fu_allocation_template)
  
  # A function to identify some rows of the spreadsheet (e_rows means "energy rows")
  e_rows <- function(which_quantity = c(e_dot, e_dot_perc), which_type = c("fd", "eiou")){
    which_quantity <- match.arg(which_quantity)
    which_type <- match.arg(which_type)
    keep_rows <- .fu_allocation_template %>% 
      # Make a column of row numbers
      tibble::remove_rownames() %>% tibble::rownames_to_column(var = .rownum)
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
  
  # Apply color formatting style for the header row
  header_row_style <- openxlsx::createStyle(fontColour = header_row_font_color, fgFill = header_row_shading_color, textDecoration = c("BOLD"))
  openxlsx::addStyle(fu_wb, fu_allocations_tab_name, style = header_row_style, rows = 1, cols = 1:ncol(.fu_allocation_template), gridExpand = TRUE)

  # Apply color formatting style for energy and energy percentage rows
  energy_row_style_fd <- openxlsx::createStyle(fontColour = energy_row_font_color_fd, fgFill = energy_row_shading_color_fd)
  energy_row_style_eiou <- openxlsx::createStyle(fontColour = energy_row_font_color_eiou, fgFill = energy_row_shading_color_eiou)
  openxlsx::addStyle(fu_wb, fu_allocations_tab_name, style = energy_row_style_fd, rows = union(e_dot_rows_fd, e_dot_perc_rows_fd), cols = 1:ncol(.fu_allocation_template), gridExpand = TRUE)
  openxlsx::addStyle(fu_wb, fu_allocations_tab_name, style = energy_row_style_eiou, rows = union(e_dot_rows_eiou, e_dot_perc_rows_eiou), cols = 1:ncol(.fu_allocation_template), gridExpand = TRUE)
  
  # Apply shading for cells that don't need to be filled
  # First, tackle the cells in the Maximum.values column.
  dont_fill_style <- openxlsx::createStyle(fgFill = dont_fill_shading_color)
  openxlsx::addStyle(fu_wb, fu_allocations_tab_name, style = dont_fill_style, rows = c_rows_indices, cols = max_values_col_index, gridExpand = TRUE)
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
    openxlsx::addStyle(fu_wb, fu_allocations_tab_name, style = dont_fill_style, 
                       rows = gray_rows_for_year_col_index, cols = col_index, stack = TRUE)
  }
  
  # Set percentage format for numbers in the e_dot_perc and C rows.
  e_dot_perc_style <- openxlsx::createStyle(numFmt = "PERCENTAGE")
  openxlsx::addStyle(fu_wb, fu_allocations_tab_name, style = e_dot_perc_style, 
                     rows = c(e_dot_perc_rows_fd, e_dot_perc_rows_eiou), cols = c(max_values_col_index, year_cols_indices), gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(fu_wb, fu_allocations_tab_name, style = e_dot_perc_style, 
                     rows = c_rows_indices, cols = year_cols_indices, gridExpand = TRUE, stack = TRUE)
  
  # Set column widths to something intelligent
  openxlsx::setColWidths(fu_wb, fu_allocations_tab_name, cols = 1:ncol(.fu_allocation_template), widths = "auto")
  
  # Now save it!
  if (!endsWith(path, ".xlsx")) {
    path <- paste0(path, ".xlsx")
  }
  openxlsx::saveWorkbook(fu_wb, path, overwrite = overwrite_file)
  # And return the path
  return(path)
}


#' Load final-to-useful allocation data
#' 
#' When performing extending an energy conversion chain from useful energy to final energy, 
#' allocations of final energy consumption to useful energy categories are defined by the analyst.  
#' The Excel file at `path` contains those allocations.
#' 
#' A final-to-useful allocation template can be 
#' generated using `fu_allocation_template()` and `write_fu_allocation_template()`.
#' 
#' A filled example can be loaded with the default value of `path`.
#'
#' @param path the path from which final-to-useful allocation data will be loaded. Default is the path to allocation data supplied with this package.
#' @param fu_allocations_tab_name the tab in `path` that contains the final-to-useful allocation data. Default is "`FU Allocations`".
#'
#' @return the `fu_allocations_tab_name` tab in `path` as a data frame.
#' 
#' @export
#'
#' @examples
#' # Loads final-to-useful allocation data supplied with the package
#' load_fu_allocation_data()
load_fu_allocation_data <- function(path = sample_fu_allocation_table_path(), 
                                    fu_allocations_tab_name = "FU Allocations"){
  openxlsx::read.xlsx(path, sheet = fu_allocations_tab_name)
}


#' Complete an FU Allocation table
#' 
#' An FU (final-to-useful) Allocation table 
#' tells how final energy carriers are allocated to final-to-useful machines
#' in each final demand sector.
#' A template for an FU Allocation table can be created with 
#' `fu_allocation_template()`.
#' If the analyst does not know some FU allocations for a given country, 
#' this function can be used to build a complete FU allocation table
#' by supplying allocations from any number of exemplar countries.
#' 
#' `fu_allocation_table` is the FU Allocation table to be completed.
#' Any missing information is obtained from the FU Allocation tables of the exemplar countries,
#' provided in the `exemplar_fu_allocation_tables` argument.
#' Each exemplar table is interrogated in order, 
#' with data taken from the first exemplar that contains the needed information.
#' 
#' `tidy_specified_iea_data` supplies information about which data are needed.
#' The `tidy_specified_iea_data` data frame should be obtained from a call to `specify_all()`. 
#' 
#' If `fu_allocation_table` can't be completed (because not enough information is available in 
#' `exemplar_fu_allocation_tables`), an error is emitted.
#' 
#' @param fu_allocation_table The FU allocation table to be completed. 
#'                            This data frame is probably read by `load_fu_allocation_data()`.
#'                            If `NULL`, the table will be constructed exclusively from 
#'                            information available in the exemplar country tables.
#'                            Only one country is allowed in this data frame.
#' @param exemplar_fu_allocation_tables A list of FU Allocation tables, each probably created by `load_fu_allocation_data()`. 
#'                                      Note that each exemplar table must contain data for a single country only. 
#'                                      If more than one country is found, an error occurs.
#' @param tidy_specified_iea_data A data frame of specified IEA data in tidy format.
#' @param country,method,energy_type,last_stage,ledger_side,flow,product,unit,e_dot,year,flow_aggregation_point See `IEATools::ieacols`.
#' @param supply,consumption See `IEATools::ledger_sides`.
#' @param eiou See `IEATools::tfc_compar_flows`.
#' @param e_dot_perc,destination,machine,eu_product,ef_product,max_vals,quantity See `IEATools::template_cols`.
#' @param source The name of a column added to output that describes the source of the allocation values (the C values). 
#'               Default is "C_source".
#' @param .values The name of a values column created internally. Default is "values".
#'
#' @return A completed tidy data frame containing an FU Allocation table to replace argument `fu_allocation_table`.
#' 
#' @export
#'
#' @examples
complete_fu_allocation_table <- function(fu_allocation_table, 
                                         exemplar_fu_allocation_tables, 
                                         tidy_specified_iea_data, 
                                         country = IEATools::iea_cols$country, 
                                         method = IEATools::iea_cols$method,
                                         energy_type = IEATools::iea_cols$energy_type,
                                         last_stage = IEATools::iea_cols$last_stage,
                                         ledger_side = IEATools::iea_cols$ledger_side,
                                         flow = IEATools::iea_cols$flow,
                                         product = IEATools::iea_cols$product,
                                         unit = IEATools::iea_cols$unit,
                                         e_dot = IEATools::iea_cols$e_dot, 
                                         year = IEATools::iea_cols$year, 
                                         flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point, 
                                         supply = IEATools::ledger_sides$supply,
                                         consumption = IEATools::ledger_sides$consumption, 
                                         eiou = IEATools::tfc_compare_flows$energy_industry_own_use, 
                                         e_dot_perc = IEATools::template_cols$e_dot_perc,
                                         destination = IEATools::template_cols$destination, 
                                         machine = IEATools::template_cols$machine,
                                         eu_product = IEATools::template_cols$eu_product,
                                         ef_product = IEATools::template_cols$ef_product,
                                         max_vals = IEATools::template_cols$maximum_values, 
                                         quantity = IEATools::template_cols$quantity,
                                         source = IEATools::template_cols$c_source,
                                         .values = IEATools::template_cols$.values) {
  # Find all countries in fu_allocation_table
  country_to_complete <- fu_allocation_table %>% 
    magrittr::extract2(country) %>% 
    unique()
  assertthat::assert_that(length(country_to_complete) == 1, 
                          msg = glue::glue("Found more than one country to complete in complete_fu_allocation_table(): {glue::glue_collapse(country_to_complete, sep = ', ', last = ' and ')}"))
  
  # Figure out the year columns in the fu_allocation_table
  year_cols <- c(max_vals, year_cols(fu_allocation_table, return_names = TRUE) %>% as.character())
  
  # Get all the IEA data for these countries
  country_iea_data <- tidy_specified_iea_data %>% 
    dplyr::filter(.data[[country]] == country_to_complete)
  # Figure out the rows for which allocations are needed, based on the IEA data.
  allocation_rows_needed <- country_iea_data %>% 
    # Only need rows where we actually have energy to be allocated.
    dplyr::filter(e_dot != 0 & !is.na(e_dot)) %>% 
    # Only need rows that indicate final consumption (on the consumption side of the ledger) or
    # EIOU (on the supply side of the ledger).
    dplyr::filter(.data[[ledger_side]] == consumption | (.data[[ledger_side]] == supply & .data[[flow_aggregation_point]] == eiou)) %>% 
    # Keep only the columns of interest to the FU Allocation process
    dplyr::select(country, method, energy_type, last_stage, ledger_side, flow_aggregation_point, unit, product, flow, year) %>% 
    # Rename the flow column to be "destination" to match the corresponding column in the FU Analysis tables.
    dplyr::rename(
      "{destination}" := .data[[flow]], 
      "{ef_product}" := .data[[product]]
    )

  # Figure out the allocations that are available from fu_allocation_table.
  fu_allocation_data_available <- fu_allocations_available(fu_allocation_table, year = year, 
                                                           .values = .values, max_vals = max_vals, quantity = quantity, 
                                                           e_dot = e_dot, e_dot_perc = e_dot_perc, country = country, source = source)
  # fu_allocation_data_available <- fu_allocation_table %>% 
  #   # Pivot the FU allocation table to a tidy data frame.
  #   tidyr::pivot_longer(cols = year_cols, names_to = year, values_to = .values) %>% 
  #   # Get rid of rows we don't want.
  #   dplyr::filter(
  #     # Rows where the year column has "Maximum.values" in it.
  #     .data[[year]] != max_vals, 
  #     # Rows where the value column has NA are rows where we don't have allocation data.
  #     !is.na(.data[[.values]]), 
  #     # Rows where quantity is E.dot or E.dot [%] aren't allocation rows
  #     !.data[[quantity]] %in% c(e_dot, e_dot_perc)
  #   ) %>% 
  #   dplyr::mutate(
  #     "{year}" := as.numeric(.data[[year]]), 
  #     "{source}" := country_to_complete
  #   )
  # allocated_rows contains the rows of final energy consumption (from the IEA data) that have already been allocated.
  # We don't need to pull data from an exemplar for these rows.
  allocated_rows <- find_allocated_rows(fu_allocation_data_available, quantity, machine, eu_product, .values)
  
  # Look in exemplar_fu_allocation_tables for missing rows.
  # Search until we have information for each of the rows_to_get_elsewhere.
  n_exemplars <- ifelse(inherits(exemplar_fu_allocation_tables, "data.frame"), 1, length(exemplar_fu_allocation_tables))
  if (n_exemplars == 1) {
    exemplar_fu_allocation_tables <- list(exemplar_fu_allocation_tables)
  }
  
  for (i in 1:n_exemplars) {
    exemplar <- exemplar_fu_allocation_tables[[i]]
    # Figure out the rows of allocations that are missing and, therefore, 
    # must be obtained from the exemplar country FU Allocations.
    rows_to_get_elsewhere <- dplyr::anti_join(allocation_rows_needed, allocated_rows, by = colnames(allocation_rows_needed))
    
    if (nrow(rows_to_get_elsewhere) == 0) {
      # If we don't need to get any rows, we can stop looping now.
      break
    }
    
    # Make sure there is only 1 country in this exemplar.
    exemplar_country <- exemplar %>% magrittr::extract2(country) %>% unique()
    assertthat::assert_that(length(exemplar_country) == 1, 
                            msg = glue::glue("Found more than one country in exemplar: {glue::glue_collapse(exemplar_country, sep = ', ', last = ' and ')}"))
    # Figure out which missing rows this exemplar can contribute 
    exemplar_info_available <- exemplar %>% 
      # Pivot the FU allocation table to a tidy data frame.
      # tidy_fu_allocation_table() %>% 
      tidyr::pivot_longer(cols = year_cols, names_to = year, values_to = .values) %>% 
      # Get rid of rows we don't want.
      dplyr::filter(
        # Rows where the year column has "Maximum.values" in it.
        .data[[year]] != max_vals, 
        # Rows where the value column has NA are rows where we don't have allocation data.
        !is.na(.data[[.values]]), 
        # Rows where quantity is E.dot or E.dot [%] aren't allocation rows
        !.data[[quantity]] %in% c(e_dot, e_dot_perc)
      ) %>% 
      dplyr::mutate(
        "{year}" := as.numeric(.data[[year]]), 
        "{source}" := .data[[country]]
      ) %>% 
      # Get rid of the country column, because we don't want two country columns in the rows_to_use data frame.
      dplyr::select(!country)
    
    # exemplar_info_available <- fu_allocations_available(exemplar, year_)

    # We can't join by country or source, because the exemplar data frame doesn't have those columns.
    exemplar_rows_to_use <- dplyr::semi_join(exemplar_info_available, 
                                    rows_to_get_elsewhere, 
                                    by = colnames(rows_to_get_elsewhere) %>% 
                                      setdiff(country) %>% 
                                      setdiff(source)) %>% 
      dplyr::mutate(
        # Add the country column
        "{country}" := country_to_complete, 
      )
    # Join the exemplar_rows_to_use to fu_allocation_data_available
    fu_allocation_data_available <- fu_allocation_data_available %>% 
      dplyr::bind_rows(exemplar_rows_to_use)
    # allocated_rows <- fu_allocation_data_available %>% 
    #   # Now keep only the columns of interest to us.
    #   dplyr::select(!c(quantity, machine, eu_product, .values)) %>% 
    #   unique()
    allocated_rows <- find_allocated_rows(fu_allocation_data_available, quantity, machine, eu_product, .values)
  } # End of big for loop.
  
  # Spread (pivot_wider) to put years in columns before returning.
  fu_allocation_data_available %>% 
    tidyr::pivot_wider(names_from = year, values_from = .values)
    
}


#' Determine which final energy uses are covered in an FU Allocation table
#' 
#' This is convenience function that reduces duplicated code in `complete_fu_allocation_table()`.
#' As such, it is not exported.
#'
#' @param .fu_allocation_table The FU Allocation table from which allocations are to be determined
#' @param max_vals The name of maximum value rows in the `fu_allocation_table`.
#' @param year The Year column in a tidy version of the `fu_allocation_table`.
#' @param .values The name of the values column in a tidy versino of the `fu_allocation_table`.
#' @param quantity The name of the quantity column in the `fu_allocation_table`.
#' @param e_dot The name of the E.dot rows in the `fu_allocation_table`.
#' @param e_dot_perc The name of the E.dot percentage rows in the `fu_allocation_table`.
#' @param country The name of the Country column in the `fu_allocation_table`.
#' @param source The name of the C_source column in the tidy `fu_allocation_table`.
#'
#' @return A data frame containing final energy consummption that has been allocated in the `fu_allocation_table`
fu_allocations_available <- function(.fu_allocation_table, max_vals, year, .values, 
                                     quantity, e_dot, e_dot_perc, country, source) {
  year_columns <- c(max_vals, year_cols(.fu_allocation_table, return_names = TRUE) %>% 
                   as.character())
  .fu_allocation_table %>% 
    # Pivot the FU allocation table to a tidy data frame.
    # tidy_fu_allocation_table() %>% 
    tidyr::pivot_longer(cols = year_columns, names_to = year, values_to = .values) %>% 
    # Get rid of rows we don't want.
    dplyr::filter(
      # Rows where the year column has "Maximum.values" in it.
      .data[[year]] != max_vals, 
      # Rows where the value column has NA are rows where we don't have allocation data.
      !is.na(.data[[.values]]), 
      # Rows where quantity is E.dot or E.dot [%] aren't allocation rows
      !.data[[quantity]] %in% c(e_dot, e_dot_perc)
    ) %>% 
    dplyr::mutate(
      "{year}" := as.numeric(.data[[year]]), 
      "{source}" := .data[[country]]
    )
}

#' Find rows of final energy consumption already allocated to final-to-useful machines.
#' 
#' This is convenience function that reduces duplicated code in `complete_fu_allocation_table()`.
#' As such, it is not exported.
#'
#' @param fu_allocation_table A tidy final-to-useful allocation table.
#' @param quantity The name of the Quantity column.
#' @param machine The name of the Machine column.
#' @param eu_product The name of the Eu.product column.
#' @param .values The name of the .valuse column.
#'
#' @return A data frame containing unique rows of final energy that are allocated in `fu_allocation_table`.
find_allocated_rows <- function(fu_allocation_table, quantity, machine, eu_product, .values) {
  fu_allocation_table %>% 
    # Now keep only the columns of interest to us.
    dplyr::select(!c(quantity, machine, eu_product, .values)) %>% 
    unique()
}


#' Final-to-useful efficiency template
#' 
#' Using a filled final-to-useful allocation table, 
#' this function generates a blank template for final-to-useful machine efficiencies.
#' 
#' The template produced by this function includes a column (`e_dot_machine_perc_max`) 
#' that contains the percentage of all energy flowing into each final-to-useful machines,
#' thereby providing guidance to the analyst about the efficiencies that carry the most weight
#' for the entire analysis.
#' The template is sorted by metadata column groups then by the values in the `e_dot_machine_perc_max` column.
#' 
#' The analyst should complete the `eta_fu` and `phi_u` rows in this template.
#' `eta_fu` is the final-to-useful energy efficiency of the `machine`.
#' `phi_u` is the exergy-to-energy ratio for the useful product of `machine` as specified in the `eu_product` column.
#' 
#' Some of the `eta_fu` and `phi_u` values can be pre-calculated to assist the analyst, and
#' the template produced by this function does just that.
#' Any row specified with  `md` (mechanical drive) as its `eu_product` will have its values for `phi.u` automatically filled 
#' with `1`,
#' because mechanical drive is work, which is pure exergy.
#' Any row specified with `eu_product` being heat ("`*TH.xxx.u`") will have its values for `phi.u` automatically filled
#' with the appropriate Carnot efficiency,
#' `1 - T_0/xxx`, where `xxx` is the temperature of the heat and `u` are the units for that temperature (one of "`C`", "`K`", "`F`", or "`R`"
#' for ° Celsius, kelvin, ° Fahrenheit, or rankine, respectively).
#' For heat rows, the value of argument `T_0` (assumed to be in kelvin) 
#' is used to compute the Carnot efficiency for the `phi.u` rows.
#' The default value for `T_0` is `298.15 K` (`25 °C`).
#' 
#' Note that the rows labeled "`C_x [%]`" are formatted by default as percentage in the Excel file.
#' When read, values are ratios, not percentages. 
#' I.e., the values read by this function are in the range 0 <= x <= 1, not 0 <= x <= 100.
#' If any read values in the `c_perc` rows are outside of the range 0 <= x <= 1, an error is thrown.
#'
#' @param .fu_allocations a data frame containing a completed final-to-useful allocation template for final demand.
#'        This data frame can be obtained from the function `load_fu_allocation_data()`.
#' @param T_0 the dead state temperature (in kelvin) for calculation of heat exergy. Default is `298.15` kelvin.
#' @param sort_by how to sort rows of eta_fu template. 
#'        Options are (1) by "useful_energy_type" and (2) by "importance". 
#'        "useful_energy_type" sorts first by `md`, `light`, `ke`, and `heat`, 
#'        then by magnitude of energy flow into the machine.
#'        "importance" sorts by magnitude of energy flow into the machine only.
#'        Default is "useful_energy_type".
#' @param ledger_side the name of the ledger side column in `.fu_allocations`. Default is "`Ledger.side`".
#' @param flow_aggregation_point the name of the flow aggregation point column in `.fu_allocations`. Default is "`Flow.aggregation.point`".
#' @param ef_product the name of the final energy product column in `.fu_allocations`. Default is "`Ef.product`".
#' @param machine the name of the machine column in `.fu_allocations`. Default is "`Machine`".
#' @param eu_product the name of the useful energy product column in `.fu_allocations`. Default is "`Eu.product`".
#' @param md the name of the mechanical drive useful energy carrier in the `eu_product` column. Default is "`MD`".
#' @param light the name of the light useful energy carrier in the `eu_product` column. Default is "`Light`".
#' @param ke the name of the kinetic energy useful energy carrier in the `eu_product` column. Default is "`KE`".
#' @param heat the string that identifies heat useful energy carriers in the `eu_product` column.  Default is "`TH`" for "temperature heat". 
#' @param eta_fu the name for final-to-useful energy efficiencies to be entered by the analyst.  Default is "`eta.fu`". 
#' @param phi_u the name for the exergy-to-energy ratio for useful energy carriers to be entered by the analyst.  Default is "`phi.u`". 
#' @param destination the name for the destination for final and useful energy (an sector). Default is "`Destination`".
#' @param quantity the name for the quantity column in `.fu_allocations`. Default is "`Quantity`".
#' @param maximum_values the name for the maximum values column in `.fu_allocations`. Default is "`Maximum.values`".
#' @param perc a string that gives the units for percent. Default is "`[%]`".
#' @param e_dot the name for energy flow rates. Default is "`E.dot`".
#' @param c_ the string prefix for allocation variables. Default is "`C_`". 
#' @param c_perc the string for generic allocation variables in percentage terms. Default is "`C [%]`".
#' @param c_ratio the string for generic allocation variables in ratio terms. Default is "`C`".
#' @param .year the name of a temporary year column. Default is "`.year`".
#' @param year_for_maximum_values the year assumed for the `maximum_values` column. Default is `0`.
#' @param .value the name of a temporary value column. Default is "`.value`".
#' @param .row_order the name of a metadata column used internally for determining row order. Default is ".row_order".
#' @param e_dot_dest the name of a temporary column containing energy flows into a destination. Default is "`E.dot_dest`".
#' @param e_dot_machine the name of a temporary column containing energy flows into final-to-useful machines. Default is "`E.dot_machine`".
#' @param e_dot_machine_max the name of a temporary column containing maximum energy flows into final-to-useful machines. Default is "E.dot_machine_max".
#' @param e_dot_machine_tot the name of a temporary column containing sums of energy flows into a final-to-useful machines. Default is "`E.dot_machine_tot`".
#' @param e_dot_machine_perc the name of a temporary column percentages of total energy flow into machines.  Default is "`E.dot_machine [%]`".
#' @param e_dot_machine_max_perc the name for a column of maximum percentages (across all years) of total energy flow into machines.  Default is "`E.dot_machine_max [%]`".
#' @param non_energy The prefix for non-energy use in the `machine` column. Default is `IEATools::tfc_flows$non_energy_use`. 
#' @param non_energy_eff The efficiency for non-energy use. Non-zero so that we can swim upstream later.
#'                       Default is 1e-6, or 0.0001%.
#'
#' @return a data frame containing row-ordered blank template for final-to-useful machine efficiencies.
#' 
#' @export
#'
#' @examples
#' load_fu_allocation_data() %>% 
#'   eta_fu_template()
eta_fu_template <- function(.fu_allocations, 
                            T_0 = 298.15, 
                            sort_by = c("useful_energy_type", "importance"),
                            ledger_side = "Ledger.side",
                            flow_aggregation_point = "Flow.aggregation.point",
                            ef_product = "Ef.product",
                            machine = "Machine",
                            eu_product = "Eu.product", 
                            md = "MD", 
                            light = "Light",
                            ke = "KE", 
                            heat = "TH", 
                            eta_fu = "eta.fu", 
                            phi_u = "phi.u",
                            destination = "Destination",
                            quantity = "Quantity", 
                            maximum_values = "Maximum.values",
                            perc = "[%]",
                            e_dot = "E.dot",
                            c_ = "C_",
                            c_perc = paste(substr(c_, 1, 1), perc),
                            c_ratio = substr(c_, 1, 1),
                            .year = ".year",
                            year_for_maximum_values = 0,
                            .value = ".value", 
                            .row_order = ".row_order",
                            e_dot_dest = paste0(e_dot, "_dest"),
                            e_dot_machine = paste0(e_dot, "_machine"), 
                            e_dot_machine_max = paste0(e_dot_machine, "_max"),
                            e_dot_machine_tot = paste0(e_dot_machine, "_tot"), 
                            e_dot_machine_perc = paste(e_dot_machine, perc), 
                            e_dot_machine_max_perc = paste0(e_dot_machine, "_max", " ", perc), 
                            non_energy = IEATools::tfc_flows$non_energy_use, 
                            non_energy_eff = 1e-6){
  sort_by <- match.arg(sort_by)
  # Preliminary stuff
  # Ensure that several columns don't exist already in 
  # Grab the years of interest.
  year_colnames <- year_cols(.fu_allocations, return_names = TRUE)
  # Columns that are not years and are not other specific columns are metadata columns.
  # We group by these columns later.
  meta_cols <- .fu_allocations %>% 
    matsindf::everything_except(c(year_colnames, ledger_side, flow_aggregation_point, ef_product, machine, eu_product, 
                                  destination, quantity, maximum_values))  
  
  # To create the e_dot_machine_max_perc column, 
  # we need to calculate the energy flowing into each f-->u machine.
  # The first step is to isolate the E.dot rows
  e_dot_info <- .fu_allocations %>%
    dplyr::filter(!!as.name(quantity) == e_dot) %>%
    dplyr::select(-maximum_values, -machine, -eu_product, -quantity) %>%
    tidyr::gather(key = .year, value = !!as.name(e_dot_dest), year_colnames) %>% 
    dplyr::filter(!is.na(!!as.name(e_dot_dest)))
  # We also isolate the allocation (C) rows
  c_info <- .fu_allocations %>%
    dplyr::filter(startsWith(!!as.name(quantity), c_) & endsWith(!!as.name(quantity), perc)) %>%
    dplyr::filter(!is.na(!!as.name(machine))) %>%
    dplyr::mutate(
      !!as.name(quantity) := dplyr::case_when(
        startsWith(!!as.name(quantity), c_) & endsWith(!!as.name(quantity), perc) ~ c_perc,
        TRUE ~ !!as.name(quantity)
      )
    ) %>%
    dplyr::select(-maximum_values, -quantity) %>%
    tidyr::gather(key = .year, value = !!as.name(c_perc), year_colnames) %>%
    dplyr::filter(!is.na(!!as.name(c_perc)))
  # Verify that all C values are between 0 and 1, inclusive.
  assertthat::assert_that(all(c_info[[c_perc]] >= 0 & c_info[[c_perc]] <= 1), msg = "Not all C values are between 0 and 1 in eta_fu_template.")
  # Now that we know that every C value is between 0 and 1, inclusive, 
  # it is not really appropriate to call these "C [%]".  
  # So, rename the column.
  c_info <- c_info %>% 
    dplyr::rename(
      !!as.name(c_ratio) := !!as.name(c_perc)
    )
  # Now we join the E.dot and C values and calculate the energy flowing into each final-to-useful machine
  input_energy <- dplyr::full_join(c_info, e_dot_info, 
                                   by = matsindf::everything_except(e_dot_info, e_dot_dest, .symbols = FALSE)) %>% 
    # There may be cases where the analyst has filled a C value, but there is no corresponding e_dot_dest value.
    # Get rid of those rows.
    dplyr::filter(!is.na(!!as.name(e_dot_dest))) %>% 
    dplyr::mutate(
      # Calcualte the energy flow into each f-->u machine
      # for each row of the table
      # (each combination of Ef.product and Machine.
      !!as.name(e_dot_machine) := !!as.name(c_ratio) * !!as.name(e_dot_dest)
    ) %>% 
    # Group by the metadata columns, year, the Machine column, and the eu_product column, because we want to calculate the 
    # amount of energy going into each machine in each year for a given purpose.
    dplyr::group_by(!!!meta_cols, !!as.name(.year), !!as.name(machine), !!as.name(eu_product)) %>% 
    # Summarise to aggregate the energy going into each machine.
    dplyr::summarise(
      !!as.name(e_dot_machine) := sum(!!as.name(e_dot_machine))
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      !!as.name(.year) := as.numeric(!!as.name(.year))
    )
  # Calculate maximum input energy for each combination of metadata variables
  input_energy_max <- input_energy %>% 
    dplyr::filter(!is.na(!!as.name(e_dot_machine))) %>% 
    dplyr::group_by(!!!meta_cols, !!as.name(machine), !!as.name(eu_product)) %>% 
    dplyr::summarise(
      !!as.name(e_dot_machine_max) := max(!!as.name(e_dot_machine))
    ) %>% 
    dplyr::ungroup()
  # Calculate total input energy for each combination of metadata variables
  input_energy_totals <- input_energy %>% 
    # When the e_dot_machine column is NA for a row, the total is also NA. 
    # That is pretty unforgiving when calculating the totals.
    # So we remove NA columns when calculating totals. 
    dplyr::filter(!is.na(!!as.name(e_dot_machine))) %>% 
    dplyr::group_by(!!!meta_cols, !!as.name(.year)) %>% 
    dplyr::summarise(
      !!as.name(e_dot_machine_tot) := sum(!!as.name(e_dot_machine))
    ) %>% 
    dplyr::ungroup()
  # Now calculate fractions of all input energy entering each fu machine in each year
  input_energy_percs <- dplyr::full_join(input_energy, input_energy_totals, 
                                         by = matsindf::everything_except(input_energy_totals, e_dot_machine_tot, .symbols = FALSE)) %>% 
    dplyr::mutate(
      # Note that this row is called e_dot_machine_perc, but its values are ratios, not percentages.
      # The intent is that these values will be formatted as percentages when the Excel sheet is written.
      !!as.name(e_dot_machine_perc) := !!as.name(e_dot_machine) / !!as.name(e_dot_machine_tot), 
      # Eliminate columns we no longer need.
      !!as.name(e_dot_machine) := NULL,
      !!as.name(e_dot_machine_tot) := NULL
    )
  # Calculate the maximum percentage of all input energy across all years for each machine and eu_product combination
  input_energy_max_percs <- input_energy_percs %>% 
    dplyr::group_by(!!!meta_cols, !!as.name(machine), !!as.name(eu_product)) %>% 
    dplyr::summarise(
      !!as.name(e_dot_machine_max_perc) := max(!!as.name(e_dot_machine_perc))
    ) %>% 
    dplyr::ungroup()
   
  # Find the maxima across years for each combination of machine and eu_product
  Maxima <- dplyr::full_join(input_energy_max, input_energy_max_percs, by = matsindf::everything_except(input_energy, .year, e_dot_machine, .symbols = FALSE)) %>% 
    dplyr::rename(
      !!as.name(e_dot_machine) := !!as.name(e_dot_machine_max),
      !!as.name(e_dot_machine_perc) := !!as.name(e_dot_machine_max_perc)
    ) %>% 
    tidyr::gather(key = !!as.name(quantity), value = !!as.name(maximum_values), !!as.name(e_dot_machine), !!as.name(e_dot_machine_perc))
  
  # Calculate the row order of meta_cols, machine, and eu_product based on maxima across years.
  if (sort_by == "importance") {
    row_order <- Maxima %>% 
      dplyr::filter(!!as.name(quantity) == e_dot_machine_perc) %>% 
      dplyr::arrange(!!!meta_cols, dplyr::desc(!!as.name(maximum_values))) %>% 
      dplyr::mutate(
        !!as.name(.row_order) := paste(!!!meta_cols, !!as.name(machine), !!as.name(eu_product), sep = "+")
      ) %>% 
      magrittr::extract2(.row_order)
  } else if (sort_by == "useful_energy_type") {
    # We need to create a list of all the Eu.products.
    eu_prods <- input_energy[[eu_product]] %>% unique()
    # Then find all the ones that are heat useful energy, identified by the 2nd and third characters being "TH".
    heat_prods <- eu_prods[which(substring(eu_prods, 2) %>% startsWith(heat))]
    # Sort the heat products by temperature
    sorted_heat_indices <- heat_prods %>%
      extract_TK() %>%
      sort.int(decreasing = TRUE, index.return = TRUE) %>%
      magrittr::extract2("ix")
    heat_prods_sorted <- heat_prods[sorted_heat_indices]
    # There may be useful products that we don't know about. Put those at the end, sorted in alphabetical order..
    leftover_eu_prods <- sort(setdiff(eu_prods, c(md, ke, light, heat_prods)))
    # Now compile the order of Eu.products for this data frame.
    eu_product_sort_order <- c(md, ke, light, heat_prods_sorted, leftover_eu_prods)
    # Sort the Maxima data frame to get the order we want.
    row_order <- Maxima %>% 
      dplyr::filter(!!as.name(quantity) == e_dot_machine_perc) %>% 
      dplyr::mutate(
        !!as.name(eu_product) := factor(!!as.name(eu_product), levels = eu_product_sort_order)
      ) %>% 
      dplyr::arrange(!!!meta_cols, !!as.name(eu_product), dplyr::desc(!!as.name(maximum_values))) %>% 
      dplyr::mutate(
        !!as.name(.row_order) := paste(!!!meta_cols, !!as.name(machine), !!as.name(eu_product), sep = "+")
      ) %>% 
      magrittr::extract2(.row_order)
  }

  # Annual format, including blanks for eta_fu and phi_u
  Annual <- dplyr::full_join(input_energy, input_energy_percs, by = matsindf::everything_except(input_energy, e_dot_machine, .symbols = FALSE)) %>% 
    dplyr::mutate(
      # The eta_fu column should be blank, because the analyst will fill it later, 
      # except for the efficiency of non-energy use, which is small.
      !!as.name(eta_fu) := dplyr::case_when(
        startsWith(.data[[machine]], non_energy) ~ as.character(non_energy_eff),
        TRUE ~ ""
      ),
      # But the phi_u column can be pre-filled with some exergy/energy ratios.
      # The first attempt uses the carnot_efficiency function,
      # which converts the heat type (e.g., HTH.600.C)
      # to a temperature in kelvin and further
      # converts to a Carnot efficiency.
      # Some of the phi_u values will end up as NA, but that's OK.
      # We'll change them later.
      !!as.name(phi_u) := carnot_efficiency(!!as.name(eu_product), T_0 = T_0),
      # All of the mecahnical drive (md) rows will have NA for phi_u, but we know that it should be 1.
      !!as.name(phi_u) := dplyr::case_when(
        # We know that mechanical drive (md) has a phi_u value of 1.
        !!as.name(eu_product) == md ~ 1,
        # Give non-energy use phi = 1.
        startsWith(.data[[machine]], non_energy) ~ 1,
        TRUE ~ !!as.name(phi_u)
      )
    ) %>% 
    tidyr::gather(key = !!as.name(quantity), value = !!as.name(.value), 
                  !!as.name(e_dot_machine), !!as.name(e_dot_machine_perc), !!as.name(eta_fu), !!as.name(phi_u)) %>% 
    tidyr::spread(key = .year, value = .value)
  
  # Prepare the outgoing data frame.
  out <- dplyr::full_join(Maxima, Annual, by = matsindf::everything_except(Maxima, maximum_values, .symbols = FALSE)) %>% 
    dplyr::mutate(
      !!as.name(quantity) := factor(!!as.name(quantity), levels = c(e_dot_machine, e_dot_machine_perc, eta_fu, phi_u)), 
      !!as.name(.row_order) := paste(!!!meta_cols, !!as.name(machine), !!as.name(eu_product), sep = "+"), 
      !!as.name(.row_order) := factor(!!as.name(.row_order), levels = row_order)
    ) %>% 
    dplyr::arrange(!!!meta_cols, !!as.name(.row_order), !!as.name(quantity), !!as.name(maximum_values)) %>% 
    dplyr::mutate(
      !!as.name(.row_order) := NULL,
      # Remove the factorization of the quantity column
      !!as.name(quantity) := as.character(!!as.name(quantity))
    )
  
  # At this point the year columns are of type character.
  # But we want them to be numeric.
  # First, find the indices of the year columns.
  year_col_indices <- year_cols(out)
  # Change each year column to be type numeric.
  for (i in year_col_indices) {
    out[[i]] <- as.numeric(out[[i]])
  }
  
  # Check for errors. If there is a problem somewhere, 
  # we will obtain NA in the Machine column.
  assertthat::assert_that(!any(out[[machine]] %>% is.na()), msg = "At least one row of out has NA in the machine column in eta_fu_template.
                          Double-check Machine and Destination names.")
  
  return(out)
  

  
  
  # The rest of the commented code here is no longer needed. 
  # I'm keeping it for historical purposes as of 23 July 2019.   
  # By Sept 2019, it can probably be removed.
  
  # The following is something that works.
  # out <- dplyr::full_join(input_energy_max, input_energy_max_percs, by = matsindf::everything_except(input_energy, .year, e_dot_machine, .symbols = FALSE)) %>% 
  #   # tidyr::gather(key = !!as.name(".key"), value = !!as.name(maximum_values), !!as.name(e_dot_machine_max), !!as.name(e_dot_machine_max_perc)) %>% View
  #   dplyr::full_join(input_energy, by = matsindf::everything_except(input_energy, e_dot_machine, .year, .symbols = FALSE)) %>% 
  #   dplyr::full_join(input_energy_percs, by = matsindf::everything_except(input_energy, e_dot_machine, .symbols = FALSE)) %>% 
  #   dplyr::mutate(
  #     # The eta_fu column should be blank, because the analyst will fill it later.
  #     !!as.name(eta_fu) := "",
  #     # But the phi_u column can be pre-filled with some exergy/energy ratios.
  #     # The first attempt uses the carnot_efficiency function,
  #     # which converts the heat type (e.g., HTH.600.C)
  #     # to a temperature in kelvin and further
  #     # converts to a Carnot efficiency.
  #     # Some of the phi_u values will end up as NA, but that's OK.
  #     # We'll change them later.
  #     !!as.name(phi_u) := carnot_efficiency(!!as.name(eu_product), T_0 = T_0),
  #     # All of the mecahnical drive (md) rows will have NA for phi_u, but we know that it should be 1.
  #     !!as.name(phi_u) := dplyr::case_when(
  #       # We know that mechanical drive (md) has a phi_u value of 1.
  #       !!as.name(eu_product) == md ~ 1,
  #       TRUE ~ !!as.name(phi_u)
  #     )
  #   ) %>% 
  #   tidyr::gather(key = !!as.name(quantity), value = !!as.name(.value), 
  #                 !!as.name(e_dot_machine), !!as.name(e_dot_machine_perc), !!as.name(eta_fu), !!as.name(phi_u)) %>% 
  #   tidyr::spread(key = .year, value = .value) %>% View
  
  
  # prelim_out <- .fu_allocations %>% 
  #   # Keep only the columns of interest to us
  #   dplyr::select(!!!meta_cols, !!as.name(machine), !!as.name(eu_product)) %>% 
  #   # Eliminate rows where the analyst didn't fill any machines or products
  #   dplyr::filter(!is.na(!!as.name(machine)) & !is.na(!!as.name(eu_product))) %>% 
  #   unique() %>% 
  #   # Join with maximum percentages of energy input to the fu machines
  #   dplyr::left_join(input_energy_max_percs, by = matsindf::everything_except(input_energy_max_percs, e_dot_machine_max_perc, .symbols = FALSE)) %>% 
  #   # Add eta and phi columns (which will become rows in a moment)
  #   dplyr::mutate(
  #     # The eta_fu column should be blank, because the analyst will fill it later.
  #     !!as.name(eta_fu) := "",
  #     # But the phi_u column can be pre-filled with some exergy/energy ratios.
  #     # The first attempt uses the carnot_efficiency function,
  #     # which converts the heat type (e.g., HTH.600.C)
  #     # to a temperature in kelvin and further
  #     # converts to a Carnot efficiency.
  #     # Some of the phi_u values will end up as NA, but that's OK.
  #     # We'll change them later.
  #     !!as.name(phi_u) := carnot_efficiency(!!as.name(eu_product), T_0 = T_0),
  #     # All of the md rows will have NA for phi_u, but we know that it should be 1.
  #     !!as.name(phi_u) := dplyr::case_when(
  #       # We know that mechanical drive (md) has a phi_u value of 1.
  #       !!as.name(eu_product) == md ~ 1, 
  #       TRUE ~ !!as.name(phi_u)
  #     )
  #   ) %>% 
  #   # Now convert the eta_fu and phi_u columns into rows to be filled by the analyst
  #   tidyr::gather(key = !!as.name(quantity), value = !!as.name(.value), !!as.name(eta_fu), !!as.name(phi_u))
  # 
  # # Establish the sort order of Eu.products for the resulting file.
  # # We need to create a list of all the Eu.products.
  # eu_prods <- prelim_out[[eu_product]] %>% unique()
  # # Then find all the ones that are heat useful energy, identified by the 2nd and third characters being "TH".
  # heat_prods <- eu_prods[which(substring(eu_prods, 2) %>% startsWith(heat))]
  # # Sort the heat products by temperature
  # sorted_heat_indices <- heat_prods %>% 
  #   extract_TK() %>% 
  #   sort.int(decreasing = TRUE, index.return = TRUE) %>% 
  #   magrittr::extract2("ix")
  # heat_prods_sorted <- heat_prods[sorted_heat_indices]
  # # There may be useful products that we don't know about. Put those at the end, sorted in alphabetical order..
  # leftover_eu_prods <- sort(setdiff(eu_prods, c(md, light, heat_prods)))
  # # Now compile the order of Eu.products for this data frame.
  # eu_product_sort_order <- c(md, ke, light, heat_prods_sorted, leftover_eu_prods)
  # 
  # # We need one of the prelim_out data frames for each year
  # out <- lapply(year_colnames, function(yr){
  #   prelim_out %>% 
  #     dplyr::mutate(
  #       !!as.name(.year) := yr
  #     )
  # }) %>% 
  #   dplyr::bind_rows() %>% 
  #   tidyr::spread(key = !!as.name(.year), value = !!as.name(.value)) %>% 
  #   # Create factors with levels being the sort order.
  #   dplyr::mutate(
  #     !!as.name(eu_product) := factor(!!as.name(eu_product), levels = eu_product_sort_order),
  #     !!as.name(quantity) := factor(!!as.name(quantity), levels = c(eta_fu, phi_u))
  #   ) %>% 
  #   # And finally sort the rows and return the result.
  #   dplyr::arrange(!!!meta_cols, !!as.name(eu_product), dplyr::desc(!!as.name(e_dot_machine_max_perc)), !!as.name(machine), !!as.name(quantity))
  # # At this point the year columns are of type character. 
  # # But we want them to be numeric.
  # # First, find the indices of the year columns. 
  # year_col_indices <- year_cols(out)
  # # Change each year column to be type numeric.
  # for (i in year_col_indices) {
  #   out[[i]] <- as.numeric(out[[i]])
  # }
  # return(out)
  
  
  
  # Tidy <- input_energy %>% 
  #   # Eliminate rows where the analyst didn't fill any machines or products
  #   dplyr::filter(!is.na(!!as.name(machine)) & !is.na(!!as.name(eu_product))) %>% 
  #   unique() %>% 
  #   # Add input energy percentages
  #   dplyr::left_join(input_energy_percs, 
  #                    by = matsindf::everything_except(input_energy_percs, e_dot_machine_perc, .symbols = FALSE)) %>% 
  #   dplyr::mutate(
  #     # The eta_fu column should be blank, because the analyst will fill it later.
  #     !!as.name(eta_fu) := "",
  #     # But the phi_u column can be pre-filled with some exergy/energy ratios.
  #     # The first attempt uses the carnot_efficiency function,
  #     # which converts the heat type (e.g., HTH.600.C)
  #     # to a temperature in kelvin and further
  #     # converts to a Carnot efficiency.
  #     # Some of the phi_u values will end up as NA, but that's OK.
  #     # We'll change them later.
  #     !!as.name(phi_u) := carnot_efficiency(!!as.name(eu_product), T_0 = T_0),
  #     # All of the md rows will have NA for phi_u, but we know that it should be 1.
  #     !!as.name(phi_u) := dplyr::case_when(
  #       # We know that mechanical drive (md) has a phi_u value of 1.
  #       !!as.name(eu_product) == md ~ 1, 
  #       TRUE ~ !!as.name(phi_u)
  #     )
  #   ) %>% 
  #   dplyr::mutate(
  #     # Create columns for eta.fu and phi.u in the Tidy data frame
  #     !!as.name(eta_fu) := NA_real_,
  #     !!as.name(phi_u) := NA_real_
  #   )
  # 
  # # Calculate the maximum energy input and energy input percentage to each Machine across all years
  # Max <- Tidy %>% 
  #   matsindf::group_by_everything_except(.year, e_dot_machine, e_dot_machine_perc, eta_fu, phi_u) %>% 
  #   dplyr::summarise(
  #     !!as.name(e_dot_machine) := max(!!as.name(e_dot_machine)), 
  #     !!as.name(e_dot_machine_perc) := max(!!as.name(e_dot_machine_perc))
  #   ) %>% 
  #   tidyr::gather(key = !!as.name(quantity), value = !!as.name(.value), !!as.name(e_dot_machine), !!as.name(e_dot_machine_perc)) %>% 
  #   dplyr::mutate(
  #     # Set the year for max values to 0 so that the max values will appear as the earliest year.
  #     !!as.name(.year) := year_for_maximum_values #,
  #     # Need to make this into a character column so that we can add it to "" in the C_ columns later.
  #     # !!as.name(.value) := as.character(!!as.name(.value))
  #   )
  # 
  # prelim_out <- Tidy %>% 
  #   tidyr::gather(key = !!as.name(quantity), value = !!as.name(.value), !!as.name(e_dot_machine), !!as.name(e_dot_machine_perc), !!as.name(eta_fu), !!as.name(phi_u)) %>%
  #   dplyr::bind_rows(Max) %>% 
  #   # Set levels for the quantity column so that we can get the right order when we spread the years
  #   dplyr::mutate(
  #     !!as.name(quantity) := factor(!!as.name(quantity), levels = c(e_dot_machine_max, e_dot_machine_max_perc, e_dot_machine, e_dot_machine_perc, eta_fu, phi_u))
  #   ) %>% 
  #   # Now spread by years across the spreadsheet.
  #   tidyr::spread(key = .year, value = .value) %>% 
  #   dplyr::rename(
  #     # Rename the year 0 column
  #     !!as.name(maximum_values) := !!as.name(year_for_maximum_values)
  #   )
  # 
  # # Establish the sort order of Eu.products for the resulting file.
  # # We need to create a list of all the Eu.products.
  # eu_prods <- prelim_out[[eu_product]] %>% unique()
  # # Then find all the ones that are heat useful energy, identified by the 2nd and third characters being "TH".
  # heat_prods <- eu_prods[which(substring(eu_prods, 2) %>% startsWith(heat))]
  # # Sort the heat products by temperature
  # sorted_heat_indices <- heat_prods %>% 
  #   extract_TK() %>% 
  #   sort.int(decreasing = TRUE, index.return = TRUE) %>% 
  #   magrittr::extract2("ix")
  # heat_prods_sorted <- heat_prods[sorted_heat_indices]
  # # There may be useful products that we don't know about. Put those at the end, sorted in alphabetical order..
  # leftover_eu_prods <- sort(setdiff(eu_prods, c(md, light, heat_prods)))
  # # Now compile the order of Eu.products for this data frame.
  # eu_product_sort_order <- c(md, ke, light, heat_prods_sorted, leftover_eu_prods)
}


#' Write a final-to-useful machine efficiencies template to a file
#' 
#' The template should be created by `eta_fu_template()`.
#'
#' @param .eta_fu_template a template for final-to-useful energy efficiency values, generated by `eta_fu_template()`.
#' @param path the file path where the eta_fu template will be written
#' @param eta_fu_tab_name the name of the final-to-useful efficiency tab. Default is "FU etas".
#' @param overwrite_file a logical telling whether to overwrite a file, if it already exists. Default is `FALSE`.
#' @param overwrite_fu_eta_tab a logical telling whether to overwrite the final-to-useful efficiency tab, if it already exists. Default is `FALSE`.
#' @param eta_fu the name of the final-to-useful efficiency rows in `.eta_fu_template`. Default is "eta.fu".
#' @param e_dot_machine a string identifying energy flow into final-to-useful machines. Default is "E.dot_machine".
#' @param perc the string identifying percentage quantities. Default is "\[%\]".
#' @param e_dot_machine_perc a string identifying percentage of total final energy flowing into final-to-useful machines. Default is "E.dot_machine \[%\]".
#' @param maximum_values a string identifying the maximum values column in the outgoing template. Default is "Maximum.values".
#' @param header_row_font_color a hex string representing the font color for the header row in the Excel file that is written by this function.
#'        Default is "#FFFFFF", white.
#' @param header_row_shading_color a hex string representing the shading color for the header row in the Excel file that is written by this function.
#'        Default is "#5A80B8", medium blue.
#' @param e_dot_machine_row_font_color a hex string representing the font color for rows of energy flows into final-to-useful machines in the Excel file that is written by this function.
#'        Default is "#8C87A0", a dark purple color.
#' @param e_dot_machine_row_shading_color a hex string representing the shading color for rows of energy flows into final-to-useful machines in the Excel file that is written by this function.
#'        Default is "#5A80B8", medium purple color.
#' @param e_dot_machine_perc_row_font_color a hex string representing the font color for rows of percentage of energy flows into final-to-useful machines in the Excel file that is written by this function.
#'        Default is "#8C87A0", a dark purple color.
#' @param e_dot_machine_perc_row_shading_color a hex string representing the shading color for rows of percentage of energy flows into final-to-useful machines in the Excel file that is written by this function.
#'        Default is "#5A80B8", medium purple color.
#' @param eta_row_font_color a hex string representing the font color for `eta_fu` rows in the Excel file that is written by this function.
#'        Default is "#000000", black.
#' @param eta_row_shading_color a hex string representing the shading color for `eta_fu` rows in the Excel file that is written by this function.
#'        Default is "#FFFFFF", white.
#' @param phi_row_font_color a hex string representing the font color for `phi_u` rows in the Excel file that is written by this function.
#'        Default is "#000000", black.
#' @param phi_row_shading_color a hex string representing the shading color for `phi_u` rows in the Excel file that is written by this function.
#'        Default is "#FFFFFF", white.
#' @param blank_shading_color a hex string representing the shading color for blank cells in the `maximum_values` column.
#'        Default is "#808080".
#' @param quantity the name of the quantity column in `.eta_fu_template`. Default is "Quantity".
#' @param e_dot_machine_max_perc the name of the rows that give maximum percentages. Default is "E.dot_machine_max \[%\]".
#' @param .rownum the name of a temporary column containing row numbers. Default is ".rownum". 
#'
#' @return the `path` argument
#' 
#' @export
#'
#' @examples
#' f <- tempfile(fileext = ".xlsx")
#' load_fu_allocation_data() %>% 
#'   eta_fu_template() %>% 
#'   write_eta_fu_template(f)
#' if (file.exists(f)) {
#'   file.remove(f)
#' }
write_eta_fu_template <- function(.eta_fu_template,
                                  path, 
                                  eta_fu_tab_name = "FU etas", 
                                  overwrite_file = FALSE, 
                                  overwrite_fu_eta_tab = FALSE,
                                  eta_fu = "eta.fu",
                                  e_dot_machine = "E.dot_machine",
                                  perc = "[%]",
                                  e_dot_machine_perc = paste(e_dot_machine, perc), 
                                  maximum_values = "Maximum.values",
                                  header_row_font_color = "#FFFFFF",
                                  header_row_shading_color = "#5A80B8",
                                  e_dot_machine_row_font_color = "#8C87A0",
                                  e_dot_machine_row_shading_color = "#E3DFEB",
                                  e_dot_machine_perc_row_font_color = "#8C87A0",
                                  e_dot_machine_perc_row_shading_color = "#E3DFEB",
                                  eta_row_font_color = "#000000",
                                  eta_row_shading_color = "#FFFFFF",
                                  phi_row_font_color = "#000000",
                                  phi_row_shading_color = "#FFFFFF",
                                  blank_shading_color = "#808080",
                                  quantity = "Quantity",
                                  e_dot_machine_max_perc = "E.dot_machine_max [%]",
                                  .rownum = ".rownum"){
  # Ensure that path ends in .xlsx
  if (!endsWith(path, ".xlsx")) {
    path <- paste0(path, ".xlsx")
  }
  # Check if path and tab exist.
  eta_tab_exists <- FALSE
  if (file.exists(path)) {
    assertthat::assert_that(overwrite_file, msg = paste(path, "File already exists!"))
    eta_wb <- openxlsx::loadWorkbook(path)
    eta_tab_exists <- eta_fu_tab_name %in% openxlsx::sheets(eta_wb)
  } else {
    eta_wb <- openxlsx::createWorkbook()
  }
  if (eta_tab_exists) {
    assertthat::assert_that(overwrite_fu_eta_tab, msg = paste(eta_fu_tab_name, "already exists. Try overwrite_tab = TRUE?"))  
  } else {
    openxlsx::addWorksheet(eta_wb, eta_fu_tab_name)
  }
  openxlsx::writeData(eta_wb, .eta_fu_template, sheet = eta_fu_tab_name)
  
  # Add colors to rows
  
  # Start with the e_dot_machine rows
  e_dot_machine_row_indices <- .eta_fu_template %>% 
    # Make a column of row numbers
    tibble::remove_rownames() %>% tibble::rownames_to_column(var = .rownum) %>% 
    # Filter to keep only the eta rows
    dplyr::filter(!!as.name(quantity) == e_dot_machine) %>% 
    # These row numbers are for the data frame, but the row numbers in Excel are 1 more, 
    # because the column names are the first row for Excel but the column names are not a row for the data frame.
    dplyr::mutate(
      !!as.name(.rownum) := as.numeric(!!as.name(.rownum)),
      !!as.name(.rownum) := !!as.name(.rownum) + 1
    ) %>% 
    dplyr::select(!!as.name(.rownum)) %>% 
    unlist() %>% 
    unname()
  # Identify the e_dot_machine_perc rows
  e_dot_machine_perc_row_indices <- e_dot_machine_row_indices + 1
  # Identify the eta_fu rows
  eta_row_indices <- e_dot_machine_row_indices + 2
  # Identify the phi.u rows.
  phi_row_indices <- e_dot_machine_row_indices + 3
  # Identify the year columns.
  year_cols_indices <- year_cols(.eta_fu_template)
  # Identify the maximum_values column.
  maximum_values_col_index <- min(year_cols_indices) - 1
  
  # Add percentage formatting to the E.dot_machine [%] rows
  e_dot_perc_style <- openxlsx::createStyle(numFmt = "PERCENTAGE")
  openxlsx::addStyle(eta_wb, eta_fu_tab_name, style = e_dot_perc_style, 
                     rows = e_dot_machine_perc_row_indices, cols = c(maximum_values_col_index, year_cols_indices), gridExpand = TRUE)
  
  # Add percentage formatting to all eta_fu rows.
  eta_perc_style <- openxlsx::createStyle(numFmt = "PERCENTAGE")
  openxlsx::addStyle(eta_wb, eta_fu_tab_name, style = eta_perc_style, 
                     rows = eta_row_indices, cols = c(maximum_values_col_index, year_cols_indices), gridExpand = TRUE)
  
  # Add number formatting to all phi_u rows. We want to ensure several decimal places.
  phi_num_style <- openxlsx::createStyle(numFmt = "0.0#####")
  openxlsx::addStyle(eta_wb, eta_fu_tab_name, style = phi_num_style, 
                     rows = phi_row_indices, cols = year_cols_indices, gridExpand = TRUE)

  # Apply color formatting style for the header row
  header_row_style <- openxlsx::createStyle(fontColour = header_row_font_color, fgFill = header_row_shading_color, textDecoration = c("BOLD"))
  openxlsx::addStyle(eta_wb, eta_fu_tab_name, style = header_row_style, rows = 1, cols = 1:ncol(.eta_fu_template), gridExpand = TRUE, stack = TRUE)
  
  # Define e_dot_machine row style
  e_dot_machine_row_style <- openxlsx::createStyle(fontColour = e_dot_machine_row_font_color, fgFill = e_dot_machine_row_shading_color)
  # Apply the e_dot_machine row style at the correct locations
  openxlsx::addStyle(eta_wb, eta_fu_tab_name, style = e_dot_machine_row_style, 
                     rows = e_dot_machine_row_indices, cols = 1:ncol(.eta_fu_template), gridExpand = TRUE, stack = TRUE)
  
  # Define e_dot_machine_perc row style
  e_dot_machine_perc_row_style <- openxlsx::createStyle(fontColour = e_dot_machine_perc_row_font_color, fgFill = e_dot_machine_perc_row_shading_color)
  # Apply the e_dot_machine_perc row style at the correct locations
  openxlsx::addStyle(eta_wb, eta_fu_tab_name, style = e_dot_machine_perc_row_style, 
                     rows = e_dot_machine_perc_row_indices, cols = 1:ncol(.eta_fu_template), gridExpand = TRUE, stack = TRUE)
  
  # Define the eta row style
  eta_row_style <- openxlsx::createStyle(fontColour = eta_row_font_color, fgFill = eta_row_shading_color)
  # Apply the eta row style at the correct locations
  openxlsx::addStyle(eta_wb, eta_fu_tab_name, style = eta_row_style, 
                     rows = eta_row_indices, cols = 1:ncol(.eta_fu_template), gridExpand = TRUE, stack = TRUE)
  
  # Define the phi row style
  phi_row_style <- openxlsx::createStyle(fontColour = phi_row_font_color, fgFill = phi_row_shading_color)
  # Apply the phi row style at the correct locations
  openxlsx::addStyle(eta_wb, eta_fu_tab_name, style = phi_row_style, 
                     rows = phi_row_indices, cols = 1:ncol(.eta_fu_template), gridExpand = TRUE, stack = TRUE)
  
  # Define the blank cell style
  blank_cell_style <- openxlsx::createStyle(fgFill = blank_shading_color)
  # Apply gray color for eta_fu and phi_u rows in the maximum_values column
  openxlsx::addStyle(eta_wb, eta_fu_tab_name, style = blank_cell_style, 
                     rows = c(eta_row_indices, phi_row_indices), cols = maximum_values_col_index, gridExpand = TRUE, stack = TRUE)
  
  # Set the column widths to "auto" so data can be seen.
  openxlsx::setColWidths(eta_wb, eta_fu_tab_name, cols = 1:ncol(.eta_fu_template), widths = "auto")
  
  # Now save it
  openxlsx::saveWorkbook(eta_wb, file = path, overwrite = overwrite_file)
  # And return the path
  return(path)
}


#' Load final-to-useful machine efficiency data
#' 
#' When performing extending an energy conversion chain from useful energy to final energy, 
#' efficiencies of final-to-useful energy conversion machines are defined by the analyst.  
#' The Excel file at `path` contains those allocations.
#' 
#' A final-to-useful machine efficiencies template can be 
#' generated using `eta_fu_template()` and `write_eta_fu_template()`.
#' 
#' A filled example can be loaded with the default value of `path`.
#'
#' @param path the path from which final-to-useful machine efficiency data will be loaded. Default is the path to sample efficiency data supplied with this package.
#' @param eta_fu_tab_name the tab in `path` that contains the final-to-useful machine efficiency data. Default is "`FU etas`".
#'
#' @return the `eta_fu_tab_name` tab in `path` as a data frame.
#' 
#' @export
#'
#' @examples
#' load_eta_fu_data()
load_eta_fu_data <- function(path = sample_eta_fu_table_path(), 
                                    eta_fu_tab_name = "FU etas"){
  openxlsx::read.xlsx(path, sheet = eta_fu_tab_name)
}


