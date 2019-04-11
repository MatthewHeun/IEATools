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
#'
#' @return a data frame containing the blank template
#' 
#' @export
#' 
#' @examples
#' load_tidy_iea_df() %>% 
#'   specify_all() %>% 
#'   write_fu_templates()
write_fu_templates <- function(.tidy_iea_df, path){
  # TODO: fill this function
}

#' Create an energy industry own use final-to-useful template
#'
#' @param .tidy_iea_df 
#'
#' @return a data frame containing the EIOU template
#' 
#' @export
#'
#' @examples
#' load_tidy_iea_df() %>% 
#'   specify_all() %>% 
#'   eiou_fu_template()
eiou_fu_template <- function(.tidy_iea_df,
                             energy_type = "Energy.type",
                             energy = "E",
                             last_stage = "Last.stage",
                             final = "Final",
                             year = "Year",
                             ledger_side = "Ledger.side",
                             flow_aggregation_point = "Flow.aggregation.point", 
                             eiou = "Energy industry own use", 
                             tpes = "Total primary energy supply",
                             flow = "Flow", 
                             product = "Product",
                             destination = "Destination",
                             quantity = "Quantity",
                             e_dot = "E.dot",
                             e_dot_total = paste0(e_dot, ".total"),
                             e_dot_perc = paste0(e_dot, ".perc"),
                             maximum_values = "Maximum values",
                             year_for_maximum_values = 0,
                             ef_product = "Ef product",
                             allocation_var = "C_",
                             n_allocation_rows = 3,
                             machine = "Machine",
                             eu_product = "Eu product",
                             .value = ".value"){
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
  # Calculate total EIOU energy consumption for each year
  Totals_eiou <- .tidy_iea_df %>% 
    dplyr::filter(!!as.name(flow_aggregation_point) == eiou) %>% 
    matsindf::group_by_everything_except(ledger_side, flow_aggregation_point, flow, product, e_dot) %>% 
    dplyr::summarise(!!as.name(e_dot_total) := sum(!!as.name(e_dot)))
  # Calculate a Tidy EIOU data frame with percentages.
  Tidy_EIOU <- .tidy_iea_df %>% 
    # Extract the EIOU data
    dplyr::filter(!!as.name(flow_aggregation_point) == eiou) %>% 
    # Add the totals to the data frame in preparation for calculating percentages
    dplyr::left_join(Totals_eiou, by = matsindf::everything_except(.tidy_iea_df, ledger_side, flow_aggregation_point, flow, product, e_dot, .symbols = FALSE)) %>% 
    dplyr::mutate(
      # Calculate percentage of all EIOU for that country and year
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
  Max <- Tidy_EIOU %>% 
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
  c_cols <- paste0(allocation_var, 1:n_allocation_rows, " [%]")
  # Add allocation columns to the data frame
  for (i in 1:n_allocation_rows) {
    Tidy_EIOU <- Tidy_EIOU %>% 
      dplyr::mutate(
        !!as.name(c_cols[[i]]) := ""
      )
  }
  # Reshape the data frame into the format that we want for an Excel spreadsheet
  out <- Tidy_EIOU %>% 
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

# # Writes an efficiency table with blanks where the final to useful data are required.
# # The format of the file provides blanks for all information required to map final energy to useful energy.
# # To use this file, first run the analysis you're interested in, 
# # making sure that all years and countries of interest are in the IEAFoodFeedWithUVY data frame.
# 
# folder <- file.path("data-raw", "Blank-TFC-Table")
# 
# dir.create(folder, recursive = TRUE, showWarnings = FALSE)
# 
# IEAData <- AllIEAData %>% 
#   filter(Country %in% Countries) %>%
#   add_matnames_iea(energy = "E.ktoe") %>% 
#   rename(
#     Industry = Flow
#   )
# 
# Y <- IEAData %>% 
#   filter(UVY == "Y") %>% # Focus on final demand
#   select(Country, Year, Industry, Product, E.ktoe)
# 
# Totals <- Y %>% 
#   group_by(Country, Year) %>% 
#   summarise(totalE.ktoe = sum(E.ktoe))
# 
# Out <- full_join(Y, Totals, by = c("Country", "Year")) %>% 
#   # Add empty columns
#   mutate(
#     `Consumption fraction` = E.ktoe/totalE.ktoe,
#     Machine = "",
#     `Eu product` = "",
#     C = "",
#     eta = "",
#     phi_product = ""
#   ) %>%
#   select(-totalE.ktoe) %>% 
#   # Gather columns
#   gather(key = Quantity, value = Value, E.ktoe, `Consumption fraction`,
#          C, eta, phi_product) %>% 
#   spread(key = Year, value = Value, fill = "") %>% 
#   mutate(
#     Quantity = factor(Quantity, levels = c("E.ktoe", "Consumption fraction", "C", "eta", "phi_product"))
#   ) %>%
#   select(Country, Industry, Product, Machine, `Eu product`, Quantity, `1971`:`2013`) %>%
#   arrange(Country, Industry, Product, Machine, `Eu product`, Quantity) %>% 
#   filter(!Industry %in% c("Exports", "International aviation bunkers", "International marine bunkers", 
#                           "Losses", "Statistical differences", "Stock changes"))
#   
#   Out %>% 
#     write.csv(file = file.path(folder, "BlankUsefulWorkEfficienciesTable.csv"), row.names = FALSE)
