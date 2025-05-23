# This file contains functions that find the dividing lines between 
# various pieces of an IEA extended energy balance data set.



#' Find row numbers for adjacent entries
#' 
#' Given `.DF`, find the row numbers that contain the first instance 
#' in `col_name` of adjacent `entries`, 
#' this function finds the row number of the adjacent `entries`.
#' 
#' This function is helpful for finding boundaries between rows of a data frame
#' in preparation for defining grouping variables.
#'
#' @param .DF the data frame in which to search for `entries` in `col_name`.
#' @param col_name the column of `.DF` in which to search for consecutive `entries`.
#' @param entries a vector of length 2 representing the consecutive entries in  `col_name` for which to search.
#'
#' @return an integer vector of length 2 giving the rows for `entries[[1]]` and `entries[[2]]`, whose difference is 1.
#'         `NULL` if no consecutive `entries` are found in `col_name`.
#' 
#' @export
#'
#' @examples
#' DF <- data.frame(C1 = c("A", "B", "C"))
#' DF %>% adjacent_rownums("C1", c("A", "B"))
#' DF %>% adjacent_rownums("C1", c("B", "C"))
adjacent_rownums <- function(.DF, col_name, entries) {
  assertthat::assert_that(length(entries) == 2, 
                          msg = paste("entries must have length 2 in adjacent_rownames. Was ", length(entries)))
  col <- .DF %>% 
    magrittr::extract2(col_name)
  prev <- utils::head(col, -1)
  later <- utils::tail(col, -1)
  out <- which(prev == entries[[1]] & later == entries[[2]])
  if (length(out) == 0) {
    return(NULL)
  }
  if (length(out) != 1) {
    stop("multiple instances of adjacent entries in adjacent_rownums")
  }
  return(c(out, out+1))
}


#' Find the split point between Supply and Consumption
#' 
#' Given a country's IEA extended energy balance data frame,
#' find the row numbers that represent the transition between
#' the Supply and Consumption sides of the ledger.
#' 
#' Arguments should be supplied by the calling function.
#' 
#' An error is given if this function fails to find the location of the split between Supply and Consumption.
#'
#' @param .ctry_tbl a country's IEA data frame
#' @param flow See `IEATools::iea_cols`.
#' @param losses See `IEATools::tfc_compare_flows`.
#' @param iron_and_steel See `IEATools::manufactuiring_flows`.
#' @param mining_and_quarrying See `IEATools::industry_flows`. 
#' @param tfc See `IEATools::aggregation_flows`. 
#' @param industry See `IEATools::tfc_flows`.
#'
#' @return a pair of integers representing the rows that straddle the split between 
#'         the Supply and Consumption sides of the ledger
find_supply_consumption_split <- function(.ctry_tbl,
                                          flow = IEATools::iea_cols$flow,
                                          losses = IEATools::tfc_compare_flows$losses,
                                          iron_and_steel = IEATools::manufacturing_flows$iron_and_steel,
                                          mining_and_quarrying = IEATools::industry_flows$mining_and_quarrying,
                                          tfc = IEATools::aggregation_flows$total_final_consumption,
                                          industry = IEATools::tfc_flows$industry) {
  # Take three attempts.
  # This is the first attempt.
  # It will work if aggregation rows remain in .ctry_tbl
  # If it doesn't work, we'll get NULL for supply_consumption_split.
  supply_consumption_split <- adjacent_rownums(.ctry_tbl, flow, c(tfc, industry)) 
  if (is.null(supply_consumption_split)) {
    # This is the second attempt.
    # This second attempt works for the 2019 release of the extended energy balances data 
    # when aggregation rows have already been removed from the data frame.
    supply_consumption_split <- adjacent_rownums(.ctry_tbl, flow, c(losses, mining_and_quarrying))
  }
  if (is.null(supply_consumption_split)) {
    # This third attempt works for the 2018 release of the extended energy balances data 
    # when aggregation rows have already been removed from the data frame.
    supply_consumption_split <- adjacent_rownums(.ctry_tbl, flow, c(losses, iron_and_steel))
  }
  # If we failed, emit an error.
  assertthat::assert_that(!is.null(supply_consumption_split),
                          msg = "Could not find the rows that separate the Supply and Consumption sides of the ledger in find_supply_consumption_split")
  return(supply_consumption_split)
}


#' Find the point where Transformation processes begin
#' 
#' Given a country's IEA extended energy balance data frame,
#' find the row numbers that represent the transition between
#' the first TFC Compare section and Transformation processes.
#' 
#' Arguments should be supplied by the calling function.
#' 
#' An error is given if this function fails to find the location of the split between 
#' TFC compare and Transformation processes.
#'
#' @param .ctry_tbl a country's IEA data frame
#' @param flow See `IEATools::iea_cols`. 
#' @param transformation_processes,statistical_differences See `IEATools::tfc_compare_flows`.
#' @param mapep See `IEATools::transformation_processes`.
#'
#' @return a pair of integers representing the rows that straddle the split between 
#'         the TFC compare and Transformation processes flows.
#'         The second integer is the first row of Transformation processes.
find_transformation_start <- function(.ctry_tbl, 
                                      flow = IEATools::iea_cols$flow,
                                      transformation_processes = IEATools::tfc_compare_flows$transformation_processes,
                                      statistical_differences = IEATools::tfc_compare_flows$statistical_differences,
                                      mapep = IEATools::transformation_processes$main_activity_producer_electricity_plants) {
  # Make two attempts at this.
  # First attempt should work when aggregation rows (specifically, "Transformation processes") 
  # remain in the data frame.
  transformation_start <- adjacent_rownums(.ctry_tbl, flow, c(statistical_differences, transformation_processes))
  if (is.null(transformation_start)) {
    # Second attempt should work if the aggregation rows have already been removed from the data frame.
    transformation_start <- adjacent_rownums(.ctry_tbl, flow, c(statistical_differences, mapep))
  }
  assertthat::assert_that(!is.null(transformation_start),
                          msg = "Could not find the rows that identify the beginning of transformation processes in find_transformation_start")
  return(transformation_start)
}


#' Find the point where Transformation processes end
#' 
#' Given a country's IEA extended energy balance data frame,
#' find the row numbers that represent the transition between
#' the Transformation processes and Energy industry own use.
#' 
#' Arguments should be supplied by the calling function.
#' 
#' An error is given if this function fails to find the location of the split between 
#' Transformation processes and Energy industry own use. 
#'
#' @param .ctry_tbl a country's IEA data frame
#' @param flow See `IEATools::iea_cols$flow`.
#' @param non_specified See `IEATools::non_specified_flows`.
#' @param eiou See `IEATools::tfc_compare_flows`. 
#' @param coal_mines the name for coal mines in the flow column. Default is "Coal mines".
#'
#' @return a pair of integers representing the rows that straddle 
#'         the split between Transformation processes and Energy industry own use
#'         The first integer is the last row of Transformation processes.
find_transformation_end <- function(.ctry_tbl,
                                    flow = IEATools::iea_cols$flow,
                                    non_specified = IEATools::non_specified_flows$non_specified,
                                    eiou = IEATools::tfc_compare_flows$energy_industry_own_use,
                                    coal_mines = IEATools::industry_flows$coal_mines) {
  # Make two attempts at this.
  # First attempt assumes that aggregation rows are still present in the IEA data frame.
  transformation_end <- adjacent_rownums(.ctry_tbl, flow, c(non_specified, eiou))
  if (is.null(transformation_end)) {
    # Second attempt assumes that aggregation rows have been removed.
    transformation_end <- adjacent_rownums(.ctry_tbl, flow, c(non_specified, coal_mines))
  }
  assertthat::assert_that(!is.null(transformation_end),
                          msg = "Could not find the rows that identify the end of Transformation Process rows and the beginning of Energy industry own use in find_transformation_end")
  return(transformation_end)  
}


#' Find the point where Energy industry own use starts
#' 
#' Given a country's IEA extended energy balance data frame,
#' find the row numbers that represent the transition between
#' the Transformation processes and Energy industry own use.
#' This function simply calls `find_transformation_end()`, because
#' the dividing line between Transformation processes and Energy industry own use
#' is also the point where Energy industry own use starts.
#' 
#' Arguments should be supplied by the calling function.
#' 
#' An error is given if this function fails to find the location of the split between 
#' Transformation processes and Energy industry own use. 
#'
#' @param .ctry_tbl a country's IEA data frame
#' @param flow See `IEATools::iea_cols$flow`.
#' @param non_specified See `IEATools::non_specified_flows`. 
#' @param eiou See `IEATools::tfc_compare_flows`. 
#' @param coal_mines the name for coal mines in the flow column. Default is "Coal mines".
#'
#' @return a pair of integers representing the rows that straddle 
#'         the split between Transformation processes and Energy industry own use
#'         The second integer is the first row of Energy industry own use.
find_eiou_start <- function(.ctry_tbl,
                            flow = IEATools::iea_cols$flow,
                            non_specified = IEATools::non_specified_flows$non_specified,
                            eiou = IEATools::tfc_compare_flows$energy_industry_own_use,
                            coal_mines = IEATools::industry_flows$coal_mines) {
  find_transformation_end(.ctry_tbl, flow = flow, non_specified = non_specified,
                          eiou = eiou, coal_mines = coal_mines)
}


#' Find the point where Energy industry own use ends
#' 
#' Given a country's IEA extended energy balance data frame,
#' find the row numbers that represent the transition between
#' the Energy industry own use and Losses (a TFC compare flow).
#' 
#' Arguments should be supplied by the calling function.
#' 
#' An error is given if this function fails to find the location of the split between 
#' Transformation processes and Energy industry own use. 
#'
#' @param .ctry_tbl a country's IEA data frame
#' @param flow See `IEATools::iea_cols$flow`.
#' @param non_specified See `IEATools::non_specified_flows`. 
#' @param losses See `IEATools::tfc_compare_flows`.
#'
#' @return a pair of integers representing the rows that straddle 
#'         the split between Energy industry own use and Losses.
#'         The first integer is the last row of Energy industry own use.
find_eiou_end <- function(.ctry_tbl,
                          flow = IEATools::iea_cols$flow,
                          non_specified = IEATools::non_specified_flows$non_specified,
                          losses = IEATools::tfc_compare_flows$losses) {
  eiou_end <- adjacent_rownums(.ctry_tbl, flow, c(non_specified, losses))
  assertthat::assert_that(!is.null(eiou_end),
                          msg = "Could not find the rows that separate Non-specified from Losses in find_eiou_end")
  return(eiou_end)
}

