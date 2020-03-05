# This file contains functions that find the dividing lines between 
# various pieces of an IEA extended energy balance data set.



#' Find row numbers for adjacent entries
#' 
#' Given `.DF`, find the row numbers that contain the first instance 
#' in `col_name` of adjacent `entries`
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
adjacent_rownums <- function(.df, col_name, entries) {
  if (length(entries) != 2) {
    stop(paste("entries must have length 2 in adjacent_rownames. Was ", length(entries)))
  }
  col <- .df %>% 
    magrittr::extract2(col_name)
  prev <- head(col, -1)
  later <- tail(col, -1)
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
#' @param flow the name of the flow column
#' @param losses the name for losses in the flow column
#' @param iron_and_steel the name for the iron and steel industry in the flow column
#' @param mining_and_quarrying the name for the mining and quarrying industry in the flow column
#' @param tfc the name for total final consumption in the flow column
#' @param industry the name for industry in the flow column
#'
#' @return a pair of integers representing the rows that straddle the split between 
#'         the Supply and Consumption sides of the ledger
find_supply_consumption_split <- function(.ctry_tbl,
                                          flow,
                                          losses,
                                          iron_and_steel,
                                          mining_and_quarrying,
                                          tfc,
                                          industry) {
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
#' @param flow the name of the flow column
#' @param statistical_differences the name for statistical difference in the flow column
#' @param transformation_processes the name for transformation processes in the flow column
#' @param main_activity_producer_elect_plants the name for main activity producer electricity plants in the flow column
#'
#' @return a pair of integers representing the rows that straddle the split between 
#'         the TFC compare and Transformation processes flows.
#'         The second integer is the first row of Transformation processes.
find_transformation_start <- function(.ctry_tbl, 
                                      flow,
                                      statistical_differences,
                                      transformation_processes,
                                      main_activity_producer_electricity_plants) {
  # Make two attempts at this.
  # First attempt should work when aggregation rows (specifically, "Transformation processes") 
  # remain in the data frame.
  transformation_start <- adjacent_rownums(.ctry_tbl, flow, c(statistical_differences, transformation_processes))
  if (is.null(transformation_start)) {
    # Second attempt should work if the aggregation rows have already been removed from the data frame.
    transformation_start <- adjacent_rownums(.ctry_tbl, flow, c(statistical_differences, main_activity_producer_electricity_plants))
  }
  assertthat::assert_that(!is.null(transformation_start),
                          msg = "Could not find the rows that identify the beginning of transformation processes in find_transformation_start")
  return(transformation_start)
}




























